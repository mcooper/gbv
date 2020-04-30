import ee
import pandas as pd
import numpy as np
import os
import time

ee.Initialize()

buffer = 5000

dat = pd.read_csv('~/mortalityblob/dhs/GBV_geo.csv')

ttc2015img = ee.Image("Oxford/MAP/accessibility_to_cities_2015_v1_0")
ttc2000img = ee.Image("users/mwcoopr/time_to_cities2000")

res = []
bad = []
completed = []

dat = dat.dropna()

alldf = []
for chunk in np.array_split(dat, 100):
	print(chunk.index[0]/dat.shape[0])
	allgeom = []
	for coord in chunk.iterrows():
		geom = ee.Geometry.Point(coord[1]['longitude'], coord[1]['latitude']).buffer(buffer)
		allgeom.append(geom)
	
	allgeomfc = ee.FeatureCollection(allgeom)
	
	ttc2000res = ttc2000img.reduceRegions(reducer=ee.Reducer.mean(), collection=allgeomfc).getInfo()
	ttc2015res = ttc2015img.reduceRegions(reducer=ee.Reducer.mean(), collection=allgeomfc).getInfo()
	
	r2000 = [x['properties']['mean'] if 'mean' in x['properties'].keys() else np.nan for x in ttc2000res['features']]
	r2015 = [x['properties']['mean'] if 'mean' in x['properties'].keys() else np.nan for x in ttc2015res['features']]
	
	df = pd.DataFrame({'ttc2000': r2000, 'ttc2015': r2015, 'code': chunk['code']})
	
	alldf.append(df)

final = pd.concat(alldf)
final.drop_duplicates().to_csv('~/mortalityblob/dhs/GBV_ttc.csv', index=False)


	try:
		raw = ttc2000img.reduceRegion(reducer=ee.Reducer.mean(),geometry=geom).getInfo()
		ttc2000 = raw['b1']
		
		raw = ttc2015img.reduceRegion(reducer=ee.Reducer.mean(),geometry=geom).getInfo()
		ttc2015 = raw['accessibility']
		
		code = coord[1][3]
		res.append({'ttc2000': ttc2000, 'ttc2015': ttc2015, 'code': code})
		completed.append(coord[1][3])
	except Exception as e:
		bad.append(coord)
		os.system('~/telegram.sh "Error ' + str(e) + '"')
		time.sleep(60*5)
	
	if len(bad) > 100:
		os.system('~/telegram.sh "Too many Bad Coords"')
		break

os.system('~/telegram.sh "Done running EE"')

final = pd.DataFrame(res)
final.drop_duplicates().to_csv('~/GBV_ttc.csv', index=False)
