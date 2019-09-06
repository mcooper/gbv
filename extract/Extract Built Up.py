import ee
import pandas as pd
import numpy as np
import os
import time

ee.Initialize()

dat1 = pd.read_csv('~/GBV_geo.csv')
dat2 = pd.read_csv('~/mortalityblob/dhs/Mortality_geodata.csv')

dat = dat1[['latitude', 'longitude', 'code']].append(dat2[['latitude', 'longitude', 'code']]).drop_duplicates()

ghsl = ee.Image('JRC/GHSL/P2016/BUILT_LDSMT_GLOBE_V1').select('built')

res = []
bad = []
completed = []

dat = dat.dropna()

for coord in dat.reset_index().iterrows():
	print(coord[0]/dat.shape[0])
	
	if coord[1][2] in completed:
		continue
	
	geom = ee.Geometry.Point(coord[1][2], coord[1][1]).buffer(15000)
	
	try:
		raw = ghsl.reduceRegion(reducer=ee.Reducer.frequencyHistogram(), geometry=geom).getInfo()
		built = raw['built']
		built['code'] = coord[1][3]
		res.append(built)
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

final.to_csv('~/DHS_ghsl.csv', index=False)
