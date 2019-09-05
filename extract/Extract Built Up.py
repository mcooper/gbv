import ee
import pandas as pd
import numpy as np
import os

ee.Initialize()

dat1 = pd.read_csv('~/GBV_geo.csv')
dat2 = pd.read_csv('~/mortalityblob/dhs/Mortality_geodata.csv')

dat = dat1[['latitude', 'longitude', 'code']].append(dat2[['latitude', 'longitude', 'code']]).drop_duplicates()

ghsl = ee.Image('JRC/GHSL/P2016/BUILT_LDSMT_GLOBE_V1').select('built')

res = []
bad = []

for coord in dat.reset_index().iterrows():
	print(coord[0]/dat.shape[0])
	
	geom = ee.Geometry.Point(coord[1][1], coord[1][0]).buffer(15000)
	
	try:
		raw = ghsl.reduceRegion(reducer=ee.Reducer.frequencyHistogram(), geometry=geom).getInfo()
		built = raw['built']
		built['code'] = coord[1][2]
		res.append(built)
	except:
		bad.append(coord[1][2])
	
	if len(bad) > 1000:
		os.system('~/telegram.sh "Too many Bad Coords"')
		break


os.system('~/telegram.sh "Done running EE"')



