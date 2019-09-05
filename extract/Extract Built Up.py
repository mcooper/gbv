import ee
import pandas as pd
import numpy as np

ee.Initialize()

dat = pd.read_csv('~/GBV_geo.csv')
dat[['year']] = 1900 + ((dat[['v008']] - 1)/12).apply(np.floor)

ghsl = ee.Image('JRC/GHSL/P2016/BUILT_LDSMT_GLOBE_V1').select('built')

ghslnone = ghsl.eq(2)
ghsl1990 = ghsl.gte(5)
ghsl2000 = ghsl.gte(4)
ghsl2014 = ghsl.gte(3)

ghsl1990dat = []
ghsl2000dat = []
ghsl2014dat = []
ghslnonedat = []

ix = list(range(dat.shape[0]))
n = 5000
batches = [ix[i * n:(i + 1) * n] for i in range((len(ix) + n - 1) // n )] 

for batch in batches:
    print(batches.index(batch)/len(batches))
    
    allpoints = []
    for row in dat.ix[batch].iterrows():
        geom = ee.Geometry.Point(row[1][1], row[1][0]).buffer(10000)
        feat = ee.Feature(geom, {'code': row[1][2], 'v008': row[1][3]})
        allpoints.append(feat)
    
    fc = ee.FeatureCollection(allpoints)
    
    ghsl1990sel = ghsl1990.reduceRegions(reducer=ee.Reducer.sum(), collection=fc).getInfo()
    ghsl2000sel = ghsl2000.reduceRegions(reducer=ee.Reducer.sum(), collection=fc).getInfo()
    ghsl2014sel = ghsl2014.reduceRegions(reducer=ee.Reducer.sum(), collection=fc).getInfo()
    ghslnonesel = ghslnone.reduceRegions(reducer=ee.Reducer.sum(), collection=fc).getInfo()
    
    ghsl1990dat = ghsl1990dat + ghsl1990sel['features']
    ghsl2000dat = ghsl2000dat + ghsl2000sel['features']
    ghsl2014dat = ghsl2014dat + ghsl2014sel['features']
    ghslnonedat = ghslnonedat + ghslnonesel['features']

def processResults(reslist, var):
    newdf = pd.DataFrame({})
    for d in reslist:
        newdf = pd.concat([newdf, pd.DataFrame(d['properties'], index=[0])])
    newdf = newdf.rename(columns={"sum": var})
    return(newdf)

ghsl1990proc = processResults(ghsl1990dat, '1990')
ghsl2000proc = processResults(ghsl2000dat, '2000')
ghsl2014proc = processResults(ghsl2014dat, '2014')
ghslnoneproc = processResults(ghslnonedat, 'none')




