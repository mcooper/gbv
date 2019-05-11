library(raster)
library(lubridate)

################################
#Process temp data
################################

# data in http://hydrology.princeton.edu/data/pgf/v3/0.25deg/daily/
# Used by Kat Grace to look at birthweight: https://doi.org/10.1016/j.gloenvcha.2015.06.010 she has a good summary of the methods there
# It originally came from this paper: https://journals.ametsoc.org/doi/abs/10.1175/JCLI3790.1
# It looks like they took reanalysis products, then resampled to fine resolution based on elevation, then adjusted to fit CRU
# In Kelvin

#This was done on a cloud computer

setwd('precip')

library(foreach)
library(doParallel)

cl <- makeCluster(7, outfile = '')
registerDoParallel(cl)
foreach(f=list.files(pattern = '.nc$'), .packages=c('raster', 'lubridate')) %dopar% {

  print(f)
  
  r <- stack(f)
  
  val <- substr(f, 1, 4)

  year <- substr(f, 12, 15)
  
  indices <- ymd(paste0(year, '-1-1')) + days(0:(dim(r)[3] - 1))
  
  for (i in 1:12){
    
    sel <- r[[which(month(indices)==i)]]
    out <- calc(sel, fun=mean)
    
    out <- crop(out, extent(0, 360, -60, 90))
    out <- rotate(out)  
    
    name <- paste0(val, year, substr(100 + i, 2, 3), '.tif')
    print(name)
    
    writeRaster(out, name, format='GTiff', overwrite=T)
  }
}

stopCluster(cl)

