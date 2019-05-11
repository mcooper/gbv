library(raster)
library(lubridate)
library(doParallel)

################################
#Process temp data
################################

# data in http://hydrology.princeton.edu/data/pgf/v3/0.25deg/daily/
# Used by Kat Grace to look at birthweight: https://doi.org/10.1016/j.gloenvcha.2015.06.010 she has a good summary of the methods there
# It originally came from this paper: https://journals.ametsoc.org/doi/abs/10.1175/JCLI3790.1
# It looks like they took reanalysis products, then resampled to fine resolution based on elevation, then adjusted to fit CRU
# In Kelvin

#This was done on a cloud computer
setwd('climatedisk')

fs <- list.files(pattern = 'tmax.*nc$')

cl <- makeCluster(4)
registerDoParallel(cl)

foreach(f=fs, .packages=c('raster', 'ncdf4', 'lubridate')) %dopar% {

  print(f)
  
  r <- stack(f)

  year <- substr(f, 12, 15)
  
  indices <- 1:(dim(r)[3])
    
  dates <- ymd(paste0(year, '-1-1')) + days(indices - 1)
  
  for (i in indices){
    
    out <- r[[i]]
    
    out <- crop(out, extent(0, 360, -60, 90))
    out <- rotate(out)  
    
    name <- paste0('tmax', dates[i], '.tif')
    print(name)
    
    writeRaster(out, name, format='GTiff', overwrite=T)
  }
}


