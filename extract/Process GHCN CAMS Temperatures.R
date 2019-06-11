setwd('G://My Drive/temperature/GHCN_CAMS')

library(ncdf4)
library(raster)
library(dplyr)
library(lubridate)

dat <- nc_open('air.mon.mean.nc')

air <- ncvar_get(dat, "air")
lat <- ncvar_get(dat, 'lat')
long <- ncvar_get(dat, 'lon')
time <- ncvar_get(dat, 'time')

time <- ymd('1800-01-01') + hours(time)

for (i in 1:length(time)){
  print(time[i])
  
  m <- air[c(361:720, 1:360), , i]
  r <- raster(t(m-273.15), xmn=-180, xmx=180, ymn=-90, ymx=90)
  
  proj4string(r) <- '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'
  
  writeRaster(r, paste0('GHCN_CAMS_', time[i], '.tif'), format='GTiff', overwrite=T)
}
