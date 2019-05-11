library(gdalUtils)
library(raster)
library(dplyr)
library(SPEI)
library(lubridate)
library(zoo)
library(foreach)
library(doParallel)

hh <- read.csv('Coords&Dates.csv')

sp_hh <- SpatialPointsDataFrame(coords=hh[ c('longitude', 'latitude')], data = hh)

r <- raster('climatedisk/chirps-v2.0.1981.01.tif')
codes <- raster(matrix(seq(1, ncell(r)), nrow=nrow(r), ncol=ncol(r)), xmx=xmax(r), xmn=xmin(r), ymx=ymax(r), ymn=ymin(r))
codes[r==-9999] <- NA

sp_hh@data$tmpcode <- extract(codes, sp_hh)

#Deal with points near a coast, coming up NA
spna <- sp_hh[is.na(sp_hh@data$tmpcode), ]
badcoords <- spna@coords

#Note: This line only works when codes is in memory, so need to be on a medium or big machine
tmpcode <- apply(X = badcoords, MARGIN = 1, FUN = function(xy) codes@data@values[which.min(replace(distanceFromPoints(codes, xy), is.na(codes), NA))])

sp_hh@data$tmpcode[is.na(sp_hh@data$tmpcode)] <- tmpcode

rll <- rasterToPoints(codes) %>% data.frame
rll <- rll[rll$layer %in% sp_hh$tmpcode, ]

#Read in precip data
precip_in_folder <- 'climatedisk/'
precip_vrt_file <- extension(rasterTmpFile(), 'ivrt')
precip_files <- list.files(precip_in_folder, pattern='chirps.*tif$')[1:432]
gdalbuildvrt(paste0(precip_in_folder, precip_files), precip_vrt_file, separate=TRUE, verbose=T, overwrite=TRUE)

#Read in tmax data
tmax_in_folder <- 'climatedisk/'
tmax_vrt_file <- extension(rasterTmpFile(), 'ivrt')
tmax_files <- list.files(tmax_in_folder, pattern='^tmax.{7}tif$')
gdalbuildvrt(paste0(tmax_in_folder, tmax_files), tmax_vrt_file, separate=TRUE, verbose=T, overwrite=TRUE)

#Read in tmin data
tmin_in_folder <- 'climatedisk/'
tmin_vrt_file <- extension(rasterTmpFile(), 'ivrt')
tmin_files <- list.files(tmin_in_folder, pattern='^tmin.{7}tif$')
gdalbuildvrt(paste0(tmin_in_folder, tmin_files), tmin_vrt_file, separate=TRUE, verbose=T, overwrite=TRUE)

cl <- makeCluster(31, outfile = '')
registerDoParallel(cl)

df <- foreach(n=1:nrow(rll), .packages=c('raster', 'lubridate', 'gdalUtils', 'SPEI', 'dplyr', 'zoo'), .combine=bind_rows) %dopar% {

  precip <- gdallocationinfo(precip_vrt_file, rll$x[n], rll$y[n], wgs84=TRUE, valonly=TRUE) %>%
    as.numeric
  
  tmax <- gdallocationinfo(tmax_vrt_file, rll$x[n], rll$y[n], wgs84=TRUE, valonly=TRUE) %>%
    as.numeric
  
  tmin <- gdallocationinfo(tmin_vrt_file, rll$x[n], rll$y[n], wgs84=TRUE, valonly=TRUE) %>%
    as.numeric
  
  PET <- hargreaves(tmin-273.15, tmax-273.15, lat=rll$y[n], Pre=precip) %>%
    as.vector
  
  s <- precip - PET
  
  interview <- data.frame(tmpcode=rll$layer[n],
                          month=month(seq(ymd('1981-01-01'), ymd('2016-12-01'), by='1 month')),
                          year=year(seq(ymd('1981-01-01'), ymd('2016-12-01'), by='1 month')),
                          spei12=as.numeric(spei(s, 12, na.rm=TRUE)$fitted),
                          spei24=as.numeric(spei(s, 24, na.rm=TRUE)$fitted),
                          spei36=as.numeric(spei(s, 36, na.rm=TRUE)$fitted),
                          spei48=as.numeric(spei(s, 48, na.rm=TRUE)$fitted),
                          spi12=as.numeric(spi(precip, 12, na.rm=TRUE)$fitted),
                          spi24=as.numeric(spi(precip, 24, na.rm=TRUE)$fitted),
                          spi36=as.numeric(spi(precip, 36, na.rm=TRUE)$fitted),
                          spi48=as.numeric(spi(precip, 48, na.rm=TRUE)$fitted))
  
  meanannual <- data.frame(tmpcode=rll$layer[n],
                           mean_annual_precip=mean(precip, na.rm=T)*12)
  
  sel <- sp_hh@data[sp_hh@data$tmpcode == rll$layer[n], ]
  sel <- Reduce(function(x, y){merge(x,y,all.x=T,all.y=F)}, list(sel, interview, meanannual))
  cat(n, round(n/nrow(rll)*100, 4), 'percent done\n')
  sel
}

df$tmpcode <- NULL

write.csv(df, 'GBV_Precip_Indices.csv', row.names=F)