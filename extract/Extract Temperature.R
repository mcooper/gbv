library(gdalUtils)
library(raster)
library(dplyr)
library(lubridate)
library(zoo)
library(foreach)
library(doParallel)

hh <- read.csv('~/GBV_geo.csv') %>%
  filter(!is.na(latitude) | !is.na(longitude) & !(latitude==0 & longitude==0))

sp_hh <- SpatialPointsDataFrame(coords=hh[ , c('longitude', 'latitude')], data = hh)

r <- raster('~/climatedisk/GHCN_CAMS/GHCN_CAMS_1999-10-01.tif')

codes <- raster(matrix(seq(1, ncell(r)), nrow=nrow(r), ncol=ncol(r)), xmx=xmax(r), xmn=xmin(r), ymx=ymax(r), ymn=ymin(r))
codes[is.na(r)] <- NA

sp_hh@data$tmpcode <- extract(codes, sp_hh)

sp_hh <- sp_hh[!is.na(sp_hh$tmpcode), ]

rll <- rasterToPoints(codes) %>% data.frame
rll <- rll[rll$layer %in% sp_hh$tmpcode, ]

monthly_in_folder <- '/home/mattcoop/climatedisk/GHCN_CAMS/'
monthly_vrt_file <- extension(rasterTmpFile(), 'ivrt')
monthly_files <- list.files(monthly_in_folder, pattern='tif$')
gdalbuildvrt(paste0(monthly_in_folder, monthly_files), monthly_vrt_file, separate=TRUE, verbose=T, overwrite=TRUE)

cl <- makeCluster(10, outfile = '')
registerDoParallel(cl)

df <- foreach(i=1:nrow(rll), .packages=c('raster', 'lubridate', 'gdalUtils', 'SPEI', 'dplyr', 'zoo'), .combine=bind_rows) %dopar% {
  
  lat <- rll$y[i]
  lng <- rll$x[i]
  
  temps <- gdallocationinfo(monthly_vrt_file, lng, lat, wgs84=TRUE, valonly=TRUE) %>%
    as.numeric
  
  v008 <- seq(577, 1432)
  
  running_mean <- rollapply(temps, width=12, FUN=mean, fill=NA, align='right')
  
  pre2000_mean <- mean(running_mean[v008 < 1201], na.rm=T)
  pre2000_std <- sd(running_mean[v008 < 1201], na.rm=T)
  
  all_mean <- mean(running_mean, na.rm=T)
  all_std <- sd(running_mean, na.rm=T)
  
  pre2000_Zscore <- (running_mean - pre2000_mean)/pre2000_std
  all_Zscore <- (running_mean - all_mean)/all_std
  
  new <- data.frame(v008, running_mean, pre2000_Zscore, all_Zscore, tmpcode=rll$layer[i])
  
  new <- merge(sp_hh@data, new, all.x=F, all.y=F) 
  
  cat(i, round(i/nrow(rll)*100, 4), 'percent done\n')
  
  new
}

stopCluster(cl)