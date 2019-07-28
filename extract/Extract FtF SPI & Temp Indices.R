library(gdalUtils)
library(raster)
library(dplyr)
library(SPEI)
library(lubridate)
library(zoo)
library(foreach)
library(doParallel)

setwd('G://My Drive/')

dat <- read.csv('Feed the Future/GBV_FTF.csv')

sp <- SpatialPointsDataFrame(coords=dat[ c('longitude', 'latitude')], data = dat)

r <- raster('CHIRPS/Monthly/chirps-v2.0.1981.01.tif')
codes <- raster(matrix(seq(1, ncell(r)), nrow=nrow(r), ncol=ncol(r)), xmx=xmax(r), xmn=xmin(r), ymx=ymax(r), ymn=ymin(r))
codes[r==-9999] <- NA

sp@data$tmpcode <- extract(codes, sp)

#Deal with points near a coast, coming up NA
spna <- sp[is.na(sp@data$tmpcode) , ]
spna$tmpcode <- NULL
badcoords <- unique(spna@coords)
tmpcode <- apply(X = badcoords, MARGIN = 1, FUN = function(xy) codes@data@values[which.min(replace(distanceFromPoints(codes, xy), is.na(codes), NA))])
badcoords <- cbind.data.frame(badcoords, tmpcode)
spna <- merge(spna@data, badcoords)
spnew <- bind_rows(spna, sp@data[!is.na(sp@data$tmpcode), ])

rll <- rasterToPoints(codes) %>%
   data.frame %>%
   dplyr::select(x, y, tmpcode=layer) %>%
   filter(tmpcode %in% spnew$tmpcode)

#Read in precip data
precip_vrt_file <- extension(rasterTmpFile(), 'ivrt')
precip_files <- list.files('chirps/Monthly', pattern='^chirps.*tif$', full.names = T)[1:432]
gdalbuildvrt(paste0('./', precip_files), precip_vrt_file, separate=TRUE, verbose=T, overwrite=TRUE)

#Read in tmax data
tmax_vrt_file <- extension(rasterTmpFile(), 'ivrt')
tmax_files <- list.files('temperature', pattern='^tmax.......tif$', full.names = T)
gdalbuildvrt(paste0('./', tmax_files), tmax_vrt_file, separate=TRUE, verbose=T, overwrite=TRUE)

#Read in tmin data
tmin_vrt_file <- extension(rasterTmpFile(), 'ivrt')
tmin_files <- list.files('temperature', pattern='^tmin.......tif$', full.names = T)
gdalbuildvrt(paste0('./', tmin_files), tmin_vrt_file, separate=TRUE, verbose=T, overwrite=TRUE)

extract <- function(vrt, x, y){
  
  dat <- gdallocationinfo(vrt, x, y, wgs84=TRUE, valonly=TRUE) %>%
    as.numeric
  
  dat[dat == -9999] <- NA
  
  return(dat)
  
}

cl <- makeCluster(7, outfile = '')
registerDoParallel(cl)

df <- foreach(n=1:nrow(rll), .packages=c('raster', 'gdalUtils', 'SPEI', 'dplyr', 'zoo', 'lubridate'), .combine=bind_rows) %dopar% {
  
  precip <- extract(precip_vrt_file, rll$x[n], rll$y[n])
  
  tmax <- extract(tmax_vrt_file, rll$x[n], rll$y[n])-273.15
  
  tmin <- extract(tmin_vrt_file, rll$x[n], rll$y[n])-273.15
  
  PET <- hargreaves(tmin, tmax, lat=rll$y[n], Pre=precip) %>%
    as.vector
  
  s <- precip - PET
  
  temp6max <- rollmax(tmax, k=6, fill=NA, na.rm=T, align='right')
  temp12max <- rollmax(tmax, k=12, fill=NA, na.rm=T, align='right')
  temp24max <- rollmax(tmax, k=24, fill=NA, na.rm=T, align='right')
  temp36max <- rollmax(tmax, k=36, fill=NA, na.rm=T, align='right')
  temp48max <- rollmax(tmax, k=48, fill=NA, na.rm=T, align='right')
  temp60max <- rollmax(tmax, k=60, fill=NA, na.rm=T, align='right')
  
  interview <- data.frame(date=seq(ymd('1981-01-01'),ymd('2016-12-01'), by='month'),
                          
                          #spei
                          spei6=as.numeric(spei(s, 6, na.rm=TRUE)$fitted),
                          spei12=as.numeric(spei(s, 12, na.rm=TRUE)$fitted),
                          spei24=as.numeric(spei(s, 24, na.rm=TRUE)$fitted),
                          spei36=as.numeric(spei(s, 36, na.rm=TRUE)$fitted),
                          spei48=as.numeric(spei(s, 48, na.rm=TRUE)$fitted),
                          spei60=as.numeric(spei(s, 60, na.rm=TRUE)$fitted),
                          
                          #TempZ
                          temp6maxZ=(temp6max - mean(temp6max, na.rm=T))/sd(temp6max, na.rm=T),
                          temp12maxZ=(temp12max - mean(temp12max, na.rm=T))/sd(temp12max, na.rm=T),
                          temp24maxZ=(temp24max - mean(temp24max, na.rm=T))/sd(temp24max, na.rm=T),
                          temp36maxZ=(temp36max - mean(temp36max, na.rm=T))/sd(temp36max, na.rm=T),
                          temp48maxZ=(temp48max - mean(temp48max, na.rm=T))/sd(temp48max, na.rm=T),
                          temp60maxZ=(temp60max - mean(temp60max, na.rm=T))/sd(temp60max, na.rm=T),
						  
						              #TempZ
                          temp6max,
                          temp12max,
                          temp24max)
  spsel <- spnew %>% 
    filter(tmpcode==rll$tmpcode[n]) %>%
    mutate(date=ymd(paste0(year, '-', month, '-01'))) %>%
    merge(interview, all.x=T, all.y=F) %>%
	mutate(mean_annual_precip=mean(precip, na.rm=T)*12,
		   mean_annual_tmax=mean(tmax, na.rm=T),
		   mean_annual_tmin=mean(tmin, na.rm=T)) %>%
	dplyr::select(-tmpcode)

  cat(n, round(n/nrow(rll)*100, 4), 'percent done\n') 
  
  spsel
}

write.csv(df, 'GBV/GBV_FTF_all.csv', row.names=F)





