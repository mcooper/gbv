library(gdalUtils)
library(raster)
library(dplyr)
library(SPEI)
library(lubridate)
library(zoo)
library(foreach)
library(doParallel)

setwd('climatedisk')

dat <- read.csv('~/GBV_geo.csv') %>%
  filter(!(latitude==0 & longitude==0))

sp <- SpatialPointsDataFrame(coords=dat[ c('longitude', 'latitude')], data = dat)

r <- raster('chirps-v2.0.1981.01.tif')
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
precip_files <- list.files('.', pattern='^chirps.*tif$')[1:432]
gdalbuildvrt(paste0('./', precip_files), precip_vrt_file, separate=TRUE, verbose=T, overwrite=TRUE)

#Read in tmax data
tmax_vrt_file <- extension(rasterTmpFile(), 'ivrt')
tmax_files <- list.files('.', pattern='^tmax.......tif$')
gdalbuildvrt(paste0('./', tmax_files), tmax_vrt_file, separate=TRUE, verbose=T, overwrite=TRUE)

#Read in tmin data
tmin_vrt_file <- extension(rasterTmpFile(), 'ivrt')
tmin_files <- list.files('.', pattern='^tmin.......tif$')
gdalbuildvrt(paste0('./', tmin_files), tmin_vrt_file, separate=TRUE, verbose=T, overwrite=TRUE)

extract <- function(vrt, x, y){
  
  dat <- gdallocationinfo(vrt, x, y, wgs84=TRUE, valonly=TRUE) %>%
    as.numeric
  
  dat[dat == -9999] <- NA
  
  return(dat)
  
}

cl <- makeCluster(32, outfile = '')
registerDoParallel(cl)

df <- foreach(n=1:nrow(rll), .packages=c('raster', 'gdalUtils', 'SPEI', 'dplyr', 'zoo'), .combine=bind_rows) %dopar% {
  
  precip <- extract(precip_vrt_file, rll$x[n], rll$y[n])
  
  tmax <- extract(tmax_vrt_file, rll$x[n], rll$y[n])-273.15
  
  tmin <- extract(tmin_vrt_file, rll$x[n], rll$y[n])-273.15
  
  PET <- hargreaves(tmin, tmax, lat=rll$y[n], Pre=precip) %>%
    as.vector
  
  s <- precip - PET
  
  temp6max <- rollmax(tmax, k=6, fill=NA, na.rm=T, align='right')
  temp12max <- rollmax(tmax, k=12, fill=NA, na.rm=T, align='right')
  temp24max <- rollmax(tmax, k=24, fill=NA, na.rm=T, align='right')
  
  interview <- data.frame(date_cmc=seq(973,1404),
                          
                          #spei
                          spei6=as.numeric(spei(s, 6, na.rm=TRUE)$fitted),
                          spei12=as.numeric(spei(s, 12, na.rm=TRUE)$fitted),
                          spei24=as.numeric(spei(s, 24, na.rm=TRUE)$fitted),
                          spei36=as.numeric(spei(s, 36, na.rm=TRUE)$fitted),
                          spei48=as.numeric(spei(s, 48, na.rm=TRUE)$fitted),
                          
                          #TempZ
                          temp6maxZ=(temp6max - mean(temp6max, na.rm=T))/sd(temp6max, na.rm=T),
                          temp12maxZ=(temp12max - mean(temp12max, na.rm=T))/sd(temp12max, na.rm=T),
                          temp24maxZ=(temp24max - mean(temp24max, na.rm=T))/sd(temp24max, na.rm=T),
						  
						  #TempZ
                          temp6max,
                          temp12max,
                          temp24max)
  spsel <- spnew %>% 
    filter(tmpcode==rll$tmpcode[n]) %>%
    rename(date_cmc=v008) %>%
    merge(interview, all.x=T, all.y=F) %>%
	mutate(mean_annual_precip=mean(precip, na.rm=T)*12,
		   mean_annual_tmax=mean(tmax, na.rm=T),
		   mean_annual_tmin=mean(tmin, na.rm=T)) %>%
	dplyr::select(-tmpcode)

  write.csv(spsel, paste0('~/climatedisk/SPItemp/', n), row.names=F)

  cat(n, round(n/nrow(rll)*100, 4), 'percent done\n') 
  
  spsel
}

setwd('~/climatedisk/SPItemp/')

df <- list.files() %>%
  lapply(FUN=read.csv, stringsAsFactors=FALSE) %>%
  Reduce(bind_rows, x=.)

write.csv(df, '~/GBV_SPI.csv', row.names=F)

system('/home/mattcoop/telegram.sh "SPI for Mortality Done!"')




