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
sp <- bind_rows(spna, sp@data[!is.na(sp@data$tmpcode), ])

rll <- sp %>% group_by(tmpcode) %>%
  summarize(x=mean(longitude),
            y=mean(latitude))

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

cl <- makeCluster(10, outfile = '')
registerDoParallel(cl)

df <- foreach(n=1:nrow(rll), .packages=c('raster', 'gdalUtils', 'SPEI', 'dplyr', 'zoo'), .combine=bind_rows) %dopar% {
  
  precip <- extract(precip_vrt_file, rll$x[n], rll$y[n])
  
  tmax <- extract(tmax_vrt_file, rll$x[n], rll$y[n])-273.15
  
  tmin <- extract(tmin_vrt_file, rll$x[n], rll$y[n])-273.15
  
  PET <- hargreaves(tmin, tmax, lat=rll$y[n], Pre=precip) %>%
    as.vector
  
  s <- precip - PET
  
  temp6 <- rollmean(tmax, k=6, fill=NA, na.rm=T, align='right')
  temp12 <- rollmean(tmax, k=12, fill=NA, na.rm=T, align='right')
  temp24 <- rollmean(tmax, k=24, fill=NA, na.rm=T, align='right')
  
  interview <- data.frame(date_cmc=seq(973,1404),
                          
                          #spei
                          spei6=as.numeric(spei(s, 6, na.rm=TRUE)$fitted),
                          spei12=as.numeric(spei(s, 12, na.rm=TRUE)$fitted),
                          spei24=as.numeric(spei(s, 24, na.rm=TRUE)$fitted),
                          spei36=as.numeric(spei(s, 36, na.rm=TRUE)$fitted),
                          spei48=as.numeric(spei(s, 48, na.rm=TRUE)$fitted),
                          
                          #spi
                          spi6=as.numeric(spi(precip, 6, na.rm=TRUE)$fitted),
                          spi12=as.numeric(spi(precip, 12, na.rm=TRUE)$fitted),
                          spi24=as.numeric(spi(precip, 24, na.rm=TRUE)$fitted),
                          spi36=as.numeric(spi(precip, 36, na.rm=TRUE)$fitted),
                          spi48=as.numeric(spi(precip, 48, na.rm=TRUE)$fitted),
                          
                          #TempZ
                          temp6monthZ=(temp6 - mean(temp6, na.rm=T))/sd(temp6, na.rm=T),
                          temp12monthZ=(temp12 - mean(temp12, na.rm=T))/sd(temp12, na.rm=T),
                          temp24monthZ=(temp24 - mean(temp24, na.rm=T))/sd(temp24, na.rm=T))
  
  spsel <- sp %>% 
    filter(tmpcode==rll$tmpcode[n]) %>%
    rename(date_cmc=v008) %>%
    merge(interview, all.x=T, all.y=F)

  cat(n, round(n/nrow(rll)*100, 4), 'percent done\n') 
  
  spsel
}

write.csv(df, '~/GBV_SPI.csv', row.names=F)

system('/home/mattcoop/telegram.sh "SPI for Mortality Done!"')




