if (Sys.info()['sysname']=='Linux'){
  data_dir <- '/home/mattcoop/mortalityblob/gbv'
  meta_dir <- '/home/mattcoop/gbv'
}

library(gdalUtils)
library(raster)
library(dplyr)
library(SPEI)
library(lubridate)
library(zoo)
library(foreach)
library(doParallel)

options(stringsAsFactors = F)

setwd('/mnt/climatedisk')

dat <- read.csv(paste0(data_dir, '/GBV_all.csv')) %>%
  dplyr::select(code, latitude, longitude, date_cmc) %>%
  unique %>%
  filter(!(latitude < 1 & longitude < 1 & latitude > -1 & longitude > -1))

sp <- SpatialPointsDataFrame(coords=dat[ c('longitude', 'latitude')], data = dat)

r <- raster('chirps-v2.0.1981.01.tif')
codes <- raster(matrix(seq(1, ncell(r)), nrow=nrow(r), ncol=ncol(r)), xmx=xmax(r), xmn=xmin(r), ymx=ymax(r), ymn=ymin(r))
codes[r==-9999] <- NA

sp@data$tmpcode <- raster::extract(codes, sp)

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
precip_files <- list.files('.', pattern='^chirps.*tif$')
gdalbuildvrt(paste0('./', precip_files), precip_vrt_file, separate=TRUE, verbose=T, overwrite=TRUE)

#Read in tave data
tave_vrt_file <- extension(rasterTmpFile(), 'ivrt')
tave_files <- list.files('.', pattern='tave.*tif$')
gdalbuildvrt(paste0('./', tave_files), tave_vrt_file, separate=TRUE, verbose=T, overwrite=TRUE)

extract <- function(vrt, x, y){
  
  dat <- gdallocationinfo(vrt, x, y, valonly=TRUE, wgs84 = TRUE) %>%
    as.numeric
  
  dat[dat == -9999] <- NA
  
  return(dat)
  
}

getPercentile <- function(vect, ix){
  if (is.na(vect[ix])){
    return(NA)
  }
  if (ix <= 12*29){
    res <- ecdf(vect[1:ix])
  }else{
    res <- ecdf(vect[(ix - 12*29):ix])
  }
  res(vect[ix])
}

cl <- makeCluster(16, outfile = '')
registerDoParallel(cl)

foreach(n=1:nrow(rll), .packages=c('raster', 'gdalUtils', 'SPEI', 'dplyr', 'zoo'), .combine=bind_rows) %dopar% {
  
  precip <- extract(precip_vrt_file, rll$x[n], rll$y[n])
  
  tave <- extract(tave_vrt_file, rll$x[n-1], rll$y[n-1])
  
  PET <- thornthwaite(tave, lat=rll$y[n]) %>%
    as.vector
  
  s <- precip - PET
  
  temp6ave <- rollmean(tave, k=6, fill=NA, na.rm=T, align='right')
  temp12ave <- rollmean(tave, k=12, fill=NA, na.rm=T, align='right')
  temp24ave <- rollmean(tave, k=24, fill=NA, na.rm=T, align='right')
  
  precip_12month_total <- rollsum(precip, k=12, fill=NA, na.rm=T, align='right')
  precip_24month_total <- rollsum(precip, k=24, fill=NA, na.rm=T, align='right')
  precip_36month_total <- rollsum(precip, k=36, fill=NA, na.rm=T, align='right')
  precip_48month_total <- rollsum(precip, k=48, fill=NA, na.rm=T, align='right')
  
  interview <- data.frame(date_cmc=seq(973,1440),
                          
                          #spei
                          spei6=as.numeric(spei(s, 6, na.rm=TRUE)$fitted),
                          spei12=as.numeric(spei(s, 12, na.rm=TRUE)$fitted),
                          spei24=as.numeric(spei(s, 24, na.rm=TRUE)$fitted),
                          spei36=as.numeric(spei(s, 36, na.rm=TRUE)$fitted),
                          spei48=as.numeric(spei(s, 48, na.rm=TRUE)$fitted),
                          
                          perc12=sapply(FUN=getPercentile, X = 1:length(precip_12month_total), vect=precip_12month_total),
                          perc24=sapply(FUN=getPercentile, X = 1:length(precip_12month_total), vect=precip_24month_total),
                          perc36=sapply(FUN=getPercentile, X = 1:length(precip_12month_total), vect=precip_36month_total),
                          perc48=sapply(FUN=getPercentile, X = 1:length(precip_12month_total), vect=precip_48month_total),
                          
                          #TempZ
                          temp6aveZ=(temp6ave - mean(temp6ave, na.rm=T))/sd(temp6ave, na.rm=T),
                          temp12aveZ=(temp12ave - mean(temp12ave, na.rm=T))/sd(temp12ave, na.rm=T),
                          temp24aveZ=(temp24ave - mean(temp24ave, na.rm=T))/sd(temp24ave, na.rm=T),
						  
						              #TempZ
                          temp6ave,
                          temp12ave,
                          temp24ave)
  spsel <- spnew %>% 
    filter(tmpcode==rll$tmpcode[n]) %>%
    merge(interview, all.x=T, all.y=F) %>%
	mutate(mean_annual_precip=mean(precip, na.rm=T)*12,
		   mean_annual_tave=mean(tave, na.rm=T)) %>%
	dplyr::select(-tmpcode)

  write.csv(spsel, paste0(data_dir, '/SPItemp/', n), row.names=F)

  cat(n, round(n/nrow(rll)*100, 4), 'percent done\n') 
}

setwd(file.path(data_dir, 'SPItemp'))

df <- list.files() %>%
  lapply(FUN=read.csv, stringsAsFactors=FALSE) %>%
  bind_rows

write.csv(df, file.path(data_dir, '/GBV_SPI.csv'), row.names=F)

system('/home/matt/telegram.sh "SPI for Mortality Done!"')





