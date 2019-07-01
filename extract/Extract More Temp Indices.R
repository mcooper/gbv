library(gdalUtils)
library(raster)
library(dplyr)
library(SPEI)
library(lubridate)
library(zoo)
library(foreach)
library(doParallel)

setwd('~/climatedisk')

dat <- read.csv('~/GBV_geo.csv') %>%
  filter(!(latitude==0 & longitude==0))

sp <- SpatialPointsDataFrame(coords=dat[ c('longitude', 'latitude')], data = dat)

r <- raster('tmaxmx201401.tif')
codes <- raster(matrix(seq(1, ncell(r)), nrow=nrow(r), ncol=ncol(r)), xmx=xmax(r), xmn=xmin(r), ymx=ymax(r), ymn=ymin(r))
codes[r==-9999] <- NA

sp@data$tmpcode <- extract(codes, sp)

rll <- sp@data %>% group_by(tmpcode) %>%
  summarize(x=mean(longitude),
            y=mean(latitude))

#Read in tmax data
tmax_vrt_file <- extension(rasterTmpFile(), 'ivrt')
tmax_files <- list.files('.', pattern='^tmaxmx.......tif$')
gdalbuildvrt(paste0('./', tmax_files), tmax_vrt_file, separate=TRUE, verbose=T, overwrite=TRUE)

cl <- makeCluster(32, outfile = '')
registerDoParallel(cl)

df <- foreach(n=1:nrow(rll), .packages=c('raster', 'gdalUtils', 'SPEI', 'dplyr', 'zoo'), .combine=bind_rows) %dopar% {
    
  tmax <- as.numeric(gdallocationinfo(tmax_vrt_file, rll$x[n], rll$y[n], wgs84=TRUE, valonly=TRUE)) - 273.15
  
  temp6max <- rollmax(tmax, k=6, fill=NA, na.rm=T, align='right')
  temp12max <- rollmax(tmax, k=12, fill=NA, na.rm=T, align='right')
  temp24max <- rollmax(tmax, k=24, fill=NA, na.rm=T, align='right')
  
  interview <- data.frame(date_cmc=seq(961,1404),
                          
						  #TempZ
                          temp6maxZ=(temp6max - mean(temp6max, na.rm=T))/sd(temp6max, na.rm=T),
                          temp12maxZ=(temp12max - mean(temp12max, na.rm=T))/sd(temp12max, na.rm=T),
                          temp24maxZ=(temp24max - mean(temp24max, na.rm=T))/sd(temp24max, na.rm=T),
						  
						  #TempZ
                          temp6max,
                          temp12max,
                          temp24max
						  )
  
  interview <- round(interview, 3)
  
  spsel <- sp@data %>% 
    filter(tmpcode==rll$tmpcode[n]) %>%
    rename(date_cmc=v008) %>%
    merge(interview, all.x=T, all.y=F)

  cat(n, round(n/nrow(rll)*100, 4), 'percent done\n') 
  
  spsel
}

write.csv(df, '~/GBV_Max_Temps.csv', row.names=F)

stopCluster(cl)

system('/home/mattcoop/telegram.sh "SPI for Mortality Done!"')




