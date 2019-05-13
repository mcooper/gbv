library(gdalUtils)
library(raster)
library(dplyr)
library(SPEI)
library(lubridate)
library(zoo)
library(foreach)
library(doParallel)

hh <- read.csv('sp_export.csv') %>%
  select(year=interview_year, month=interview_month, latitude, longitude, code) %>%
  unique

sp_hh <- SpatialPointsDataFrame(coords=hh[ c('longitude', 'latitude')], data = hh)

daily_in_folder <- 'climatedisk/'
daily_vrt_file <- extension(rasterTmpFile(), 'ivrt')
daily_files <- list.files(daily_in_folder, pattern='^tmax.{11}tif$')[11324:13149] #Just 2011 to 2015
gdalbuildvrt(paste0(daily_in_folder, daily_files), daily_vrt_file, separate=TRUE, verbose=T, overwrite=TRUE)

monthly_in_folder <- 'climatedisk/'
monthly_vrt_file <- extension(rasterTmpFile(), 'ivrt')
monthly_files <- list.files(monthly_in_folder, pattern='^tmax.{7}tif$')
gdalbuildvrt(paste0(monthly_in_folder, monthly_files), monthly_vrt_file, separate=TRUE, verbose=T, overwrite=TRUE)

getZ <- function(x, pop){
  (x - mean(pop, na.rm=T))/sd(pop, na.rm=T)
}

cl <- makeCluster(31, outfile = '')
registerDoParallel(cl)

tryCatch({
  df <- foreach(i=1:nrow(sp_hh), .packages=c('raster', 'lubridate', 'gdalUtils', 'SPEI', 'dplyr', 'zoo'), .combine=bind_rows) %dopar% {
    
    lat <- sp_hh$latitude[i]
    lng <- sp_hh$longitude[i]
    day <- 1
    month <- sp_hh$month[i]
    year <- sp_hh$year[i]
    
    new <- sp_hh@data[i, ]
    
    month_tmp <- gdallocationinfo(monthly_vrt_file, lng, lat, wgs84=TRUE, valonly=TRUE) %>%
      as.numeric
    
    month2_tmp <- rollmean(month_tmp, k=2, fill=NA, na.rm=T, align='right')
    month3_tmp <- rollmean(month_tmp, k=3, fill=NA, na.rm=T, align='right')
    month12_tmp <- rollmean(month_tmp, k=12, fill=NA, na.rm=T, align='right')
    
    day_tmp <- gdallocationinfo(daily_vrt_file, lng, lat, wgs84=TRUE, valonly=TRUE) %>%
      as.numeric
    
    month_ix <- seq(ymd('1981-01-01'), ymd('2016-12-01'), by='month')
    day_ix <- seq(ymd('2011-01-01'), ymd('2015-12-31'), by='day')
    
    #Get last month dev from LTN
    new$zsq_1m <- getZ(month_tmp[month_ix == ymd(paste0(year, '-', month, '-01'))], 
                   month_tmp[month(month_ix) == month])
    
    #Get last 2 month dev from LTN
    new$zsq_2m <- getZ(month2_tmp[month_ix == ymd(paste0(year, '-', month, '-01'))], 
                   month2_tmp[month(month_ix) == month])
    
    #Get last 3 month dev from LTN
    new$zsq_3m <- getZ(month3_tmp[month_ix == ymd(paste0(year, '-', month, '-01'))], 
                   month3_tmp[month(month_ix) == month])
    
    #Get last 12 month dev from LTN
    new$zsq_12m <- getZ(month12_tmp[month_ix == ymd(paste0(year, '-', month, '-01'))], 
                       month12_tmp[month(month_ix) == month])
    
    #Get last month ct days > 37.5
    date <- ymd(paste0(year, '-', month, '-', day))
    new$ct37.5_1m <- sum(day_tmp[day_ix < date & day_ix > date - months(1)] > (37.5 + 273.15))
    
    #Get last month ct days > 40
    new$ct40_1m <- sum(day_tmp[day_ix < date & day_ix > date - months(1)] > (40 + 273.15))
    
    #Get last 2 month ct days > 37.5
    new$ct37.5_2m <- sum(day_tmp[day_ix < date & day_ix > date - months(2)] > (37.5 + 273.15))
    
    #Get last 2 month ct days > 40
    new$ct40_2m <- sum(day_tmp[day_ix < date & day_ix > date - months(2)] > (40 + 273.15))
    
    #Get last 3 month ct days > 37.5
    new$ct37.5_3m <- sum(day_tmp[day_ix < date & day_ix > date - months(3)] > (37.5 + 273.15))
    
    #Get last 3 month ct days > 40
    new$ct40_3m <- sum(day_tmp[day_ix < date & day_ix > date - months(3)] > (40 + 273.15))
    
    new$mean_max_temp <- mean(month_tmp, na.rm=T)
    
    cat(i, round(i/nrow(sp_hh)*100, 4), 'percent done\n')
    
    new
  }
    
  write.csv(df, 'GBV_DHS_Temps.csv', row.names=F)
  
  system('./telegram.sh "Done!"')
}, error = function(e){
    cat(e)
    system('./telegram.sh "Error :-("')
  }
)