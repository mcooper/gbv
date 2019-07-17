library(raster)
library(dplyr)

setwd('G://My Drive/temperature')

dat <- read.csv('G://My Drive/GBV/Coords&Dates.csv') %>%
  filter(country=="Bangladesh")

sp <- SpatialPointsDataFrame(coords=dat[ c('longitude', 'latitude')], data = dat)

#Read in tmax data
tmax_files <- list.files('.', pattern='^tmax.......tif$')
tmax_dat <- lapply(tmax_files, raster)
tmax_agg <- Reduce('+', tmax_dat)
tmax <- tmax_agg/length(tmax_files)

sp@data$mean_annual_tmax <- extract(tmax, sp) - 273.15

sel <- sp@data %>%
  dplyr::select(hh_refno, mean_annual_tmax) %>%
  unique

write.csv(sel, 'G://My Drive/GBV/BGD_Temp_LTN.csv', row.names=F)
