library(raster)
library(dplyr)

setwd('~/climatedisk')

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
spclean <- bind_rows(spna, sp@data[!is.na(sp@data$tmpcode), ]) %>%
  dplyr::select(code, tmpcode) %>%
  unique

coords <- rasterToPoints(codes)
spdf <- SpatialPointsDataFrame(coords=coords[ , c('x', 'y')], data=data.frame(coords))

#Read in precip data
precip_files <- list.files('.', pattern='^chirps.*tif$')[1:432]
precip_dat <- lapply(precip_files, raster)
precip_agg <- Reduce('+', precip_dat)
precip <- precip_agg/36

#Read in tmax data
tmax_files <- list.files('.', pattern='^tmax.......tif$')
tmax_dat <- lapply(tmax_files, raster)
tmax_agg <- Reduce('+', tmax_dat)
tmax <- tmax_agg/length(tmax_files)

#Read in tmin data
tmin_files <- list.files('.', pattern='^tmin.......tif$')
tmin_dat <- lapply(tmin_files, raster)
tmin_agg <- Reduce('+', tmin_dat)
tmin <- tmin_agg/length(tmax_files)

#Extract
spdf@data$mean_annual_precip <- extract(precip, spdf)
spdf@data$mean_annual_tmax <- extract(tmax, spdf) - 273.15
spdf@data$mean_annual_tmin <- extract(tmin, spdf) - 273.15
spdf@data$codes <- extract(codes, spdf)

sel <- spdf@data %>% dplyr::select(tmpcode=layer, mean_annual_precip, mean_annual_tmax, mean_annual_tmin)

final <- merge(spclean, sel, all.x=T, all.y=F)

write.csv(final, '~/GBV_LTNs.csv', row.names=F)

system('/home/mattcoop/telegram.sh "LTNs for GBV Done!"')




