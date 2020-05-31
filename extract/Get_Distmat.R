if (Sys.info()['sysname']=='Linux'){
  data_dir <- '/home/mattcoop/mortalityblob/gbv'
  meta_dir <- '/home/mattcoop/gbv'
} else if(Sys.info()['sysname']=='Windows'){
  data_dir <- 'G://My Drive/GBV'
  meta_dir <- 'C://Users/matt/gbv'
}

library(tidyverse)
library(sf)

options(stringsAsFactors=F)

dat <- read.csv(file.path(data_dir, 'GBV_sel.csv')) %>%
	select(ind_code, code, latitude, longitude)

sel <- dat %>%
	select(code, latitude, longitude) %>%
	unique %>%
	st_as_sf(coords=c('longitude', 'latitude'), crs=4326) %>%
	st_transform(crs='+proj=tpeqd +lat_1=-0.32365520502926515 +lon_1=16.875 +lat_2=45.56 +lon_2=90.56')

sel[ , c('X', 'Y')] <- st_coordinates(sel)

sel <- sel %>% st_drop_geometry()

dm <- as.matrix(dist(sel[ , c('X', 'Y')]))

dm <- 1/dm
diag(dm) <- 0

dimnames(dm) <- list(sel$code, sel$code) 

saveRDS(dm, file = file.path(data_dir, "gbv_InvDistMat.rds"))
