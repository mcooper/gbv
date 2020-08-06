library(sf)
library(tidyverse)
library(ape)
library(mgcv)
library(fastglm)
library(moranfast) #From github.com/mcooper/moranfast

#####################################
# Read in Data
#####################################

dat <- read.csv('~/mortalityblob/gbv/GBV_sel.csv') %>%
  mutate(drought_cat=relevel(drought_cat, ref = 'normal'),
         in_asi = in_asia)

#######################################
# Get all models and model attributes
#######################################

mods <- list.files('~/mortalityblob/gbv_gams', recursive=T, full.names=T)

mdf <- data.frame(file=mods, stringsAsFactors=F) %>%
	mutate(model=basename(file),
         outcome = substr(model, 1, 4),
				 order = as.numeric(gsub('.RDS', '', substr(model, 10, 14))),
         order = ifelse(is.na(order), 0, order),
				 method = basename(gsub('...._..._.*RDS$', '', file)),
				 region = substr(model, 6, 8)) %>%
  filter(region %in% c('afr', 'asi', 'lac')) %>%
	arrange(outcome, model, region, order) %>%
	group_by(outcome, model, region) %>%
	filter(order==max(order)) %>%
  ungroup %>%
  mutate(observed=NA,
         expected=NA,
         sd=NA,
         p.value=NA)

##########################
# Project Coordinates
########################
dat <- dat %>%
  st_as_sf(coords=c('longitude', 'latitude'), crs=4326) %>%
  st_transform(crs='+proj=tpeqd +lat_1=-0.32365520502926515 +lon_1=16.875 +lat_2=45.56 +lon_2=90.56')

dat[ , c('X', 'Y')] <- st_coordinates(dat)

dat <- dat %>% st_drop_geometry()

###############################
# Now run moran's I tests  
#############################

for (i in 1:nrow(mdf)){
  cat('calculating Morans I for', mdf$model[i], '\n')
  mod <- readRDS(mdf$file[i])
  
  dat$region <- dat[ , paste0('in_', mdf$region[i])]
  dat[dat$region , 'residuals'] <- mod$residuals

  sel <- dat %>%
    filter(region)

  mi <- data.frame(moranfast(sel$residuals, sel$X, sel$Y))

  mdf$observed[i] <- mi$observed
  mdf$expected[i] <- mi$expected
  mdf$sd[i] <- mi$sd
  mdf$p.value[i] <- mi$p.value

}

write.csv(mdf, '~/mortalityblob/gbv/moran_results.csv', row.names=F)


