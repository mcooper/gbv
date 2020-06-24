library(sf)
library(tidyverse)
library(ape)
library(mgcv)
library(fastglm)


#####################################
# Read in Data
#####################################

dat <- read.csv('~/mortalityblob/gbv/GBV_sel.csv') %>%
  mutate(drought_cat=relevel(drought_cat, ref = 'normal'),
         dd=paste0(round(latitude), '_', round(longitude)),
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

#################################
# Generate all distance matrices
###################################

dmats <- expand.grid(list(scale=c("code", 'dd', 'GDLcode'),
                          region=c('afr', 'asi', 'lac')),
                     stringsAsFactors=F)

for (i in 1:nrow(dmats)){
  dat[ , 'scale'] <- dat[ , dmats$scale[i]]
  dat[ , 'region'] <- dat[ , paste0('in_', dmats$region[i])]

  sel <- dat %>%
    filter(region) %>%
    group_by(scale) %>%
    summarize(latitude=median(latitude, na.rm=T),
              longitude=median(longitude, na.rm=T)) %>%
    st_as_sf(coords=c('longitude', 'latitude'), crs=4326) %>%
    st_transform(crs='+proj=tpeqd +lat_1=-0.32365520502926515 +lon_1=16.875 +lat_2=45.56 +lon_2=90.56')

  sel[ , c('X', 'Y')] <- st_coordinates(sel)

  sel <- sel %>% st_drop_geometry()

	dmat <- as.matrix(dist(sel[ , c('X', 'Y')]))
	dmat <- 1/dmat
	diag(dmat) <- 0

  assign(paste0(dmats$region[i], '_', dmats$scale[i]), 
         dmat)
}

save.image('~/mnt/dmats.RData')

##############################
# Now run moran's I tests  
#############################

for (i in 1:nrow(mdf)){
  mod <- readRDS(mdf$file[i])
  
  for (scale in c('code', 'dd', 'GDLcode')){
    dat$region <- dat[ , paste0('in_', mdf$region[i])]
    dat[dat$region , 'residuals'] <- mod$residuals
    dat$scale <- dat[ , scale]

    sel <- dat %>%
      filter(region) %>%
      group_by(scale) %>%
      summarize(residuals=mean(residuals))

    dmat <- get(paste0(mdf$region[i], '_', mdf$scale[i]))   
    
	  mi <- data.frame(Moran.I(sel$residuals, dmat))

    mdf$observed[i] <- mi$observed
    mdf$expected[i] <- mi$expected
    mdf$sd[i] <- mi$sd
    mdf$p.value[i] <- mi$p.value

  }
}

write.csv(mdf, '~/mortalityblob/gbv/moran_results.csv', row.names=F)


