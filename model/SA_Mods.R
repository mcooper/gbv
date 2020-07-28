if (Sys.info()['sysname']=='Linux'){
  data_dir <- '/home/mattcoop/mortalityblob/gbv'
  meta_dir <- '/home/mattcoop/gbv'
} else if(Sys.info()['sysname']=='Windows'){
  data_dir <- 'G://My Drive/GBV'
  meta_dir <- 'C://Users/matt/gbv'
}

library(tidyverse)
library(mgcv)
library(ape)
#library(parallel)

#cl <- makeCluster(4)

#############################
# Define Helper Functions
#############################
plotvars <- function(dat, vars){
	#Given a datafram with columns latitude, longitude, and a list of vars
	#	Will sum the vars,
	# Then the the mean at each lat-lon integer combo
	# Then plot it

	dat$outcome <- rowSums(dat[ vars])

	res_sum <- dat %>%
		mutate(latitude = round(latitude, 0),
					 longitude = round(longitude, 0)) %>%
		group_by(latitude, longitude) %>%
		summarize(outcome = mean(outcome))

	ggplot(res_sum) + geom_raster(aes(x=longitude, y=latitude, fill=outcome)) + 
	 theme_void() + guides(fill=F)	

}

getMoransI <- function(data, residuals){
	data$residual <- residuals
	
	resid_sum <- data %>%
		group_by(GDLcode) %>%
		summarize(residual=mean(residual),
              latitude=median(latitude, na.rm=T),
              longitude=median(longitude, na.rm=T))

	dmat <- as.matrix(dist(resid_sum[ , c('longitude', 'latitude')]))
	dmat <- 1/dmat
	diag(dmat) <- 0

	mi <- data.frame(Moran.I(resid_sum$residual, dmat))

	return(mi$p.value)
}

runModel <- function(savename, data, knots=c(100, 500, 1000)){
	#Run a model, adding more and more knots to the SOS basis function,
	#until there is no more spatial autocorrelation
	#Then save the final model

	if (grepl('asi', savename)){
		data <- data %>% filter(in_asia)
	}
	if (grepl('lac', savename)){
		data <- data %>% filter(in_lac)
	}
	if (grepl('afr', savename)){
		data <- data %>% filter(in_afr)
	}

	outcome <- paste0('viol_', substr(savename, 1, 4))

	for (k in knots){
    file <- paste0(savename, 
													'_', substr(100000 + k, 2, 6), '.RDS')

    if (file %in% list.files('/home/mattcoop/mortalityblob/gbv_gams/gam_splines/')){
      next
    }

    cat(as.character(Sys.time()), '-', savename, ': Running with', k, 'knot spline \n')

    form <- paste0(outcome, 
                   ' ~ plos_age + woman_literate + is_married + 
                  plos_births + plos_hhsize + 
                  plos_rural + husband_education_level + 
                  plos_husband_age + drought_cat + survey_code',
                paste0(' + s(latitude, longitude, bs="sos", k=', k, ')'))
    
    mod <- gam(as.formula(form),
               data=data, 
               family=binomial(link = 'logit'))#, cluster=cl)
    
    saveRDS(mod, paste0('~/mortalityblob/gbv_gams/gam_splines/', 
                        savename, 
                        '_', substr(100000 + k, 2, 6), '.RDS'))
    
	}
}

##########################################################
# Read in and process data
#########################################################

dat <- read.csv(file.path(data_dir, 'GBV_sel.csv')) %>%
  mutate(drought_cat=relevel(drought_cat, ref = 'normal'))

dat$survey_code <- as.character(dat$survey_code)

############################################################
# Run global models
###############################################################

mods <- c(#'phys_afr', 
					#'sexu_afr', 
          #'emot_afr', 
					#'cont_afr',
          #'phys_lac', 
					#'sexu_lac', 
          #'emot_lac', 
					#'cont_lac',
					#'phys_asi', 
					#'sexu_asi', 
          #'emot_asi', 
					'cont_asi'
          )


for(mod in mods){
	runModel(mod, dat, knots=c(1250, 1500, 1750, 2000))
}

system('~/telegram.sh "Models Done~"')


system('poweroff')

