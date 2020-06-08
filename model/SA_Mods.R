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
library(parallel)

cl <- makeCluster(4)

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
		mutate(latitude = round(latitude, 0),
					 longitude = round(longitude, 0)) %>%
		group_by(latitude, longitude) %>%
		summarize(residual=mean(residual))

	dmat <- as.matrix(dist(resid_sum[ , c('longitude', 'latitude')]))
	dmat <- 1/dmat
	diag(dmat) <- 0

	mi <- data.frame(Moran.I(resid_sum$residual, dmat))

	return(mi$p.value)
}

runModelUntilNoSA <- function(savename, data, allvars=TRUE, knots=c(50, 100, 500, 1000)){
	#Run a model, adding higher and higher Legendre polynomials,
	#until there is no more spatial autocorrelation
	#Then save the final model
	
	if (grepl('all', savename)){
		data <- data
	}
	if (grepl('plos', savename)){
		data <- data %>% filter(in_plos_paper)
	}
	if (grepl('cty', savename)){
		data <- data %>% filter(in_cty)
	}
	if (grepl('afr', savename)){
		data <- data %>% filter(in_afr)
	}

	outcome <- paste0('viol_', substr(savename, 1, gregexpr('_mod', savename)[[1]][1] - 1))

	SA <- TRUE
	for (k in knots){
		if(SA){
			cat(savename, ': Running with', k, 'knot spline \n')

			form <- paste0(outcome, 
										ifelse(allvars, ' ~ plos_age + woman_literate + is_married + 
																				plos_births + plos_hhsize + 
																				plos_rural + husband_education_level + 
																				plos_husband_age + drought_cat + ',
																			' ~ drought_cat + '),
									paste0('s(latitude, longitude, bs="sos", k=', k, ')'))
			
			mod <- bam(as.formula(form),
								 data=data, 
								 family=binomial(link = 'logit'), cluster=cl)
			
			mi <- getMoransI(data, residuals(mod))
			cat(savename, ': \t\tMorans I of', mi, '\n')
				
			saveRDS(mod, paste0('~/mortalityblob/gbv_gams/', 
													savename, 
													ifelse(allvars, '_gam_allvars_', '_gam_'),
													k, '.RDS'))
			
			if (mi > 0.01){
				SA <- FALSE
			}	
		}
	}
}

##########################################################
# Read in and process data
#########################################################

dat <- read.csv(file.path(data_dir, 'GBV_sel.csv')) %>%
  mutate(drought_cat=relevel(drought_cat, ref = 'normal'))

dat$in_plos_paper <- dat$survey_code %in% c("SL-6-1", "TG-6-1", "BJ-7-1", "CI-6-1", "CM-6-1",
                                            "GA-6-1", "TD-7-1", "CD-6-1", "RW-7-1", "BU-7-1",
                                            "UG-7-2", "KE-7-1", "TZ-7-2", "MW-7-2", "MZ-6-1",
                                            "ZW-7-1", "ZM-6-1", "NM-6-1", "AO-7-1")

dat$in_cty <- dat$country %in% c("SL", "TG", "BJ", "CI", "CM",
                                 "GA", "TD", "CD", "RW", "BU", 
                                 "UG", "KE", "TZ", "MW", "MZ",
                                 "ZW", "ZM", "NM", "AO")

dat$in_afr <- dat$latitude < 23 & dat$longitude > -20 & dat$longitude < 50

dat$survey_code <- as.character(dat$survey_code)

############################################################
# Run global models
###############################################################

mods <- c('phys_mod_plos', 'sex_mod_plos', 'emot_mod_plos', 'cont_mod_plos', 
					'phys_mod_cty', 'sex_mod_cty', 'emot_mod_cty', 'cont_mod_cty', 
					'phys_mod_afr', 'sex_mod_afr', 'emot_mod_afr', 'cont_mod_afr',
					'phys_mod_all', 'sex_mod_all', 'emot_mod_all', 'cont_mod_all')


for(mod in mods){
	runModelUntilNoSA(mod, dat, allvars=F)
}

for(mod in mods){
	runModelUntilNoSA(mod, dat, allvars=T)
}

system('~/telegram.sh "Models Done~"')


system('poweroff')

