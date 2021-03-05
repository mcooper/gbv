data_dir <- '/home/mattcoop/mortalityblob/gbv'
meta_dir <- '/home/mattcoop/gbv'

library(tidyverse)
library(mgcv)

#############################
# Define Helper Functions
#############################
runModel <- function(savename, data, knots=c(100, 500, 1000)){
	#Run a model, adding more and more knots to the SOS basis function,
	#until there is no more spatial autocorrelation
	#Then save the final model

	if (grepl('asi', savename)){
		data <- data %>% 
      filter(in_asia)
	}
	if (grepl('lac', savename)){
		data <- data %>% 
      filter(in_lac)
	}
	if (grepl('afr', savename)){
		data <- data %>% 
      filter(in_afr)
	}

	outcome <- paste0('viol_', substr(savename, 1, 4))

  for (s in c('spei12', 'spei24', 'spei36')){
    for (k in knots){
      file <- paste0(savename, '_', s, '_', substr(100000 + k, 2, 6), '.RDS')

      if (file %in% list.files('/home/mattcoop/mortalityblob/gbv_gams/gam_splines/')){
        next
      }

      cat(as.character(Sys.time()), '-', savename, '_', s, ': Running with', k, 'knot spline \n')

      form <- paste0(outcome, 
                     ' ~ plos_age + woman_literate + is_married + 
                    plos_births + plos_hhsize + 
                    plos_rural + husband_education_level + 
                    plos_husband_age + ', s, ' + survey_code + mean_annual_precip',
                  paste0(' + s(latitude, longitude, bs="sos", k=', k, ')'))
     
      sel <- data[data[ , s] != 'wet', ] 
      sel[ , s] <- relevel(factor(sel[ , s]), ref='normal')

      mod <- gam(as.formula(form),
                 data=sel, 
                 family=binomial(link = 'logit'))#, cluster=cl)
      
      saveRDS(mod, paste0('~/mortalityblob/gbv_gams/gam_splines/', 
                          savename, 
                          '_', substr(100000 + k, 2, 6), '.RDS'))
      
    }
  }
}

##########################################################
# Read in and process data
#########################################################

dat <- read.csv(file.path(data_dir, 'GBV_SPI_COMB.csv'))

dat$survey_code <- as.character(dat$survey_code)

############################################################
# Run global models
###############################################################

mods <- c('phys_afr', 
					'sexu_afr', 
          'emot_afr', 
					'cont_afr',
          'phys_lac', 
					'sexu_lac', 
          'emot_lac', 
					'cont_lac',
					'phys_asi', 
					'sexu_asi', 
          'emot_asi', 
					'cont_asi'
          )


for (mod in mods){
	runModel(mod, dat)
}

system('~/telegram.sh "Models Done~"')



system('poweroff')

