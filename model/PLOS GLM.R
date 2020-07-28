library(tidyverse)
library(mgcv)

#############################
# Define Helper Functions
#############################

runModel <- function(savename, data){
	#Run a GLM model
  #Based on given parameters
  #Then save the final model
	
	if (grepl('lac', savename)){
		data <- data %>% filter(in_lac)
	}
	if (grepl('asi', savename)){
		data <- data %>% filter(in_asia)
	}
	if (grepl('afr', savename)){
		data <- data %>% filter(in_afr)
	}

	outcome <- paste0('viol_', substr(savename, 1, 4))

  form <- paste0(outcome, 
                ' ~ plos_age + woman_literate + is_married + 
                                    plos_births + plos_hhsize + 
                                    plos_rural + husband_education_level + 
                                    plos_husband_age + drought_cat + survey_code')
 
  mod <- gam(as.formula(form), 
                 data=data, 
                 family=binomial(link = 'logit'),
                 )
  
  saveRDS(mod, paste0('~/mortalityblob/gbv_gams/epstein/', 
                      savename, 
                      '_PLOS.RDS'))

}

##########################################################
# Read in and process data
#########################################################

dat <- read.csv('~/mortalityblob/gbv/GBV_sel.csv') %>%
  mutate(drought_cat=relevel(drought_cat, ref = 'normal'))

dat$survey_code <- as.character(dat$survey_code)

############################################################
# Run global models
###############################################################

mods <- c('phys_asi', 'sexu_asi', 'emot_asi', 'cont_asi', 
					'phys_lac', 'sexu_lac', 'emot_lac', 'cont_lac', 
					'phys_afr', 'sexu_afr', 'emot_afr', 'cont_afr')


for(mod in mods){
  print(mod)
  runModel(mod, dat)
}

system('~/telegram.sh "Models Done~"')
