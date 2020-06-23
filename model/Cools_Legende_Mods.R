data_dir <- '/home/mattcoop/mortalityblob/gbv'
meta_dir <- '/home/mattcoop/gbv'

library(tidyverse)
library(fastglm)

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

  cat(savename, ': Running\n')

  form <- paste0(outcome, 
                ' ~ plos_age + woman_literate + is_married + 
                                    plos_births + plos_hhsize + 
                                    plos_rural + husband_education_level + 
                                    plos_husband_age + drought_cat + survey_code')
  
  X <- model.matrix(as.formula(form), data)

  mod <- fastglm(x=X, 
                 y=as.numeric(data[ , outcome]), 
                 data=data, 
                 family=binomial(link = 'logit'),
                 method=3)
  
  saveRDS(mod, paste0('~/mortalityblob/gbv_gams/epstein/', 
                      savename, 
                      '_PLOS.RDS'))

}

##########################################################
# Read in and process data
#########################################################

dat <- read.csv(file.path(data_dir, 'GBV_sel.csv')) %>%
  mutate(drought_cat=relevel(drought_cat, ref = 'normal'))

############################################################
# Run global models
###############################################################

mods <- c('phys_asi', 'sexu_asi', 'emot_asi', 'cont_asi', 
					'phys_lac', 'sexu_lac', 'emot_lac', 'cont_lac', 
					'phys_afr', 'sexu_afr', 'emot_afr', 'cont_afr')


for(mod in mods){
	runModel(mod, dat)
}

system('~/telegram.sh "Models Done~"')


