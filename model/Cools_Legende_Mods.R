data_dir <- '/home/mattcoop/mortalityblob/gbv'
meta_dir <- '/home/mattcoop/gbv'

library(tidyverse)
library(fastglm)

#############################
# Define Helper Functions
#############################
derive_legendre <- function(x, y, n){
    #Given X and Y coords and an Nth order,
    #Derive the Legendre polynomials, a la Cools, et al 2020

    #Derive Legende polynomials
    legcoef <- legendre.polynomials(n=n, normalized=TRUE)
  leg <- as.data.frame(c(polynomial.values(polynomials=legcoef, 
                                                                                      x=scaleX(dat$longitude, u=-1, v=1)), 
                                                  polynomial.values(polynomials=legcoef, 
                                                                                                                x=scaleX(dat$latitude, u=-1, v=1))))
    names(leg) <- c(paste0("leg", 0:n, "x"),
                                      paste0("leg", 0:n, "y"))

    for (l in 0:n){
          for (k in 0:n){
                  leg[ paste0('l', l, 'k', k)] <- leg[ , paste0('leg', l, 'x')]*leg[ , paste0('leg', k, 'y')]
        }
      }
      
      leg <- leg %>% select(-matches('leg'))
        
        return(leg)
}

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

dat <- cbind(dat, derive_legendre(dat$longtiude, dat$latitude, n=10))

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


