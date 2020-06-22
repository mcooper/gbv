#######################################################
# New approach to Legendre mods
# Just do 0 to 3
# At all scales
# Don't test Moran's I, just do it and report results
#######################################################

if (Sys.info()['sysname']=='Linux'){
  data_dir <- '/home/mattcoop/mortalityblob/gbv'
  meta_dir <- '/home/mattcoop/gbv'
} else if(Sys.info()['sysname']=='Windows'){
  data_dir <- 'G://My Drive/GBV'
  meta_dir <- 'C://Users/matt/gbv'
}

library(tidyverse)
library(fastglm)
library(texreg)
library(ape)
library(orthopolynom)

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

runModel <- function(savename, data, start=0, end=3){
	#Run a model, adding higher and higher Legendre polynomials,
	#Saving each model

	if (!grepl('all', savename)){
    code <- substr(savename, 6, 9)
    col <- names(dat)[grepl(code, names(dat))]
    data <- data[data[ , col], ]
  }

	outcome <- paste0('viol_', substr(savename, 1, 4))
  for (i in start:end){
    cat(as.character(Sys.time()), savename, ': Running with', i, 'order polynomial\n')
    
    if (i == 0){
      fe <- ''
    }

		fe <- crossing(l=0:i, k=0:i) %>%
			rowwise() %>%
			mutate(var = paste0(' + survey_code*l', l, 'k', k)) %>%
      filter(!(l==0 & k==0))

		form <- paste0(outcome, 
									' ~ plos_age + woman_literate + is_married + 
																			plos_births + plos_hhsize + 
																			plos_rural + husband_education_level + 
																			plos_husband_age + drought_cat + 
                                      survey_code',
								paste0(fe$var, collapse=''))
		
		X <- model.matrix(as.formula(form), data)
	
		mod <- fastglm(x=X, 
									 y=as.numeric(data[ , outcome]), 
									 data=data, 
									 family=binomial(link = 'logit'),
									 method=3)
			
		saveRDS(mod, paste0('~/mortalityblob/gbv_gams/legendre2/', 
												savename, '_',
												i, '.RDS'))
		
	}

}

##########################################################
# Read in and process data
#########################################################

dat <- read.csv(file.path(data_dir, 'GBV_sel.csv')) %>%
  mutate(drought_cat=relevel(drought_cat, ref = 'normal'))

dat <- cbind(dat, derive_legendre(dat$longtiude, 
																	 dat$latitude,
																	 n=3))

dat$survey_code <- as.character(dat$survey_code)

############################################################
# Run global models
###############################################################
mods <- c("cont_all", "sexu_all", "phys_all", "emot_all", 
  "cont_afr", "sexu_afr", "phys_afr", "emot_afr", 
  "cont_asi", "sexu_asi", "phys_asi", "emot_asi",
  "cont_lac", "sexu_lac", "phys_lac", "emot_lac",
  "cont_cty", "sexu_cty", "phys_cty", "emot_cty",
  "cont_pap", "sexu_pap", "phys_pap", "emot_pap")   

for(mod in mods){
	runModel(mod, dat)

  system(paste0('~/telegram.sh "', mod, ' Done"'))
}

system('~/telegram.sh "ALL MODELS DONE"')


system('poweroff')

