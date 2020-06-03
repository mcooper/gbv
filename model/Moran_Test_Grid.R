library(tidyverse)
library(ape)

setwd('~/mortalityblob/')

#############################
#Read in Data
################################
dat <- read.csv('gbv/GBV_sel.csv')
dat$in_plos_paper <- dat$survey_code %in% c("SL-6-1", "TG-6-1", "BJ-7-1", "CI-6-1", "CM-6-1",
                                            "GA-6-1", "TD-7-1", "CD-6-1", "RW-7-1", "BU-7-1", 
                                            "UG-7-2", "KE-7-1", "TZ-7-2", "MW-7-2", "MZ-6-1",
                                            "ZW-7-1", "ZM-6-1", "NM-6-1", "AO-7-1")
dat$in_afr <- dat$latitude < 23 & dat$longitude > -20 & dat$longitude < 50
dat$in_cty <- dat$country %in% c("SL", "TG", "BJ", "CI", "CM",
                                 "GA", "TD", "CD", "RW", "BU", 
                                 "UG", "KE", "TZ", "MW", "MZ",
                                 "ZW", "ZM", "NM", "AO")

##################################
#Get residuals for each model
################################
resids <- dat %>%
	select(ind_code, code, latitude, longitude)

for (mod in list.files('gbv_gams', pattern='RDS', full.names=T)){
	print(mod)

	modvar <- substr(mod, 10, nchar(mod)-4)

	assign(modvar, readRDS(mod))	

  if (grepl('all', mod)){
    sel <- dat %>%
			select(ind_code)
		sel[ , mod] <- residuals(eval(parse(text=modvar)))
  }
  if (grepl('afr', mod)){
    sel <- dat %>% 
			filter(in_afr) %>%
			select(ind_code)
		sel[ , mod] <- residuals(eval(parse(text=modvar)))
  }
  if (grepl('cty', mod)){
    sel <- dat %>% 
			filter(in_cty) %>%
			select(ind_code)
		sel[ , mod] <- residuals(eval(parse(text=modvar)))
  }
  if (grepl('_plos_', mod)){
    sel <- dat %>% 
			filter(in_plos_paper) %>%
			select(ind_code)
		sel[ , mod] <- residuals(eval(parse(text=modvar)))
  }
	
	resids <- merge(resids, sel, all.x=T, all.y=T)
}

resid_sum <- resids %>%
	mutate(latitude = round(latitude, 0),
				 longitude = round(longitude, 0)) %>%
	group_by(latitude, longitude) %>%
	summarize_if(is.numeric, mean) %>%
	ungroup %>%
	mutate(code = 1:n())

dmat <- as.matrix(dist(resid_sum[ , c('longitude', 'latitude')]))
dmat <- 1/dmat
diag(dmat) <- 0

######################################
# Run Moran's I Tests for DHS site residuals
######################################

moran_df <- data.frame()
for (col in names(resid_sum)[!names(resid_sum) %in% c('latitude', 'longitude', 'code')]){
	print(col)
	sel <- resid_sum[!is.na(resid_sum[ , col]), ]
	
	dmatsel <- dmat[rownames(dmat) %in% sel$code, colnames(dmat) %in% sel$code]
	
	mi <- data.frame(Moran.I(sel[ , col, drop=T], dmatsel))
	mi$mod <- col

	moran_df <- bind_rows(moran_df, mi)
}


