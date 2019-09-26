library(dplyr)
library(mgcv)
library(parallel)

#Note, for some reason, bam() does NOT work with tidyverse and read_csv()
#MUST use dplyr and read.csv()

dat <- read.csv('/home/mattcoop/mortalityblob/dhs/GBV_all.csv') %>%
  filter(mean_annual_precip > 200 & builtup < 0.1)

dat$viol_any <- dat$viol_phys | dat$viol_sex

cl <- makeCluster(20)

###########################################
#Make a helper function
###########################################

runAndWrite <- function(name, formula){
  assign(name, bam(formula, data=dat, family='binomial', cluster=cl))
  save(list=name, file = paste0('/home/mattcoop/mortalityblob/gbv_gams/', name, '.Rdata'))
  rm(list=name)
}

##########################
#SPEI24
###########################

runAndWrite('spei24_phys', viol_phys ~ s(spei24) + s(latitude, longitude, bs='sos') + 
              wealth_factor_harmonized + hhsize + date_cmc + years_education + 
              country)

runAndWrite('spei24_sex', viol_sex ~ s(spei24) + s(latitude, longitude, bs='sos') + 
              wealth_factor_harmonized + hhsize + date_cmc + years_education + 
              country)

runAndWrite('spei24_any', viol_any ~ s(spei24) + s(latitude, longitude, bs='sos') + 
              wealth_factor_harmonized + hhsize + date_cmc + years_education + 
              country)

##########################
#temp12maxZ
###########################

runAndWrite('temp12maxZ_phys', viol_phys ~ s(temp12maxZ) + s(latitude, longitude, bs='sos') + 
              wealth_factor_harmonized + hhsize + date_cmc + years_education + 
              country)

runAndWrite('temp12maxZ_sex', viol_sex ~ s(temp12maxZ) + s(latitude, longitude, bs='sos') + 
              wealth_factor_harmonized + hhsize + date_cmc + years_education + 
              country)

runAndWrite('temp12maxZ_any', viol_any ~ s(temp12maxZ) + s(latitude, longitude, bs='sos') + 
              wealth_factor_harmonized + hhsize + date_cmc + years_education + 
              country)

##########################
#temp12max
###########################

runAndWrite('temp12max_phys', viol_phys ~ s(temp12max) + s(latitude, longitude, bs='sos') + 
              wealth_factor_harmonized + hhsize + date_cmc + years_education + 
              country)

runAndWrite('temp12max_sex', viol_sex ~ s(temp12max) + s(latitude, longitude, bs='sos') + 
              wealth_factor_harmonized + hhsize + date_cmc + years_education + 
              country)

runAndWrite('temp12max_any', viol_any ~ s(temp12max) + s(latitude, longitude, bs='sos') + 
              wealth_factor_harmonized + hhsize + date_cmc + years_education + 
              country)

##########################################
#Try Varying SPEI effect
##########################################


runAndWrite('spei24_phys_ve', viol_phys ~ s(spei24) + s(latitude, longitude, bs='sos') + s(latitude, longitude, by=spei24, bs='sos') +
              wealth_factor_harmonized + hhsize + date_cmc + years_education + 
              country)

runAndWrite('spei24_sex_ve', viol_sex ~ s(spei24) + s(latitude, longitude, bs='sos') + s(latitude, longitude, by=spei24, bs='sos') +
              wealth_factor_harmonized + hhsize + date_cmc + years_education + 
              country)

runAndWrite('spei24_any_ve', viol_any ~ s(spei24) + s(latitude, longitude, bs='sos') + s(latitude, longitude, by=spei24, bs='sos') +
              wealth_factor_harmonized + hhsize + date_cmc + years_education + 
              country)

system('~/telegram.sh "Done with SPEI splines"')


