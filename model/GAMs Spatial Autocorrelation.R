if (Sys.info()['sysname']=='Linux'){
  data_dir <- '/home/mattcoop/mortalityblob/gbv'
}

library(dplyr)
library(mgcv)
library(parallel)

#Note, for some reason, bam() does NOT work with tidyverse and read_csv()
#MUST use dplyr and read.csv()

setwd(data_dir)

dat <- read.csv('GBV_all.csv')

cl <- makeCluster(16)

###########################################
#Make a helper function
###########################################

runAndWrite <- function(name, formula){
  assign(name, bam(formula, data=dat, family='binomial', cluster=cl))
  save(list=name, file = paste0('aprilmods/', name, '.Rdata'))
  rm(list=name)
}

##########################
#Compare All SPEI Windows
###########################

#12
runAndWrite('spei12_phys', viol_phys ~ s(spei12) + s(latitude, longitude, bs='sos') + 
              wealth_factor_harmonized + hhsize + date_cmc + woman_education_years  + 
              country)

runAndWrite('spei12_sex', viol_sex ~ s(spei12) + s(latitude, longitude, bs='sos') + 
              wealth_factor_harmonized + hhsize + date_cmc + woman_education_years  + 
              country)

#24
runAndWrite('spei24_phys', viol_phys ~ s(spei24) + s(latitude, longitude, bs='sos') + 
              wealth_factor_harmonized + hhsize + date_cmc + woman_education_years  + 
              country)

runAndWrite('spei24_sex', viol_sex ~ s(spei24) + s(latitude, longitude, bs='sos') + 
              wealth_factor_harmonized + hhsize + date_cmc + woman_education_years  + 
              country)

#36
runAndWrite('spei36_phys', viol_phys ~ s(spei36) + s(latitude, longitude, bs='sos') + 
              wealth_factor_harmonized + hhsize + date_cmc + woman_education_years  + 
              country)

runAndWrite('spei36_sex', viol_sex ~ s(spei36) + s(latitude, longitude, bs='sos') + 
              wealth_factor_harmonized + hhsize + date_cmc + woman_education_years  + 
              country)
			  
#48
runAndWrite('spei48_phys', viol_phys ~ s(spei48) + s(latitude, longitude, bs='sos') + 
              wealth_factor_harmonized + hhsize + date_cmc + woman_education_years  + 
              country)

runAndWrite('spei48_sex', viol_sex ~ s(spei48) + s(latitude, longitude, bs='sos') + 
              wealth_factor_harmonized + hhsize + date_cmc + woman_education_years  + 
              country)
			  

###################################
#Re-run basic models with cubic regression and p-splines, to test robustness
###################################
#CR
runAndWrite('spei12_phys_cr', viol_phys ~ s(spei12, bs='cr') + s(latitude, longitude, bs='sos') + 
              wealth_factor_harmonized + hhsize + date_cmc + woman_education_years  + 
              country)

runAndWrite('spei24_phys_cr', viol_phys ~ s(spei24, bs='cr') + s(latitude, longitude, bs='sos') + 
              wealth_factor_harmonized + hhsize + date_cmc + woman_education_years  + 
              country)

runAndWrite('spei36_phys_cr', viol_phys ~ s(spei36, bs='cr') + s(latitude, longitude, bs='sos') + 
              wealth_factor_harmonized + hhsize + date_cmc + woman_education_years  + 
              country)

runAndWrite('spei48_phys_cr', viol_phys ~ s(spei48, bs='cr') + s(latitude, longitude, bs='sos') + 
              wealth_factor_harmonized + hhsize + date_cmc + woman_education_years  + 
              country)

runAndWrite('spei12_sex_cr', viol_sex ~ s(spei12, bs='cr') + s(latitude, longitude, bs='sos') + 
              wealth_factor_harmonized + hhsize + date_cmc + woman_education_years  + 
              country)

runAndWrite('spei24_sex_cr', viol_sex ~ s(spei24, bs='cr') + s(latitude, longitude, bs='sos') + 
              wealth_factor_harmonized + hhsize + date_cmc + woman_education_years  + 
              country)

runAndWrite('spei36_sex_cr', viol_sex ~ s(spei36, bs='cr') + s(latitude, longitude, bs='sos') + 
              wealth_factor_harmonized + hhsize + date_cmc + woman_education_years  + 
              country)

runAndWrite('spei48_sex_cr', viol_sex ~ s(spei48, bs='cr') + s(latitude, longitude, bs='sos') + 
              wealth_factor_harmonized + hhsize + date_cmc + woman_education_years  + 
              country)
#PS
runAndWrite('spei12_phys_ps', viol_phys ~ s(spei12, bs='ps') + s(latitude, longitude, bs='sos') + 
              wealth_factor_harmonized + hhsize + date_cmc + woman_education_years  + 
              country)

runAndWrite('spei24_phys_ps', viol_phys ~ s(spei24, bs='ps') + s(latitude, longitude, bs='sos') + 
              wealth_factor_harmonized + hhsize + date_cmc + woman_education_years  + 
              country)

runAndWrite('spei36_phys_ps', viol_phys ~ s(spei36, bs='ps') + s(latitude, longitude, bs='sos') + 
              wealth_factor_harmonized + hhsize + date_cmc + woman_education_years  + 
              country)

runAndWrite('spei48_phys_ps', viol_phys ~ s(spei48, bs='ps') + s(latitude, longitude, bs='sos') + 
              wealth_factor_harmonized + hhsize + date_cmc + woman_education_years  + 
              country)

runAndWrite('spei12_sex_ps', viol_sex ~ s(spei12, bs='ps') + s(latitude, longitude, bs='sos') + 
              wealth_factor_harmonized + hhsize + date_cmc + woman_education_years  + 
              country)

runAndWrite('spei24_sex_ps', viol_sex ~ s(spei24, bs='ps') + s(latitude, longitude, bs='sos') + 
              wealth_factor_harmonized + hhsize + date_cmc + woman_education_years  + 
              country)

runAndWrite('spei36_sex_ps', viol_sex ~ s(spei36, bs='ps') + s(latitude, longitude, bs='sos') + 
              wealth_factor_harmonized + hhsize + date_cmc + woman_education_years  + 
              country)

runAndWrite('spei48_sex_ps', viol_sex ~ s(spei48, bs='ps') + s(latitude, longitude, bs='sos') + 
              wealth_factor_harmonized + hhsize + date_cmc + woman_education_years  + 
              country)

##################################################
#Try interactions with empowerment
##################################################

dat$empowered_education <- as.factor(dat$woman_education_years  %in% c("Secondary", "Higher"))
dat$empowered_age_marriage <- as.factor(dat$age_marriage >= 18)
dat$empowered_age_sex <- as.factor(dat$age_first_sex >= 18)
dat$empowered_gbv_notok <- as.factor(dat$empowered_gbv_notok)
dat$empowered_decisions <- as.factor(dat$empowered_decisions)


runAndWrite('spei36_phys_empowered_education', viol_phys ~ s(spei36, by=empowered_education) + s(latitude, longitude, bs='sos') + 
              wealth_factor_harmonized + hhsize + date_cmc + woman_education_years  + 
              country)

runAndWrite('spei36_phys_empowered_age_marriage', viol_phys ~ s(spei36, by=empowered_age_marriage) + s(latitude, longitude, bs='sos') + 
              wealth_factor_harmonized + hhsize + date_cmc + woman_education_years  + 
              country)

runAndWrite('spei36_phys_empowered_age_sex', viol_phys ~ s(spei36, by=empowered_age_sex) + s(latitude, longitude, bs='sos') + 
              wealth_factor_harmonized + hhsize + date_cmc + woman_education_years  + 
              country)

runAndWrite('spei36_phys_empowered_gbv_notok', viol_phys ~ s(spei36, by=empowered_gbv_notok) + s(latitude, longitude, bs='sos') + 
              wealth_factor_harmonized + hhsize + date_cmc + woman_education_years  + 
              country)

runAndWrite('spei36_phys_empowered_decisions', viol_phys ~ s(spei36, by=empowered_decisions) + s(latitude, longitude, bs='sos') + 
              wealth_factor_harmonized + hhsize + date_cmc + woman_education_years  + 
              country)

#########################################################################
#Try looking at whether woman or husband work in agricultural labor
##########################################################################

dat$ag_husband <- as.factor(dat$husband_works_agriculture == 'Agriculture')
dat$ag_woman <- as.factor(dat$woman_works_agriculture == 'Agriculture')

runAndWrite('spei36_phys_husb_ag', viol_phys ~ s(spei36, by=ag_husband) + s(latitude, longitude, bs='sos') + 
              wealth_factor_harmonized + hhsize + date_cmc + woman_education_years  + 
              country)

runAndWrite('spei36_phys_woman_ag', viol_phys ~ s(spei36, by=ag_woman) + s(latitude, longitude, bs='sos') + 
              wealth_factor_harmonized + hhsize + date_cmc + woman_education_years  + 
              country)

##########################################
#Try Varying SPEI effect
##########################################
runAndWrite('spei36_phys_ve_k200', viol_phys ~ s(spei36) + s(latitude, longitude, bs='sos', k=200) + s(latitude, longitude, by=spei36, bs='sos', k=200) +
              wealth_factor_harmonized + hhsize + date_cmc + woman_education_years  + 
              country)

######################################################
#Try Varying SPEI effect with country random effect
#######################################################
runAndWrite('spei36_phys_ve_k200_re', viol_phys ~ s(spei36) + s(latitude, longitude, bs='sos', k=200) + s(latitude, longitude, by=spei36, bs='sos', k=200) +
              s(country, by=spei36, bs='re') + 
              wealth_factor_harmonized + hhsize + date_cmc + woman_education_years  + 
              country)

system('~/telegram.sh "Done with SPEI splines"')




