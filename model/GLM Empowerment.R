setwd('G://My Drive/DHS Processed')

library(tidyverse)
library(broom)
library(texreg)
library(MASS)

options(stringsAsFactors=FALSE)

gbv <- read.csv('GBV_all.csv') %>%
  filter(mean_annual_precip > 200 & builtup < 0.1)

gbv$viol_any <- gbv$viol_sex | gbv$viol_phys

gbv$empowered_education <- gbv$years_education %in% c("Secondary", "Higher")
gbv$empowered_age_marriage <- gbv$age_marriage >= 18
gbv$empowered_age_sex <- gbv$age_first_sex >= 18

mod_gbv_notok <- glm(viol_phys  ~ years_education + 
                      wealth_factor_harmonized + hhsize + date_cmc + country + 
                      mean_annual_precip + mean_annual_tmax + spei36*empowered_gbv_notok, 
                    data=gbv, family = 'binomial')

mod_decisions <- glm(viol_phys  ~ years_education + 
                      wealth_factor_harmonized + hhsize + date_cmc + country + 
                      mean_annual_precip + mean_annual_tmax + spei36*empowered_decisions, 
                    data=gbv, family = 'binomial')

mod_education <- glm(viol_phys  ~ years_education + 
                      wealth_factor_harmonized + hhsize + date_cmc + country + 
                      mean_annual_precip + mean_annual_tmax + spei36*years_education, 
                    data=gbv, family = 'binomial')

mod_age_marriage <- glm(viol_phys  ~ years_education + 
                      wealth_factor_harmonized + hhsize + date_cmc + country + 
                      mean_annual_precip + mean_annual_tmax + spei36*empowered_age_marriage, 
                    data=gbv, family = 'binomial')

mod_age_sex <- glm(viol_phys  ~ years_education + 
                      wealth_factor_harmonized + hhsize + date_cmc + country + 
                      mean_annual_precip + mean_annual_tmax + spei36*empowered_age_sex, 
                    data=gbv, family = 'binomial')



