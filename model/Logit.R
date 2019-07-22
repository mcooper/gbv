setwd('G://My Drive/GBV')

library(dplyr)
library(lme4)
library(texreg)

gbv <- read.csv('../DHS Processed/GBV_all.csv', stringsAsFactors=F)

gbv$gbv_year <- gbv$gbv_year != 'never'

gbv$surveycode <- substr(gbv$code, 1, 6)

gbv$survey_year <- floor(gbv$date_cmc/12) + 1900
   
gbv <- gbv %>%
  filter(!is.na(wealth_factor_harmonized) & !is.na(hhsize) & !is.infinite(spei24))

gbv$date_cmc_res <- (gbv$date_cmc - mean(gbv$date_cmc))/sd(gbv$date_cmc)

gbv$hhsize_res <- (gbv$hhsize - mean(gbv$hhsize))/sd(gbv$hhsize)

gbv$temp12maxZ_split <- gbv$temp12maxZ > 1.5
gbv$spei24_split <- gbv$spei24 < -1.5

#############################
#Temperature + SPEI
#############################
#Base level model
mod_spi <- glm(gbv_year ~ spei24 + mean_annual_precip + mean_annual_tmax + 
              wealth_factor_harmonized + hhsize + date_cmc + years_education, data=gbv, family = 'binomial')
summary(mod_spi)


#Model for empowered with decision-making
mod_spi_decisions <- glm(gbv_year ~ spei24*empowered_decisions + mean_annual_precip + mean_annual_tmax + 
                           wealth_factor_harmonized + hhsize + date_cmc + years_education, data=gbv, family = 'binomial')
summary(mod_spi_decisions)

#Model for empowered by disbelieving in GBV being OK
mod_spi_w_gbv_notok <- glm(gbv_year ~ spei24*empowered_gbv_notok + mean_annual_precip + mean_annual_tmax + 
                             wealth_factor_harmonized + hhsize + date_cmc + years_education + 
                             country, data=gbv, family = 'binomial')
summary(mod_spi_w_gbv_notok)

