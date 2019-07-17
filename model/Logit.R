setwd('G://My Drive/GBV')

library(dplyr)
library(lme4)
library(texreg)

gbv <- read.csv('../DHS Processed/GBV_all.csv', stringsAsFactors=F)
ltn <- read.csv('GBV_LTNs.csv') %>%
  dplyr::select(code, mean_annual_precip, mean_annual_tmax, mean_annual_tmin) %>%
  unique

gbv <- merge(gbv, ltn, all.x=T, all.y=F)

gbv$gbv_year <- gbv$gbv_year != 'never'

gbv$surveycode <- substr(gbv$code, 1, 6)

gbv$survey_year <- floor(gbv$date_cmc/12) + 1900

gbv <- gbv %>%
  filter(!is.na(wealth_factor_harmonized) & !is.na(hhsize) & !is.infinite(spei36))

gbv$date_cmc_res <- (gbv$date_cmc - mean(gbv$date_cmc))/sd(gbv$date_cmc)

gbv$hhsize_res <- (gbv$hhsize - mean(gbv$hhsize))/sd(gbv$hhsize)

gbv$temp12maxZ_split <- gbv$temp12maxZ > 1.5
gbv$spei36_split <- gbv$spei36 < -1.5

#############################
#Temperature + SPEI
#############################
#Base level model
mod_spi <- glm(gbv_year ~ spei36_split + temp12maxZ_split + mean_annual_precip + mean_annual_tmax + 
              wealth_factor_harmonized + hhsize + date_cmc +
              country, data=gbv, family = 'binomial')
summary(mod_spi)

#Base level model
mod_spi <- glm(gbv_year ~ spei36_split + temp12maxZ_split + 
                 wealth_factor_harmonized + hhsize + date_cmc +
                 country, data=gbv, family = 'binomial')
summary(mod_spi)


#Model for empowered with decision-making
mod_spi_decisions <- glm(gbv_year ~ temp12max*empowered_decisions + spei36*empowered_decisions +
                      wealth_factor_harmonized + hhsize + date_cmc +
                        country, data=gbv, family = 'binomial')
summary(mod_spi_decisions)

#Model for empowered by disbelieving in GBV being OK
mod_spi_w_gbv_notok <- glm(gbv_year ~ temp12max*empowered_gbv_notok + spei36*empowered_gbv_notok +  
                        wealth_factor_harmonized + hhsize + date_cmc +
                          country, data=gbv, family = 'binomial')
summary(mod_spi_w_gbv_notok)

