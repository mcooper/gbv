setwd('G://My Drive/DHS Processed')

library(dplyr)
library(lme4)
library(texreg)

gbv <- read.csv('GBV_all.csv', stringsAsFactors=F)

gbv$gbv_year <- gbv$gbv_year != 'never'

gbv$surveycode <- substr(gbv$code, 1, 6)

gbv$survey_year <- floor(gbv$date_cmc/12) + 1900

gbv <- gbv %>%
  filter(!is.na(wealth_factor_harmonized) & !is.na(hhsize) & !is.infinite(spei36))

gbv$date_cmc_res <- (gbv$date_cmc - mean(gbv$date_cmc))/sd(gbv$date_cmc)

gbv$hhsize_res <- (gbv$hhsize - mean(gbv$hhsize))/sd(gbv$hhsize)


#############################
#Temperature + SPEI
#############################
#Base level model
mod_spi <- glm(gbv_year ~ temp12max + spei36 + temp12maxZ + 
              wealth_factor_harmonized + hhsize_res + date_cmc_res +
              country, data=gbv, family = 'binomial')
summary(mod_spi)

#Model for empowered with decision-making
mod_spi_decisions <- glm(gbv_year ~ temp12max*empowered_decisions + spei36*empowered_decisions + temp12maxZ*empowered_decisions + 
                      wealth_factor_harmonized + hhsize_res + date_cmc_res +
                        country, data=gbv, family = 'binomial')
summary(mod_spi_decisions)

#Model for empowered by disbelieving in GBV being OK
mod_spi_w_gbv_notok <- glm(gbv_year ~ temp12max*empowered_gbv_notok + spei36*empowered_gbv_notok + temp12maxZ*empowered_gbv_notok + 
                        wealth_factor_harmonized + hhsize_res + date_cmc_res +
                          country, data=gbv, family = 'binomial')
summary(mod_spi_w_gbv_notok)

