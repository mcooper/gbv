setwd('G://My Drive/DHS Processed')

library(tidyverse)
library(lme4)
library(broom)
library(texreg)
library(ordinal)

options(stringsAsFactors=FALSE)

gbv <- read_csv('GBV_all.csv') %>%
  mutate(gbv_year_bin = gbv_year != 'never',
         gbv_year = factor(gbv_year, levels = c('never', 'sometimes', 'often')),
         date_cmc_res = (date_cmc - mean(date_cmc))/sd(date_cmc),
         hhsize_res = (hhsize - mean(hhsize, na.rm=T))/sd(hhsize, na.rm=T),
         surveycode=substr(code, 1, 6)) %>%
  select(gbv_year, gbv_year_bin, wealth_factor_harmonized, hhsize_res, date_cmc_res, country, 
         surveycode, spei12, spei24, spei36, spei48, spi12, spi24, spi36, spi48) %>%
  na.omit %>%
  filter(!is.infinite(spei36) & !is.infinite(spei48))

############################
#Binary Logit
########################
modspei12 <- glm(gbv_year_bin  ~ spei12 + 
                   wealth_factor_harmonized + hhsize_res + date_cmc_res + country, 
                 data=gbv, family = 'binomial')

modspei24 <- glm(gbv_year_bin  ~ spei24 + 
                   wealth_factor_harmonized + hhsize_res + date_cmc_res + country, 
                 data=gbv, family = 'binomial')

modspei36 <- glm(gbv_year_bin  ~ spei36 + 
                   wealth_factor_harmonized + hhsize_res + date_cmc_res + country, 
                 data=gbv, family = 'binomial')

modspei48 <- glm(gbv_year_bin  ~ spei48 + 
                   wealth_factor_harmonized + hhsize_res + date_cmc_res + country, 
                 data=gbv, family = 'binomial')

AIC(modspei12)
AIC(modspei24)
AIC(modspei36)
AIC(modspei48)

prep <- function(mod){
  df <- tidy(mod) %>%
    filter(grepl('^sp', term))
  df
}

comb <- Reduce(bind_rows, 
               Map(prep, list(modspei12, modspei24, modspei36, modspei48)))


