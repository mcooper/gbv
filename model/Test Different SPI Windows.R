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
         hhsize_res = (hhsize - mean(hhsize, na.rm=T))/sd(hhsize, na.rm=T)) %>%
  select(gbv_year, gbv_year_bin, wealth_factor_harmonized, hhsize_res, date_cmc_res, country, 
         spei12, spei24, spei36, spei48, spi12, spi24, spi36, spi48) %>%
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

modspi12 <- glm(gbv_year_bin  ~ spi12 + 
                  wealth_factor_harmonized + hhsize_res + date_cmc_res + country, 
                data=gbv, family = 'binomial')

modspi24 <- glm(gbv_year_bin  ~ spi24 + 
                  wealth_factor_harmonized + hhsize_res + date_cmc_res + country, 
                data=gbv, family = 'binomial')

modspi36 <- glm(gbv_year_bin  ~ spi36 + 
                  wealth_factor_harmonized + hhsize_res + date_cmc_res + country, 
                data=gbv, family = 'binomial')

modspi48 <- glm(gbv_year_bin  ~ spi48 + 
                  wealth_factor_harmonized + hhsize_res + date_cmc_res + country, 
                data=gbv, family = 'binomial')

AIC(modspei12)
AIC(modspei24)
AIC(modspei36)
AIC(modspei48)
AIC(modspi12)
AIC(modspi24)
AIC(modspi36)
AIC(modspi48)

prep <- function(mod){
  df <- tidy(mod) %>%
    filter(grepl('^sp', term))
  df
}

comb <- Reduce(bind_rows, 
               Map(prep, list(modspei12, modspei24, modspei36, modspei48, modspi12, modspi24, modspi36, modspi48)))



############################
#Ordinal Logit
########################
modspei12ord <- clm(gbv_year ~ spei12 + 
                   wealth_factor_harmonized + hhsize_res + date_cmc_res + country, 
                 data=gbv, Hess=TRUE)

modspei24ord <- clm(gbv_year  ~ spei24 + 
                   wealth_factor_harmonized + hhsize_res + date_cmc_res + country, 
                 data=gbv, Hess=TRUE)

modspei36ord <- clm(gbv_year  ~ spei36 + 
                   wealth_factor_harmonized + hhsize_res + date_cmc_res + country, 
                 data=gbv, Hess=TRUE)

modspei48ord <- clm(gbv_year  ~ spei48 + 
                   wealth_factor_harmonized + hhsize_res + date_cmc_res + country, 
                 data=gbv, Hess=TRUE)

modspi12ord <- clm(gbv_year  ~ spi12 + 
                  wealth_factor_harmonized + hhsize_res + date_cmc_res + country, 
                data=gbv, Hess=TRUE)

modspi24ord <- clm(gbv_year  ~ spi24 + 
                  wealth_factor_harmonized + hhsize_res + date_cmc_res + country, 
                data=gbv, Hess=TRUE)

modspi36ord <- clm(gbv_year  ~ spi36 + 
                  wealth_factor_harmonized + hhsize_res + date_cmc_res + country, 
                data=gbv, Hess=TRUE)

modspi48ord <- clm(gbv_year  ~ spi48 + 
                  wealth_factor_harmonized + hhsize_res + date_cmc_res + country, 
                data=gbv, Hess=TRUE)

AIC(modspei12ord)
AIC(modspei24ord)
AIC(modspei36ord)
AIC(modspei48ord)
AIC(modspi12ord)
AIC(modspi24ord)
AIC(modspi36ord)
AIC(modspi48ord)

prep2 <- function(mod){
  df <- summary(mod) %>%
    .$coefficients %>%
    as.data.frame %>%
    .[3, ]
  df$spei <- row.names(df)
  df
}

comb <- Reduce(bind_rows, 
               Map(prep2, list(modspei12ord, modspei24ord, modspei36ord, modspei48ord, modspi12ord, modspi24ord, modspi36ord, modspi48ord)))


