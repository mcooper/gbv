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
modspei12ord <- clmm(gbv_year ~ spei12 + 
                   wealth_factor_harmonized + hhsize_res + date_cmc_res + (1|country) + (1|surveycode), 
                 data=gbv, Hess=TRUE)

modspei24ord <- clmm(gbv_year  ~ spei24 + 
                   wealth_factor_harmonized + hhsize_res + date_cmc_res + (1|country) + (1|surveycode), 
                 data=gbv, Hess=TRUE)

modspei36ord <- clmm(gbv_year  ~ spei36 + 
                   wealth_factor_harmonized + hhsize_res + date_cmc_res + (1|country) + (1|surveycode), 
                 data=gbv, Hess=TRUE)

modspei48ord <- clmm(gbv_year  ~ spei48 + 
                   wealth_factor_harmonized + hhsize_res + date_cmc_res + (1|country) + (1|surveycode), 
                 data=gbv, Hess=TRUE)

modspi12ord <- clmm(gbv_year  ~ spi12 + 
                  wealth_factor_harmonized + hhsize_res + date_cmc_res + (1|country) + (1|surveycode), 
                data=gbv, Hess=TRUE)

modspi24ord <- clmm(gbv_year  ~ spi24 + 
                  wealth_factor_harmonized + hhsize_res + date_cmc_res + (1|country) + (1|surveycode), 
                data=gbv, Hess=TRUE)

modspi36ord <- clmm(gbv_year  ~ spi36 + 
                  wealth_factor_harmonized + hhsize_res + date_cmc_res + (1|country) + (1|surveycode), 
                data=gbv, Hess=TRUE)

modspi48ord <- clmm(gbv_year  ~ spi48 + 
                  wealth_factor_harmonized + hhsize_res + date_cmc_res + (1|country) + (1|surveycode), 
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

# > AIC(modspei12ord)
# [1] 651669
# > AIC(modspei24ord)
# [1] 651636.6
# > AIC(modspei36ord)
# [1] 651652.6
# > AIC(modspei48ord)
# [1] 651679.4
# > AIC(modspi12ord)
# [1] 651648.8
# > AIC(modspi24ord)
# [1] 651599.8
# > AIC(modspi36ord)
# [1] 651576.4
# > AIC(modspi48ord)
# [1] 651625.1

# > comb
#       Estimate  Std. Error    z value     Pr(>|z|)   spei
# 1 -0.017021035 0.004878042  -3.489317 4.842559e-04 spei12
# 2 -0.033273075 0.004985135  -6.674458 2.481468e-11 spei24
# 3 -0.026322866 0.004923267  -5.346626 8.960913e-08 spei36
# 4 -0.006368822 0.004820350  -1.321236 1.864226e-01 spei48
# 5 -0.027643814 0.004856288  -5.692375 1.252843e-08  spi12
# 6 -0.045580254 0.005054346  -9.018032 1.914984e-19  spi24
# 7 -0.051839834 0.005064386 -10.236155 1.365564e-24  spi36
# 8 -0.037572500 0.005017006  -7.489028 6.938539e-14  spi48
