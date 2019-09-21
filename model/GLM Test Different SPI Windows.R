setwd('G://My Drive/DHS Processed')

library(tidyverse)
library(broom)
library(texreg)
library(MASS)

options(stringsAsFactors=FALSE)

gbv <- read.csv('GBV_all.csv') %>%
  mutate(surveycode=substr(code, 1, 6),
         mean_annual_precip=mean_annual_precip*1000,
         mean_annual_tmax=mean_annual_tmax*10) %>%
  na.omit %>%
  filter(!is.infinite(spei36) & !is.infinite(spei48))

############################
#Binary Logit
########################
modspei12 <- glm(viol_phys_ip  ~ years_education + 
                   wealth_factor_harmonized + hhsize + date_cmc + country + 
                   mean_annual_precip + mean_annual_tmax + spei12, 
                 data=gbv, family = 'binomial')

modspei24 <- glm(viol_phys_ip  ~ years_education +
                   wealth_factor_harmonized + hhsize + date_cmc + country + 
                   mean_annual_precip + mean_annual_tmax + spei24, 
                 data=gbv, family = 'binomial')

modspei36 <- glm(viol_phys_ip  ~ years_education + 
                   wealth_factor_harmonized + hhsize + date_cmc + country + 
                   mean_annual_precip + mean_annual_tmax + spei36, 
                 data=gbv, family = 'binomial')

modspei48 <- glm(viol_phys_ip  ~ years_education + 
                   wealth_factor_harmonized + hhsize + date_cmc + country + 
                   mean_annual_precip + mean_annual_tmax + spei48, 
                 data=gbv, family = 'binomial')

getORcoef <- function(mod){
  return(exp(mod$coefficients))
}

getORse <- function(mod){
  or <- getORcoef(mod)
  var.diag = diag(vcov(mod))
  or.se = sqrt(or^2 * var.diag)
  return(or.se)
}

texreg(l = list(modspei12, modspei24, modspei36, modspei48), 
       file = 'C://Users/matt/gbv-tex/tables/testspei.tex',
       custom.model.names=c('12 Months', '24 Months', '36 Months', '48 Months'),
       custom.coef.names=c('Intercept',
                           'Years of Education', 
                           'Harmonized Wealth Factor',
                           'Household Size',
                           'Century-Month-Code',
                           'Mean Annual Precipitation (1000 mm)',
                           'Mean Monthly Maximum Temp (10 $^{\\circ}$C)',
                           '12-Month SPEI',
                           '24-Month SPEI',
                           '36-Month SPEI',
                           '48-Month SPEI'),
       omit.coef='country',
       digits=3,
       override.coef=list(getORcoef(modspei12),
                          getORcoef(modspei24),
                          getORcoef(modspei36),
                          getORcoef(modspei48)),
       override.se=list(getORse(modspei12),
                        getORse(modspei24),
                        getORse(modspei36),
                        getORse(modspei48)),
       caption='Modeling the relationship between rainfall anomalies (SPEI) and women experiencing GBV in the previous year at 12, 24, 36, and 48-month timescales.  Reported coefficients and confidence intervals are in odd-ratios',
       label = 'table:testspei',
       float.pos = 'H')
