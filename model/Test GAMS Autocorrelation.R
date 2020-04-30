library(mgcv)
library(parallel)
library(dplyr)
library(ggplot2)
library(ggeffects)
library(cowplot)

dat <- read.csv('G://My Drive/DHS Processed/GBV_all.csv')

dat$gbv_year <- dat$gbv_year != 'never'

dat <- dat %>%
  filter(!is.infinite(spei36) & !is.infinite(spei48))

cl <- makeCluster(4)

#########################
#Do GAM models
##########################

p1 <- bam(gbv_year ~ s(spei24) + mean_annual_precip + mean_annual_tmax + 
                    wealth_factor_harmonized + hhsize + date_cmc + years_education + 
                    country, data=dat, family = 'binomial', cluster=cl)

p2 <- bam(gbv_year ~ s(spei24) + s(latitude) + s(longitude) + mean_annual_precip + mean_annual_tmax + 
            wealth_factor_harmonized + hhsize + date_cmc + years_education, data=dat, family = 'binomial', cluster=cl)

p3 <- bam(gbv_year ~ s(spei24) + s(latitude, longitude) + mean_annual_precip + mean_annual_tmax + 
            wealth_factor_harmonized + hhsize + date_cmc + years_education, data=dat, family = 'binomial', cluster=cl)

p4 <- bam(gbv_year ~ s(spei24) + ti(latitude, longitude) + mean_annual_precip + mean_annual_tmax + 
            wealth_factor_harmonized + hhsize + date_cmc + years_education, data=dat, family = 'binomial', cluster=cl)

p5 <- bam(gbv_year ~ s(spei24) + s(latitude) + s(longitude) + mean_annual_precip + mean_annual_tmax + 
            wealth_factor_harmonized + hhsize + date_cmc + years_education + 
            country, data=dat, family = 'binomial', cluster=cl)

p6a <- bam(gbv_year ~ s(spei24) + s(latitude, longitude, bs='sos') + mean_annual_precip + mean_annual_tmax + 
            wealth_factor_harmonized + hhsize + date_cmc + years_education + 
            country, data=dat, family = 'binomial', cluster=cl)

p6b <- bam(gbv_year ~ s(spei24) + s(latitude, longitude, bs='sos') + s(country, bs='re') + mean_annual_precip + mean_annual_tmax + 
             wealth_factor_harmonized + hhsize + date_cmc + years_education, data=dat, family = 'binomial', cluster=cl)

p7 <- bam(gbv_year ~ s(spei24) + ti(latitude, longitude) + mean_annual_precip + mean_annual_tmax + 
            wealth_factor_harmonized + hhsize + date_cmc + years_education + 
            country, data=dat, family = 'binomial', cluster=cl)


t1 <- bam(gbv_year ~ s(temp12maxZ) + mean_annual_precip + mean_annual_tmax + 
            wealth_factor_harmonized + hhsize + date_cmc + years_education + 
            country, data=dat, family = 'binomial', cluster=cl)

t2 <- bam(gbv_year ~ s(temp12maxZ) + s(latitude) + s(longitude) + mean_annual_precip + mean_annual_tmax + 
            wealth_factor_harmonized + hhsize + date_cmc + years_education, data=dat, family = 'binomial', cluster=cl)

t3 <- bam(gbv_year ~ s(temp12maxZ) + s(latitude, longitude) + mean_annual_precip + mean_annual_tmax + 
            wealth_factor_harmonized + hhsize + date_cmc + years_education, data=dat, family = 'binomial', cluster=cl)

t4 <- bam(gbv_year ~ s(temp12maxZ) + ti(latitude, longitude) + mean_annual_precip + mean_annual_tmax + 
            wealth_factor_harmonized + hhsize + date_cmc + years_education, data=dat, family = 'binomial', cluster=cl)

t5 <- bam(gbv_year ~ s(temp12maxZ) + s(latitude) + s(longitude) + mean_annual_precip + mean_annual_tmax + 
            wealth_factor_harmonized + hhsize + date_cmc + years_education + 
            country, data=dat, family = 'binomial', cluster=cl)

t6a <- bam(gbv_year ~ s(temp12maxZ) + s(latitude, longitude, bs='sos') + mean_annual_precip + mean_annual_tmax + 
            wealth_factor_harmonized + hhsize + date_cmc + years_education + 
            country, data=dat, family = 'binomial', cluster=cl)

t6b <- bam(gbv_year ~ s(temp12maxZ) + s(latitude, longitude, bs='sos') + 
             s(country, bs='re') + 
             mean_annual_precip + mean_annual_tmax + 
             wealth_factor_harmonized + hhsize + date_cmc + years_education + 
             country, data=dat, family = 'binomial', cluster=cl)

t7 <- bam(gbv_year ~ s(temp12maxZ) + ti(latitude, longitude) + mean_annual_precip + mean_annual_tmax + 
            wealth_factor_harmonized + hhsize + date_cmc + years_education + 
            country, data=dat, family = 'binomial', cluster=cl)



AIC(p1)
AIC(p2)
AIC(p3)
AIC(p4)
AIC(p5)
AIC(p6a)
AIC(p6b)
AIC(p7)


AIC(t1)
AIC(t2)
AIC(t3)
AIC(t4)
AIC(t5)
AIC(t6a)
AIC(t6b)
AIC(t7)

> AIC(p1)
[1] 364127
> AIC(p2)
[1] 368129.8
> AIC(p3)
[1] 364482.2
> AIC(p4)
[1] 369652.3
> AIC(p5)
[1] 362534.7
> AIC(p6a)
[1] 361566.3
> AIC(p6b)
[1] 361609.4
> AIC(p7)
[1] 363349.8
> 
  > 
  > AIC(t1)
[1] 363860
> AIC(t2)
[1] 367735.9
> AIC(t3)
[1] 364301.7
> AIC(t4)
[1] 369622
> AIC(t5)
[1] 362543.3
> AIC(t6a)
[1] 361601.5
> AIC(t6b)
[1] 361601.5
> AIC(t7)
[1] 363254.8














