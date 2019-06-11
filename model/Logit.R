setwd('G://My Drive/DHS Processed')

library(tidyverse)
library(lme4)
library(mgcv)
library(MASS)
library(ordinal)

gbv <- read.csv('GBV_all.csv')



gbv$gbv_year <- factor(gbv$gbv_year, levels = c('never', 'sometimes', 'often'))

mod12 <- polr(gbv_year ~ temp12monthZ +
                wealth_factor_harmonized + hhsize + 
                empowered_decisions + empowered_gbv_notok, data=gbv)

mod_all <- polr(gbv_year ~ all_Zscore +
                wealth_factor_harmonized + hhsize + 
                empowered_decisions + empowered_gbv_notok, data=gbv)

mod_abs <- polr(gbv_year ~ running_mean +
                wealth_factor_harmonized + hhsize + 
                empowered_decisions + empowered_gbv_notok, data=gbv)



mod_fe <- clmm(gbv_year ~ (temp12monthZ|country) + (temp12monthZ|country) + 
                  wealth_factor_harmonized + hhsize + 
                  empowered_decisions + empowered_gbv_notok, data=gbv, Hess = TRUE)

summary(mod_fe)


mod_fe2 <- clmm(gbv_year ~ temp12monthZ + (temp12monthZ|country) + 
                 wealth_factor_harmonized + hhsize + 
                 empowered_decisions + empowered_gbv_notok, data=gbv, Hess = TRUE)

summary(mod_fe2)


#High empowerment
mod_he <- clmm(gbv_year ~ temp12monthZ + (1|country) + 
                 wealth_factor_harmonized + hhsize + empowered_gbv_notok, 
               data=gbv %>% filter(empowered_gbv_notok), 
               Hess = TRUE)

summary(mod_he)

#Low empowerment
mod_le <- clmm(gbv_year ~ temp12monthZ + (1|country) + 
                 wealth_factor_harmonized + hhsize + empowered_gbv_notok, 
               data=gbv %>% filter(!empowered_gbv_notok), 
               Hess = TRUE)

summary(mod_le)


#Empowerment RE
mod_re <- clmm(gbv_year ~ (temp12monthZ|empowered_gbv_notok) + (1|country) + 
                 wealth_factor_harmonized + hhsize + empowered_gbv_notok, 
               data=gbv, 
               Hess = TRUE)

summary(mod_re)


gbv %>% filter(latitude < 10 & latitude > 8 & longitude < 75 & longitude > 70)
