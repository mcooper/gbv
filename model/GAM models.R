library(mgcv)
library(parallel)

dat <- read.csv('/home/mattcoop/GBV_all.csv')

cl <- makeCluster(4)

mod_spi <- bam(gbv_year ~ s(spei24) + mean_annual_precip + mean_annual_tmax + 
                 wealth_factor_harmonized + hhsize + date_cmc + years_education + 
                 country, data=dat, family = 'binomial', cluster=cl)
summary(mod_spi)


mod_temp <- bam(gbv_year ~ s(temp12maxZ) + mean_annual_precip + mean_annual_tmax + 
                 wealth_factor_harmonized + hhsize + date_cmc + years_education + 
                 country, data=dat, family = 'binomial', cluster=cl)
summary(mod_temp)


mod_temp_abs <- bam(gbv_year ~ s(temp12max) + mean_annual_precip + mean_annual_tmax + 
                  wealth_factor_harmonized + hhsize + date_cmc + years_education + 
                  country, data=dat, family = 'binomial', cluster=cl)
summary(mod_temp_abs)

system('~/telegram.sh "Smooths done"')
