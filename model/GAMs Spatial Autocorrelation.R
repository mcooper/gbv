library(dplyr)
library(mgcv)
library(parallel)

#Note, for some reason, bam() does NOT work with tidyverse and read_csv()
#MUST use dplyr and read.csv()

dat <- read.csv('~/../mattcoop/mortalityblob/dhs/GBV_all.csv')

dat <- dat %>%
  filter(!is.infinite(spei36) & !is.infinite(spei48))

dat$vio_any <- dat$viol_phys_nip | dat$viol_phys_ip | dat$viol_sex_ip

cl <- makeCluster(12)

##########################
#SPEI24
###########################

spei24_viol_phys_nip <- bam(viol_phys_nip ~ s(spei24) + s(latitude, longitude, bs='sos') + 
                   mean_annual_precip + mean_annual_tmax + 
             wealth_factor_harmonized + hhsize + date_cmc + years_education + 
             country, data=dat, family = 'binomial', cluster=cl)
save(spei24_viol_phys_nip, file = '/home/mattcoop/mortalityblob/gbv_gams/spei24_viol_phys_nip.Rdata')
rm(spei24_viol_phys_nip)


spei24_viol_phys_ip <- bam(viol_phys_ip ~ s(spei24) + s(latitude, longitude, bs='sos') + 
                   mean_annual_precip + mean_annual_tmax + 
                   wealth_factor_harmonized + hhsize + date_cmc + years_education + 
                   country, data=dat, family = 'binomial', cluster=cl)
save(spei24_viol_phys_ip, file = '/home/mattcoop/mortalityblob/gbv_gams/spei24_viol_phys_ip_ac.Rdata')
rm(spei24_viol_phys_ip)


spei24_viol_sex_ip <- bam(viol_sex_ip ~ s(spei24) + s(latitude, longitude, bs='sos') + 
                   mean_annual_precip + mean_annual_tmax + 
                   wealth_factor_harmonized + hhsize + date_cmc + years_education + 
                   country, data=dat, family = 'binomial', cluster=cl)
save(spei24_viol_sex_ip, file = '/home/mattcoop/mortalityblob/gbv_gams/spei24_viol_sex_ip.Rdata')
rm(spei24_viol_sex_ip)


spei24_viol_any <- bam(viol_any ~ s(spei24) + s(latitude, longitude, bs='sos') + 
                            mean_annual_precip + mean_annual_tmax + 
                            wealth_factor_harmonized + hhsize + date_cmc + years_education + 
                            country, data=dat, family = 'binomial', cluster=cl)
save(spei24_viol_any, file = '/home/mattcoop/mortalityblob/gbv_gams/spei24_viol_any.Rdata')
rm(spei24_viol_any)


system('~/telegram.sh "Done with SPEI splines"')


##########################
#temp12maxZ
###########################

temp12maxZ_viol_phys_nip <- bam(viol_phys_nip ~ s(temp12maxZ) + s(latitude, longitude, bs='sos') + 
                              mean_annual_precip + mean_annual_tmax + 
                              wealth_factor_harmonized + hhsize + date_cmc + years_education + 
                              country, data=dat, family = 'binomial', cluster=cl)
save(temp12maxZ_viol_phys_nip, file = '/home/mattcoop/mortalityblob/gbv_gams/temp12maxZ_viol_phys_nip.Rdata')
rm(temp12maxZ_viol_phys_nip)


temp12maxZ_viol_phys_ip <- bam(viol_phys_ip ~ s(temp12maxZ) + s(latitude, longitude, bs='sos') + 
                             mean_annual_precip + mean_annual_tmax + 
                             wealth_factor_harmonized + hhsize + date_cmc + years_education + 
                             country, data=dat, family = 'binomial', cluster=cl)
save(temp12maxZ_viol_phys_ip, file = '/home/mattcoop/mortalityblob/gbv_gams/temp12maxZ_viol_phys_ip.Rdata')
rm(temp12maxZ_viol_phys_ip)


temp12maxZ_viol_sex_ip <- bam(viol_sex_ip ~ s(temp12maxZ) + s(latitude, longitude, bs='sos') + 
                            mean_annual_precip + mean_annual_tmax + 
                            wealth_factor_harmonized + hhsize + date_cmc + years_education + 
                            country, data=dat, family = 'binomial', cluster=cl)
save(temp12maxZ_viol_sex_ip, file = '/home/mattcoop/mortalityblob/gbv_gams/temp12maxZ_viol_sex_ip.Rdata')
rm(temp12maxZ_viol_sex_ip)


temp12maxZ_viol_any <- bam(viol_any ~ s(temp12maxZ) + s(latitude, longitude, bs='sos') + 
                         mean_annual_precip + mean_annual_tmax + 
                         wealth_factor_harmonized + hhsize + date_cmc + years_education + 
                         country, data=dat, family = 'binomial', cluster=cl)
save(temp12maxZ_viol_any, file = '/home/mattcoop/mortalityblob/gbv_gams/temp12maxZ_viol_any.Rdata')
rm(temp12maxZ_viol_any)



##########################
#Temperature No CO
###########################

tempZ_ac_noco <- bam(gbv_year ~ s(temp12maxZ) + s(latitude, longitude, bs='sos') + mean_annual_precip + mean_annual_tmax + 
                  wealth_factor_harmonized + hhsize + date_cmc + years_education + 
                  country, data=dat %>% filter(country != 'CO'), family = 'binomial', cluster=cl)
save(tempZ_ac_noco, file = '/home/mattcoop/mortalityblob/gbv_gams/tempZ_ac_noco.Rdata')
rm(tempZ_ac_noco)

tempZ_ac_vc_noco <- bam(gbv_year ~ s(latitude, longitude, by=temp12maxZ) + s(latitude, longitude, bs='sos') + mean_annual_precip + mean_annual_tmax + 
                     wealth_factor_harmonized + hhsize + date_cmc + years_education + 
                     country, data=dat %>% filter(country != 'CO'), family = 'binomial', cluster=cl)
save(tempZ_ac_vc_noco, file = '/home/mattcoop/mortalityblob/gbv_gams/tempZ_ac_vc_noco.Rdata')
rm(tempZ_ac_vc_noco)

tempZ_ac_vc_fe_noco <- bam(gbv_year ~ s(temp12maxZ) + s(latitude, longitude, by=temp12maxZ) + s(latitude, longitude, bs='sos') + mean_annual_precip + mean_annual_tmax + 
                        wealth_factor_harmonized + hhsize + date_cmc + years_education + 
                        country, data=dat %>% filter(country != 'CO'), family = 'binomial', cluster=cl)
save(tempZ_ac_vc_fe_noco, file = '/home/mattcoop/mortalityblob/gbv_gams/tempZ_ac_vc_fe_noco.Rdata')
rm(tempZ_ac_vc_fe_noco)

system('~/telegram.sh "Done with Temp No CO splines"')

##########################
#Empowerment 
###########################

spei24_ac_att <- bam(gbv_year ~ s(spei24, by=empowered_gbv_notok) + s(latitude, longitude, bs='sos') + mean_annual_precip + mean_annual_tmax + 
                   wealth_factor_harmonized + hhsize + date_cmc + years_education + 
                   country, data=dat, family = 'binomial', cluster=cl)
save(spei24_ac_att, file = '/home/mattcoop/mortalityblob/gbv_gams/spei24_ac_att.Rdata')
rm(spei24_ac_att)

tempZ_ac_att <- bam(gbv_year ~ s(temp12maxZ, by=empowered_gbv_notok) + s(latitude, longitude, bs='sos') + mean_annual_precip + mean_annual_tmax + 
                  wealth_factor_harmonized + hhsize + date_cmc + years_education + 
                  country, data=dat, family = 'binomial', cluster=cl)
save(tempZ_ac_att, file = '/home/mattcoop/mortalityblob/gbv_gams/tempZ_ac_att.Rdata')
rm(tempZ_ac_att)

spei24_ac_desc <- bam(gbv_year ~ s(spei24, by=empowered_decisions) + s(latitude, longitude, bs='sos') + mean_annual_precip + mean_annual_tmax + 
                       wealth_factor_harmonized + hhsize + date_cmc + years_education + 
                       country, data=dat, family = 'binomial', cluster=cl)
save(spei24_ac_desc, file = '/home/mattcoop/mortalityblob/gbv_gams/spei24_ac_desc.Rdata')
rm(spei24_ac_desc)

tempZ_ac_desc <- bam(gbv_year ~ s(temp12maxZ, by=empowered_decisions) + s(latitude, longitude, bs='sos') + mean_annual_precip + mean_annual_tmax + 
                      wealth_factor_harmonized + hhsize + date_cmc + years_education + 
                      country, data=dat, family = 'binomial', cluster=cl)
save(tempZ_ac_desc, file = '/home/mattcoop/mortalityblob/gbv_gams/tempZ_ac_desc.Rdata')
rm(tempZ_ac_desc)

spei24_ac_urban <- bam(gbv_year ~ s(spei24, by=urban) + s(latitude, longitude, bs='sos') + mean_annual_precip + mean_annual_tmax + 
                        wealth_factor_harmonized + hhsize + date_cmc + years_education + 
                        country, data=dat, family = 'binomial', cluster=cl)
save(spei24_ac_urban, file = '/home/mattcoop/mortalityblob/gbv_gams/spei24_ac_urban.Rdata')
rm(spei24_ac_urban)

tempZ_ac_urban <- bam(gbv_year ~ s(temp12maxZ, by=urban) + s(latitude, longitude, bs='sos') + mean_annual_precip + mean_annual_tmax + 
                       wealth_factor_harmonized + hhsize + date_cmc + years_education + 
                       country, data=dat, family = 'binomial', cluster=cl)
save(tempZ_ac_urban, file = '/home/mattcoop/mortalityblob/gbv_gams/tempZ_ac_urban.Rdata')
rm(tempZ_ac_urban)

system('~/telegram.sh "Done with Interaction splines splines"')


##########################
#Empowerment No CO
###########################

tempZ_ac_att <- bam(gbv_year ~ s(temp12maxZ, by=empowered_gbv_notok) + s(latitude, longitude, bs='sos') + mean_annual_precip + mean_annual_tmax + 
                      wealth_factor_harmonized + hhsize + date_cmc + years_education + 
                      country, data=dat %>% filter(country != 'CO'), family = 'binomial', cluster=cl)
save(tempZ_ac_att, file = '/home/mattcoop/mortalityblob/gbv_gams/tempZ_ac_att.Rdata')
rm(tempZ_ac_att)

tempZ_ac_desc <- bam(gbv_year ~ s(temp12maxZ, by=empowered_decisions) + s(latitude, longitude, bs='sos') + mean_annual_precip + mean_annual_tmax + 
                       wealth_factor_harmonized + hhsize + date_cmc + years_education + 
                       country, data=dat %>% filter(country != 'CO'), family = 'binomial', cluster=cl)
save(tempZ_ac_desc, file = '/home/mattcoop/mortalityblob/gbv_gams/tempZ_ac_desc.Rdata')
rm(tempZ_ac_desc)

tempZ_ac_urban <- bam(gbv_year ~ s(temp12maxZ, by=urban) + s(latitude, longitude, bs='sos') + mean_annual_precip + mean_annual_tmax + 
                        wealth_factor_harmonized + hhsize + date_cmc + years_education + 
                        country, data=dat %>% filter(country != 'CO'), family = 'binomial', cluster=cl)
save(tempZ_ac_urban, file = '/home/mattcoop/mortalityblob/gbv_gams/tempZ_ac_urban.Rdata')
rm(tempZ_ac_urban)

system('~/telegram.sh "Done with Interaction No CO splines"')

############################
#Test different m values for sos
############################

spei24_ac_m0 <- bam(gbv_year ~ s(spei24) + s(latitude, longitude, bs='sos', m=0) + mean_annual_precip + mean_annual_tmax + 
                   wealth_factor_harmonized + hhsize + date_cmc + years_education + 
                   country, data=dat, family = 'binomial', cluster=cl)
save(spei24_ac_m0, file = '/home/mattcoop/mortalityblob/gbv_gams/spei24_ac_m0.Rdata')
rm(spei24_ac_m0)

spei24_ac_m1 <- bam(gbv_year ~ s(spei24) + s(latitude, longitude, bs='sos', m=1) + mean_annual_precip + mean_annual_tmax + 
                      wealth_factor_harmonized + hhsize + date_cmc + years_education + 
                      country, data=dat, family = 'binomial', cluster=cl)
save(spei24_ac_m1, file = '/home/mattcoop/mortalityblob/gbv_gams/spei24_ac_m1.Rdata')
rm(spei24_ac_m1)

spei24_ac_mn1 <- bam(gbv_year ~ s(spei24) + s(latitude, longitude, bs='sos', m=-1) + mean_annual_precip + mean_annual_tmax + 
                      wealth_factor_harmonized + hhsize + date_cmc + years_education + 
                      country, data=dat, family = 'binomial', cluster=cl)
save(spei24_ac_mn1, file = '/home/mattcoop/mortalityblob/gbv_gams/spei24_ac_mn1.Rdata')
rm(spei24_ac_mn1)

tempZ_ac_m0 <- bam(gbv_year ~ s(temp12maxZ) + s(latitude, longitude, bs='sos', m=0) + mean_annual_precip + mean_annual_tmax + 
                  wealth_factor_harmonized + hhsize + date_cmc + years_education + 
                  country, data=dat, family = 'binomial', cluster=cl)
save(tempZ_ac_m0, file = '/home/mattcoop/mortalityblob/gbv_gams/tempZ_ac_m0.Rdata')
rm(tempZ_ac_m0)

tempZ_ac_m1 <- bam(gbv_year ~ s(temp12maxZ) + s(latitude, longitude, bs='sos', m=1) + mean_annual_precip + mean_annual_tmax + 
                  wealth_factor_harmonized + hhsize + date_cmc + years_education + 
                  country, data=dat, family = 'binomial', cluster=cl)
save(tempZ_ac_m1, file = '/home/mattcoop/mortalityblob/gbv_gams/tempZ_ac_m1.Rdata')
rm(tempZ_ac_m1)

tempZ_ac_mn1 <- bam(gbv_year ~ s(temp12maxZ) + s(latitude, longitude, bs='sos', m=-1) + mean_annual_precip + mean_annual_tmax + 
                  wealth_factor_harmonized + hhsize + date_cmc + years_education + 
                  country, data=dat, family = 'binomial', cluster=cl)
save(tempZ_ac_mn1, file = '/home/mattcoop/mortalityblob/gbv_gams/tempZ_ac_mn1.Rdata')
rm(tempZ_ac_mn1)

#############################
#Test different k values for sos
############################

spei24_ac_k50 <- bam(gbv_year ~ s(spei24) + s(latitude, longitude, bs='sos', k=50) + mean_annual_precip + mean_annual_tmax + 
                   wealth_factor_harmonized + hhsize + date_cmc + years_education + 
                   country, data=dat, family = 'binomial', cluster=cl)
save(spei24_ac_k50, file = '/home/mattcoop/mortalityblob/gbv_gams/spei24_ac_k50.Rdata')
rm(spei24_ac_k50)

spei24_ac_k100 <- bam(gbv_year ~ s(spei24) + s(latitude, longitude, bs='sos', k=100) + mean_annual_precip + mean_annual_tmax + 
                       wealth_factor_harmonized + hhsize + date_cmc + years_education + 
                       country, data=dat, family = 'binomial', cluster=cl)
save(spei24_ac_k100, file = '/home/mattcoop/mortalityblob/gbv_gams/spei24_ac_k100.Rdata')
rm(spei24_ac_k100)

spei24_ac_k1000 <- bam(gbv_year ~ s(spei24) + s(latitude, longitude, bs='sos', k=1000) + mean_annual_precip + mean_annual_tmax + 
                       wealth_factor_harmonized + hhsize + date_cmc + years_education + 
                       country, data=dat, family = 'binomial', cluster=cl)
save(spei24_ac_k1000, file = '/home/mattcoop/mortalityblob/gbv_gams/spei24_ac_k1000.Rdata')
rm(spei24_ac_k1000)

tempZ_ac_k50 <- bam(gbv_year ~ s(temp12maxZ) + s(latitude, longitude, bs='sos', k=50) + mean_annual_precip + mean_annual_tmax + 
                  wealth_factor_harmonized + hhsize + date_cmc + years_education + 
                  country, data=dat, family = 'binomial', cluster=cl)
save(tempZ_ac_k50, file = '/home/mattcoop/mortalityblob/gbv_gams/tempZ_ac_k50.Rdata')
rm(tempZ_ac_k50)

tempZ_ac_k100 <- bam(gbv_year ~ s(temp12maxZ) + s(latitude, longitude, bs='sos', k=100) + mean_annual_precip + mean_annual_tmax + 
                  wealth_factor_harmonized + hhsize + date_cmc + years_education + 
                  country, data=dat, family = 'binomial', cluster=cl)
save(tempZ_ac_k100, file = '/home/mattcoop/mortalityblob/gbv_gams/tempZ_ac_k100.Rdata')
rm(tempZ_ac_k100)

tempZ_ac_k1000 <- bam(gbv_year ~ s(temp12maxZ) + s(latitude, longitude, bs='sos', k=1000) + mean_annual_precip + mean_annual_tmax + 
                  wealth_factor_harmonized + hhsize + date_cmc + years_education + 
                  country, data=dat, family = 'binomial', cluster=cl)
save(tempZ_ac_k1000, file = '/home/mattcoop/mortalityblob/gbv_gams/tempZ_ac_k1000.Rdata')
rm(tempZ_ac_k1000)


##########################
#Other SPEI Windows
###########################

spei12_ac <- bam(gbv_year ~ s(spei12) + s(latitude, longitude, bs='sos') + 
                   mean_annual_precip + mean_annual_tmax + 
                   wealth_factor_harmonized + hhsize + date_cmc + years_education + 
                   country, data=dat, family = 'binomial', cluster=cl)
save(spei12_ac, file = '/home/mattcoop/mortalityblob/gbv_gams/spei12_ac.Rdata')
rm(spei12_ac)

spei36_ac <- bam(gbv_year ~ s(spei36) + s(latitude, longitude, bs='sos') + 
                   mean_annual_precip + mean_annual_tmax + 
                   wealth_factor_harmonized + hhsize + date_cmc + years_education + 
                   country, data=dat, family = 'binomial', cluster=cl)
save(spei36_ac, file = '/home/mattcoop/mortalityblob/gbv_gams/spei36_ac.Rdata')
rm(spei36_ac)

spei48_ac <- bam(gbv_year ~ s(spei48) + s(latitude, longitude, bs='sos') + 
                   mean_annual_precip + mean_annual_tmax + 
                   wealth_factor_harmonized + hhsize + date_cmc + years_education + 
                   country, data=dat, family = 'binomial', cluster=cl)
save(spei48_ac, file = '/home/mattcoop/mortalityblob/gbv_gams/spei48_ac.Rdata')
rm(spei48_ac)

system('~/telegram.sh "Done with Alternative SPEI Windows"')

