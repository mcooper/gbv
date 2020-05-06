if (Sys.info()['sysname']=='Linux'){
  data_dir <- '/home/mattcoop/mortalityblob/gbv'
  meta_dir <- '/home/mattcoop/gbv'
}

library(ordinal)
library(tidyverse)

dat <- read.csv(file.path(data_dir, 'GBV_all.csv'))

dat$drought_cat <- relevel(cut(dat$perc12, breaks = c(0, 0.1, 0.3, 1), labels=c('severe', 'drought', 'normal')), ref='normal')

dat$plos_age <- cut(dat$woman_age, c(15, 19, 29, 39, 49))
dat$plos_births <- cut(dat$number_births, c(0, 0.5, 2, 4, max(dat$number_births, na.rm=T)))
dat$plos_hhsize <- cut(dat$hhsize, c(0, 3, 5, max(dat$hhsize, na.rm=T)))
dat$plos_rural <- dat$urban_rural == 'rural'
dat$plos_husband_age <- cut(dat$husband_age, c(0, 19, 29, 39, 49, max(dat$husband_age, na.rm=T)))

phys_mod <- glm(viol_phys ~ plos_age + woman_literate + is_married + plos_births + plos_hhsize + 
             plos_rural + husband_education_level + husband_age + country + drought_cat, data=dat, family = 'binomial')
sex_mod <- glm(viol_sex ~ plos_age + woman_literate + is_married + plos_births + plos_hhsize + 
                  plos_rural + husband_education_level + husband_age + country + drought_cat, data=dat, family = 'binomial')
emot_mod <- glm(viol_emot ~ plos_age + woman_literate + is_married + plos_births + plos_hhsize + 
                  plos_rural + husband_education_level + husband_age + country + drought_cat, data=dat, family = 'binomial')
cont_mod <- glm(viol_cont ~ plos_age + woman_literate + is_married + plos_births + plos_hhsize + 
                  plos_rural + husband_education_level + husband_age + country + drought_cat, data=dat, family = 'binomial')

summary(phys_mod)
summary(sex_mod)
summary(emot_mod)
summary(cont_mod)

####################################################
#Mark data from PLOS article regress only that data
#####################################################
dat$survey_code <- substr(dat$code, 1, 6)
dat$in_plos_paper <- dat$survey_code %in% c("SL-6-1", "TG-6-1", "BJ-7-1", "CI-6-1", "CM-6-1",
                                            "GA-6-1", "TD-7-1", "CD-6-1", "RW-7-1", "BU-7-1", 
                                            "UG-7-2", "KE-7-1", "TZ-7-2", "MW-7-2", "MZ-6-1",
                                            "ZW-7-1", "ZM-6-1", "NM-6-1", "AO-7-1")

phys_mod_plos <- glm(viol_phys ~ plos_age + woman_literate + is_married + plos_births + plos_hhsize + 
                  plos_rural + husband_education_level + husband_age + country + drought_cat, data=dat %>% filter(in_plos_paper), family = 'binomial')
sex_mod_plos <- glm(viol_sex ~ plos_age + woman_literate + is_married + plos_births + plos_hhsize + 
                 plos_rural + husband_education_level + husband_age + country + drought_cat, data=dat %>% filter(in_plos_paper), family = 'binomial')
emot_mod_plos <- glm(viol_emot ~ plos_age + woman_literate + is_married + plos_births + plos_hhsize + 
                  plos_rural + husband_education_level + husband_age + country + drought_cat, data=dat %>% filter(in_plos_paper), family = 'binomial')
cont_mod_plos <- glm(viol_cont ~ plos_age + woman_literate + is_married + plos_births + plos_hhsize + 
                  plos_rural + husband_education_level + husband_age + country + drought_cat, data=dat %>% filter(in_plos_paper), family = 'binomial')

summary(phys_mod_plos)
summary(sex_mod_plos)
summary(emot_mod_plos)
summary(cont_mod_plos)


####################################################
#See if it's an effect specific to those countries
#####################################################
dat$in_plos_cty <- dat$country %in% c("SL", "TG", "BJ", "CI", "CM",
                                            "GA", "TD", "CD", "RW", "BU", 
                                            "UG", "KE", "TZ", "MW", "MZ",
                                            "ZW", "ZM", "NM", "AO")

phys_mod_plos_cty <- glm(viol_phys ~ plos_age + woman_literate + is_married + plos_births + plos_hhsize + 
                       plos_rural + husband_education_level + husband_age + country + drought_cat, data=dat %>% filter(in_plos_cty), family = 'binomial')
sex_mod_plos_cty <- glm(viol_sex ~ plos_age + woman_literate + is_married + plos_births + plos_hhsize + 
                      plos_rural + husband_education_level + husband_age + country + drought_cat, data=dat %>% filter(in_plos_cty), family = 'binomial')
emot_mod_plos_cty <- glm(viol_emot ~ plos_age + woman_literate + is_married + plos_births + plos_hhsize + 
                       plos_rural + husband_education_level + husband_age + country + drought_cat, data=dat %>% filter(in_plos_cty), family = 'binomial')
cont_mod_plos_cty <- glm(viol_cont ~ plos_age + woman_literate + is_married + plos_births + plos_hhsize + 
                       plos_rural + husband_education_level + husband_age + country + drought_cat, data=dat %>% filter(in_plos_cty), family = 'binomial')

summary(phys_mod_plos_cty)
summary(sex_mod_plos_cty)
summary(emot_mod_plos_cty)
summary(cont_mod_plos_cty)


####################################################
#See if it's an Africa specific effect
#####################################################
dat$in_afr <- dat$latitude < 23 & dat$longitude > -20 & dat$longitude < 50


phys_mod_afr <- glm(viol_phys ~ plos_age + woman_literate + is_married + plos_births + plos_hhsize + 
                           plos_rural + husband_education_level + husband_age + country + drought_cat, data=dat %>% filter(in_afr), family = 'binomial')
sex_mod_afr <- glm(viol_sex ~ plos_age + woman_literate + is_married + plos_births + plos_hhsize + 
                          plos_rural + husband_education_level + husband_age + country + drought_cat, data=dat %>% filter(in_afr), family = 'binomial')
emot_mod_afr <- glm(viol_emot ~ plos_age + woman_literate + is_married + plos_births + plos_hhsize + 
                           plos_rural + husband_education_level + husband_age + country + drought_cat, data=dat %>% filter(in_afr), family = 'binomial')
cont_mod_afr <- glm(viol_cont ~ plos_age + woman_literate + is_married + plos_births + plos_hhsize + 
                           plos_rural + husband_education_level + husband_age + country + drought_cat, data=dat %>% filter(in_afr), family = 'binomial')

summary(phys_mod_afr)
summary(sex_mod_afr)
summary(emot_mod_afr)
summary(cont_mod_afr)



####################################################
#Mark data from PLOS article regress only that data, include spatial autocorrelation
#####################################################
library(mgcv)

phys_mod_plos_sa <- gam(viol_phys ~ plos_age + woman_literate + is_married + plos_births + plos_hhsize + 
                       plos_rural + husband_education_level + husband_age + country + drought_cat + s(latitude, longitude, bs = 'sos'), data=dat %>% filter(in_plos_paper), family = 'binomial')
sex_mod_plos_sa <- gam(viol_sex ~ plos_age + woman_literate + is_married + plos_births + plos_hhsize + 
                      plos_rural + husband_education_level + husband_age + country + drought_cat + s(latitude, longitude, bs = 'sos'), data=dat %>% filter(in_plos_paper), family = 'binomial')
emot_mod_plos_sa <- gam(viol_emot ~ plos_age + woman_literate + is_married + plos_births + plos_hhsize + 
                       plos_rural + husband_education_level + husband_age + country + drought_cat + s(latitude, longitude, bs = 'sos'), data=dat %>% filter(in_plos_paper), family = 'binomial')
cont_mod_plos_sa <- gam(viol_cont ~ plos_age + woman_literate + is_married + plos_births + plos_hhsize + 
                       plos_rural + husband_education_level + husband_age + country + drought_cat + s(latitude, longitude, bs = 'sos'), data=dat %>% filter(in_plos_paper), family = 'binomial')

summary(phys_mod_plos_sa)
summary(sex_mod_plos_sa)
summary(emot_mod_plos_sa)
summary(cont_mod_plos_sa)


























