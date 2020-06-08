if (Sys.info()['sysname']=='Linux'){
  data_dir <- '/home/mattcoop/mortalityblob/gbv'
  meta_dir <- '/home/mattcoop/gbv'
} else if(Sys.info()['sysname']=='Windows'){
  data_dir <- 'G://My Drive/GBV'
  meta_dir <- 'C://Users/matt/gbv'
}

library(tidyverse)
library(broom)
library(texreg)

dat <- read.csv(file.path(data_dir, 'GBV_sel.csv')) %>%
  mutate(drought_cat=relevel(drought_cat, ref = 'normal'))

phys_mod_all <- glm(viol_phys ~ plos_age + woman_literate + is_married + plos_births + plos_hhsize + 
                      plos_rural + husband_education_level + plos_husband_age + country + drought_cat, data=dat, family = 'binomial')
sex_mod_all <- glm(viol_sex ~ plos_age + woman_literate + is_married + plos_births + plos_hhsize + 
                     plos_rural + husband_education_level + plos_husband_age + country + drought_cat, data=dat, family = 'binomial')
emot_mod_all <- glm(viol_emot ~ plos_age + woman_literate + is_married + plos_births + plos_hhsize + 
                      plos_rural + husband_education_level + plos_husband_age + country + drought_cat, data=dat, family = 'binomial')
cont_mod_all <- glm(viol_cont ~ plos_age + woman_literate + is_married + plos_births + plos_hhsize + 
                      plos_rural + husband_education_level + plos_husband_age + country + drought_cat, data=dat, family = 'binomial')

####################################################
#Mark data from PLOS article regress only that data
#####################################################
dat$in_plos_paper <- dat$survey_code %in% c("SL-6-1", "TG-6-1", "BJ-7-1", "CI-6-1", "CM-6-1",
                                            "GA-6-1", "TD-7-1", "CD-6-1", "RW-7-1", "BU-7-1", 
                                            "UG-7-2", "KE-7-1", "TZ-7-2", "MW-7-2", "MZ-6-1",
                                            "ZW-7-1", "ZM-6-1", "NM-6-1", "AO-7-1")

phys_mod_plos <- glm(viol_phys ~ plos_age + woman_literate + is_married + plos_births + plos_hhsize + 
                       plos_rural + husband_education_level + plos_husband_age + country + drought_cat, data=dat %>% filter(in_plos_paper), family = 'binomial')
sex_mod_plos <- glm(viol_sex ~ plos_age + woman_literate + is_married + plos_births + plos_hhsize + 
                      plos_rural + husband_education_level + plos_husband_age + country + drought_cat, data=dat %>% filter(in_plos_paper), family = 'binomial')
emot_mod_plos <- glm(viol_emot ~ plos_age + woman_literate + is_married + plos_births + plos_hhsize + 
                       plos_rural + husband_education_level + plos_husband_age + country + drought_cat, data=dat %>% filter(in_plos_paper), family = 'binomial')
cont_mod_plos <- glm(viol_cont ~ plos_age + woman_literate + is_married + plos_births + plos_hhsize + 
                       plos_rural + husband_education_level + plos_husband_age + country + drought_cat, data=dat %>% filter(in_plos_paper), family = 'binomial')

####################################################
#See if it's an effect specific to those countries
#####################################################
dat$in_cty <- dat$country %in% c("SL", "TG", "BJ", "CI", "CM",
                                 "GA", "TD", "CD", "RW", "BU", 
                                 "UG", "KE", "TZ", "MW", "MZ",
                                 "ZW", "ZM", "NM", "AO")

phys_mod_cty <- glm(viol_phys ~ plos_age + woman_literate + is_married + plos_births + plos_hhsize + 
                      plos_rural + husband_education_level + plos_husband_age + country + drought_cat, data=dat %>% filter(in_cty), family = 'binomial')
sex_mod_cty <- glm(viol_sex ~ plos_age + woman_literate + is_married + plos_births + plos_hhsize + 
                     plos_rural + husband_education_level + plos_husband_age + country + drought_cat, data=dat %>% filter(in_cty), family = 'binomial')
emot_mod_cty <- glm(viol_emot ~ plos_age + woman_literate + is_married + plos_births + plos_hhsize + 
                      plos_rural + husband_education_level + plos_husband_age + country + drought_cat, data=dat %>% filter(in_cty), family = 'binomial')
cont_mod_cty <- glm(viol_cont ~ plos_age + woman_literate + is_married + plos_births + plos_hhsize + 
                      plos_rural + husband_education_level + plos_husband_age + country + drought_cat, data=dat %>% filter(in_cty), family = 'binomial')

####################################################
#See if it's an Africa specific effect
#####################################################
dat$in_afr <- dat$latitude < 23 & dat$longitude > -20 & dat$longitude < 50


phys_mod_afr <- glm(viol_phys ~ plos_age + woman_literate + is_married + plos_births + plos_hhsize + 
                      plos_rural + husband_education_level + plos_husband_age + country + drought_cat, data=dat %>% filter(in_afr), family = 'binomial')
sex_mod_afr <- glm(viol_sex ~ plos_age + woman_literate + is_married + plos_births + plos_hhsize + 
                     plos_rural + husband_education_level + plos_husband_age + country + drought_cat, data=dat %>% filter(in_afr), family = 'binomial')
emot_mod_afr <- glm(viol_emot ~ plos_age + woman_literate + is_married + plos_births + plos_hhsize + 
                      plos_rural + husband_education_level + plos_husband_age + country + drought_cat, data=dat %>% filter(in_afr), family = 'binomial')
cont_mod_afr <- glm(viol_cont ~ plos_age + woman_literate + is_married + plos_births + plos_hhsize + 
                      plos_rural + husband_education_level + plos_husband_age + country + drought_cat, data=dat %>% filter(in_afr), family = 'binomial')


########################################
#Get AMEs
########################################
if ('mod' %in% ls()){rm('mod')}

for (mod in ls()[grepl('mod', ls())]){
  print(mod)
	
	saveRDS(eval(parse(text=mod)), file=paste0('~/mortalityblob/gbv_gams/', mod, '_plos.RDS'))
}
