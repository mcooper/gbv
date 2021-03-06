if (Sys.info()['sysname']=='Linux'){
  data_dir <- '/home/mattcoop/mortalityblob/gbv'
  meta_dir <- '/home/mattcoop/gbv'
}

setwd(data_dir)

library(tidyverse)

#Data from initial analysis
w <- read.csv('GBV_women_raw.csv') %>%
  filter(v502_int == 1) 

#New data, with extra vars
w2 <- read.csv('GBV_women_raw2.csv') %>%
  filter(v502_int == 1) 

#Subset new data to only households in old data
w <- w2 %>%
  filter(hh_code %in% w$hh_code)


#######################
#Physical Violence - Just by husband (IPV)
#########################
vars <- c('d105a_int', 'd105b_int', 'd105c_int', 'd105d_int', 'd105e_int', 'd105f_int', 'd105g_int', 'd105j_int', 'd105l_int', 'd105m_int', 'd105n_int')
w$viol_phys <- rowSums(w[ , vars] == 1 | w[ , vars] == 2, na.rm=T) > 0

w$viol_phys[apply(X=is.na(w[ , vars]), MARGIN = 1, FUN = all, na.rm=T)] <- NA

#######################
#Sexual Violence - Just by husband (IPV)
#########################
vars <- c('d105h_int', 'd105i_int', 'd105k_int')
w$viol_sex <- rowSums(w[ , vars] == 1 | w[ , vars] == 2, na.rm=T) > 0

w$viol_sex[apply(X=is.na(w[ , vars]), MARGIN = 1, FUN = all, na.rm=T)] <- NA

################################################
#Emotional Violence - Just by husband (IPV)
############################################
vars <- c('d103a_int', 'd103b_int', 'd103c_int', 'd103d_int', 'd103e_int', 'd103f_int')
w$viol_emot <- rowSums(w[ , vars] == 1 | w[ , vars] == 2, na.rm=T) > 0

w$viol_emot[apply(X=is.na(w[ , vars]), MARGIN = 1, FUN = all, na.rm=T)] <- NA

#################################
#Controling Behavior
#################################
vars <- c('d101a_int', 'd101b_int', 'd101c_int', 'd101d_int', 'd101e_int', 'd101f_int', 'd101g_int', 'd101h_int', 'd101i_int', 'd101j_int')
w$viol_cont <- rowSums(w[ , vars] == 1, na.rm=T) > 0

w$viol_cont[apply(X=is.na(w[ , vars]), MARGIN = 1, FUN = all, na.rm=T)] <- NA


###################################
#Education
##################################
w$woman_education_level <- ifelse(w$v106_int == 0, "None",
                                  ifelse(w$v106_int == 1, "Primary", 
                                         ifelse(w$v106_int == 2, "Secondary", 
                                                ifelse(w$v106_int == 3, "Higher", NA))))

w$woman_education_years <- w$v133_int
w$woman_education_years[w$woman_education_years > 25] <- NA

w$husband_education_level <- ifelse(w$v701_int == 0, "None",
                                    ifelse(w$v701_int == 1, "Primary", 
                                           ifelse(w$v701_int == 2, "Secondary", 
                                                  ifelse(w$v701_int == 3, "Higher", NA))))

w$husband_education_years <- w$v715_int
w$husband_education_years[w$husband_education_years > 25] <- NA


##################################
#Participation in Decision Making
##################################

#NOTE: for Health and Purchases, it's who SHOULD have more say
# only for visits is it who DOES have more say

w$decision_health_own <- w$v743a_int %in% c(1, 2) | w$v743a %in% c(1, 2)
w$decision_purchases_own <- w$v743b_int %in% c(1, 2) | w$v743b %in% c(1, 2)
w$decision_visits_own <- w$v743d_int %in% c(1, 2) | w$v743d %in% c(1, 2)
w$empowered_decisions <- apply(X=w[ , c('decision_health_own', 'decision_purchases_own', 'decision_visits_own')], MARGIN = 1, FUN = all, na.rm=T)

#Unfortinately if all are NA, all() gives TRUE, so find these cases and make them NA
w$empowered_decisions[apply(X=is.na(w[ , c('v743a_int', 'v743b_int', 'v743d_int', 'v743a', 'v743b', 'v743d')]), MARGIN = 1, FUN = all, na.rm=T)] <- NA

#################################
#Attitude toward Wife Beating
##################################

## Women
w$gbv_notok_burnedfood <- w$v744e_int != 1
w$gbv_notok_arguing <- w$v744c_int != 1
w$gbv_notok_goingout <- w$v744a_int != 1
w$gbv_notok_neglectingkids <- w$v744b_int != 1
w$gbv_notok_refusingsex <- w$v744d_int != 1

vars <- c('gbv_notok_burnedfood', 'gbv_notok_arguing', 'gbv_notok_goingout', 'gbv_notok_neglectingkids', 'gbv_notok_refusingsex')
w$empowered_gbv_notok <- apply(X=w[ , vars], MARGIN = 1, FUN = all, na.rm=T)

#Unfortinately if all are NA, all() gives TRUE, so find these cases and make them NA
w$empowered_gbv_notok[apply(X=is.na(w[ , vars]), MARGIN = 1, FUN = all, na.rm=T)] <- NA

##############################
#Is Or Was Married
##############################
w$is_married <- w$v501_int == 1

##############################
#Age at First Marriage
##############################
vars <- c('v511_int', 'v511')

w$age_marriage <- rowSums(w[ , vars], na.rm=T)
w$age_marriage[apply(X=is.na(w[ , vars]), MARGIN = 1, FUN = all, na.rm=T)] <- NA

##############################
#Age at First Sex
##############################
w$age_first_sex <- w$v525_int
w$age_first_sex[which(w$age_first_sex==96)] <- w$age_marriage[which(w$age_first_sex==96)]

################################
#Occupations
###############################
occ <- read.csv(paste0(meta_dir, '/scope/occupation_mapping.csv'))

w <- w %>%
  merge(occ %>% rename(v705_chr=occupation_category,
                       woman_works_agriculture=works_agriculture), all.x=T, all.y=F) %>%
  merge(occ %>% rename(v717_chr=occupation_category,
                       husband_works_agriculture=works_agriculture), all.x=T, all.y=F)

w$woman_works_category <- w$v705_chr
w$husband_works_category <- w$v717_ch

############################
# Contraception
###############################
w$woman_contraception <- w$v313_chr

############################
# Circumcision
############################
w$woman_circumcised <- w$g102_chr
w$woman_circumcised[w$woman_circumcised == '9'] <- NA

##############################
# Woman's Age
##############################
w$woman_age <- w$v012

############################
# Woman is literate
#############################
w$woman_literate <- w$v155_int == 2

#########################
# Number of births
########################
w$number_births <- w$v201

###########################
# Urban Rural
############################
w$urban_rural <- w$v025_chr

#########################
# Husband's Age
##########################
w$husband_age <- w$v730_int

################################
# Respondent worked in past twelve months
############################
w$woman_employed <- w$v731_chr %in% c(1, 2, 3)

############################
# Water sources
############################
w$water_source_drinking <- case_when(w$v113_int >= 10 & w$v113_int < 20 ~ 'Piped Water',
                                     w$v113_int >= 20 & w$v113_int < 30 ~ 'Tube Well Water',
                                     w$v113_int >= 30 & w$v113_int < 40 ~ 'Dug Well Water',
                                     w$v113_int >= 40 & w$v113_int < 50 ~ 'Surface Water',
                                     w$v113_int >= 50 & w$v113_int < 60 ~ 'Rain Water',
                                     w$v113_int >= 60 & w$v113_int < 75 ~ 'Purchased Water',
                                     w$v113_int == 96 ~ 'Other',
                                     w$v113_int >= 97 ~ NA_character_)


############################
# Distance to water sources
############################
w$v115_int[w$v115_chr %in% c('not dejure resident', 'not a dejure resident', 'not a de jure resident', "don't know ***", "don't know", "dk")] <- NA
w$v115_int[w$v115_chr %in% c('delivered water', 'on premises')] <- 0
w$v115_int[w$v115_chr == 'more than 12 hours'] <- 12*60
w$v115_int[w$v115_chr %in% c(998, 999)] <- NA


w$distance_to_water <- w$v115_int

###############################
# Urban or Rural
##############################

#############################
# Womon's Employer
#############################
w$v719_chr[w$v719_chr == '9'] <- NA
w$employer <- w$v719_chr

#############################
# Woman's Employment
###########################
w$employment <- case_when(w$v717_int %in% c(0, 96, 97)  ~ 'Unemployed',
                          w$v717_int %in% c(1, 2, 3, 10) ~ 'Professional',
                          w$v717_int %in% c(4, 5) ~ 'Agriculture',
                          w$v717_int %in% c(6, 7, 8) ~ 'Manual Labor')

##################################
#Get rates by DHS site & combine
#################################

women <- w %>%
  select(code, date_cmc=v008, hh_code, 
         viol_phys, viol_sex, viol_emot, viol_cont, 
         woman_education_level, woman_education_years, 
         husband_education_level, husband_education_years, 
         decision_health_own, decision_purchases_own, 
         decision_visits_own, empowered_decisions, 
         gbv_notok_burnedfood, gbv_notok_arguing, 
         gbv_notok_goingout, gbv_notok_neglectingkids, 
         gbv_notok_refusingsex, empowered_gbv_notok, 
         is_married, age_marriage, age_first_sex, 
         woman_works_agriculture, husband_works_agriculture, 
         woman_works_category, husband_works_category, 
         woman_contraception, woman_circumcised, woman_age, 
         woman_literate, number_births, urban_rural, 
         husband_age, woman_employed,
         employer, employment, distance_to_water, 
         water_source_drinking) %>%
  mutate(country=substr(code, 1, 2),
         year=1900 + floor((date_cmc - 1)/12))

spi <- read.csv('GBV_SPI.csv')

gbv_geo <- read.csv('GBV_geo.csv') %>%
  select(code, latitude, longitude) %>%
  unique

market <- read.csv('GBV_ttc.csv')

wealth <- read.csv('../dhs/hh_wealth_harmonized.csv') %>%
  select(hh_code, code, wealth_factor_harmonized, hhsize)

built <- read.csv('../dhs/DHS_ghsl_annual.csv')

all <- Reduce(function(x,y){merge(x, y, all.x=T, all.y=F)}, list(women, spi, wealth, market, gbv_geo, built))

write.csv(all, 'GBV_all.csv', row.names=F)
