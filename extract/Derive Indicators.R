setwd('~/mortalityblob/dhs/')

library(tidyverse)

w <- read.csv('GBV_women_raw.csv') %>%
  filter(v044_int == 1)

###################################
#Education
##################################
w$years_education <- ifelse(w$v106_int == 0, "None",
                            ifelse(w$v106_int == 1, "Primary", 
                                   ifelse(w$v106_int == 2, "Secondary", 
                                          ifelse(w$v106_int == 3, "Higher", NA))))

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
w$is_married <- w$v502_int > 0

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

###################################
#Physical Violence by Non-Husband
######################################
#Too rare, skip for now
# 
# vars <- c('d117a_int', 'd117a')
# 
# w$viol_phys_nip <- rowSums(w[ , vars] > 0, na.rm=T) > 0
# w$viol_phys_nip[apply(X=is.na(w[ , vars]), MARGIN = 1, FUN = all, na.rm=T)] <- NA

##############################
#Sexual Violence by Non-Husband
##############################
vars <- c('d124')
#Forgot to extract this variable.  May need to re-run?

#######################
#Physical Violence - Just by husband (IPV)
#########################
vars <- c('d105a_int', 'd105b_int', 'd105c_int', 'd105d_int', 'd105e_int', 'd105f_int', 'd105g_int', 'd105j_int')
w$viol_phys <- rowSums(w[ , vars] == 1 | w[ , vars] == 2, na.rm=T) > 0

w$viol_phys[apply(X=is.na(w[ , vars]), MARGIN = 1, FUN = all, na.rm=T)] <- NA

#######################
#Sexual Violence - Just by husband (IPV)
#########################
vars <- c('d105h_int', 'd105i_int', 'd105k_int')
w$viol_sex <- rowSums(w[ , vars] == 1 | w[ , vars] == 2, na.rm=T) > 0

w$viol_sex[apply(X=is.na(w[ , vars]), MARGIN = 1, FUN = all, na.rm=T)] <- NA

###############################
#Years education
###############################
w$woman_years_ed <- w$v702_int
w$woman_years_ed[w$woman_years_ed > 20] <- NA

w$husband_years_ed <- w$v715_int
w$husband_years_ed[w$husband_years_ed > 20] <- NA

################################
#Occupations
###############################
occ <- read.csv('occupation_mapping.csv')

w <- w %>%
  merge(occ %>% rename(v705_chr=occupation_category,
                       woman_works_agriculture=works_agriculture), all.x=T, all.y=F) %>%
  merge(occ %>% rename(v717_chr=occupation_category,
                       husband_works_agriculture=works_agriculture), all.x=T, all.y=F)

w$woman_works_category <- w$v705_chr
w$husband_works_category <- w$v717_ch

##################################
#Get rates by DHS site & combine
#################################

women <- w %>%
  select(code, date_cmc=v008, hh_code, 
         empowered_decisions, empowered_gbv_notok, years_education,
         viol_phys, viol_sex, age_marriage, age_first_sex,
         is_married, woman_years_ed, husband_years_ed, 
         woman_works_category, husband_works_category,
         woman_works_agriculture, husband_works_agriculture) %>%
  mutate(country=substr(code, 1, 2),
         year=1900 + floor((date_cmc - 1)/12))

spi <- read.csv('GBV_SPI.csv')

gbv_geo <- read.csv('GBV_geo.csv') %>%
  select(code, latitude, longitude) %>%
  unique

wealth <- read.csv('hh_wealth_harmonized.csv') %>%
  select(hh_code, code, wealth_factor_harmonized, hhsize)

built <- read.csv('DHS_ghsl_annual.csv')

all <- Reduce(function(x,y){merge(x, y, all.x=T, all.y=F)}, list(women, spi, wealth, gbv_geo, built))

all <- all %>% filter(!is.infinite(spei36) & !is.infinite(spei48))

write.csv(all, 'GBV_all.csv', row.names=F)
