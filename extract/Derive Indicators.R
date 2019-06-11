setwd('G://My Drive/DHS Processed')

library(tidyverse)

m <- read.csv('GBV_men_raw.csv') %>%
  filter(mv150_int == 1)

w <- read.csv('GBV_women_raw.csv') %>%
  filter(v044_int == 1)

##################################
#Participation in Decision Making
##################################

#NOTE: for Health and Purchases, it's who SHOULD have more say
# only for visits is it who DOES have more say

w$decision_health_own <- w$v743a_int %in% c(1, 2)

w$decision_purchases_own <- w$v743b_int %in% c(1, 2)

w$decision_visits_own <- w$v743d_int %in% c(1, 2)

w$empowered_decisions <- apply(X=w[ , c('decision_health_own', 'decision_purchases_own', 'decision_visits_own')], MARGIN = 1, FUN = all, na.rm=T)

#Unfortinately if all are NA, all() gives TRUE, so find these cases and make them NA
w$empowered_decisions[apply(X=is.na(w[ , c('decision_health_own', 'decision_purchases_own', 'decision_visits_own')]), MARGIN = 1, FUN = all, na.rm=T)] <- NA

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

## Men
m$gbv_notok_burnedfood <- m$mv744e_int != 1
m$gbv_notok_arguing <- m$mv744c_int != 1
m$gbv_notok_goingout <- m$mv744a_int != 1
m$gbv_notok_neglectingkids <- m$mv744b_int != 1
m$gbv_notok_refusingsex <- m$mv744d_int != 1

vars <- c('gbv_notok_burnedfood', 'gbv_notok_arguing', 'gbv_notok_goingout', 'gbv_notok_neglectingkids', 'gbv_notok_refusingsex')
m$men_gbv_notok <- apply(X=m[ , vars], MARGIN = 1, FUN = all, na.rm=T)

#Unfortinately if all are NA, all() gives TRUE, so find these cases and make them NA
m$men_gbv_notok[apply(X=is.na(m[ , vars]), MARGIN = 1, FUN = all, na.rm=T)] <- NA

#######################
#Physical Violence
#########################
vars <- c('d105a_int', 'd105b_int', 'd105c_int', 'd105d_int', 'd105e_int', 'd105f_int', 'd105g_int', 'd105j_int', 'd117a_int')
w$gbv_year_often <- rowSums(w[ , vars]==1, na.rm=T) > 0
w$gbv_year_often[apply(X=is.na(w[ , vars]), MARGIN = 1, FUN = all, na.rm=T)] <- NA

w$gbv_year_sometimes <- rowSums(w[ , vars]==2, na.rm=T) > 0
w$gbv_year_sometimes[apply(X=is.na(w[ , vars]), MARGIN = 1, FUN = all, na.rm=T)] <- NA

w$gbv_year <- ifelse(w$gbv_year_often, "often", 
                     ifelse(w$gbv_year_sometime, "sometimes", "never"))


##################################
#Get rates by DHS site & combine
#################################

women <- w %>%
  select(code, date_cmc=v008, hh_code, 
         empowered_decisions, empowered_gbv_notok, gbv_year) %>%
  mutate(country=substr(code, 1, 2)) %>%
  filter(!is.na(gbv_year))

men <- m %>%
  select(code, date_cmc=mv008, hh_code, 
         men_gbv_notok) %>%
  unique %>%
  filter(!(duplicated(hh_code) | duplicated(hh_code, fromLast=TRUE))) %>%
  mutate(country=substr(code, 1, 2))

temperature <- read.csv('GBV_Annual_Temp.csv') %>%
  select(code, date_cmc=v008, latitude, longitude, running_mean, pre2000_Zscore, all_Zscore)

spi <- read.csv('GBV_SPI.csv')

weai <- read.csv('country_weai.csv')

wealth <- read.csv('hh_wealth_harmonized.csv') %>%
  select(hh_code, code, date_cmc=survey_cmc, wealth_factor_harmonized, hhsize)

all <- Reduce(function(x,y){merge(x, y, all.x=T, all.y=F)}, list(women, men, temperature, spi, wealth, weai))

write.csv(all, 'GBV_all.csv', row.names=F)
