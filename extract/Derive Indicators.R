setwd('G://My Drive/DHS Processed')

library(tidyverse)

m <- read.csv('GBV_men_raw.csv')
w <- read.csv('GBV_women_raw.csv')

##############################
#Employement and Cash earnings
###############################
#Employed in the past 12 months
w$employed <- w$v731_int %in% c(1, 2, 3) #If the woman was employed
w$employed[w$v502_int != 1] <- NA  #Subset to married women

m$employed <- m$mv731_int %in% c(1, 2, 3) #If the man was employed
m$employed[m$mv502_int !=1] <- NA #Subset to married men

#Of those employed
#Paid in Cash Only
w$paid_cash_only <- (w$v741_int == 1)
w$paid_cash_only[!w$employed] <- NA

m$paid_cash_only <- (m$mv741_int == 1)
m$paid_cash_only[!m$employed] <- NA

#Paid in Cash & in-kind
w$paid_cash_inkind <- (w$v741_int == 2)
w$paid_cash_inkind[!w$employed] <- NA

m$paid_cash_inkind <- (m$mv741_int == 2)
m$paid_cash_inkind[!m$employed] <- NA

#Paid in in-kind only
w$paid_inkind_only <- (w$v741_int == 3)
w$paid_inkind_only[!w$employed] <- NA

m$paid_inkind_only <- (m$mv741_int == 3)
m$paid_inkind_only[!m$employed] <- NA

#Not paid
w$paid_nothing <- (w$v741_int == 0)
w$paid_nothing[!w$employed] <- NA

m$paid_nothing <- (m$mv741_int == 0)
m$paid_nothing[!m$employed] <- NA


##############################
#Control over Women's Cash Earnings
###############################

#Categories dont match numbers well, so use labels
w$cashcontrol_all <- w$v739_chr %in% c("respondent", "respondent alone")
w$cashcontrol_some <- w$v739_chr %in% c("respondent & husband/partner jointly", "respondent & someone else jointly", "jointly with partner", "jointly with someone", "respondent and husband", "respondent and husband/partner", "respondent and other person")
w$cashcontrol_none <- w$v739_chr %in% c("other", "other *", "partner", "husband alone", "husband/partner", "husband/partner alone", "someone else")

w$cashcontrol_all[!(w$paid_cash_only | w$paid_cash_inkind)] <- NA
w$cashcontrol_some[!(w$paid_cash_only | w$paid_cash_inkind)] <- NA
w$cashcontrol_none[!(w$paid_cash_only | w$paid_cash_inkind)] <- NA

w$earns_more <- w$v746_int %in% c(1, 4)
w$earns_same <- w$v746_int %in% c(3)
w$earns_less <- w$v746_int %in% c(2, 5)

w$earns_more[!(w$paid_cash_only | w$paid_cash_inkind)] <- NA
w$earns_same[!(w$paid_cash_only | w$paid_cash_inkind)] <- NA
w$earns_less[!(w$paid_cash_only | w$paid_cash_inkind)] <- NA

##############################
#Control over Men's Cash Earnings
###############################

m$cashcontrol_all <- m$mv739_chr %in% c(1)
m$cashcontrol_some <- m$mv739_chr %in% c(2, 3)
m$cashcontrol_none <- m$mv739_chr %in% c(4, 5, 6, 8)

m$cashcontrol_all[!(m$paid_cash_only | m$paid_cash_inkind)] <- NA
m$cashcontrol_some[!(m$paid_cash_only | m$paid_cash_inkind)] <- NA
m$cashcontrol_none[!(m$paid_cash_only | m$paid_cash_inkind)] <- NA


#############################
#Ownership of Assets
############################

w$owns_house_alone <- w$v745a_int == 1
w$owns_house_alone_jointly <- w$v745a_int == 3
w$owns_house_jointly <- w$v745a_int == 2
w$owns_no_house <- w$v745a_int == 0

w$owns_land_alone <- w$v745b_int == 1
w$owns_land_alone_jointly <- w$v745b_int == 3
w$owns_land_jointly <- w$v745b_int == 2
w$owns_no_land <- w$v745b_int == 0

m$owns_house_alone <- m$mv745a_int == 1
m$owns_house_alone_jointly <- m$mv745a_int == 3
m$owns_house_jointly <- m$mv745a_int == 2
m$owns_no_house <- m$mv745a_int == 0

m$owns_land_alone <- m$mv745b_int == 1
m$owns_land_alone_jointly <- m$mv745b_int == 3
m$owns_land_jointly <- m$mv745b_int == 2
m$owns_no_land <- m$mv745b_int == 0

##################################################
#Ownsership of Title or Deed for House or Land
#################################################

#Skip for now

########################################################
#Ownership and Use of Bank Accounts and Mobile Phones
########################################################

w$has_bankacct <- w$v170_int == 1
w$has_phone <- w$v169a_int == 1
w$uses_phone_bank <- w$v169b_int == 1
w$uses_phone_bank[!w$has_phone] <- NA

m$has_bankacct <- m$mv170_int == 1
m$has_phone <- m$mv169a_int == 1
m$uses_phone_bank <- m$mv169b_int == 1
m$uses_phone_bank[!m$has_phone] <- NA

##################################
#Participation in Decision Making
##################################

#NOTE: for Health and Purchases, it's who SHOULD have more say
# only for visits is it who DOES have more say

w$decision_health_own <- w$v743a_int %in% c(1)
w$decision_health_both <- w$v743a_int %in% c(2, 3)
w$decision_health_other <- w$v743a_int %in% c(4, 5)

w$decision_purchases_own <- w$v743b_int %in% c(1)
w$decision_purchases_both <- w$v743b_int %in% c(2, 3)
w$decision_purchases_other <- w$v743b_int %in% c(4, 5)

w$decision_visits_own <- w$v743d_int %in% c(1)
w$decision_visits_both <- w$v743d_int %in% c(2, 3)
w$decision_visits_other <- w$v743d_int %in% c(4, 5)

w$decision_health_own[w$v502_int != 1] <- NA
w$decision_health_both[w$v502_int != 1] <- NA
w$decision_health_other[w$v502_int != 1] <- NA

w$decision_purchases_own[w$v502_int != 1] <- NA
w$decision_purchases_both[w$v502_int != 1] <- NA
w$decision_purchases_other[w$v502_int != 1] <- NA

w$decision_visits_own[w$v502_int != 1] <- NA
w$decision_visits_both[w$v502_int != 1] <- NA
w$decision_visits_other[w$v502_int != 1] <- NA

#Men
m$decision_health_own <- m$mv743a_int %in% c(1)
m$decision_health_both <- m$mv743a_int %in% c(2, 3)
m$decision_health_other <- m$mv743a_int %in% c(4, 5)

m$decision_purchases_own <- m$mv743b_int %in% c(1)
m$decision_purchases_both <- m$mv743b_int %in% c(2, 3)
m$decision_purchases_other <- m$mv743b_int %in% c(4, 5)

m$decision_health_own[m$mv502_int != 1] <- NA
m$decision_health_both[m$mv502_int != 1] <- NA
m$decision_health_other[m$mv502_int != 1] <- NA

m$decision_purchases_own[m$mv502_int != 1] <- NA
m$decision_purchases_both[m$mv502_int != 1] <- NA
m$decision_purchases_other[m$mv502_int != 1] <- NA

#################################
#Attitude toward Wife Beating
##################################

w$gbv_ok_burnedfood <- w$v744e_int == 1
w$gbv_ok_arguing <- w$v744c_int == 1
w$gbv_ok_goingout <- w$v744a_int == 1
w$gbv_ok_neglectingkids <- w$v744b_int == 1
w$gbv_ok_refusingsex <- w$v744d_int == 1
w$gbv_ok <- rowSums(w[ , c('gbv_ok_burnedfood', 'gbv_ok_arguing', 'gbv_ok_goingout', 'gbv_ok_neglectingkids', 'gbv_ok_refusingsex')]) > 0

m$gbv_ok_burnedfood <- m$mv744e_int == 1
m$gbv_ok_arguing <- m$mv744c_int == 1
m$gbv_ok_goingout <- m$mv744a_int == 1
m$gbv_ok_neglectingkids <- m$mv744b_int == 1
m$gbv_ok_refusingsex <- m$mv744d_int == 1
m$gbv_ok <- rowSums(m[ , c('gbv_ok_burnedfood', 'gbv_ok_arguing', 'gbv_ok_goingout', 'gbv_ok_neglectingkids', 'gbv_ok_refusingsex')]) > 0


######################################
#Attitude toward Negotiating Safe Sex
######################################

w$sexrefuse_ok_promiscuous <- w$v633b_int == 1
w$sexcondom_ok_spi <- w$v822_int == 1

m$sexrefuse_ok_promiscuous <- m$mv633b_int == 1
m$sexcondom_ok_spi <- m$mv822_int == 1

######################################
#Ability to Negotiate Safe Sex
######################################

w$can_sex_refuse <- w$v850a_int == 1
w$can_sex_condom <- w$v850b_int == 1

w$can_sex_refuse[w$v502_int != 1] <- NA
w$can_sex_condom[w$v502_int != 1] <- NA

#######################
#Physical Violence
#########################

w$gbv_ever_husband <- (w$d105a_int %in% 1:4) | (w$d105b_int %in% 1:4) | (w$d105c_int %in% 1:4) | (w$d105d_int %in% 1:4) | (w$d105e_int %in% 1:4) | (w$d105f_int %in% 1:4) | (w$d105g_int %in% 1:4) | (w$d105j_int %in% 1:4) | (w$d130a_int %in% 1:4)
w$gbv_ever_other <- w$d115y_int == 0
w$gbv_ever_pregnant <- w$d118y_int == 0

w$gbv_year_often <- (w$d105a_int == 1) | (w$d105b_int == 1) | (w$d105c_int == 1) | (w$d105d_int == 1) | (w$d105e_int == 1) | (w$d105f_int == 1) | (w$d105g_int == 1) | (w$d105j_int == 1) | (w$d130a_int == 1)
w$gbv_year_sometimes <- (w$d105a_int == 2) | (w$d105b_int == 2) | (w$d105c_int == 2) | (w$d105d_int == 2) | (w$d105e_int == 2) | (w$d105f_int == 2) | (w$d105g_int == 2) | (w$d105j_int == 2) | (w$d130a_int == 2)

w$gbv_ever_husband[w$v044_int != 1] <- NA
w$gbv_ever_other[w$v044_int != 1] <- NA
w$gbv_ever_pregnant[w$v044_int != 1] <- NA

w$gbv_year_often[w$v044_int != 1] <- NA
w$gbv_year_sometimes[w$v044_int != 1] <- NA


##########################
#Sexual Violence
###########################

w$sexbv_ever_husband <- (w$d105h_int %in% 1:4) | (w$d105i_int %in% 1:4) | (w$d105k_int %in% 1:4) | (w$d130b_int %in% 1:4)
w$sexbv_year_other <- w$d124_int == 0
w$sexbv_ever_unwanted <- w$d125_int == 1

w$sexbv_year_often <- (w$d105h_int == 1) | (w$d105i_int == 1) | (w$d105k_int == 1) 
w$sexbv_year_sometimes <- (w$d105h_int == 2) | (w$d105i_int == 2) | (w$d105k_int == 2) 

w$sexbv_ever_husband[w$v044_int != 1] <- NA
w$sexbv_year_other[w$v044_int != 1] <- NA
w$sexbv_ever_unwanted[w$v044_int != 1] <- NA

w$sexbv_year_often[w$v044_int != 1] <- NA
w$sexbv_year_sometimes[w$v044_int != 1] <- NA



##########################
#Emotional Violence
###########################

w$emobv_year_humiliate <- w$d103a_int %in% 1:4
w$emobv_year_threatened <- w$d103b_int %in% 1:4
w$emobv_year_insulted <- w$d103c_int %in% 1:4

w$emobv_year_often <- w$d103a_int == 1 | w$d103b_int == 1 | w$d103c_int == 1
w$emobv_year_somtimes <- w$d103a_int == 2 | w$d103b_int == 2 | w$d103c_int == 2

##################################
#Get rates by DHS site & combine
#################################

#Get SPI to DHS site level
spi <- read.csv('GBV_SPI.csv') %>%
  group_by(code) %>%
  summarize(spei6=mean(spei6, na.rm=T),
            spei12=mean(spei12, na.rm=T),
            spei24=mean(spei24, na.rm=T),
            spei36=mean(spei36, na.rm=T),
            spei48=mean(spei48, na.rm=T),
            spi6=mean(spi6, na.rm=T),
            spi12=mean(spi12, na.rm=T),
            spi24=mean(spi24, na.rm=T),
            spi36=mean(spi36, na.rm=T),
            spi48=mean(spi48, na.rm=T),
            temp6monthZ=mean(temp6monthZ, na.rm=T),
            temp12monthZ=mean(temp12monthZ, na.rm=T),
            temp24monthZ=mean(temp24monthZ, na.rm=T)) %>%
  mutate(country=substr(code, 1, 2))

men <- m %>%
  group_by(code) %>%
  summarize(employed = mean(employed, na.rm=T),
            paid_cash_only = mean(paid_cash_only, na.rm=T),
            paid_cash_inkind = mean(paid_cash_inkind, na.rm=T),
            paid_inkind_only = mean(paid_inkind_only, na.rm=T),
            paid_nothing = mean(paid_nothing, na.rm=T),
            cashcontrol_all = mean(cashcontrol_all, na.rm=T),
            cashcontrol_some = mean(cashcontrol_some, na.rm=T),
            cashcontrol_none = mean(cashcontrol_none, na.rm=T),
            owns_house_alone = mean(owns_house_alone, na.rm=T),
            owns_house_alone_jointly = mean(owns_house_alone_jointly, na.rm=T),
            owns_house_jointly = mean(owns_house_jointly, na.rm=T),
            owns_no_house = mean(owns_no_house, na.rm=T),
            owns_land_alone = mean(owns_land_alone, na.rm=T),
            owns_land_alone_jointly = mean(owns_land_alone_jointly, na.rm=T),
            owns_land_jointly = mean(owns_land_jointly, na.rm=T),
            owns_no_land = mean(owns_no_land, na.rm=T),
            has_bankacct = mean(has_bankacct, na.rm=T),
            has_phone = mean(has_phone, na.rm=T),
            uses_phone_bank = mean(uses_phone_bank, na.rm=T),
            decision_health_own = mean(decision_health_own, na.rm=T),
            decision_health_both = mean(decision_health_both, na.rm=T),
            decision_health_other = mean(decision_health_other, na.rm=T),
            decision_purchases_own = mean(decision_purchases_own, na.rm=T),
            decision_purchases_both = mean(decision_purchases_both, na.rm=T),
            decision_purchases_other = mean(decision_purchases_other, na.rm=T),
            gbv_ok_burnedfood = mean(gbv_ok_burnedfood, na.rm=T),
            gbv_ok_arguing = mean(gbv_ok_arguing, na.rm=T),
            gbv_ok_goingout = mean(gbv_ok_goingout, na.rm=T),
            gbv_ok_neglectingkids = mean(gbv_ok_neglectingkids, na.rm=T),
            gbv_ok_refusingsex = mean(gbv_ok_refusingsex, na.rm=T),
            gbv_ok = mean(gbv_ok, na.rm=T),
            sexrefuse_ok_promiscuous = mean(sexrefuse_ok_promiscuous, na.rm=T),
            sexcondom_ok_spi = mean(sexcondom_ok_spi, na.rm=T))

for (n in names(men)[names(men) != 'code']){
  men[paste0('m_', n)] <- men[n]
  men[n] <- NULL
}

women <- w %>%
  group_by(code) %>%
  summarize(employed = mean(employed, na.rm=T),
            paid_cash_only = mean(paid_cash_only, na.rm=T),
            paid_cash_inkind = mean(paid_cash_inkind, na.rm=T),
            paid_inkind_only = mean(paid_inkind_only, na.rm=T),
            paid_nothing = mean(paid_nothing, na.rm=T),
            cashcontrol_all = mean(cashcontrol_all, na.rm=T),
            cashcontrol_some = mean(cashcontrol_some, na.rm=T),
            cashcontrol_none = mean(cashcontrol_none, na.rm=T),
            earns_more = mean(earns_more, na.rm=T),
            earns_same = mean(earns_same, na.rm=T),
            earns_less = mean(earns_less, na.rm=T),
            owns_house_alone = mean(owns_house_alone, na.rm=T),
            owns_house_alone_jointly = mean(owns_house_alone_jointly, na.rm=T),
            owns_house_jointly = mean(owns_house_jointly, na.rm=T),
            owns_no_house = mean(owns_no_house, na.rm=T),
            owns_land_alone = mean(owns_land_alone, na.rm=T),
            owns_land_alone_jointly = mean(owns_land_alone_jointly, na.rm=T),
            owns_land_jointly = mean(owns_land_jointly, na.rm=T),
            owns_no_land = mean(owns_no_land, na.rm=T),
            has_bankacct = mean(has_bankacct, na.rm=T),
            has_phone = mean(has_phone, na.rm=T),
            uses_phone_bank = mean(uses_phone_bank, na.rm=T),
            decision_health_own = mean(decision_health_own, na.rm=T),
            decision_health_both = mean(decision_health_both, na.rm=T),
            decision_health_other = mean(decision_health_other, na.rm=T),
            decision_purchases_own = mean(decision_purchases_own, na.rm=T),
            decision_purchases_both = mean(decision_purchases_both, na.rm=T),
            decision_purchases_other = mean(decision_purchases_other, na.rm=T),
            decision_visits_own = mean(decision_visits_own, na.rm=T),
            decision_visits_both = mean(decision_visits_both, na.rm=T),
            decision_visits_other = mean(decision_visits_other, na.rm=T),
            gbv_ok_burnedfood = mean(gbv_ok_burnedfood, na.rm=T),
            gbv_ok_arguing = mean(gbv_ok_arguing, na.rm=T),
            gbv_ok_goingout = mean(gbv_ok_goingout, na.rm=T),
            gbv_ok_neglectingkids = mean(gbv_ok_neglectingkids, na.rm=T),
            gbv_ok_refusingsex = mean(gbv_ok_refusingsex, na.rm=T),
            gbv_ok = mean(gbv_ok, na.rm=T),
            sexrefuse_ok_promiscuous = mean(sexrefuse_ok_promiscuous, na.rm=T),
            sexcondom_ok_spi = mean(sexcondom_ok_spi, na.rm=T),
            can_sex_refuse = mean(can_sex_refuse, na.rm=T),
            can_sex_condom = mean(can_sex_condom, na.rm=T),
            gbv_ever_husband = mean(gbv_ever_husband, na.rm=T),
            gbv_ever_other = mean(gbv_ever_other, na.rm=T),
            gbv_ever_pregnant = mean(gbv_ever_pregnant, na.rm=T),
            gbv_year_often = mean(gbv_year_often, na.rm=T),
            gbv_year_sometimes = mean(gbv_year_sometimes, na.rm=T),
            sexbv_ever_husband = mean(sexbv_ever_husband, na.rm=T),
            sexbv_year_other = mean(sexbv_year_other, na.rm=T),
            sexbv_ever_unwanted = mean(sexbv_ever_unwanted, na.rm=T),
            sexbv_year_often = mean(sexbv_year_often, na.rm=T),
            sexbv_year_sometimes = mean(sexbv_year_sometimes, na.rm=T),
            emobv_year_humiliate = mean(emobv_year_humiliate, na.rm=T),
            emobv_year_threatened = mean(emobv_year_threatened, na.rm=T),
            emobv_year_insulted = mean(emobv_year_insulted, na.rm=T),
            emobv_year_often = mean(emobv_year_often, na.rm=T),
            emobv_year_somtimes = mean(emobv_year_somtimes, na.rm=T))

for (n in names(women)[names(women) != 'code']){
  women[paste0('w_', n)] <- women[n]
  women[n] <- NULL
}

weai <- read.csv('country_weai.csv')

all <- Reduce(function(x,y){merge(x, y, all.x=T, all.y=F)}, list(women, men, spi, weai))

write.csv(all, 'GBV_all.csv', row.names=F)




