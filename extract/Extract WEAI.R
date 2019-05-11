setwd('G://My Drive/Feed the Future/')

library(haven)
library(tidyverse)
library(readxl)
library(tidyr)

#####################
#ZAM
#####################

dat1 <- read_dta('ZMB-RALS-15/Rawdata/FtF Interim Data/ZMB_WEAI_RECODE_PR.dta') %>%
  mutate(hh_refno=paste0('ZMB-', pbs_id)) %>%
  select(pbs_id, weai_wt, weaisample, 
         feelinputdecagr, #Production - Input in productive decisions
         jown_count,      #Resources - Ownership of Assets
         jrightanyagr,    #Resources - Purchase, sale or transfer of assets
         credjanydec_any, #Resources - Access to and decisions on credit
         incdec_count,    #Income - Control over use of income
         groupmember_any, #Leadership - Group member
         speakpublic_any, #Leadership - Speaking in public
         npoor_z105,      #Time - Workload?
         leisuretime)     #Time - Leisure 

dat2 <- read_xlsx('ZMB-RALS-12/Zambia ftf/georeferenced WEAI/WEAI_cluster GPS.xlsx') %>%
  mutate(hh_refno=paste0('ZMB-', pbs_id)) %>%
  select(pbs_id, wt_weai_1, wt_weai_2, weaiwght, 
         feelinputdecagr, #Production - Input in productive decisions
         raiprod_any,     #Production - Autonomy in production
         jown_count,      #Resources - Ownership of assets
         jrightanyagr,    #Resources - Purchase, sale or transfer of assets
         credjanydec_any, #Resources - Access to and dicisions on credit
         incdec_count,    #Income - Control over use of income
         groupmember_any, #Leadership - Group member
         speakpublic_any, #Leadership - Speaking in Public
         npoor_z105,	    #Time - Workload?
         leisuretime      #Time - Leisure
  )

################
#Bangladesh
################

bgd <- read_xlsx('Bangladesh/4. BIHS_household_2011_15.xlsx') %>%
  mutate(hh_refno=paste0('BGD-', a01)) %>%
  select(hh_refno, year=survey_year,
         feelinputdecagr=f_weai_feelinputdecagr,
         incdec_count=f_weai_incdec_count,
         raiprod_any=f_weai_raiprod_any,
         jown_count=f_weai_jown_count,
         jrightanyagr=f_weai_jrightanyagr,
         credjanydec_any=f_weai_credjanydec_any,
         speakpublic_any=f_weai_speakpublic_any,
         groupmember_any=f_weai_groupmember_any,
         leisuretime=f_weai_leisuretime,
         npoor_z105=f_weai_npoor_z105
  )


#####################
#Extract Ghana
#####################



######################
#Combine and Write
#####################

write.csv(bgd, 'G://My Drive/GBV/BGD_WEAI.csv', row.names=F)


