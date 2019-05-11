library(haven)
library(tidyverse)
library(readxl)
library(tidyr)

setwd('G://My Drive/Feed the Future')

source('C://Git/gbv/utils/utils.R')

##################################
#Extract hh vars for round 1
##################################
household1 <- read_dta('ZMB-RALS-12/Zambia ftf/Zambia_household_PR.dta')

household1$asset_index <- PCA_assets(household1[ , paste0('d0', seq(1, 8))], ntiles=5)
household1 <- household1 %>%
  mutate(cluster=as.factor(a02)) %>%
  select(hhs=hungerscale,
         pbs_id,
         hh_size,
         asset_index,
         cluster)

edumap <- data.frame(zic_edu_lvl=seq(0, 17),
                     hhhead_education=c('None', 'Standard', 'Standard', 'Standard', 'Standard', 'Standard', 'Standard', 'Standard', 'Form', 'Form', 'Form', 'Form', 'Form', 'Form', 'Form', 'University', 'University', 'University'))

hhmembers1 <- read_dta('ZMB-RALS-12/Zambia ftf/Zambia_hhmembers_PR.dta') %>%
  filter(c03 == 1) %>%
  filter(!duplicated(pbs_id) & c02!=99) %>%
  merge(edumap, all.x=T, all.y=F) %>%
  select(pbs_id, 
         hhhead_sex=c02, 
         hhhead_age=c04,
         hhhead_education,
         hhhead_literate=zic_literacy) %>%
  mutate(hhhead_sex=factor(hhhead_sex, levels=c(1, 2), labels=c('Male', 'Female')),
         hhhead_education=factor(hhhead_education, levels=c('None', 'Standard', 'Form', 'University')),
         hhhead_literate=hhhead_literate!=1 & hhhead_literate != 99)

depend_ratio1 <- read_dta('ZMB-RALS-12/Zambia ftf/Zambia_hhmembers_PR.dta') %>%
  mutate(dependant = c04 < 15 | c04 > 65,
         workers = c04 > 15 & c04 < 65) %>%
  group_by(pbs_id) %>%
  summarize(dependents=sum(dependant),
            workers=sum(workers))

match1 <- read_dta('ZMB-RALS-12/Zambia ftf/georeferenced WEAI/Zambia_CrossWalkFile.dta') %>%
  mutate(hh_refno=paste0('ZAM-', cluster, '-', hh)) %>%
  mutate(pbs_id=PBS_ID) %>%
  select(-PBS_ID)

hh1 <- Reduce(f=function(x, y) merge(x, y, all.x=T, all.y=F), list(household1, hhmembers1, depend_ratio1, match1)) %>%
  mutate(dependents=dependents/hh_size,
         workers=workers/hh_size)

############################################################
#Extract for round 2
#############################################################
household2 <- read_dta('ZMB-RALS-15/Rawdata/Data/Stata/household.dta') %>%
  data.frame

household2$asset_index <- PCA_assets(household2[ , paste0('d0', seq(1, 8))], ntiles=5)
household2 <- household2 %>%
  mutate(cluster=as.factor(a02)) %>%
  select(hhs=hungerscale,
         pbs_id,
         hh_size,
         asset_index,
         cluster)

edumap <- data.frame(zic_edu_lvl=seq(0, 17),
                     hhhead_education=c('None', 'Standard', 'Standard', 'Standard', 'Standard', 'Standard', 'Standard', 'Standard', 'Form', 'Form', 'Form', 'Form', 'Form', 'Form', 'Form', 'University', 'University', 'University'))




save(file="ZAM_data.Rdata", list=c("allhh", "allchild"))

