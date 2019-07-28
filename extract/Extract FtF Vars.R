library(haven)
library(dplyr)
library(lubridate)

setwd('G://My Drive/Feed the Future/')

###################################
#Ghana
###################################
#"The fieldwork itself took place between July 1 and August 17, 2012", please look at page 26 of the pdf at this link: http://www.metss-ghana.k-state.edu/PBS_Items/PBSReport22814export.pdf
gha <- read_dta('GHA-PBS-12/Ghana WEAI indicators.dta') %>%
  filter(sex==2 & latitude > 5 & longitude < 5) %>%
  mutate(hh_refno = paste0('GHA-', hhserial),
         month = 7,
         year = 2012,
         country = 'Ghana') %>%
  select(hh_refno, 
         latitude, 
         longitude, 
         month,
         year,
         country, 
         feelinputdecagr, incdec_count, raiprod_any, jown_count, jrightanyagr, credjanydec_any, speakpublic_any, groupmember_any, leisuretime, npoor_z105)

##############
#Zambia 
#############
match <- read_dta('ZMB-RALS-12/Zambia ftf/georeferenced WEAI/Zambia_CrossWalkFile.dta') %>%
  select(cluster, hh, pbs_id=PBS_ID)

#No dates given, but country report says most were done between late Nov and early Dec
zam1 <- read_dta('ZMB-RALS-12/Zambia ftf/georeferenced WEAI/WEAI_cluster GPS.dta') %>%
  filter(a05==2) %>%
  mutate(month = 11,
         year = 2012,
         country = 'Zambia',
         latitude = -cluster_south,
         longitude = cluster_east) %>%
  select(cluster,
         pbs_id,
         latitude,
         longitude,
         month,
         year,
         country,
         feelinputdecagr, incdec_count, raiprod_any, jown_count, jrightanyagr, 
         credjanydec_any, speakpublic_any, groupmember_any, leisuretime, npoor_z105) %>%
  merge(match)

zam2 <- read_dta('ZMB-RALS-15/processed/Date_GPS_WEAI_2015.dta') %>%
  filter(!is.na(f_weai_feelinputdecagr)) %>%
  mutate(month = month(dmy_hms(quest_date)),
         year = year(dmy_hms(quest_date)),
         latitude = -GPS_South,
         longitude = GPS_North,
         country='Zambia') %>%
  select(cluster, hh, latitude, longitude, month, year, country,
         f_weai_feelinputdecagr, f_weai_incdec_count, f_weai_jown_count, 
         f_weai_jrightanyagr, f_weai_credjanydec_any, f_weai_speakpublic_any, f_weai_groupmember_any, 
         f_weai_leisuretime, f_weai_npoor_z105) %>%
  merge(match)

names(zam2) <- gsub('f_weai_', '', names(zam2))

zam <- bind_rows(zam1, zam2) %>%
  mutate(hh_refno = paste0('ZAM-', cluster, '-', hh)) %>%
  select(-cluster, -pbs_id, -hh)

###################################
#Bangladesh
###################################
bgd1 <- read_dta('Bangladesh/4. BIHS_household_2011_15.dta') %>%
  mutate(hh_refno = paste0(ISO, '-', a01),
         round = ifelse(survey_year==2011, 1, 2)) %>%
  select(hh_refno,
         latitude=bio_latitude,
         longitude=bio_longitude,
         round,
         f_weai_feelinputdecagr, f_weai_incdec_count, f_weai_raiprod_any, f_weai_jown_count, 
         f_weai_jrightanyagr, f_weai_credjanydec_any, f_weai_speakpublic_any, f_weai_groupmember_any, 
         f_weai_leisuretime, f_weai_npoor_z105)

bgd2.1 <- read_dta('Bangladesh/BIHS Raw Data (2011)/001_mod_a_male.dta') %>%
  mutate(hh_refno = paste0('BGD-', a01),
         round = 1) %>%
  select(hh_refno,
         round,
         year=a16_yy,
         month=a16_mm)

bgd2.2 <- read_dta('Bangladesh/BIHS Raw Data (2015)/001_r2_mod_a_male_updated.dta') %>%
  mutate(hh_refno = paste0('BGD-', a01),
         round = 2) %>%
  select(hh_refno,
         round,
         year=a16_yy,
         month=a16_mm)

bgd2 <- bind_rows(bgd2.1, bgd2.2)

bgd <- merge(bgd1, bgd2) %>%
  mutate(country = 'Bangladesh') %>%
  select(-round)

names(bgd) <- gsub('f_weai_', '', names(bgd))

#################################
#Combine and Write
#################################

all <- Reduce(bind_rows, list(gha, zam, bgd)) %>%
  filter(!(latitude==0 & longitude==0) & !all(is.na(feelinputdecagr), is.na(incdec_count), 
                                              is.na(incdec_count), is.na(raiprod_any), is.na(jown_count),
                                              is.na(jrightanyagr), is.na(credjanydec_any), 
                                              is.na(speakpublic_any), is.na(groupmember_any),
                                              is.na(leisuretime), is.na(npoor_z105)))

write.csv(all, 'GBV_FTF.csv', row.names = F)
