setwd('G://My Drive/GBV')

library(tidyverse)
library(lubridate)

dat <- read.csv('GBV_FTF_all.csv') %>%
  mutate(round = ifelse(ymd(date) < ymd('2013-01-01'), 1, 2))

dat2 <- dat %>%
  select(-date, -latitude, -longitude, -month, -year) %>%
  gather(key, value, -round, -country, -hh_refno)

dat3 <- merge(dat2 %>% filter(round==1) %>% rename(value1 = value) %>% select(-round),
              dat2 %>% filter(round==2) %>% rename(value2 = value) %>% select(-round))

dat4 <- dat3 %>%
  mutate(value = value2 - value1) %>%
  select(-value1, -value2) %>%
  spread(key, value)

dat5 <- merge(dat4, dat %>% filter(round==1) %>% select(hh_refno, latitude, longitude))

dat6 <- dat5 %>%
  rowwise %>%
  filter(!(any(is.na(credjanydec_any), is.na(feelinputdecagr), is.na(groupmember_any), is.na(incdec_count), is.na(jown_count),
             is.na(jrightanyagr), is.na(leisuretime), is.na(npoor_z105), is.na(speakpublic_any)) | (country=='Bangladesh' & is.na(raiprod_any)))) %>%
  data.frame

write.csv(dat6, 'GBV_FtF_DiD.csv', row.names=F)
