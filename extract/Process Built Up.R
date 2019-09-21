library(tidyverse)
library(zoo)

dat <- read.csv('G://My Drive/DHS Processed/DHS_ghsl.csv')
dat[is.na(dat)] <- 0

dat$nonwater <- rowSums(dat[ , c('X2', 'X3', 'X4', 'X5', 'X6')])
dat$b1975 <- dat$X6/(dat$nonwater)
dat$b1990 <- rowSums(dat[ , c('X6', 'X5')])/dat$nonwater
dat$b2000 <- rowSums(dat[ , c('X6', 'X5', 'X4')])/dat$nonwater
dat$b2014 <- rowSums(dat[ , c('X6', 'X5', 'X4', 'X3')])/dat$nonwater

dat <- dat[!is.na(dat$b1975), ] #Remove 1154 sites that are so coastal there is no data

#Full range of possible years in mortality data
mergedf <- expand.grid(list(year=seq(1950, 2018), code=unique(dat$code)))

reshape <- dat %>% 
  filter(!is.na(b1975)) %>% 
  select(b1975, b1990, b2000, b2014, code) %>%
  gather(year, builtup, -code) %>%
  mutate(year = as.numeric(substr(year, 2, 5))) %>%
  merge(mergedf, all.x=T, all.y=T) 

final <- reshape %>%
  group_by(code) %>%
  mutate(builtup=na.approx(builtup, rule=2))

write.csv(final, 'G://My Drive/DHS Processed/DHS_ghsl_annual.csv', row.names=F)
