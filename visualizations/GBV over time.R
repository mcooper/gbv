setwd('G://My Drive/DHS Processed')

library(tidyverse)
library(lme4)
library(broom)
library(texreg)
library(ordinal)

options(stringsAsFactors=FALSE)

gbv <- read.csv('GBV_all.csv') %>%
  mutate(gbv_year_bin = gbv_year != 'never',
         surveycode=substr(code, 1, 6),
         year=floor((date_cmc - 1)/12) + 1900) %>%
  group_by(year) %>% 
  summarize(rate=mean(gbv_year_bin), 
            ct=length(unique(surveycode)), 
            n()) %>%
  mutate(perc_total=`n()`/sum(`n()`))

ggplot(gbv) + 
  geom_point(aes(x=year, y=rate)) +
  geom_bar(aes(x=year, y=perc_total), stat='identity')
