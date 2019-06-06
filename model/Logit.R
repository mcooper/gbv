setwd('G://My Drive/DHS Processed')

library(tidyverse)
library(lme4)

gbv <- read.csv('GBV_all.csv')
precip <- read.csv('GBV_SPI.csv')
temp <- read.csv('GBV_DHS_Temps.csv') %>%
  rename(interview_year=year, interview_month=month)
wealth <- read.csv('hh_wealth_harmonized.csv')
weai <- read.csv('country_weai.csv')

all <- Reduce(merge, list(gbv, precip, temp))

all$cc <- substr(all$surveycode, 1, 2)

countries_from_beliyou <- c("BD", "ET", "GH", "GU", "HN", "KE", "ML", "NP", "NI", "NG", "SN", "UG")
countries_with_weai <- c("BD", "ET", "GH", "GU", "HN", "KE", "ML", "NP", "NI", "NG", "SN", "UG", 'KH', 'HT', 'LB', 'MW', 'RW', 'TJ', 'ZM')
countries_with_data <- c("BD", "ET", "GH", "GU", "HN", "KE", "NP", "SN", "UG", 'KH', 'HT', 'LB', 'MW', 'RW', 'TJ', 'ZM')
countries_from_beliyou_with_data <- c("BD", "ET", "GH", "GU", "HN", "KE", "NP", "SN", "UG")

all$interview_year_resc <- all$interview_year - min(all$interview_year, na.rm=T)

summary(glmer(violence_12months ~ zsq_12m + spei12 + (1|country) + (1|surveycode) + interview_year_resc, data=all, family='binomial'))
