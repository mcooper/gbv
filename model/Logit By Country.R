setwd('G://My Drive/DHS Processed')

library(dplyr)
library(lme4)
library(broom)
library(texreg)

gbv <- read.csv('GBV_all.csv', stringsAsFactors=F)

gbv$gbv_year <- gbv$gbv_year != 'never'

gbv$surveycode <- substr(gbv$code, 1, 6)

gbv$survey_year <- floor(gbv$date_cmc/12) + 1900

gbv <- gbv %>%
  filter(!is.na(wealth_factor_harmonized) & !is.na(hhsize) & !is.na(spei24))

gbv$date_cmc_res <- (gbv$date_cmc - mean(gbv$date_cmc))/sd(gbv$date_cmc)

gbv$hhsize_res <- (gbv$hhsize - mean(gbv$hhsize))/sd(gbv$hhsize)


#############################
#Model
pre2000_Zscore <- data.frame()
for (i in unique(gbv$country)){
  
  mod_spi <- glm(gbv_year ~ pre2000_Zscore + 
                     wealth_factor_harmonized + hhsize_res + date_cmc_res, 
                 data=gbv %>% filter(country==i), family = 'binomial')
  res <- tidy(mod_spi)
  res$country <- i
  pre2000_Zscore <- bind_rows(pre2000_Zscore, res)
  
  print(i)
}

temp12monthZ <- data.frame()
for (i in unique(gbv$country)){
  
  mod_spi <- glm(gbv_year ~ temp12monthZ + 
                   wealth_factor_harmonized + hhsize_res + date_cmc_res, 
                 data=gbv %>% filter(country==i), family = 'binomial')
  res <- tidy(mod_spi)
  res$country <- i
  temp12monthZ <- bind_rows(temp12monthZ, res)
  
  print(i)
}

a <- temp12monthZ %>%
  filter(term=='temp12monthZ') %>%
  select(temp12monthZ=estimate, country)

b <- pre2000_Zscore %>%
  filter(term=='pre2000_Zscore') %>%
  select(pre2000_Zscore=estimate, country)

comb <- merge(a, b)
