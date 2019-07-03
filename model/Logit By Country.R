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
mod1 <- data.frame()
for (i in unique(gbv$country)){
  
  sel <- gbv %>%
    filter(country == i)
  
  if (length(unique(sel$surveycode)) > 1){
    mod_spi <- glm(gbv_year ~ spi36 + 
                     wealth_factor_harmonized + hhsize_res + date_cmc_res + 
                     surveycode, 
                   data=sel, family = 'binomial')
  } else{
    mod_spi <- glm(gbv_year ~ spi36 + 
                     wealth_factor_harmonized + hhsize_res + date_cmc_res, 
                   data=sel, family = 'binomial')
  }

  res <- tidy(mod_spi)
  res$country <- i
  res$n <- nrow(sel)
  res$surveys <- length(unique(sel$surveycode))
  mod1 <- bind_rows(mod1, res)
  
  print(i)
}

mod2 <- data.frame()
for (i in unique(gbv$country)){
  
  
  sel <- gbv %>%
    filter(country == i)
  
  if (length(unique(sel$surveycode)) > 1){
    mod_spi <- glm(gbv_year ~ temp12max + 
                     wealth_factor_harmonized + hhsize_res + date_cmc_res + 
                     surveycode, 
                   data=sel, family = 'binomial')
  } else{
    mod_spi <- glm(gbv_year ~ temp12max + 
                     wealth_factor_harmonized + hhsize_res + date_cmc_res, 
                   data=sel, family = 'binomial')
  }
  
  res <- tidy(mod_spi)
  res$country <- i
  res$n <- nrow(sel)
  res$surveys <- length(unique(sel$surveycode))
  mod2 <- bind_rows(mod2, res)
  
  print(i)
}

a <- mod1 %>%
  filter(term=='spi36') %>%
  select(spi36=estimate, spi36p=p.value, country, n, surveys)

b <- mod2 %>%
  filter(term=='temp12max') %>%
  select(temp12max=estimate, tmaxp=p.value, country, n, surveys)

comb <- merge(a, b)
