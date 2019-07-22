setwd('G://My Drive/DHS Processed')

library(dplyr)
library(sp)
library(spdep)
library(broom)

gbv <- read.csv('GBV_all.csv', stringsAsFactors=F)


#############################
#Model
for (i in unique(gbv$country)){
  
  sel <- gbv %>%
    filter(country == i)
  
  if (length(unique(sel$surveycode)) > 1){
    mod_spi <- glm(gbv_year ~ spei24 + years_education + 
                     wealth_factor_harmonized + hhsize_res + date_cmc_res + 
                     surveycode, 
                   data=sel, family = 'binomial')
  } else{
    mod_spi <- glm(gbv_year ~ spei24 + years_education + 
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

