setwd('G://My Drive/DHS Processed')

library(tidyverse)
library(broom)

gbv <- read.csv('GBV_all.csv') %>%
  filter(!is.infinite(spei36) & !is.infinite(spei48))

gbv$gbv_year_bin <- gbv$gbv_year != 'never'

all <- data.frame()
for (var in c('spei12', 'spei24', 'spei36', 'spei48', 'temp12maxZ')){
  gbv$cut <- cut(gbv[ , var], breaks = c(-Inf, -1, 1, Inf))
  
  for (lvl in levels(gbv$cut)){
    
    sel <- gbv %>% filter(cut == lvl)
    
    mod <- glm(as.formula(paste0('gbv_year_bin  ~ years_education + 
                 wealth_factor_harmonized + hhsize + date_cmc + country + 
                 mean_annual_precip + mean_annual_tmax + ', var)), 
               data=sel, family = 'binomial')
    
    res <- tidy(mod) %>%
      filter(term==var) %>%
      mutate(cut=lvl)
    
    all <- bind_rows(all, res)
  }
}
