setwd('G://My Drive/DHS Processed')

library(tidyverse)
library(broom)

gbv <- read.csv('GBV_all.csv') %>%
  filter(mean_annual_precip > 500 & builtup < 0.05)

all <- data.frame()
for (var in c('spei12', 'spei24', 'spei36', 'spei48', 'temp12maxZ')){
  gbv$cut <- cut(gbv[ , var], breaks = c(-Inf, -1, 0, 1, Inf))
  
  for (lvl in levels(gbv$cut)){
    
    sel <- gbv %>% filter(cut == lvl)
    
    mod <- glm(as.formula(paste0('viol_phys  ~ years_education + 
                 wealth_factor_harmonized + hhsize + date_cmc + country + ', var)), 
               data=sel, family = 'binomial')
    
    res <- tidy(mod) %>%
      filter(term==var) %>%
      mutate(cut=lvl)
    
    all <- bind_rows(all, res)
  }
}
