setwd('G://My Drive/DHS Processed')

library(tidyverse)
library(broom)

gbv$speicut <- cut(gbv$spei24, breaks = c(-Inf, -1, 1, Inf))

all <- data.frame()
for (spei in levels(gbv$speicut)){
  sel <- gbv %>% filter(speicut == spei)
  
  mod <- glm(gbv_year_bin  ~ years_education + 
               wealth_factor_harmonized + hhsize + date_cmc + country + 
               mean_annual_precip + mean_annual_tmax + spei24, 
             data=sel, family = 'binomial')
  
  res <- tidy(mod) %>%
    filter(term=='spei24') %>%
    mutate(range=spei)
  
  all <- bind_rows(all, res)
  
}
