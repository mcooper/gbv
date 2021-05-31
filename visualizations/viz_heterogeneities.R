library(tidyverse)
library(xtable)

setwd('~/mortalityblob/gbv/')

wealth <- read.csv('hetero_wealth_quintile.csv') %>%
  mutate(table='Wealth')
employ <- read.csv('hetero_employment.csv') %>%
  mutate(table='Employment')
water <- read.csv('hetero_water_source_drinking.csv') %>%
  mutate(table='Water Source')
urban <- read.csv('hetero_urban_rural.csv') %>%
  mutate(table='Urbanization')

bonferroni <- function(a, m){
  a/m
}

m <- 4



t <- bind_rows(wealth, employ, water, urban) %>%
  gather(var, val, -Country, -table) %>%
  mutate(outcome = substr(var, 1, 3),
         var = substr(var, 5, 6),
         outcome = case_when(outcome=='con' ~ "Controlling",
                             outcome=='emo' ~ "Emotional",
                             outcome=='phy' ~ "Physical",
                             outcome=='sex' ~ "Sexual")) %>%
  spread(var, val) %>%
  mutate(c = round(c, 3),
         c = case_when(p > bonferroni(0.05, m) ~ paste0(c, ''), 
                       p > bonferroni(0.01, m) ~ paste0(c, '*'),
                       p > bonferroni(0.001, m) ~ paste0(c, '**'),
                       TRUE ~ paste0(c, '***'))) %>%
  select(-p) %>%
  spread(outcome, c) %>%
  mutate(Country = str_to_title(Country)) %>%
  select(Variable = table, Category=Country, Controlling, Emotional, Physical, Sexual) %>%
  mutate(Variable = case_when(Variable == lag(Variable) ~ '',
                              TRUE ~ Variable))

print(xtable(t,
             caption=gsub('BONF', m, 'Average Marginal Effect (AME) of drought on a woman\'s probability of experiencing IPV, for women in different categories of wealth, employment, drinking water source, and urbanization.  Significance values are Bonferroni-corrected for testing BONF hypotheses, so *$p < 0.05/BONF$, **$p < 0.01/BONF$, ***$p<0.001/BONF$)'),
             label='tab:country'),
      file='~/ipv-rep-tex/tables/heterogeneities.tex',
      include.rownames=F,
      table.placement='H')


