library(tidyverse)
library(xtable)
library(countrycode)

res <- read.csv('~/mortalityblob/gbv/country_results.csv')

res <- res[!is.na(res$phy.p), ]
rownames(res) <- 1:nrow(res)
res$Country <- countrycode(res$Country, 'dhs', 'country.name')

bonferroni <- function(a, m){
  a/m
}
m <- 39*4

t <- res %>%
  gather(var, val, -Country) %>%
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
  spread(outcome, c)


print(xtable(t,
             caption='Country-level results for the Average Marginal Effect (AME) of drought on a woman\'s probability of experiencing IPV. Significance values are Bonferroni-corrected for testing 156 hypothesis, so *$p < 0.05/156$, **$p < 0.01/156$, ***$p<0.001/156$)',
             label='tab:country'),
      file='~/ipv-rep-tex/tables/country.tex',
      include.rownames=F,
      table.placement='H')


