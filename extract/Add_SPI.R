if (Sys.info()['sysname']=='Linux'){
  data_dir <- '/home/mattcoop/mortalityblob/gbv'
  meta_dir <- '/home/mattcoop/gbv'
}

setwd(data_dir)

library(tidyverse)

gbv <- read.csv('GBV_sel.csv')
clim <- read.csv('GBV_SPI.csv')

make_categories <- function(x){
  x <- case_when(x < qnorm(0.025) ~ 'extreme',
                 x < qnorm(0.1) ~ 'severe',
                 x < qnorm(0.3) ~ 'moderate',
                 x > qnorm(0.7) ~ 'wet',
                 TRUE ~ 'normal')
  x
}


clim <- clim %>%
  select(code, date_cmc, spei12, spei24, spei36, mean_annual_tave, mean_annual_precip) %>%
  mutate(spei12 = make_categories(spei12),
         spei24 = make_categories(spei24),
         spei36 = make_categories(spei36))

m <- merge(gbv, clim, all.x=T, all.y=F)

write.csv(m, 'GBV_SPI_COMB.csv')

