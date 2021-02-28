if (Sys.info()['sysname']=='Linux'){
  data_dir <- '/home/mattcoop/mortalityblob/gbv'
  meta_dir <- '/home/mattcoop/gbv'
}

setwd(data_dir)

library(tidyverse)

gbv <- read.csv('GBV_sel.csv')
clim <- read.csv('GBV_SPI.csv')

clim <- clim %>%
  select(code, date_cmc, spei12, spei24, spei36) %>%
  mutate(spei12 = 

dat$drought_cat <- cut(dat$perc12, breaks = c(0, 0.025, 0.1, 0.3, 1), 
                       labels=c('extreme', 'severe', 'moderate', 'normal'))


