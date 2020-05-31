if (Sys.info()['sysname']=='Linux'){
  data_dir <- '/home/mattcoop/mortalityblob/gbv'
  meta_dir <- '/home/mattcoop/gbv'
} else if(Sys.info()['sysname']=='Windows'){
  data_dir <- 'G://My Drive/GBV'
  meta_dir <- 'C://Users/matt/gbv'
}

library(mattr)
library(tidyverse)
library(broom)

dat <- read.csv(file.path(data_dir, 'GBV_all.csv'))

dat$drought_cat <- relevel(cut(dat$perc12, breaks = c(0, 0.1, 0.3, 1), labels=c('severe', 'drought', 'normal')), ref='normal')

dat$plos_age <- cut(dat$woman_age, c(14, 19, 29, 39, 49))
dat$plos_births <- cut(dat$number_births, c(-1, 0.5, 2, 4, max(dat$number_births, na.rm=T)))
dat$plos_hhsize <- cut(dat$hhsize, c(1, 3, 5, max(dat$hhsize, na.rm=T)))
dat$plos_rural <- dat$urban_rural == 'rural'
dat$plos_husband_age <- cut(dat$husband_age, c(14, 19, 29, 39, 49, max(dat$husband_age, na.rm=T)))

sel <-  dat %>%
  select(viol_phys, viol_emot, viol_sex, viol_cont, plos_age, woman_literate, is_married, plos_births, plos_hhsize, 
         plos_rural, husband_education_level, plos_husband_age, country, drought_cat,
         code, year, latitude, longitude) %>%
  mutate(survey_code=substr(code, 1, 6)) %>%
  na.omit %>%
	group_by(code) %>%
	mutate(ind_code = paste0(code, '-', row_number()))

write.csv(sel, file.path(data_dir, 'GBV_sel.csv'), row.names=F)
