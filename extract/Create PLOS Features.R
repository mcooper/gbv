if (Sys.info()['sysname']=='Linux'){
  data_dir <- '/home/mattcoop/mortalityblob/gbv'
  meta_dir <- '/home/mattcoop/gbv'
} else if(Sys.info()['sysname']=='Windows'){
  data_dir <- 'G://My Drive/GBV'
  meta_dir <- 'C://Users/matt/gbv'
}

library(tidyverse)
library(sf)
dat <- read.csv(file.path(data_dir, 'GBV_all.csv'))

dat$drought_cat <- cut(dat$perc12, breaks = c(0, 0.025, 0.1, 0.3, 1), 
                       labels=c('extreme', 'severe', 'moderate', 'normal'))

dat$plos_age <- cut(dat$woman_age, c(14, 19, 29, 39, 49))
dat$plos_births <- cut(dat$number_births, c(-1, 0.5, 2, 4, max(dat$number_births, na.rm=T)))
dat$plos_hhsize <- cut(dat$hhsize, c(1, 3, 5, max(dat$hhsize, na.rm=T)))
dat$plos_rural <- dat$urban_rural == 'rural'
dat$plos_husband_age <- cut(dat$husband_age, c(14, 19, 29, 39, 49, max(dat$husband_age, na.rm=T)))

sel <-  dat %>%
  #Rename viol_sex to viol_sexu to make 4-char parsing possible
  select(viol_phys, viol_emot, viol_sexu=viol_sex, viol_cont, plos_age, woman_literate, 
         is_married, plos_births, plos_hhsize, 
         plos_rural, husband_education_level, plos_husband_age, country, drought_cat,
         code, year, latitude, longitude, hh_code
         ) %>%
  mutate(survey_code=substr(code, 1, 6)) %>%
  na.omit %>%
	group_by(code) %>%
	mutate(ind_code = paste0(code, '-', row_number()))

canbena <- dat %>%
  select(hh_code, wealth_factor_harmonized, urban_rural, employer, employment, distance_to_water, 
         water_source_drinking)

sel <- merge(sel, canbena, all.x=T, all.y=F)

sel$in_plos_paper <- sel$survey_code %in% c("SL-6-1", "TG-6-1", "BJ-7-1", "CI-6-1", "CM-6-1",
                                            "GA-6-1", "TD-7-1", "CD-6-1", "RW-7-1", "BU-7-1",
                                            "UG-7-2", "KE-7-1", "TZ-7-2", "MW-7-2", "MZ-6-1",
                                            "ZW-7-1", "ZM-6-1", "NM-6-1", "AO-7-1")

sel$in_cty <- sel$country %in% c("SL", "TG", "BJ", "CI", "CM",
                                 "GA", "TD", "CD", "RW", "BU", 
                                 "UG", "KE", "TZ", "MW", "MZ",
                                 "ZW", "ZM", "NM", "AO")

sel$in_afr <- sel$latitude < 23 & sel$longitude > -20 & sel$longitude < 50
sel$in_lac <- sel$longitude < -30
sel$in_asia <- sel$longitude > 51

#Add GDL Admin ares
#sp <- read_sf('~', 'GDL Shapefiles V4')
#selsp <- st_as_sf(sel, coords=c('longitude', 'latitude'), remove=F) %>%
#  st_set_crs(4326)

#sel <- st_join(selsp, sp[ , 'GDLcode']) %>%
#  st_drop_geometry

write.csv(sel, file.path(data_dir, 'GBV_sel.csv'), row.names=F)
