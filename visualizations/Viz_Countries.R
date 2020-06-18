if (Sys.info()['sysname']=='Linux'){
  data_dir <- '/home/mattcoop/mortalityblob/gbv'
  meta_dir <- '/home/mattcoop/gbv'
	mod_dir <- '/home/mattcoop/mortalityblob/gbv_gams'
} else if(Sys.info()['sysname']=='Windows'){
  data_dir <- 'G://My Drive/GBV'
  meta_dir <- 'C://Users/matt/gbv'
}

library(tidyverse)

dat <- read.csv(file.path(data_dir, 'GBV_sel.csv')) %>%
  mutate(drought_cat=relevel(drought_cat, ref = 'normal'))

s <- dat %>% 
  mutate(continent = case_when(in_afr ~ "Africa",
                               in_asia ~ "Asia",
                               in_lac ~ "LAC",
                               TRUE ~ "Other")) %>%
  select(continent, survey_code) %>%
  unique %>%
  mutate(country=substr(survey_code, 1, 2)) %>%
  group_by(continent, country) %>%
  summarize(n())
  
  

mod <- dat %>%
  mutate(continent = case_when(in_afr ~ "Africa",
                               in_asia ~ "Asia",
                               in_lac ~ "LAC",
                               TRUE ~ "Other"),
         group = paste(continent, country)) %>%
  group_by(group) %>%
  summarize(severe = mean(drought_cat == 'severe'),
            moderate = mean(drought_cat == 'moderate'),
            extreme = mean(drought_cat == 'extreme'),
            normal = mean(drought_cat == 'normal')) %>%
  gather(drought, perc, -group) %>%
  mutate(drought=factor(drought, levels=c('normal', 'moderate', 'severe', 'extreme')))

ggplot(mod) + 
  geom_bar(aes(x = group, y=perc, fill=drought),
           position='stack', stat='identity') +
  coord_flip()
