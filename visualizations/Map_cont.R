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

mods <- list.files('/home/mattcoop/mortalityblob/gbv_gams/', full.names=T)

mods <- mods[grepl('cont', mods)]

for (modf in mods){
  mod <- readRDS(modf)
 
  if (grepl('afr', modf)){
    dat[dat$in_afr , basename(modf)] <- residuals(mod)
  }
  if (grepl('lac', modf)){
    dat[dat$in_lac, basename(modf)] <- residuals(mod)
  }
  if (grepl('asi', modf)){
    dat[dat$in_asia, basename(modf)] <- residuals(mod)
  }
}

sum <- dat %>% 
  mutate(lat=round(latitude),
         lon=round(longitude)) %>%
  group_by(lat, lon) %>%
  summarize_at(vars(matches('cont|viol')),
               mean)

for (n in names(sum)[grepl('cont|viol', names(sum))]){
  sum$var <- sum[ , n, drop=TRUE]
       
  res <- ggplot(sum) + 
    geom_raster(aes(x=lon, y=lat, fill=var)) + 
    labs(title=n) + 
    theme_void()

  ggsave(res, filename=paste0('~/ipv-rep-tex/img/cont/', n, '.png'), width=8, height=5)
}
