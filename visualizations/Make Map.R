library(sf)
library(tidyverse)
library(ggimage)
library(purrr)

#########################
#Read Data
########################

cty <- ne_countries(returnclass = 'sf')

data <- read.csv('~/mortalityblob/gbv/GBV_sel.csv') %>%
  filter(in_afr | in_lac | in_asia) %>%
  select(latitude, longitude) %>%
  unique %>%
  st_as_sf(coords=c('longitude', 'latitude'), crs=4326, remove=F)

ggplot() + 
  geom_sf(data=cty) + 
  geom_sf(data=data, color='#bd0026', size=0.01, show.legend='point') + 
  coord_sf(xlim = c(min(data$longitude), max(data$longitude)), ylim = c(min(data$latitude), max(data$latitude))) + 
  theme_void()

ggsave('~/ipv-rep-tex/img/Map.pdf', width=10, height=3)

##########################################
## Try plot of pies over map
###########################################
# url = 'https://api.dhsprogram.com/rest/dhs/countries?select=DHS_CountryCode,CountryName&f=json'
# dhs = jsonlite::fromJSON(url)[['Data']] %>%
#   select(country=DHS_CountryCode, iso_a3=ISO3_CountryCode)
# 
# piedat <- read.csv('G://My Drive/GBV/GBV_all.csv') %>%
#   merge(dhs, all.x=T, all.y=F) %>% 
#   mutate(drought_cat=cut(perc12, breaks = c(0, 0.1, 0.3, 1), labels=c('Severe Drought', 'Moderate Drought', 'Normal'))) %>%
#   group_by(iso_a3) %>%
#   summarize(sev=mean(drought_cat == 'Severe Drought', na.rm=T),
#             mod=mean(drought_cat == 'Moderate Drought', na.rm=T),
#             nor=mean(drought_cat == 'Normal', na.rm=T)) %>%
#   gather(metric, value, -iso_a3)
# 
# cty_center = cty %>%
#   st_centroid()
# cty_center$area <- as.numeric(st_area(cty))
# cty_center[ , c('lon', 'lat')] <- st_coordinates(cty_center)
# 
# eth <- cty_center$area[which(cty_center$iso_a3 == 'ETH')]
# 
# pies = cty_center %>%
#   st_drop_geometry() %>%
#   select(iso_a3, lon, lat, area) %>%
#   filter(iso_a3 %in% piedat$iso_a3) %>%
#   as_tibble() %>%
#   mutate(pie = map(iso_a3,
#                    function(i){
#                      ggplot(piedat %>% filter(iso_a3==i), aes(x=1, value, fill=metric)) +
#                        geom_bar(stat="identity", width=1, show.legend=F) + 
#                        coord_polar(theta="y") + 
#                        xlab(NULL) + 
#                        ylab(NULL) + 
#                        theme_void() +
#                        theme_transparent() + 
#                        scale_fill_manual(values=c('#dfc27d', '#80cdc1', '#a6611a')) + 
#                        theme(plot.margin=unit(c(0,0,0,0),"mm"))
#                    }))
# 
# 
# gd <- expand.grid(15:16, 15:16)
# gd$fill <- c('Severe Drought', 'Moderate Drought', 'Normal', 'Normal')
# 
# p <- ggplot() +
#   geom_tile(data=gd, aes(x=Var1, y=Var2, fill=fill)) + 
#   geom_sf(data=cty) + 
#   coord_sf(xlim = c(max(data$longitude), 
#                     min(data$longitude)), 
#            ylim = c(min(data$latitude), 
#                     max(data$latitude))) + 
#   theme_void() + 
#   scale_fill_manual(values=c('Moderate Drought'='#dfc27d', 
#                              'Normal'='#80cdc1', 
#                              'Severe Drought'='#a6611a')) + 
#   geom_subview(data=pies,
#                aes(x=lon, y=lat,
#                subview=pie),
#                width=4, height=4)
# 
# ggsave(p, filename = 'C://Users/matt/ipv-rep-tex/img/MapPies.pdf', width=10, height=3)
