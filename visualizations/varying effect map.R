library(mgcv)
library(tidyverse)
library(ggplot2)
library(raster)
library(sf)
library(stars)
library(rnaturalearth)
library(rgeos)
library(rdhs)
library(countrycode)
library(viridis)

setwd('G://My Drive/GBV/models')

#####################################
#Get Grids Only Within DHS IPV Countries
#####################################

cty <- ne_countries()

gbv_countries <- c("AM", "AO", "BF", "BU", "CD", "CI", 
                   "CM", "CO", "DR", "ET", "GA", "GH", "GU", "HN", "HT", "IA", "KE", 
                   "KH", "KM", "KY", "LB", "MB", "ML", "MM", "MW", "MZ", "NG", "NM", 
                   "NP", "PE", "PH", "RW", "SL", "TD", "TG", "TJ", "TL", "TZ", "UG", 
                   "ZA", "ZM", "ZW")

#ids <- dhs_countries(returnFields=c("CountryName", "DHS_CountryCode")) %>%
ids <- read.csv('C://Users/matt/Desktop/DHSCountryCode.csv') %>%
  dplyr::select(country=DHS_CountryCode, CountryName) %>%
  mutate(iso_a3 = countrycode(CountryName, 'country.name', 'iso3c')) %>%
  filter(country %in% gbv_countries)

cty$GBV <- cty$sov_a3 %in% ids$iso_a3

data <- expand.grid(list(latitude=seq(-90, 90, 0.5),
                         longitude=seq(-180, 180, 0.5)))

datsp <- SpatialPoints(data[ , c('longitude', 'latitude')], proj4string = CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'))
ctym <- aggregate(cty[cty$GBV, ], dissolve=T)

within <- gWithin(datsp, ctym, byid=TRUE)

data <- data[within[1 , ], ]


################################################
#Make Predictions for Drought and Normal Years
################################################

data$wealth_factor_harmonized=0
data$hhsize=5
data$date_cmc=1300
data$years_education='Higher'
data$country='AM'

#Using SPEI36, but could use 24
load('spei36_phys_ve_k200.Rdata')

logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

prob_change <- function(coef){
  res <- logit2prob(coef) - logit2prob(0)
  res
}

makeRast <- function(df, field){
  sp <- SpatialPointsDataFrame(df[ , c('longitude', 'latitude')], data = df[ , field, drop=FALSE])
  
  ref <- raster(nrow=360, ncol=720, xmx=180, xmn=-180, ymx=90, ymn=-90)
  
  r <- rasterize(sp, ref, field=field)
  
  r
}


data$effect <- predict(spei36_phys_ve_k15000, data %>% mutate(spei36=1), type='terms')[ , 's(latitude,longitude):spei36']

data$diff <- prob_change(data$effect)


diff_rast <- makeRast(data, 'diff')
diff_rast <- prob_change(diff_rast)

diff_stars <- st_as_stars(crop(diff_rast, extent(-100, 135, -40, 50)))
cty_sf <- st_as_sf(crop(cty, extent(-100, 135, -40, 50)))

ggplot() + 
  geom_stars(data=diff_stars) +
  geom_sf(data=cty_sf, aes(alpha=!GBV)) +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0)) + 
  scale_alpha_discrete(guide=FALSE) + 
  scale_fill_viridis(direction = -1) + 
  theme_void() + 
  theme(legend.position = 'bottom',
        legend.box='horizontal') + 
  labs(fill='Change in Probability of IPV with change in SPEI of 1') #+ 
  #guides(fill=guide_legend(title.position='top'))
  
ggsave('C://Users/matt/gbv-tex/EffectMap.pdf', height=4, width=8.5)






