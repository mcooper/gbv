setwd('G://My Drive/DHS Processed')

library(dplyr)
library(lme4)
library(broom)
library(texreg)
library(rgdal)
library(rdhs)
library(countrycode)
library(ggplot2)
library(raster)
library(cowplot)
library(RColorBrewer)

gbv <- read.csv('GBV_all.csv', stringsAsFactors=F) %>%
  filter(builtup < 0.1 & !is.infinite(spei36))

gbv$surveycode <- substr(gbv$code, 1, 6)

#############################
#Model
mod1 <- data.frame()
for (i in unique(gbv$country)){
  
  sel <- gbv %>%
    filter(country == i)
  
  try({
    mod_spi <- glm(viol_phys_ip ~ years_education +
                     wealth_factor_harmonized + hhsize + date_cmc +
                     mean_annual_precip + mean_annual_tmax + spei36,
                   data=sel, family = 'binomial')
  
    res <- tidy(mod_spi) %>%
      filter(grepl('spei', term))
    res$country <- i
    res$n <- nrow(sel)
    res$surveys <- length(unique(sel$surveycode))
    mod1 <- bind_rows(mod1, res)
  })
  
  print(i)
}

ids <- dhs_countries(returnFields=c("CountryName", "DHS_CountryCode")) %>%
  dplyr::select(country=DHS_CountryCode, CountryName)

names <- merge(mod1, ids, all.x=T, all.y=F)
names$ISO_A2 <- countrycode(sourcevar = names$CountryName, origin='country.name', destination = 'iso2c')

sp <- readOGR('G://My Drive/DHS Spatial Covars/Global Codes and Shapefile', 'ne_50m_admin_0_countries')

sp <- sp[!sp$ADMIN %in% c('French Southern and Antarctic Lands',
                       'Heard Island and McDonald Islands'), ]

spnew <- sp::merge(sp, names)

#Make Visualization

spnew <- crop(spnew, extent(-100, 135, -60, 55))

spnew@data$id <- rownames(spnew@data)


spf <- fortify(spnew, region='id')
spf$rownum <- 1:nrow(spf)

spf2 <- merge(spf, spnew@data, all.x=T, all.y=F) %>%
  arrange(rownum)

spf2$border <- ifelse(spf2$p.value < 0.05, "Significant", "Insignificant")

map <- ggplot() + 
  geom_polygon(data=spf2, aes(x=long, y=lat, group=group, fill=estimate), color="#000000") + 
  geom_polygon(data=spf2, aes(x=long, y=lat, group=group, size=border), fill="transparent", color="#000000") + 
  scale_fill_gradient2(low='#d7191c', high='#2c7bb6', mid = '#ffffbf', midpoint=0, na.value = "#888888") + 
  scale_size_manual(values=c(Significant=1, Insignficant=0.4)) + 
  guides(fill=FALSE, color=FALSE, size=FALSE) + 
  theme_void() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0))

convert <- function(x){
  odds <- exp(x)
  round(odds, 2)
}

legend <- ggplot(mod1) + 
  geom_histogram(aes(x=estimate, fill=..x..), bins=20, color='#000000') + 
  scale_fill_gradient2(low='#d7191c', high='#2c7bb6', mid = '#ffffbf', midpoint=0)  +
  theme(panel.background = element_rect(fill = "transparent"), 
        plot.background = element_rect(fill = "transparent", color = NA), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) + 
  guides(fill=FALSE) + 
  scale_x_continuous(labels=convert, breaks = c(-0.4, -0.2, 0, 0.2, 0.4)) +
  xlab('Change in Odds of Experiencing GBV with a 1-Unit SPEI Increase')


ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.5, 0, 0.45, 0.45)

ggsave('C://Users/matt/gbv-tex/Map.pdf', width=10, height=5)
