library(rasterVis)
library(raster)
library(rgdal)
library(sp)
library(RColorBrewer)
library(dplyr)

#########################
#Read Data
########################

sp <- readOGR('G://My Drive/DHS Spatial Covars/Global Codes and Shapefile',
              'ne_50m_admin_0_countries')

spt <- spTransform(sp, CRS("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))

data <- read.csv('G://My Drive/DHS Processed/GBV_all.csv') %>%
  filter(mean_annual_precip > 200 & builtup < 0.1) %>%
  group_by(latitude, longitude) %>%
  summarize(GBV_rate=mean(viol_phys))

spdat <- SpatialPointsDataFrame(coords=data[ , c('longitude', 'latitude')],
                                data=data, proj4string = CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'))

spdat_t <- spTransform(spdat, CRS("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))

spdat_t@data[ , c('longitude', 'latitude')] <- spdat_t@coords

####################
#Make Images
####################

setwd('C://Users/matt/gbv-tex')

##################
#DHS Points
###################

plt <- spplot(spdat_t, "GBV_rate", 
               col.regions = c("#780000", "#780000", "#780000"), 
               cex = 0.1, 
               sp.layout=list('sp.polygons', spt, fill="#DDDDDD"))

plt$legend$bottom$args$key$text[[1]] <- c("", "", "")
plt$legend$bottom$args$key$points$cex <- c(0,0,0)

pdf("DHSPoints.pdf", width=8, height=6)
plot(plt)
dev.off()

system("pdfcrop DHSPoints.pdf DHSPoints.pdf")

