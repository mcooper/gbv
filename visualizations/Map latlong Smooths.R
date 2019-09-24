setwd('G://My Drive/GBV/models')

library(tidyverse)
library(mgcv)
library(raster)

res <- 0.5

############################
#Get Spatial Autocorrelation
############################

load('spei24_ac.Rdata')

mod <- spei24_ac

df <- expand.grid(list(latitude=seq(90 - res/2, -90 + res/2, -res),
                       longitude=seq(-180 + res/2, 180 - res/2, res),
                       spei24=1))

for (c in names(mod$model)) {
  if (c == 'latitude' | c == 'longitude' | c == 'gbv_year'){
    next
  }

  if (class(mod$model[ , c]) == 'numeric'){
    df[ , c] <- 0
  }
  else if (class(mod$model[ , c]) == 'integer'){
    df[ , c] <- 0
  }
  else if (class(mod$model[ , c]) == 'factor'){
    df[ , c] <- levels(mod$model[ , c])[1]
  }
}

result <- cbind(df, predict(mod, df, type='terms', se.fit=TRUE))

m <- matrix(result$`se.fit.s(latitude,longitude):spei24`, nrow=180/res, ncol=360/res)

r <- raster(m, xmn=-180, xmx=180, ymn=-90, ymx=90)

plot(r)
points(mod$model$longitude, mod$model$latitude, pch=16, cex=0.1)

############################
#Get Varying Intercept
############################

load('spei24_viol_phys_any_ve.Rdata')
load('spei24_viol_phys_ip_ve.Rdata')
load('spei24_viol_sex_ip_ac_ve.Rdata')

mod <- spei24_viol_sex_ip_ve

df2 <- expand.grid(list(latitude=seq(90 - res/2, -90 + res/2, -res),
                       longitude=seq(-180 + res/2, 180 - res/2, res),
                       spei24=1))

for (c in names(mod$model)) {
  if (c == 'latitude' | c == 'longitude' | c == 'gbv_year' | c == 'spei24'){
    next
  }
  
  if (class(mod$model[ , c]) == 'numeric'){
    df[ , c] <- 0
  }
  else if (class(mod$model[ , c]) == 'integer'){
    df[ , c] <- 0
  }
  else if (class(mod$model[ , c]) == 'factor'){
    df[ , c] <- levels(mod$model[ , c])[1]
  }
}

result2 <- cbind(df, predict(mod, df2, type='terms', se.fit=TRUE))

m <- matrix(result$`fit.s(latitude,longitude):spei24` + result$`fit.s(spei24)`
            , nrow=180/res, ncol=360/res)

r <- raster(m, xmn=-180, xmx=180, ymn=-90, ymx=90)

se.m <- matrix(result$`se.fit.s(latitude,longitude):spei24` + result$`se.fit.s(spei24)`, nrow=180/res, ncol=360/res)
se.r <- raster(se.m, xmn=-180, xmx=180, ymn=-90, ymx=90)

signif <- (abs(r) - se.r) > 0

plot(r)
points(mod$model$longitude, mod$model$latitude, pch=16, cex=0.1)
