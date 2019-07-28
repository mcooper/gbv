setwd('G://My Drive/GBV')

library(tidyverse)
library(broom)

dat <- read.csv('GBV_FtF_DiD.csv')

weai <- c("credjanydec_any", "feelinputdecagr", 
  "groupmember_any", "incdec_count", "jown_count", "jrightanyagr", 
  "leisuretime", 
  "npoor_z105", "raiprod_any", "speakpublic_any")

climate <- c("spei12", "spei24", 
  "spei36", "spei48", "spei6", "temp12max", "temp12maxZ", "temp24max", 
  "temp24maxZ", "temp6max", "temp6maxZ")

cty <- c('Bangladesh', 'Zambia')

all <- expand.grid(list(weai=weai, climate=climate, cty=cty)) %>%
  filter(!(cty=='Zambia' & weai=='raiprod_any'))

all$statistic <- NA
all$coef <- NA

accum <- data.frame()
for (i in 1:nrow(all)){
  
  f <- paste0(all$weai[i], ' ~ ', all$climate[i])
  
  mod <- lm(f, data=dat[dat$country==all$cty[i], ])
  
  all$statistic[i] <- tidy(mod)$statistic[2] 
  all$coef[i] <- tidy(mod)$estimate[2]
  
}

all$stat_mag <- abs(all$stat)
