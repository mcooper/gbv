library(mgcv)
library(tidyverse)
library(ggplot2)
library(texreg)

setwd('G://My Drive/GBV/models/')

for (f in list.files(pattern='spei.._sex.Rdata|spei.._phys.Rdata')){
  load(f)
  print(f)
  print(length(residuals(get(gsub('.Rdata', '', f)))))
  
}

##############################
#Make Tables of Effects
##############################
# getAMEprobSE <- function(mod){
#   #Average marginal effect, expressed as a probability
#   
#   #Get coefficients
#   se <- summary(mod)$se
#   se <- se[!grepl('country|^s\\(', names(se))]
#   log_odds <- exp(se)
#   
#   prob <- rep(0, 50) #For some reason it expects 50 coefficients???
#   prob[1:length(se)] <- exp(se)/(1+exp(se))
# 
#   return(c(prob)*100)
# }
# 
# getAMEprob <- function(mod){
#   #Average marginal effect, expressed as a probability
#   
#   #Get coefficients
#   coefs <- mod$coefficients
#   coefs <- coefs[!grepl('country|^s\\(', names(coefs))]
#   log_odds <- exp(coefs)
#   ame <- 1 - log_odds
#   
#   prob <- rep(0, 50) #For some reason it expects 50 coefficients???
#   prob[1:length(ame)] <- ame/(1 + ame)
#   
#   return(c(prob)*100)
# }


#Physical Violence
texreg(l=list(spei12_phys, spei24_phys, spei36_phys, spei48_phys),
       file='C://Users/matt/gbv-tex/tables/testspei_phys.tex',
       custom.model.names=c('12 Months', '24 Months', '36 Months', '48 Months'),
       custom.coef.map=list('(Intercept)'='Intercept',
                            'wealth_factor_harmonized'='Harmonized Wealth Factor',
                            'hhsize'='Household Size',
                            'date_cmc'='Century-Month-Code',
                            'years_educationNone'='Education - None',
                            'years_educationPrimary'='Education - Primary',
                            'years_educationSecondary'='Education - Secondary',
                            'EDF: s(spei12)'='12-Month SPEI Smooth EDF',
                            'EDF: s(spei24)'='24-Month SPEI Smooth EDF',
                            'EDF: s(spei36)'='36-Month SPEI Smooth EDF',
                            'EDF: s(spei48)'='48-Month SPEI Smooth EDF',
                            'EDF: s(latitude,longitude)'='Lat-Long Smooth EDF'),
       omit.coef='country|^s\\(',
       digits=3,
       # override.coef=list(getAMEprob(spei12_phys),
       #                    getAMEprob(spei24_phys),
       #                    getAMEprob(spei36_phys),
       #                    getAMEprob(spei48_phys)),
       # override.se=list(getAMEprob(spei12_phys),
       #                  getAMEprob(spei24_phys),
       #                  getAMEprob(spei36_phys),
       #                  getAMEprob(spei48_phys)),
       caption='Modeling the relationship between rainfall anomalies (SPEI) at 12, 24, 36, and 48-month intervals and women experiencing physical IPV in the previous year.  Reported coefficients and confidence intervals are in log-odds.',
       label = 'table:testspei_phys',
       float.pos = 'H')



#Physical Violence
texreg(l=list(spei12_sex, spei24_sex, spei36_sex, spei48_sex),
       file='C://Users/matt/gbv-tex/tables/testspei_sex.tex',
       custom.model.names=c('12 Months', '24 Months', '36 Months', '48 Months'),
       custom.coef.map=list('(Intercept)'='Intercept',
                            'wealth_factor_harmonized'='Harmonized Wealth Factor',
                            'hhsize'='Household Size',
                            'date_cmc'='Century-Month-Code',
                            'years_educationNone'='Education - None',
                            'years_educationPrimary'='Education - Primary',
                            'years_educationSecondary'='Education - Secondary',
                            'EDF: s(spei12)'='12-Month SPEI Smooth EDF',
                            'EDF: s(spei24)'='24-Month SPEI Smooth EDF',
                            'EDF: s(spei36)'='36-Month SPEI Smooth EDF',
                            'EDF: s(spei48)'='48-Month SPEI Smooth EDF',
                            'EDF: s(latitude,longitude)'='Lat-Long Smooth EDF'),
       omit.coef='country|^s\\(',
       digits=3,
       # override.coef=list(getAMEprob(spei12_phys),
       #                    getAMEprob(spei24_phys),
       #                    getAMEprob(spei36_phys),
       #                    getAMEprob(spei48_phys)),
       # override.se=list(getAMEprob(spei12_phys),
       #                  getAMEprob(spei24_phys),
       #                  getAMEprob(spei36_phys),
       #                  getAMEprob(spei48_phys)),
       caption='Modeling the relationship between rainfall anomalies (SPEI) at 12, 24, 36, and 48-month intervals and women experiencing sexual IPV in the previous year.  Reported coefficients and confidence intervals are in log-odds.',
       label = 'table:testspei_sex',
       float.pos = 'H')

##############################
#Make Graphs of Effects
##############################
getData <- function(mod, data, window, outcome, smooth){
  names(data)[names(data)=='spei'] <- paste0('spei', window)
  
  res <- predict(mod, data, type='terms', se.fit = TRUE)
  
  spei <- data[ , paste0('spei', window)]
  pred <- res$fit[ , paste0('s(spei', window, ')')]
  se <- res$se.fit[ , paste0('s(spei', window, ')')]
  
  final <- data.frame(spei,
                      pred,
                      se,
                      window,
                      outcome,
                      smooth)
  
  return(final)
}

data <- data.frame(wealth_factor_harmonized=0,
                   hhsize=5,
                   date_cmc=13000,
                   years_education='Higher',
                   country='AM', 
                   spei=seq(-2.75, 2.75, by = 0.1),
                   latitude=0,
                   longitude=0)

spei12_phys_dat <- getData(spei12_phys, data, window='12', outcome='Physical Violence', smooth='tp')
spei24_phys_dat <- getData(spei24_phys, data, window='24', outcome='Physical Violence', smooth='tp')
spei36_phys_dat <- getData(spei36_phys, data, window='36', outcome='Physical Violence', smooth='tp')
spei48_phys_dat <- getData(spei48_phys, data, window='48', outcome='Physical Violence', smooth='tp')
spei12_sex_dat <- getData(spei12_sex, data, window='12', outcome='Sexual Violence', smooth='tp')
spei24_sex_dat <- getData(spei24_sex, data, window='24', outcome='Sexual Violence', smooth='tp')
spei36_sex_dat <- getData(spei36_sex, data, window='36', outcome='Sexual Violence', smooth='tp')
spei48_sex_dat <- getData(spei48_sex, data, window='48', outcome='Sexual Violence', smooth='tp')

all <- bind_rows(spei12_phys_dat,
                 spei24_phys_dat,
                 spei36_phys_dat,
                 spei48_phys_dat,
                 spei12_sex_dat,
                 spei24_sex_dat,
                 spei36_sex_dat,
                 spei48_sex_dat)

all$max <- all$pred + all$se*2
all$min <- all$pred - all$se*2

logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

prob_change <- function(coef){
  res <- logit2prob(coef) - logit2prob(0)
  round(res, 3)
}

ggplot(all) + 
  geom_hline(aes(yintercept=0), linetype=2) + 
  geom_line(aes(x=spei, y=pred, color=window), size=1) + 
  geom_ribbon(aes(x=spei, ymin=min, ymax=max, fill=window), alpha=0.1) + 
  facet_wrap(. ~ outcome) + 
  theme_minimal() + 
  theme(strip.text.x=element_text(size = 15)) + 
  scale_y_continuous(labels=prob_change) + 
  labs(x='Standardized Precipitation-Evapotranspiration Index',
       y='Change in Probability of Experincing IPV (Percent)',
       color='Precip\nAnomaly\nWindow\n(Months)', fill='Precip\nAnomaly\nWindow\n(Months)')

ggsave('C://Users/matt/gbv-tex/SPEI-Compare.pdf', height=5, width=10)

