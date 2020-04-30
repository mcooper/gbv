library(mgcv)
library(tidyverse)
library(ggplot2)
library(texreg)

setwd('G://My Drive/GBV/models/')

for (f in list.files(pattern='spei.._sex.Rdata|spei.._phys.Rdata|spei.._sex_cr.Rdata|spei.._phys_cr.Rdata|spei.._sex_ps.Rdata|spei.._phys_ps.Rdata')){
  load(f)
  print(f)
  print(length(residuals(get(gsub('.Rdata', '', f)))))
  
}

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
                   date_cmc=1300,
                   years_education='Higher',
                   country='AM', 
                   spei=seq(-2.75, 2.75, by = 0.1),
                   latitude=0,
                   longitude=0)

comb <- expand.grid(list(window=c('12', '24', '36', '48'),
                         outcome=c('Physical Violence', 'Sexual Violence'),
                         smooth=c('tp', 'cr', 'ps')),
                    stringsAsFactors=F)

all <- data.frame()
for (i in 1:nrow(comb)){
  print(comb[i, ])
  
  mod <- get(paste0('spei', comb$window[i], '_', 
                    ifelse(comb$outcome[i]=='Physical Violence', 'phys', 'sex'),
                    ifelse(comb$smooth[i]=='cr', '_cr',
                           ifelse(comb$smooth[i]=='ps', '_ps', ''))))
  
  names(data)[grepl('spei', names(data))] <- paste0('spei', comb$window[i])
  
  res <- getData(mod, data, window=comb$window[i], outcome=comb$outcome[i], smooth=comb$smooth[i])

  all <- bind_rows(all, res)
}

all$max <- all$pred + all$se*2
all$min <- all$pred - all$se*2

logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

prob_change <- function(coef){
  res <- logit2prob(coef) - logit2prob(0)
  res
}

all$pred_me <- prob_change(all$pred)
all$max_me <- prob_change(all$max)
all$min_me <- prob_change(all$min)

###################################
#Graph for publication
###################################
ggplot(all %>% filter(smooth=='tp')) + 
  geom_hline(aes(yintercept=0), linetype=2) + 
  geom_line(aes(x=spei, y=pred, color=window), size=1) + 
  geom_ribbon(aes(x=spei, ymin=min, ymax=max, fill=window), alpha=0.1) + 
  facet_wrap(. ~ outcome) + 
  theme_minimal() + 
  theme(strip.text.x=element_text(size = 15)) + 
  labs(x='Standardized Precipitation-Evapotranspiration Index',
       y='Change in Probability of Experincing IPV (Percent)',
       color='Precip\nAnomaly\nWindow\n(Months)', fill='Precip\nAnomaly\nWindow\n(Months)')

ggsave('C://Users/matt/gbv-tex/SPEI-Compare.pdf', height=5, width=10)


###################################
#Graph for appendix
###################################
ggplot(all) + 
  geom_hline(aes(yintercept=0), linetype=2) + 
  geom_line(aes(x=spei, y=pred_me, color=window), size=1) + 
  geom_ribbon(aes(x=spei, ymin=min_me, ymax=max_me, fill=window), alpha=0.1) + 
  facet_grid(smooth ~ outcome, labeller = labeller(smooth=c(cr="Cubic Spline", tp="Thin Plate Splines", ps="Penalized B-Splines"))) + 
  theme_minimal() + 
  theme(strip.text.x=element_text(size = 15)) + 
  labs(x='Standardized Precipitation-Evapotranspiration Index',
       y='Change in Probability of Experincing IPV (Percent)',
       color='Precip\nAnomaly\nWindow\n(Months)', fill='Precip\nAnomaly\nWindow\n(Months)')

ggsave('C://Users/matt/gbv-tex/SPEI-Compare_Smooths.pdf', height=15, width=10)



##############################
#Make Tables of Effects
##############################
getAMEprobSE <- function(mod){
  #Average marginal effect, expressed as a probability

  #Get coefficients
  se <- summary(mod)$se
  se <- se[!grepl('country|^s\\(', names(se))]
  log_odds <- exp(se)

  prob <- rep(0, 50) #For some reason it expects 50 coefficients???
  prob[1:length(se)] <- exp(se)/(1+exp(se))

  return(c(prob)*100)
}

getAMEprob <- function(mod){
  #Average marginal effect, expressed as a probability

  #Get coefficients
  coefs <- mod$coefficients
  coefs <- coefs[!grepl('country|^s\\(', names(coefs))]
  odds <- exp(coefs)
  prob <- odds/(1 + odds)
  mar_prob <- prob - 0.5

  res <- rep(0, 50) #For some reason it expects 50 coefficients???
  res[1:length(mar_prob)] <- mar_prob

  edf.table <- summary(mod)$s.table

  res[(length(mar_prob) + 1):(length(mar_prob) + nrow(edf.table))]
  
  return(res)
}


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



#Sexual Violence
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
