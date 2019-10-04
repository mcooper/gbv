setwd('G://My Drive/GBV/models')

library(tidyverse)
library(mgcv)

load('spei36_phys_empowered_age_marriage.Rdata') #Good
load('spei36_phys_empowered_age_sex.Rdata') #Good
load('spei36_phys_empowered_education.Rdata') #OK
load('spei36_phys_empowered_decisions.Rdata')
load('spei36_phys_empowered_gbv_notok.Rdata')



##############################
#Make Graphs of Effects
##############################
getData <- function(mod, data, emp, lab){
  names(data)[names(data)=='empowered'] <- emp

  res <- predict(mod, data, type='terms', se.fit = TRUE)
  
  pred.emp <- res$fit[ , paste0('s(spei36):', emp, 'TRUE')]
  pred.notemp <- res$fit[ , paste0('s(spei36):', emp, 'FALSE')]
  se.emp <- res$se.fit[ , paste0('s(spei36):', emp, 'TRUE')]
  se.notemp <- res$se.fit[ , paste0('s(spei36):', emp, 'FALSE')]
  
  
  final <- bind_rows(data.frame(spei=data$spei36,
                                pred=pred.emp,
                                se=se.emp,
                                metric=lab,
                                empowered=data[ , emp]),
                     data.frame(spei=spei,
                                pred=pred.notemp,
                                se=se.notemp,
                                metric=lab,
                                empowered=data[ , emp])) %>%
    filter(pred!=0)
  
  return(final)
}

data <- expand.grid(list(spei36=seq(-2.75, 2.75, by = 0.1),
                         empowered=c(TRUE, FALSE)))

data$wealth_factor_harmonized <- 0
data$hhsize <- 5
data$date_cmc <- 13000
data$years_education <- 'Higher'
data$country <- 'AM' 
data$latitude <- 0
data$longitude <- 0

age_marriage_dat <- getData(spei36_phys_empowered_age_marriage, data, emp='empowered_age_marriage', lab='Marriage Age')
age_sex_dat <- getData(spei36_phys_empowered_age_sex, data, emp='empowered_age_sex', lab='First Sex Age')
education_dat <- getData(spei36_phys_empowered_education, data, emp='empowered_education', lab='Education')
decisions_dat <- getData(spei36_phys_empowered_decisions, data, emp='empowered_decisions', lab='Decisions')
gbv_notok_dat <- getData(spei36_phys_empowered_gbv_notok, data, emp='empowered_gbv_notok', lab='IPV Never OK')

all <- bind_rows(age_marriage_dat,
                 age_sex_dat,
                 education_dat,
                 decisions_dat,
                 gbv_notok_dat)

all$max <- all$pred + all$se*2
all$min <- all$pred - all$se*2

ggplot(all) + 
  geom_hline(aes(yintercept=0), linetype=2) + 
  geom_line(aes(x=spei, y=pred, color=empowered), size=1) + 
  geom_ribbon(aes(x=spei, ymin=min, ymax=max, fill=empowered), alpha=0.1) + 
  scale_color_manual(values = c("FALSE"="#ca0020", "TRUE"="#0571b0"), labels=c("No", "Yes")) + 
  scale_fill_manual(values = c("FALSE"="#ca0020", "TRUE"="#0571b0"), labels=c("No", "Yes")) + 
  facet_wrap(. ~ metric) + 
  theme_minimal() + 
  theme(strip.text.x=element_text(size = 15),
        legend.position = c(0.85, 0.25)) + 
  labs(x='36-Month Standardized Precipitation-Evapotranspiration Index',
       y='Change in Log-Odds of Experincing IPV',
       fill='Is Empowered?', color='Is Empowered?')

ggsave('C://Users/matt/gbv-tex/Empowerment.pdf', width=8.5, height=5)
