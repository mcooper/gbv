setwd('G://My Drive/GBV')

library(lme4)
library(tidyverse)
library(broom)

temp <- read.csv('GBV_Temps.csv') %>%
  filter(country=='Bangladesh')
temp$year[temp$year==2012] <- 2011

precip <- read.csv('GBV_Precip_Indices.csv') %>%
  filter(country=='Bangladesh')
precip$year[precip$year==2012] <- 2011

hh <- read.csv('BGD_hh.csv')
weai <- read.csv('BGD_WEAI.csv') %>%
  mutate(feelinputdecagr=feelinputdecagr=="Yes", 
         incdec_count=incdec_count=="Yes", 
         raiprod_any=raiprod_any=="Yes", 
         jown_count=jown_count=="Yes", 
         jrightanyagr=jrightanyagr=="Yes", 
         credjanydec_any=credjanydec_any=="Yes", 
         speakpublic_any=speakpublic_any=="Yes",
         groupmember_any=groupmember_any=="Yes", 
         leisuretime=leisuretime=="Yes", 
         npoor_z105=npoor_z105=="Yes")

all <- Reduce(merge, list(hh, temp, precip, weai))

#feelinputdecagr
feelinputdecagr <- glmer(feelinputdecagr ~ hhhead_education + hhhead_literate + hhhead_sex + hhhead_age + 
                         hhhead_religion + hh_size + irrigation + dependents + asset_index + 
                         ct37.5_3m + spei24 + zsq_1m + (1|year) + (1|admin1),
                       data=all, family = 'binomial')
summary(feelinputdecagr)

#leisuretime
leisuretime <- glmer(leisuretime ~ hhhead_education + hhhead_literate + hhhead_sex + hhhead_age + 
      hhhead_religion + hh_size + irrigation + dependents + asset_index + 
      ct37.5_3m + spei24 + zsq_1m + (1|year) + (1|admin1),
      data=all, family = 'binomial')
summary(leisuretime)

#incdec_count
incdec_count <- glmer(incdec_count ~ hhhead_education + hhhead_literate + hhhead_sex + hhhead_age + 
                         hhhead_religion + hh_size + irrigation + dependents + asset_index + 
                         ct37.5_3m + spei24 + zsq_1m + (1|year) + (1|admin1),
                       data=all, family = 'binomial')
summary(incdec_count)

#raiprod_any
raiprod_any <- glmer(raiprod_any ~ hhhead_education + hhhead_literate + hhhead_sex + hhhead_age + 
                         hhhead_religion + hh_size + irrigation + dependents + asset_index + 
                         ct37.5_3m + spei24 + zsq_1m + (1|year) + (1|admin1),
                       data=all, family = 'binomial')
summary(raiprod_any)

#jown_count
jown_count <- glmer(jown_count ~ hhhead_education + hhhead_literate + hhhead_sex + hhhead_age + 
                         hhhead_religion + hh_size + irrigation + dependents + asset_index + 
                         ct37.5_3m + spei24 + zsq_1m + (1|year) + (1|admin1),
                       data=all, family = 'binomial')
summary(jown_count)

#jrightanyagr
jrightanyagr <- glmer(jrightanyagr ~ hhhead_education + hhhead_literate + hhhead_sex + hhhead_age + 
                         hhhead_religion + hh_size + irrigation + dependents + asset_index + 
                         ct37.5_3m + spei24 + zsq_1m + (1|year) + (1|admin1),
                       data=all, family = 'binomial')
summary(jrightanyagr)

#credjanydec_any
credjanydec_any <- glmer(credjanydec_any ~ hhhead_education + hhhead_literate + hhhead_sex + hhhead_age + 
                         hhhead_religion + hh_size + irrigation + dependents + asset_index + 
                         ct37.5_3m + spei24 + zsq_1m + (1|year) + (1|admin1),
                       data=all, family = 'binomial')
summary(credjanydec_any)

#speakpublic_any
speakpublic_any <- glmer(speakpublic_any ~ hhhead_education + hhhead_literate + hhhead_sex + hhhead_age + 
                         hhhead_religion + hh_size + irrigation + dependents + asset_index + 
                         ct37.5_3m + spei24 + zsq_1m + (1|year) + (1|admin1),
                       data=all, family = 'binomial')
summary(speakpublic_any)

#groupmember_any
groupmember_any <- glmer(groupmember_any ~ hhhead_education + hhhead_literate + hhhead_sex + hhhead_age + 
                         hhhead_religion + hh_size + irrigation + dependents + asset_index + 
                         ct37.5_3m + spei24 + zsq_1m + (1|year) + (1|admin1),
                       data=all, family = 'binomial')
summary(groupmember_any)

#npoor_z105
npoor_z105 <- glmer(npoor_z105 ~ hhhead_education + hhhead_literate + hhhead_sex + hhhead_age + 
                         hhhead_religion + hh_size + irrigation + dependents + asset_index + 
                         ct37.5_3m + spei24 + zsq_1m + (1|year) + (1|admin1),
                       data=all, family = 'binomial')
summary(npoor_z105)


##This is a panel, so do difference-in-differences

dd <- all %>% 
  select(year, hh_refno, admin1, admin2, zsq_1m, ct37.5_3m, spei36, 
         feelinputdecagr, incdec_count, raiprod_any, jown_count, 
         jrightanyagr, credjanydec_any, speakpublic_any, groupmember_any, 
         leisuretime, npoor_z105) %>%
  gather(key, value, -hh_refno, -year, -admin1, -admin2) %>%
  unite(key_year, key, year) %>%
  spread(key_year, value) %>%
  na.omit %>%
  mutate(ct37.5_3m = ct37.5_3m_2015 - ct37.5_3m_2011,
         spei36 = spei36_2015 - spei36_2011,
         zsq_1m = zsq_1m_2015 - zsq_1m_2011,
         feelinputdecagr = feelinputdecagr_2015 - feelinputdecagr_2011,
         incdec_count = incdec_count_2015 - incdec_count_2011,
         raiprod_any = raiprod_any_2015 - raiprod_any_2011,
         jown_count = jown_count_2015 - jown_count_2011,
         jrightanyagr = jrightanyagr_2015 - jrightanyagr_2011,
         credjanydec_any = credjanydec_any_2015 - credjanydec_any_2011,
         speakpublic_any = speakpublic_any_2015 - speakpublic_any_2011,
         groupmember_any = groupmember_any_2015 - groupmember_any_2011,
         leisuretime = leisuretime_2015 - leisuretime_2011,
         npoor_z105 = npoor_z105_2015 - npoor_z105_2011) %>%
  select(hh_refno, admin1, admin2, zsq_1m, ct37.5_3m, spei36, 
         feelinputdecagr, incdec_count, raiprod_any, jown_count, 
         jrightanyagr, credjanydec_any, speakpublic_any, groupmember_any, 
         leisuretime, npoor_z105)

feelinputdecagr <- lmer(feelinputdecagr ~ spei36 + ct37.5_3m + zsq_1m + (1|admin1) + (1|admin2), data=dd)
summary(feelinputdecagr)

incdec_count <- lmer(incdec_count ~ spei36 + ct37.5_3m + zsq_1m + (1|admin1) + (1|admin2), data=dd)
summary(incdec_count)

raiprod_any <- lmer(raiprod_any ~ spei36 + ct37.5_3m + zsq_1m + (1|admin1) + (1|admin2), data=dd)
summary(raiprod_any)

jown_count <- lmer(jown_count ~ spei36 + ct37.5_3m + zsq_1m + (1|admin1) + (1|admin2), data=dd)
summary(jown_count)

jrightanyagr <- lmer(jrightanyagr ~ spei36 + ct37.5_3m + zsq_1m + (1|admin1) + (1|admin2), data=dd)
summary(jrightanyagr)

credjanydec_any <- lmer(credjanydec_any ~ spei36 + ct37.5_3m + zsq_1m + (1|admin1) + (1|admin2), data=dd)
summary(credjanydec_any)

speakpublic_any <- lmer(speakpublic_any ~ spei36 + ct37.5_3m + zsq_1m + (1|admin1) + (1|admin2), data=dd)
summary(speakpublic_any)

groupmember_any <- lmer(groupmember_any ~ spei36 + ct37.5_3m + zsq_1m + (1|admin1) + (1|admin2), data=dd)
summary(groupmember_any)

leisuretime <- lmer(leisuretime ~ spei36 + ct37.5_3m + zsq_1m + (1|admin1) + (1|admin2), data=dd)
summary(leisuretime)

npoor_z105 <- lmer(npoor_z105 ~ spei36 + ct37.5_3m + zsq_1m + (1|admin1) + (1|admin2), data=dd)
summary(npoor_z105)

allres <- data.frame()
for (mod in c(feelinputdecagr, incdec_count, raiprod_any, jown_count, jrightanyagr,
              credjanydec_any, speakpublic_any, groupmember_any, leisuretime, npoor_z105)){
  
  clean <- tidy(mod) %>%
    filter(group=='fixed' & term != '(Intercept)') %>%
    select(-group)
  
  clean$outcome <- gsub(' ~ spei36 + ct37.5_3m + zsq_1m + (1 | admin1) + (1 | admin2)', '', as.character(mod@call)[2], fixed = T)
  
  allres <- bind_rows(allres, clean)
}

allres$pvalue <- dnorm(allres$statistic, 0, 1)

lab1 <- data.frame(outcome=c("feelinputdecagr", 
                             "incdec_count", 
                             "raiprod_any", 
                             "jown_count", 
                             "jrightanyagr", 
                             "credjanydec_any", 
                             "speakpublic_any", 
                             "groupmember_any", 
                             "leisuretime", 
                             "npoor_z105"),
                   outcome_lab=c("Input in productive decisions",
                                 "Control over use of income",
                                 "Autonomy in production",
                                 "Ownership of asset",
                                 "Purchase, sale or transfer of asset",
                                 "Access to and decision on credit",
                                 "Speaking in Public",
                                 "Group memeber",
                                 "Leisure",
                                 "Workload"))
lab2 <- data.frame(term=c('zsq_1m', 'spei36', 'ct37.5_3m'),
                   term_lab=c('Z-score of mean maximum temperature for previous month',
                              '36-month Standardized Precipitation-Evapotranspiration Index (SPEI)',
                              'Count of days above 37.5C/100F in previous 3 months'))

dat <- Reduce(merge, list(allres, lab1, lab2)) %>%
  select(-term, -outcome)

write.csv(dat, 'G://My Drive/GBV/BGD diff-in-diff results.csv', row.names=F)
write.csv(dd, 'G://My Drive/GBV/BGD diff-in-diff data.csv', row.names=F)
write.csv(all, 'G://My Drive/GBV/BGD all data.csv', row.names=F)