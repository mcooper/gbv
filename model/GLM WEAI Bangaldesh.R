setwd('G://My Drive/GBV')

library(xtable)
library(tidyverse)
library(broom)
library(readxl)

temp <- read.csv('GBV_FtF_Max_Temps.csv') %>%
  filter(country=='Bangladesh')
temp$year[temp$year==2012] <- 2011

precip <- read.csv('GBV_Precip_Indices.csv') %>%
  filter(country=='Bangladesh')
precip$year[precip$year==2012] <- 2011

tmax <- read.csv('BGD_Temp_LTN.csv')

hh <- read.csv('BGD_hh.csv')
weai <- read_xlsx('../Feed the Future/Bangladesh/4. BIHS_household_2011_15.xlsx') %>%
  mutate(hh_refno=paste0('BGD-', a01),
         feelinputdecagr=f_weai_feelinputdecagr == 'Yes',
         incdec_count=f_weai_incdec_count == 'Yes',
         raiprod_any=f_weai_raiprod_any == 'Yes',
         jown_count=f_weai_jown_count == 'Yes',
         jrightanyagr=f_weai_jrightanyagr == 'Yes',
         credjanydec_any=f_weai_credjanydec_any == 'Yes',
         speakpublic_any=f_weai_speakpublic_any == 'Yes',
         groupmember_any=f_weai_groupmember_any == 'Yes',
         leisuretime=f_weai_leisuretime == 'Yes',
         npoor_z105=f_weai_npoor_z105 == 'Yes') %>%
  select(hh_refno, year=survey_year,fivede=f_weai_fivede,
         feelinputdecagr, incdec_count, raiprod_any, jown_count,
         jrightanyagr, credjanydec_any, speakpublic_any, groupmember_any, 
         leisuretime, npoor_z105)
weai$year[weai$year==2012] <- 2011

all <- Reduce(merge, list(hh, temp, precip, weai, tmax))

##This is a panel, so do difference-in-differences

dd <- all %>% 
  select(year, hh_refno, admin1, admin2, temp12maxZ, spei24, 
         feelinputdecagr, incdec_count, raiprod_any, jown_count, 
         jrightanyagr, credjanydec_any, speakpublic_any, groupmember_any, 
         leisuretime, npoor_z105, fivede) %>%
  gather(key, value, -hh_refno, -year, -admin1, -admin2) %>%
  unite(key_year, key, year) %>%
  spread(key_year, value) %>%
  na.omit %>%
  mutate(temp12maxZ = temp12maxZ_2015 - temp12maxZ_2011,
         spei24 = spei24_2015 - spei24_2011,
         feelinputdecagr = feelinputdecagr_2015 - feelinputdecagr_2011,
         incdec_count = incdec_count_2015 - incdec_count_2011,
         raiprod_any = raiprod_any_2015 - raiprod_any_2011,
         jown_count = jown_count_2015 - jown_count_2011,
         jrightanyagr = jrightanyagr_2015 - jrightanyagr_2011,
         credjanydec_any = credjanydec_any_2015 - credjanydec_any_2011,
         speakpublic_any = speakpublic_any_2015 - speakpublic_any_2011,
         groupmember_any = groupmember_any_2015 - groupmember_any_2011,
         leisuretime = leisuretime_2015 - leisuretime_2011,
         npoor_z105 = npoor_z105_2015 - npoor_z105_2011,
         fivede = fivede_2015 - fivede_2011) %>%
  select(hh_refno, admin1, admin2, temp12maxZ, spei24, 
         feelinputdecagr, incdec_count, raiprod_any, jown_count, 
         jrightanyagr, credjanydec_any, speakpublic_any, groupmember_any, 
         leisuretime, npoor_z105, fivede)

feelinputdecagr <- lm(feelinputdecagr ~ spei24, data=dd)
summary(feelinputdecagr)

incdec_count <- lm(incdec_count ~ spei24, data=dd)
summary(incdec_count)

raiprod_any <- lm(raiprod_any ~ spei24, data=dd)
summary(raiprod_any)

jown_count <- lm(jown_count ~ spei24, data=dd)
summary(jown_count)

jrightanyagr <- lm(jrightanyagr ~ spei24, data=dd)
summary(jrightanyagr)

credjanydec_any <- lm(credjanydec_any ~ spei24, data=dd)
summary(credjanydec_any)

speakpublic_any <- lm(speakpublic_any ~ spei24, data=dd)
summary(speakpublic_any)

groupmember_any <- lm(groupmember_any ~ spei24, data=dd)
summary(groupmember_any)

leisuretime <- lm(leisuretime ~ spei24, data=dd)
summary(leisuretime)

npoor_z105 <- lm(npoor_z105 ~ spei24, data=dd)
summary(npoor_z105)

fivede <- lm(fivede ~ spei24, data=dd)
summary(fivede)

allres <- data.frame()
for (mod in list(feelinputdecagr, incdec_count, raiprod_any, jown_count, jrightanyagr,
              credjanydec_any, speakpublic_any, groupmember_any, leisuretime, npoor_z105,
              fivede)){

  clean <- tidy(mod)
  
  clean$outcome <- gsub(' ~ spei24', '', as.character(mod$call)[2], fixed = T)
  
  allres <- bind_rows(allres, clean)
}

lab1 <- data.frame(outcome=c("feelinputdecagr", 
                             "incdec_count", 
                             "raiprod_any", 
                             "jown_count", 
                             "jrightanyagr", 
                             "credjanydec_any", 
                             "speakpublic_any", 
                             "groupmember_any", 
                             "leisuretime", 
                             "npoor_z105",
                             "fivede"),
                   outcome_lab=c("Input in productive decisions",
                                 "Control over use of income",
                                 "Autonomy in production",
                                 "Ownership of assets",
                                 "Purchase, sale or transfer of asset",
                                 "Access to and decision on credit",
                                 "Speaking in Public",
                                 "Group memeber",
                                 "Leisure",
                                 "Workload",
                                 "Five Domains of Empowerment"))

dat <- Reduce(merge, list(allres, lab1)) %>%
  filter(term=='spei24' & outcome != 'fivede') %>%
  arrange(p.value) %>%
  mutate(Significance = ifelse(p.value*11 > 0.05, '',
                               ifelse(p.value*11 > 0.01, "*", 
                                      ifelse(p.value*11 > 0.001, '**', '***')))) %>%
  select(`Empowerment Indicator`=outcome_lab, `Estimate`=estimate, `Std. Error`=std.error, 
         `Test Statistic`=statistic, `P-Value`=p.value, ` `=Significance)

print(xtable(dat,
             caption='Association between changes in SPEI and changes in various indicators of empowerment.  Because mutiple hypotheses were tested, the stars for p-values shown have been Bonferroni corrected. ***\\textit{p} \\textless 0.001, **\\textit{p} \\textless 0.01, *\\textit{p} \\textless 0.05.',
             label='tab:WEAI',
             align='llrrrrl',
             digits=c(0, 0, 3, 3, 3, 4, 0)),
      include.rownames=FALSE,
      file='C://Users/matt/gbv-tex/tables/WEAI.tex')
