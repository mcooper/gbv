library(tidyverse)
library(xtable)
library(lubridate)
library(countrycode)
library(mattr)
library(recipes)

dat <- read.csv('G://My Drive/GBV/GBV_sel.csv')

##################################
# Summary of Variables
######################################

summary <- dat %>%
  mutate(`plos_age(15,19]` = plos_age == '(15,19]',
         `plos_age(19,29]` = plos_age == '(19,29]',
         `plos_age(29,39]` = plos_age == '(29,39]',
         `plos_age(39,49]` = plos_age == '(39,49]',
         `plos_births(0.5,2]` = plos_births == '(0.5,2]',
         `plos_births(2,4]` = plos_births == '(2,4]',
         `plos_births(4,48]` = plos_births == '(4,48]',
         `plos_hhsize(0,3]` = plos_hhsize == '(0,3]',
         `plos_hhsize(3,5]` = plos_hhsize == '(3,5]',
         `plos_hhsize(5,48]` = plos_hhsize == '(5,48]',
         `plos_husband_age(0,19]` = plos_husband_age == '(0,19]',
         `plos_husband_age(19,29]` = plos_husband_age == '(19,29]',
         `plos_husband_age(29,39]` = plos_husband_age == '(29,39]',
         `plos_husband_age(39,49]` = plos_husband_age == '(39,49]',
         `plos_husband_age(49,99]` = plos_husband_age == '(49,99]',
         `husband_education_levelHigher` = husband_education_level == 'Higher',
         `husband_education_levelNone` = husband_education_level == 'None',
         `husband_education_levelPrimary` = husband_education_level == 'Primary',
         `husband_education_levelSecondary` = husband_education_level == 'Secondary',
         drought_catnormal = drought_cat == 'normal',
         drought_catdrought = drought_cat == 'drought',
         drought_catsevere = drought_cat == 'severe') %>%
  select_if(is.logical) %>%
  gather(Var, Val) %>%
  group_by(Var) %>%
  summarize(Percent = paste0(round(mean(Val)*100, 1), '%'),
            Count = format(sum(Val), big.mark=','))

#write.csv(summary$Var, 'C://Users/matt/gbv/visualizations/labels.csv', row.names=F)
labs <- read.csv('C://Users/matt/gbv/visualizations/labels.csv')

summaryl <- merge(summary, labs) %>%
  arrange(Order) %>%
  select(`  `=Category, ` `=Label, Count, Percent, -Order, -Var)

hline <- c(-1,0, which(summaryl$`  ` != '') - 1, nrow(summaryl))
htype <- c("\\toprule ", "\\midrule ", rep("\\midrule ", sum(summaryl$`  ` != '')), "\\bottomrule ")
print(xtable(summaryl, 
             caption='Summary of Variables Used in Regressions',
             label='tab:var_sum'),
      include.rownames=FALSE,
      hline.after = NULL,
      add.to.row = list(pos = as.list(hline),
                        command = htype),
      file='C://Users/matt/ipv-rep-tex/tables/variable_summary.tex')

########################################
# Summary of Observations by Country and Year
#########################################
t <- addmargins(table(countrycode(dat$country, 'dhs', 'country.name'), 
                      dat$year))

print(xtable(t, 
             caption='Count of Observations by Country and Year',
             label='tab:cty_year',
             digits=0),
      file='C://Users/matt/ipv-rep-tex/tables/country_year.tex', 
      #size="\\fontsize{8pt}{8pt}\\selectfont", 
      floating.environment = "sidewaystable")




