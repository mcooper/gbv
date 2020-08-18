library(tidyverse)
library(xtable)
library(lubridate)
library(countrycode)

dat <- read.csv('~/mortalityblob/gbv/GBV_sel.csv')

##################################
# Summary of Variables
######################################

africa <- dat %>%
  filter(in_afr) %>%
  mutate(`plos_age(14,19]` = plos_age == '(14,19]',
         `plos_age(19,29]` = plos_age == '(19,29]',
         `plos_age(29,39]` = plos_age == '(29,39]',
         `plos_age(39,49]` = plos_age == '(39,49]',
         `plos_births(0.5,2]` = plos_births == '(0.5,2]',
         `plos_births(2,4]` = plos_births == '(2,4]',
         `plos_births(4,48]` = plos_births == '(4,48]',
         `plos_hhsize(1,3]` = plos_hhsize == '(1,3]',
         `plos_hhsize(3,5]` = plos_hhsize == '(3,5]',
         `plos_hhsize(5,48]` = plos_hhsize == '(5,48]',
         `plos_husband_age(14,19]` = plos_husband_age == '(14,19]',
         `plos_husband_age(19,29]` = plos_husband_age == '(19,29]',
         `plos_husband_age(29,39]` = plos_husband_age == '(29,39]',
         `plos_husband_age(39,49]` = plos_husband_age == '(39,49]',
         `plos_husband_age(49,99]` = plos_husband_age == '(49,99]',
         `husband_education_levelHigher` = husband_education_level == 'Higher',
         `husband_education_levelNone` = husband_education_level == 'None',
         `husband_education_levelPrimary` = husband_education_level == 'Primary',
         `husband_education_levelSecondary` = husband_education_level == 'Secondary',
         drought_catnormal = drought_cat == 'normal',
         drought_catmoderate = drought_cat == 'moderate',
         drought_catsevere = drought_cat == 'severe',
         drought_catextreme = drought_cat == 'extreme',
         Total = TRUE) %>%
  select_if(is.logical) %>%
  gather(Var, Val) %>%
  group_by(Var) %>%
  summarize(Afr.Count = format(sum(Val), big.mark=','),
            Afr.Percent = paste0('(', round(mean(Val)*100, 1), '%)'))

asia <- dat %>%
  filter(in_asia) %>%
  mutate(`plos_age(14,19]` = plos_age == '(14,19]',
         `plos_age(19,29]` = plos_age == '(19,29]',
         `plos_age(29,39]` = plos_age == '(29,39]',
         `plos_age(39,49]` = plos_age == '(39,49]',
         `plos_births(0.5,2]` = plos_births == '(0.5,2]',
         `plos_births(2,4]` = plos_births == '(2,4]',
         `plos_births(4,48]` = plos_births == '(4,48]',
         `plos_hhsize(1,3]` = plos_hhsize == '(1,3]',
         `plos_hhsize(3,5]` = plos_hhsize == '(3,5]',
         `plos_hhsize(5,48]` = plos_hhsize == '(5,48]',
         `plos_husband_age(14,19]` = plos_husband_age == '(14,19]',
         `plos_husband_age(19,29]` = plos_husband_age == '(19,29]',
         `plos_husband_age(29,39]` = plos_husband_age == '(29,39]',
         `plos_husband_age(39,49]` = plos_husband_age == '(39,49]',
         `plos_husband_age(49,99]` = plos_husband_age == '(49,99]',
         `husband_education_levelHigher` = husband_education_level == 'Higher',
         `husband_education_levelNone` = husband_education_level == 'None',
         `husband_education_levelPrimary` = husband_education_level == 'Primary',
         `husband_education_levelSecondary` = husband_education_level == 'Secondary',
         drought_catnormal = drought_cat == 'normal',
         drought_catmoderate = drought_cat == 'moderate',
         drought_catsevere = drought_cat == 'severe',
         drought_catextreme = drought_cat == 'extreme',
         Total = TRUE) %>%
  select_if(is.logical) %>%
  gather(Var, Val) %>%
  group_by(Var) %>%
  summarize(Asi.Count = format(sum(Val), big.mark=','),
            Asi.Percent = paste0('(', round(mean(Val)*100, 1), '%)'))

lac <- dat %>%
  filter(in_lac) %>%
  mutate(`plos_age(14,19]` = plos_age == '(14,19]',
         `plos_age(19,29]` = plos_age == '(19,29]',
         `plos_age(29,39]` = plos_age == '(29,39]',
         `plos_age(39,49]` = plos_age == '(39,49]',
         `plos_births(0.5,2]` = plos_births == '(0.5,2]',
         `plos_births(2,4]` = plos_births == '(2,4]',
         `plos_births(4,48]` = plos_births == '(4,48]',
         `plos_hhsize(1,3]` = plos_hhsize == '(1,3]',
         `plos_hhsize(3,5]` = plos_hhsize == '(3,5]',
         `plos_hhsize(5,48]` = plos_hhsize == '(5,48]',
         `plos_husband_age(14,19]` = plos_husband_age == '(14,19]',
         `plos_husband_age(19,29]` = plos_husband_age == '(19,29]',
         `plos_husband_age(29,39]` = plos_husband_age == '(29,39]',
         `plos_husband_age(39,49]` = plos_husband_age == '(39,49]',
         `plos_husband_age(49,99]` = plos_husband_age == '(49,99]',
         `husband_education_levelHigher` = husband_education_level == 'Higher',
         `husband_education_levelNone` = husband_education_level == 'None',
         `husband_education_levelPrimary` = husband_education_level == 'Primary',
         `husband_education_levelSecondary` = husband_education_level == 'Secondary',
         drought_catnormal = drought_cat == 'normal',
         drought_catmoderate = drought_cat == 'moderate',
         drought_catsevere = drought_cat == 'severe',
         drought_catextreme = drought_cat == 'extreme',
         Total = TRUE) %>%
  select_if(is.logical) %>%
  gather(Var, Val) %>%
  group_by(Var) %>%
  summarize(Lac.Count = format(sum(Val), big.mark=','),
            Lac.Percent = paste0('(', round(mean(Val)*100, 1), '%)'))

#write.csv(summary$Var, 'C://Users/matt/gbv/visualizations/labels.csv', row.names=F)
labs <- read.csv('~/gbv/visualizations/labels.csv')

summaryl <- Reduce(merge, list(africa, asia, lac, labs)) %>%
  arrange(Order) %>%
  select(`  `=Category, ` `=Label, matches('Count|Percent'), -Order, -Var)

summaryl[summaryl=='(100%)'] <- ''

hline <- c(-1, 0, which(summaryl$`  ` != '') - 1, nrow(summaryl))
htype <- c("\\toprule & & \\multicolumn{2}{c}{Africa}& \\multicolumn{2}{c}{Asia}& \\multicolumn{2}{c}{LAC}\\\\",
           " & & \\multicolumn{1}{c}{\\textit{n}} & \\multicolumn{1}{c}{\\%} & \\multicolumn{1}{c}{\\textit{n}} & \\multicolumn{1}{c}{\\%} & \\multicolumn{1}{c}{\\textit{n}} & \\multicolumn{1}{c}{\\%}\\\\",
           rep("\\midrule ", sum(summaryl$`  ` != '')), "\\bottomrule ")
print(xtable(summaryl, 
             caption='Summary of Variables Used in Regressions',
             label='tab:var_sum'),
      include.rownames=FALSE,
      include.colnames=FALSE,
      table.placement="H",
      hline.after = NULL,
      add.to.row = list(pos = as.list(hline),
                        command = htype),
      file='~/ipv-rep-tex/tables/variable_summary.tex')

########################################
# Summary of Observations by Country and Year
#########################################
sel <- dat %>%
  filter(in_afr | in_lac | in_asia)

t <- addmargins(table(countrycode(sel$country, 'dhs', 'country.name'), 
                      sel$year))

print(xtable(t, 
             caption='Count of Observations by Country and Year',
             label='tab:cty_year',
             digits=0),
      file='~/ipv-rep-tex/tables/country_year.tex', 
      floating.environment = "sidewaystable",
      scalebox=0.95)




