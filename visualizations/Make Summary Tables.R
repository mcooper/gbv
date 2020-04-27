library(tidyverse)
library(xtable)
library(lubridate)
library(rdhs)

options(stringsAsFactors=F)

dat <- read.csv('G://My Drive/DHS Processed/GBV_all.csv') %>%
  filter(mean_annual_precip > 200 & builtup < 0.1)

wealth_factor_harmonized + hhsize + date_cmc + years_education + 
  country, spei12, spei24, spei36, spei48, 

empowered_decisions, empowered_gbv_notok,
age_first_sex, age_marriage, 

viol_sex
viol_phys

summary <- dat %>%
  mutate(`Education - Higher`=years_education=='Higher',
         `Education - Secondary`=years_education=='Secondary',
         `Education - Primary`=years_education=='Primary',
         `Education - None`=years_education=='None') %>%
  select(`Wealth Factor`=wealth_factor_harmonized, 
         `Household Size`=hhsize, 
         `Date (CMC)`=date_cmc, 
         `Education - Higher`,
         `Education - Secondary`,
         `Education - Primary`,
         `Education - None`,
         `SPEI 12-Month`=spei12,
         `SPEI 24-Month`=spei24,
         `SPEI 36-Month`=spei36,
         `SPEI 48-Month`=spei48,
         `Empowered in Decisionmaking`=empowered_decisions, 
         `Empowered in IPC Never OK`=empowered_gbv_notok, 
         `Age at First Sex`=age_first_sex, 
         `Age at Marriage`=age_marriage, 
         `Experienced Sexual Violence`=viol_sex, 
         `Experienced Physical Violence`=viol_phys) %>%
  gather(Variable, Value) %>%
  group_by(Variable) %>%
  summarize(Mean=as.character(round(mean(Value), 2)),
            Median=as.character(round(median(Value), 2)),
            Max=as.character(round(max(Value), 2)),
            Min=as.character(round(min(Value), 2)),
            `Std Dev`=as.character(round(sd(Value), 2)))


####################
#Make Date Interpretable
####################

cmc_to_date <- function(cmc){
  cmc <- as.numeric(cmc)
  year <- 1900 + floor((cmc-1)/12)
  month <- floor(cmc - 12*(year - 1900))
  paste0(month, ' - ', year)
}

mean <- cmc_to_date(summary[summary$Variable=='Date (CMC)', 'Mean'])
median <- cmc_to_date(summary[summary$Variable=='Date (CMC)', 'Median'])
max <- cmc_to_date(summary[summary$Variable=='Date (CMC)', 'Max'])
min <- cmc_to_date(summary[summary$Variable=='Date (CMC)', 'Min'])

summary <- bind_rows(summary, 
                     tibble(Variable='Date (Month - Year)',
                                Mean=mean, Median=median, Max=max, Min=min, `Std Dev`='')) %>%
  arrange(Variable) %>%
  data.frame

print(xtable(summary, 
             caption='Summary of Variables Used in Regressions',
             label='tab:var_sum'),
      include.rownames=FALSE,
      file='C://Users/matt/gbv-tex/tables/variable_summary.tex')

ids <- dhs_countries(returnFields=c("CountryName", "DHS_CountryCode")) %>%
  dplyr::select(country=DHS_CountryCode, CountryName)

datm <- merge(dat, ids, all.x=T, all.y=F)

t <- addmargins(table(datm$CountryName, datm$year))

print(xtable(t, 
             caption='Count of Observations by Country and Year',
             label='tab:cty_year',
             digits=0),
      file='C://Users/matt/gbv-tex/tables/country_year.tex', 
      #size="\\fontsize{8pt}{8pt}\\selectfont", 
      floating.environment = "sidewaystable")






