if (Sys.info()['sysname']=='Linux'){
  data_dir <- '/home/mattcoop/mortalityblob/gbv'
  meta_dir <- '/home/mattcoop/gbv'
} else if(Sys.info()['sysname']=='Windows'){
  data_dir <- 'G://My Drive/GBV'
  meta_dir <- 'C://Users/matt/gbv'
}

library(tidyverse)
library(broom)
library(texreg)

dat <- read.csv(file.path(data_dir, 'GBV_sel.csv')) %>%
  mutate(drought_cat=relevel(drought_cat, ref = 'normal'))

logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

getAME <- function(mod, df){
  df$drought_cat <- NULL
  
  df <- bind_rows(df %>% mutate(drought_cat='severe'),
                  df %>% mutate(drought_cat='drought'),
                  df %>% mutate(drought_cat='normal'))
  
  pred <- predict(mod, df, se.fit=TRUE)
  
  df$fit <- logit2prob(pred$fit)
  df$min <- logit2prob(pred$fit + pred$se.fit*qnorm(0.025, 0, 1))
  df$max <- logit2prob(pred$fit + pred$se.fit*qnorm(0.975, 0, 1))
  
  df %>%
    group_by(drought_cat) %>%
    summarize(fit=mean(fit),
              min=mean(min),
              max=mean(max)) %>%
    gather(var, val, -drought_cat) %>%
    spread(drought_cat, val) %>%
    mutate(drought = drought - normal,
           severe = severe - normal,
           normal = NULL)
}

phys_mod_all <- glm(viol_phys ~ plos_age + woman_literate + is_married + plos_births + plos_hhsize + 
                      plos_rural + husband_education_level + plos_husband_age + country + drought_cat, data=dat, family = 'binomial')
sex_mod_all <- glm(viol_sex ~ plos_age + woman_literate + is_married + plos_births + plos_hhsize + 
                     plos_rural + husband_education_level + plos_husband_age + country + drought_cat, data=dat, family = 'binomial')
emot_mod_all <- glm(viol_emot ~ plos_age + woman_literate + is_married + plos_births + plos_hhsize + 
                      plos_rural + husband_education_level + plos_husband_age + country + drought_cat, data=dat, family = 'binomial')
cont_mod_all <- glm(viol_cont ~ plos_age + woman_literate + is_married + plos_births + plos_hhsize + 
                      plos_rural + husband_education_level + plos_husband_age + country + drought_cat, data=dat, family = 'binomial')

####################################################
#Mark data from PLOS article regress only that data
#####################################################
dat$in_plos_paper <- dat$survey_code %in% c("SL-6-1", "TG-6-1", "BJ-7-1", "CI-6-1", "CM-6-1",
                                            "GA-6-1", "TD-7-1", "CD-6-1", "RW-7-1", "BU-7-1", 
                                            "UG-7-2", "KE-7-1", "TZ-7-2", "MW-7-2", "MZ-6-1",
                                            "ZW-7-1", "ZM-6-1", "NM-6-1", "AO-7-1")

phys_mod_plos <- glm(viol_phys ~ plos_age + woman_literate + is_married + plos_births + plos_hhsize + 
                       plos_rural + husband_education_level + plos_husband_age + country + drought_cat, data=dat %>% filter(in_plos_paper), family = 'binomial')
sex_mod_plos <- glm(viol_sex ~ plos_age + woman_literate + is_married + plos_births + plos_hhsize + 
                      plos_rural + husband_education_level + plos_husband_age + country + drought_cat, data=dat %>% filter(in_plos_paper), family = 'binomial')
emot_mod_plos <- glm(viol_emot ~ plos_age + woman_literate + is_married + plos_births + plos_hhsize + 
                       plos_rural + husband_education_level + plos_husband_age + country + drought_cat, data=dat %>% filter(in_plos_paper), family = 'binomial')
cont_mod_plos <- glm(viol_cont ~ plos_age + woman_literate + is_married + plos_births + plos_hhsize + 
                       plos_rural + husband_education_level + plos_husband_age + country + drought_cat, data=dat %>% filter(in_plos_paper), family = 'binomial')

####################################################
#See if it's an effect specific to those countries
#####################################################
dat$in_cty <- dat$country %in% c("SL", "TG", "BJ", "CI", "CM",
                                 "GA", "TD", "CD", "RW", "BU", 
                                 "UG", "KE", "TZ", "MW", "MZ",
                                 "ZW", "ZM", "NM", "AO")

phys_mod_cty <- glm(viol_phys ~ plos_age + woman_literate + is_married + plos_births + plos_hhsize + 
                      plos_rural + husband_education_level + plos_husband_age + country + drought_cat, data=dat %>% filter(in_cty), family = 'binomial')
sex_mod_cty <- glm(viol_sex ~ plos_age + woman_literate + is_married + plos_births + plos_hhsize + 
                     plos_rural + husband_education_level + plos_husband_age + country + drought_cat, data=dat %>% filter(in_cty), family = 'binomial')
emot_mod_cty <- glm(viol_emot ~ plos_age + woman_literate + is_married + plos_births + plos_hhsize + 
                      plos_rural + husband_education_level + plos_husband_age + country + drought_cat, data=dat %>% filter(in_cty), family = 'binomial')
cont_mod_cty <- glm(viol_cont ~ plos_age + woman_literate + is_married + plos_births + plos_hhsize + 
                      plos_rural + husband_education_level + plos_husband_age + country + drought_cat, data=dat %>% filter(in_cty), family = 'binomial')

####################################################
#See if it's an Africa specific effect
#####################################################
dat$in_afr <- dat$latitude < 23 & dat$longitude > -20 & dat$longitude < 50


phys_mod_afr <- glm(viol_phys ~ plos_age + woman_literate + is_married + plos_births + plos_hhsize + 
                      plos_rural + husband_education_level + plos_husband_age + country + drought_cat, data=dat %>% filter(in_afr), family = 'binomial')
sex_mod_afr <- glm(viol_sex ~ plos_age + woman_literate + is_married + plos_births + plos_hhsize + 
                     plos_rural + husband_education_level + plos_husband_age + country + drought_cat, data=dat %>% filter(in_afr), family = 'binomial')
emot_mod_afr <- glm(viol_emot ~ plos_age + woman_literate + is_married + plos_births + plos_hhsize + 
                      plos_rural + husband_education_level + plos_husband_age + country + drought_cat, data=dat %>% filter(in_afr), family = 'binomial')
cont_mod_afr <- glm(viol_cont ~ plos_age + woman_literate + is_married + plos_births + plos_hhsize + 
                      plos_rural + husband_education_level + plos_husband_age + country + drought_cat, data=dat %>% filter(in_afr), family = 'binomial')


########################################
#Get AMEs
########################################
if ('mod' %in% ls()){rm('mod')}

all <- data.frame()
for (mod in ls()[grepl('mod', ls())]){
  print(mod)
	
	saveRDS(eval(parse(text=mod)), file=paste0('~/mortalityblob/gbv_gams/', mod, '_plos.RDS'))
  
	r <- tidy(eval(parse(text=mod))) %>%
    filter(term %in% c('drought_catsevere', 'drought_catdrought'))
  
  if (grepl('all', mod)){
    sel <- dat
  }
  if (grepl('afr', mod)){
    sel <- dat %>% filter(in_afr)
  }
  if (grepl('cty', mod)){
    sel <- dat %>% filter(in_cty)
  }
  if (grepl('plos', mod)){
    sel <- dat %>% filter(in_plos_paper)
  }
  
  me <- getAME(eval(parse(text=mod)), sel)
  
  r <- r %>%
    gather(var, value, -term) %>%
    spread(term, value) %>%
    rename(drought=drought_catdrought,
           severe=drought_catsevere) %>%
    bind_rows(me)
  
  r$mod <- mod
  
  all <- bind_rows(all, r)
}

allm <- all %>%
  gather(drought, value, -var, -mod) %>%
  spread(var, value) %>%
  mutate(outcome = factor(substr(mod, 1, 4),
                          levels=c('cont', 'emot', 'phys', 'sex_'),
                          labels=paste0(c('Controlling', 'Emotional', 'Physical', 'Sexual'),
                                        '\nViolence')),
         scale = factor(substr(mod, nchar(mod) - 3, nchar(mod)),
                        levels=c('plos', '_cty', '_afr', '_all'),
                        labels=c('Previous\n Analysis\n(n=83,970)', 'Previous\nCountries\nMore Surveys\n(n=123,488)',
                               'All\nAfrican\nSurveys\n(n=194,820)', 'All\nAvailable\nSurveys\n(n=380,100)')),
         drought = factor(drought, levels=c('drought', 'severe'), 
                          labels=c('Moderate', 'Severe')),
         stars = case_when(p.value > 0.05 ~ '',
                           p.value > 0.01 ~ '*',
                           p.value > 0.001 ~ '**',
                           TRUE ~ '***'))

ggplot(allm) + 
  geom_bar(aes(x=drought, y=fit, fill=drought), stat='identity',
           show.legend=F) +
  #geom_errorbar(aes(x=drought, ymin=min, ymax=max)) +
  geom_text(aes(x=drought, y=fit + 0.002, label=stars)) + 
  facet_grid(outcome ~ scale) + 
  scale_fill_manual(values=c('#ff7f00', '#e41a1c')) +
  scale_y_continuous(limits=c(min(allm$fit), max(allm$fit) + 0.004), sec.axis = ) + 
  theme_bw() + 
  labs(x='Drought Status', y='Average Marginal Effect (Probability)')

ggsave('C://Users/matt/ipv-rep-tex/img/mod_results.pdf', width=6, height=6)

############################
# Make texreg results
############################

labs <- read.csv('C://Users/matt/gbv/visualizations/labels.csv')

#Physical Violence Models
texreg(l=list(phys_mod_plos, phys_mod_cty, phys_mod_afr, phys_mod_all),
       file='C://Users/matt/ipv-rep-tex/tables/phys_mods.tex',
       custom.model.names = c('Mod1', 'Mod2', 'Mod3', 'Mod4'),
       label='tab:phys_mod',
       caption="Results of models with outcome variable of Physical IPV. Mod1 refers to a model using the same surveys used by Epstein et al \\cite{Epstein2020}. Mod2 refers to a model using the same countries as Epstein et al, but with all available surveys.  Mod3 refers to a model with all African countries, and Mod4 refers to a model with a global datasets of all DHS countries with IPV data.",
       longtable=T,
       use.packages=F)

#Sexual Violence Models
texreg(l=list(sex_mod_plos, sex_mod_cty, sex_mod_afr, sex_mod_all),
       file='C://Users/matt/ipv-rep-tex/tables/sex_mods.tex',
       custom.model.names = c('Mod1', 'Mod2', 'Mod3', 'Mod4'),
       label='tab:sex_mod',
       caption="Results of models with outcome variable of Sexual IPV. Mod1 refers to a model using the same surveys used by Epstein et al \\cite{Epstein2020}. Mod2 refers to a model using the same countries as Epstein et al, but with all available surveys.  Mod3 refers to a model with all African countries, and Mod4 refers to a model with a global datasets of all DHS countries with IPV data.",
       longtable=T,
       use.packages=F)

#Emotional Violence Models
texreg(l=list(emot_mod_plos, emot_mod_cty, emot_mod_afr, emot_mod_all),
       file='C://Users/matt/ipv-rep-tex/tables/emot_mods.tex',
       custom.model.names = c('Mod1', 'Mod2', 'Mod3', 'Mod4'),
       label='tab:emot_mod',
       caption="Results of models with outcome variable of Emotional IPV. Mod1 refers to a model using the same surveys used by Epstein et al \\cite{Epstein2020}. Mod2 refers to a model using the same countries as Epstein et al, but with all available surveys.  Mod3 refers to a model with all African countries, and Mod4 refers to a model with a global datasets of all DHS countries with IPV data.",
       longtable=T,
       use.packages=F)

#Controlling Violence Models
texreg(l=list(cont_mod_plos, cont_mod_cty, cont_mod_afr, cont_mod_all),
       file='C://Users/matt/ipv-rep-tex/tables/cont_mods.tex',
       custom.model.names = c('Mod1', 'Mod2', 'Mod3', 'Mod4'),
       label='tab:cont_mod',
       caption="Results of models with outcome variable of Controlling IPV. Mod1 refers to a model using the same surveys used by Epstein et al \\cite{Epstein2020}. Mod2 refers to a model using the same countries as Epstein et al, but with all available surveys.  Mod3 refers to a model with all African countries, and Mod4 refers to a model with a global datasets of all DHS countries with IPV data.",
       longtable=T,
       use.packages=F)
