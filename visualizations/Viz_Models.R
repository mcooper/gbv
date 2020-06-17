if (Sys.info()['sysname']=='Linux'){
  data_dir <- '/home/mattcoop/mortalityblob/gbv'
  meta_dir <- '/home/mattcoop/gbv'
	mod_dir <- '/home/mattcoop/mortalityblob/gbv_gams'
} else if(Sys.info()['sysname']=='Windows'){
  data_dir <- 'G://My Drive/GBV'
  meta_dir <- 'C://Users/matt/gbv'
}

library(tidyverse)
library(mgcv)
library(broom)
library(ape)

#########################################
# Define Functions
###########################################

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

getLogOdds <- function(mod){
  if ('lm' %in% class(mod)){
    m <- coef(summary(mod))[, 1]
  }
  if ('gam' %in% class(mod)){
    m <- summary(mod)$p.coef
  }
  m <- m[names(m) %in% c('drought_catdrought', 'drought_catsevere')]
  return(m)
}

getPvals <- function(mod){
  if ('lm' %in% class(mod)){
    m <- coef(summary(mod))[, 4]
  }
  if ('gam' %in% class(mod)){
    m <- summary(mod)$p.pv
  }
  m <- m[names(m) %in% c('drought_catdrought', 'drought_catsevere')]
  return(m)
}

getMoransI <- function(sel, mod){
  resid <- residuals(mod)

  sel$residual <- resid
  
  resid_sum <- sel %>%
    mutate(latitude = round(latitude, 0),
                            longitude = round(longitude, 0)) %>%
    group_by(latitude, longitude) %>%
    summarize(residual=mean(residual))
  
  dmat <- as.matrix(dist(resid_sum[ , c('longitude', 'latitude')]))
  dmat <- 1/dmat
  diag(dmat) <- 0

  mi <- data.frame(Moran.I(resid_sum$residual, dmat))

  return(mi$p.value)
}

#####################################
# Read in Data
#####################################

dat <- read.csv(file.path(data_dir, 'GBV_sel.csv')) %>%
  mutate(drought_cat=relevel(drought_cat, ref = 'normal'))


########################################
#Get AMEs
########################################
mods <- list.files(mod_dir, pattern='gam|plos.RDS$')

mdf <- data.frame(file=mods, stringsAsFactors=F) %>%
	mutate(outcome = case_when(grepl('phys', file) ~ 'phys',
														 grepl('sex', file) ~ 'sex',
														 grepl('cont', file) ~ 'cont',
														 grepl('emot', file) ~ 'emot'),
				 order = as.numeric(str_extract(file, '\\d+')),
				 order = ifelse(is.na(order), 0, order),
				 model = case_when(grepl('plos.RDS', file) ~ 'plos',
													 grepl('allvars', file) ~ 'gam-allvars',
													 TRUE ~ 'gam'),
				 scale = case_when(grepl('afr', file) ~ 'afr',
													 grepl('cty', file) ~ 'cty',
													 grepl('all_', file) ~ 'all',
													 TRUE ~ 'plos')) %>%
	arrange(outcome, model, scale, order) %>%
	group_by(outcome, model, scale) %>%
	filter(order==max(order)) %>%
  mutate(drought=NA,
         severe=NA,
         drought.pval=NA,
         severe.pval=NA) %>%
	data.frame

for (i in 1:nrow(mdf)){
  print(mdf$file[i])
	
	mod <- readRDS(file=file.path(mod_dir, mdf$file[i]))
	
   if (mdf$scale[i]=='all'){
     sel <- dat
   }
   if (mdf$scale[i]=='afr'){
     sel <- dat %>% filter(in_afr)
   }
   if (mdf$scale[i]=='cty'){
     sel <- dat %>% filter(in_cty)
   }
   if (mdf$scale[i]=='plos'){
     sel <- dat %>% filter(in_plos_paper)
   }
   
#   me <- getAME(mod, sel)
#   
# 	mdf$drought[i] <- me$drought[1]
# 	mdf$severe[i] <- me$severe[1]

  coef <- getLogOdds(mod)
  
  mdf$drought[i] <- coef['drought_catdrought']
  mdf$severe[i] <- coef['drought_catsevere']

	ps <- getPvals(mod)

	mdf$drought.pval[i] <- ps['drought_catdrought']
	mdf$severe.pval[i] <- ps['drought_catsevere']
  mdf$moran.pval[i] <- getMoransI(sel, mod)
  
}


for (i in 1:nrow(mdf)){
  print(mdf$file[i])
	
	mod <- readRDS(file=file.path(mod_dir, mdf$file[i]))
  mdf$AIC[i] <- AIC(mod)
}

allm <- mdf %>%
	select(-order) %>%
  filter(grepl('allvars', file)) %>%
  gather(drought, value, -file, -outcome, -model, -scale, -moran.pval) %>%
  mutate(outcome = factor(outcome,
                          levels=c('cont', 'emot', 'phys', 'sex'),
                          labels=paste0(c('Controlling', 'Emotional', 'Physical', 'Sexual'),
                                        '\nViolence')),
         scale = factor(scale,
                        levels=c('plos', 'cty', 'afr', 'all'),
                        labels=c('Previous\n Analysis\n(n=83,970)', 
																 'Previous\nCountries\nMore Surveys\n(n=123,488)',
																 'All\nAfrican\nSurveys\n(n=194,820)', 
																 'All\nAvailable\nSurveys\n(n=380,100)')),
         var = ifelse(grepl('pval', drought), 'Pvalue', 'AME'), 
				 drought = factor(ifelse(grepl('drought', drought), 'drought', 'severe'), 
													levels=c('drought', 'severe'), 
                          labels=c('Moderate', 'Severe'))) %>%
	spread(var, value) %>%
  mutate(stars = case_when(Pvalue > 0.05 ~ '',
                           Pvalue > 0.01 ~ '*',
                           Pvalue > 0.001 ~ '**',
                           TRUE ~ '***'))

allm$outcome <- paste0(allm$model, '-', allm$outcome)

res <- ggplot(allm) + 
  geom_bar(aes(x=drought, y=AME, fill=drought, color=moran.pval > 0.01), stat='identity',
           show.legend=F) +
  #geom_errorbar(aes(x=drought, ymin=min, ymax=max)) +
  geom_text(aes(x=drought, y=AME + 0.002, label=stars)) + 
  facet_grid(outcome ~ scale) + 
  scale_fill_manual(values=c('#ff7f00', '#e41a1c')) +
  scale_y_continuous(limits=c(min(allm$AME), max(allm$AME) + 0.004), sec.axis = ) + 
  theme_bw() + 
  labs(x='Drought Status', y='Average Marginal Effect (Probability)')

ggsave(res <- '~/ipv-rep-tex/img/mod_results_new.pdf', width=6, height=6)

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
