if (Sys.info()['sysname']=='Linux'){
  data_dir <- '/home/mattcoop/mortalityblob/gbv'
  meta_dir <- '/home/mattcoop/gbv'
	mod_dir <- '/home/mattcoop/mortalityblob/gbv_gams'
} else if(Sys.info()['sysname']=='Windows'){
  data_dir <- 'G://My Drive/GBV'
  meta_dir <- 'C://Users/matt/gbv'
}

library(fastglm)
library(tidyverse)
library(orthopolynom)
library(broom)

#########################################
# Define Functions
###########################################

logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

mypredict <- function(mod, df){
	#Custom predict function that can handle both GLM and fastGLM models
	if (class(mod) == 'glm'){
		res <- predict(mod, df)
	}
	
	if (class(mod) == 'fastglm'){
		order <- max(as.numeric(substr(names(mod$coefficients), 
																	 nchar(names(mod$coefficients)), 
																	 nchar(names(mod$coefficients)))), na.rm=T)

		fe <- crossing(l=0:order, k=0:order) %>%
			rowwise() %>%
			mutate(var = paste0(' + survey_code*l', l, 'k', k))

		#determine which vars were uses in fitting
		if ('woman_literateTRUE' %in% names(mod$coefficients)){
			form <- paste0('viol_phys ~ plos_age + woman_literate + is_married + 
												plos_births + plos_hhsize + 
												plos_rural + husband_education_level + 
												plos_husband_age + drought_cat',
									paste0(fe$var, collapse=''))
			
			X <- model.matrix(as.formula(form), df)	
		} else{
			form <- paste0('viol_phys ~ drought_cat',
									paste0(fe$var, collapse=''))
			
			X <- model.matrix(as.formula(form), df)	
		}

		res <- predict(mod, X)
	}

	return(res)

}

getAME <- function(mod, df){
  df$drought_cat <- NULL
  
  df <- bind_rows(df %>% mutate(drought_cat='severe'),
                  df %>% mutate(drought_cat='drought'),
                  df %>% mutate(drought_cat='normal'))
  
  pred <- mypredict(mod, df)
  
  df$fit <- logit2prob(pred)
  
  df <- df %>%
    group_by(drought_cat) %>%
    summarize(fit=mean(fit)) %>%
    gather(var, val, -drought_cat) %>%
    spread(drought_cat, val) %>%
    mutate(drought = drought - normal,
           severe = severe - normal,
           normal = NULL)

	return(df)
}

getPvals <- function(mod){
	m <- coef(summary(mod))[, 4]
	m <- m[names(m) %in% c('drought_catdrought', 'drought_catsevere')]
	return(m)
}

derive_legendre <- function(x, y, n){
	#Given X and Y coords and an Nth order,
	#Derive the Legendre polynomials, a la Cools, et al 2020

	#Derive Legende polynomials
	legcoef <- legendre.polynomials(n=n, normalized=TRUE)
	leg <- as.data.frame(c(polynomial.values(polynomials=legcoef, 
																						x=scaleX(dat$longitude, u=-1, v=1)), 
													polynomial.values(polynomials=legcoef, 
																						x=scaleX(dat$latitude, u=-1, v=1))))
	names(leg) <- c(paste0("leg", 0:n, "x"),
									paste0("leg", 0:n, "y"))

	for (l in 0:n){
		for (k in 0:n){
			leg[ paste0('l', l, 'k', k)] <- leg[ , paste0('leg', l, 'x')]*leg[ , paste0('leg', k, 'y')]
		}
	}
	
	leg <- leg %>% select(-matches('leg'))
	
	return(leg)
}

#####################################
# Read in Data
#####################################

dat <- read.csv(file.path(data_dir, 'GBV_sel.csv')) %>%
  mutate(drought_cat=relevel(drought_cat, ref = 'normal'))


dat <- cbind(dat, derive_legendre(dat$longtiude, 
																	 dat$latitude,
																	 n=6))

dat$in_plos_paper <- dat$survey_code %in% c("SL-6-1", "TG-6-1", "BJ-7-1", "CI-6-1", "CM-6-1",
                                            "GA-6-1", "TD-7-1", "CD-6-1", "RW-7-1", "BU-7-1",
                                            "UG-7-2", "KE-7-1", "TZ-7-2", "MW-7-2", "MZ-6-1",
                                            "ZW-7-1", "ZM-6-1", "NM-6-1", "AO-7-1")

dat$in_cty <- dat$country %in% c("SL", "TG", "BJ", "CI", "CM",
                                 "GA", "TD", "CD", "RW", "BU", 
                                 "UG", "KE", "TZ", "MW", "MZ",
                                 "ZW", "ZM", "NM", "AO")

dat$in_afr <- dat$latitude < 23 & dat$longitude > -20 & dat$longitude < 50

dat$survey_code <- as.character(dat$survey_code)


########################################
#Get AMEs
########################################
mods <- list.files(mod_dir, pattern='mod')

mdf <- data.frame(file=mods, stringsAsFactors=F) %>%
	mutate(outcome = case_when(grepl('phys', file) ~ 'phys',
														 grepl('sex', file) ~ 'sex',
														 grepl('cont', file) ~ 'cont',
														 grepl('emot', file) ~ 'emot'),
				 order = str_extract(file, '\\d'),
				 order = ifelse(is.na(order), "0", order),
				 model = case_when(grepl('plos.RDS', file) ~ 'plos',
													 grepl('allvars...RDS', file) ~ 'allvars',
													 grepl('cools...RDS', file) ~ 'cools'),
				 scale = case_when(grepl('afr', file) ~ 'afr',
													 grepl('cty', file) ~ 'cty',
													 grepl('all_', file) ~ 'all',
													 TRUE ~ 'plos')) %>%
	arrange(outcome, model, scale, order) %>%
	group_by(outcome, model, scale) %>%
	filter(order==max(order)) %>%
	data.frame

for (i in 1:nrow(mdf)){
  print(mdf$file[i])
	
	mod <- readRDS(file=file.path(mod_dir, mdf$file[i]))
 
	#Skip old mods without interaction terms
	if ('l0k0' %in% names(mod$coefficients) & !("survey_codeSL-6-1:l0k0" %in% names(mod$coefficients))){
		next
	}

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
  
  me <- getAME(mod, sel)
  
	mdf$drought[i] <- me$drought
	mdf$severe[i] <- me$severe

	ps <- getPvals(mod)

	mdf$drought.pval[i] <- ps['drought_catdrought']
	mdf$severe.pval[i] <- ps['drought_catsevere']
}

allm <- mdf %>%
	select(-order) %>%
  gather(drought, value, -file, -outcome, -model, -scale) %>%
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

allm$outcome <- paste0(allm$outcome, '-', allm$model)

ggplot(allm) + 
  geom_bar(aes(x=drought, y=AME, fill=drought), stat='identity',
           show.legend=F) +
  #geom_errorbar(aes(x=drought, ymin=min, ymax=max)) +
  geom_text(aes(x=drought, y=AME + 0.002, label=stars)) + 
  facet_grid(outcome ~ scale) + 
  scale_fill_manual(values=c('#ff7f00', '#e41a1c')) +
  scale_y_continuous(limits=c(min(allm$AME), max(allm$AME) + 0.004), sec.axis = ) + 
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
