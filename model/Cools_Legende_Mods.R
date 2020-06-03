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
library(ape)
library(orthopolynom)
library(foreach)
library(doParallel)

#############################
# Define Helper Functions
#############################

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

plotvars <- function(dat, vars){
	#Given a datafram with columns latitude, longitude, and a list of vars
	#	Will sum the vars,
	# Then the the mean at each lat-lon integer combo
	# Then plot it

	dat$outcome <- rowSums(dat[ vars])

	res_sum <- dat %>%
		mutate(latitude = round(latitude, 0),
					 longitude = round(longitude, 0)) %>%
		group_by(latitude, longitude) %>%
		summarize(outcome = mean(outcome))

	ggplot(res_sum) + geom_raster(aes(x=longitude, y=latitude, fill=outcome)) + 
	 theme_void() + guides(fill=F)	

}


getMoransI <- function(data, residuals){
	data$residual <- residuals
	
	resid_sum <- data %>%
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

runModelUntilNoSA <- function(savename, data){
	#Run a model, adding higher and higher Legendre polynomials,
	#until there is no more spatial autocorrelation
	#Then save the final model
	
	if (grepl('all', savename)){
		data <- data
	}
	if (grepl('plos', savename)){
		data <- data %>% filter(in_plos_paper)
	}
	if (grepl('cty', savename)){
		data <- data %>% filter(in_cty)
	}
	if (grepl('afr', savename)){
		data <- data %>% filter(in_afr)
	}

	outcome <- paste0('viol_', substr(savename, 1, gregexpr('_mod', savename)[[1]][1] - 1))

	SA <- TRUE
	i <- 1
	while (SA & i <= 10){
		cat(savename, ': Running with', i, 'order polynomial \n')

		fe <- crossing(l=0:i, k=0:i) %>%
			rowwise() %>%
			mutate(var = paste0(' + survey_code*l', l, 'k', k))

		form <- paste0(outcome, 
									' ~ plos_age + woman_literate + is_married + plos_births + 
									plos_hhsize + plos_rural + husband_education_level + 
									plos_husband_age + drought_cat',
								paste0(fe$var, collapse=''))
		
		mod <- glm(as.formula(form), data=data, family=binomial(link = 'logit'))

		mi <- getMoransI(data, residuals(mod))
		cat(savename, ': \t\tMorans I of', mi, '\n')
		
		if (mi > 0.01){
			SA <- FALSE
			saveRDS(mod, paste0('~/mortalityblob/gbv_gams/', savename, '.RDS'))
		}	

		i <- i + 1

	}
	
	if (SA){
		cat(savename, ': Still SA with 10-degree polynomial\n',
				file='~/gbv_moran_res', append=T)
		saveRDS(mod, paste0('~/mortalityblob/gbv_gams/', savename, '.RDS'))
	}
}

##########################################################
# Read in and process data
#########################################################

dat <- read.csv(file.path(data_dir, 'GBV_sel.csv')) %>%
  mutate(drought_cat=relevel(drought_cat, ref = 'normal'))

dat <- cbind(dat, derive_legendre(dat$longtiude, 
																	 dat$latitude,
																	 n=10))

dat$in_plos_paper <- dat$survey_code %in% c("SL-6-1", "TG-6-1", "BJ-7-1", "CI-6-1", "CM-6-1",
                                            "GA-6-1", "TD-7-1", "CD-6-1", "RW-7-1", "BU-7-1", 
                                            "UG-7-2", "KE-7-1", "TZ-7-2", "MW-7-2", "MZ-6-1",
                                            "ZW-7-1", "ZM-6-1", "NM-6-1", "AO-7-1")

dat$in_cty <- dat$country %in% c("SL", "TG", "BJ", "CI", "CM",
                                 "GA", "TD", "CD", "RW", "BU", 
                                 "UG", "KE", "TZ", "MW", "MZ",
                                 "ZW", "ZM", "NM", "AO")

dat$in_afr <- dat$latitude < 23 & dat$longitude > -20 & dat$longitude < 50


############################################################
# Run global models
###############################################################

mods <- c('phys_mod_all', 'sex_mod_all', 'emot_mod_all', 'cont_mod_all', 
					'phys_mod_plos', 'sex_mod_plos', 'emot_mod_plos', 'cont_mod_plos', 
					'phys_mod_cty', 'sex_mod_cty', 'emot_mod_cty', 'cont_mod_cty', 
					'phys_mod_afr', 'sex_mod_afr', 'emot_mod_afr', 'cont_mod_afr')

cl <- makeCluster(4, outfile = '')
registerDoParallel(cl)

foreach(mod=mods, .packages=c('ape', 'tidyverse', 'orthopolynom')) %dopar% {
	runModelUntilNoSA(mod, dat)
}

system('~/telegram.sh "Models Done~"')


system('poweroff')



















all <- data.frame()
for (mod in ls()[grepl('mod', ls())]){
  print(mod)
	saveRDS(eval(parse(text=mod)), file=paste0('~/mortalityblob/gbv_gams/', mod, '_cools.RDS'))
  
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
                        labels=c('Previous\n Analysis\n(n=83,970)', 
																 'Previous\nCountries\nMore Surveys\n(n=123,488)',
																 'All\nAfrican\nSurveys\n(n=194,820)',
																 'All\nAvailable\nSurveys\n(n=380,100)')),
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

ggsave('~/ipv-rep-tex/img/mod_cools_results.pdf', width=6, height=6)

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
