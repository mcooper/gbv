data_dir <- '/home/mattcoop/mortalityblob/gbv'
meta_dir <- '/home/mattcoop/gbv'
mod_dir <- '/home/mattcoop/mortalityblob/gbv_gams/epstein/'

library(tidyverse)
library(fastglm)

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
  if ('fastglm' %in% class(mod)){
    m <- coef(mod)
  }
  m <- m[names(m) %in% c('drought_catextreme', 'drought_catmoderate', 'drought_catsevere')]
  return(m)
}

getPvals <- function(mod){
  if ('lm' %in% class(mod)){
    m <- coef(summary(mod))[, 4]
  }
  if ('gam' %in% class(mod)){
    m <- summary(mod)$p.pv
  }
  if ('fastglm' %in% class(mod)){
    m <- summary(mod)$coefficients[ , 4]
  }
  m <- m[names(m) %in% c('drought_catextreme', 'drought_catmoderate', 'drought_catsevere')]
  return(m)
}

#####################################
# Read in Data
#####################################

dat <- read.csv(file.path(data_dir, 'GBV_sel.csv')) %>%
  mutate(drought_cat=relevel(drought_cat, ref = 'normal'))


########################################
#Get AMEs
########################################
mods <- list.files(mod_dir)

mdf <- data.frame(file=mods, stringsasfactors=f) %>%
	mutate(outcome = substr(file, 1, 4),
				 model = "plos",
				 scale = substr(file, 6, 8),
         extreme=na,
         moderate=na,
         severe=na,
         extreme.pval=na,
         moderate.pval=na,
         severe.pval=na) %>%
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
   
  coef <- getLogOdds(mod)
  
  mdf$extreme[i] <- coef['drought_catextreme']
  mdf$severe[i] <- coef['drought_catsevere']
  mdf$moderate[i] <- coef['drought_catmoderate']

	ps <- getPvals(mod)
  
  mdf$moderate.pval[i] <- ps['drought_catmoderate']
	mdf$extreme.pval[i] <- ps['drought_catextreme']
	mdf$severe.pval[i] <- ps['drought_catsevere']
  
}

allm <- mdf %>%
  gather(drought, value, -file, -outcome, -model, -scale) %>%
  mutate(outcome = factor(outcome,
                          levels=c('cont', 'emot', 'phys', 'sexu'),
                          labels=paste0(c('controlling', 'emotional', 'physical', 'sexual'),
                                        '\nviolence')),
        #  scale = factor(scale,
        #                 levels=c('plos', 'cty', 'afr', 'all'),
        #                 labels=c('previous\n analysis\n(n=83,970)', 
				# 												 'previous\ncountries\nmore surveys\n(n=123,488)',
				# 												 'all\nafrican\nsurveys\n(n=194,820)', 
				# 												 'all\navailable\nsurveys\n(n=380,100)')),
         var = ifelse(grepl('pval', drought), 'pvalue', 'ame'), 
				 drought = gsub('.pval', '', drought)) %>%
	spread(var, value) %>%
  mutate(stars = case_when(pvalue > 0.05 ~ '',
                           pvalue > 0.01 ~ '*',
                           pvalue > 0.001 ~ '**',
                           TRUE ~ '***'),
         drought = factor(drought, levels=c('moderate', 'severe', 'extreme')))

res <- ggplot(allm) + 
  geom_bar(aes(x=drought, y=ame, fill=drought), stat='identity',
           show.legend=F) +
  #geom_errorbar(aes(x=drought, ymin=min, ymax=max)) +
  geom_text(aes(x=drought, y=ame + 0.002, label=stars)) + 
  facet_grid(outcome ~ scale) + 
  scale_fill_manual(values=c('#fe9929', '#d95f0e', '#993404')) +
  scale_y_continuous(limits=c(min(allm$ame), max(allm$ame) + 0.004), sec.axis = ) + 
  theme_bw() + 
  labs(x='drought status', y='average marginal effect (probability)')

ggsave(plot=res, filename='~/ipv-rep-tex/img/mod_results_plos.pdf', width=6, height=6)

############################
# make texreg results
############################

labs <- read.csv('c://users/matt/gbv/visualizations/labels.csv')

#physical violence models
texreg(l=list(phys_mod_plos, phys_mod_cty, phys_mod_afr, phys_mod_all),
       file='c://users/matt/ipv-rep-tex/tables/phys_mods.tex',
       custom.model.names = c('mod1', 'mod2', 'mod3', 'mod4'),
       label='tab:phys_mod',
       caption="results of models with outcome variable of physical ipv. mod1 refers to a model using the same surveys used by epstein et al \\cite{epstein2020}. mod2 refers to a model using the same countries as epstein et al, but with all available surveys.  mod3 refers to a model with all african countries, and mod4 refers to a model with a global datasets of all dhs countries with ipv data.",
       longtable=t,
       use.packages=f)

#sexual violence models
texreg(l=list(sex_mod_plos, sex_mod_cty, sex_mod_afr, sex_mod_all),
       file='c://users/matt/ipv-rep-tex/tables/sex_mods.tex',
       custom.model.names = c('mod1', 'mod2', 'mod3', 'mod4'),
       label='tab:sex_mod',
       caption="results of models with outcome variable of sexual ipv. mod1 refers to a model using the same surveys used by epstein et al \\cite{epstein2020}. mod2 refers to a model using the same countries as epstein et al, but with all available surveys.  mod3 refers to a model with all african countries, and mod4 refers to a model with a global datasets of all dhs countries with ipv data.",
       longtable=t,
       use.packages=f)

#emotional violence models
texreg(l=list(emot_mod_plos, emot_mod_cty, emot_mod_afr, emot_mod_all),
       file='c://users/matt/ipv-rep-tex/tables/emot_mods.tex',
       custom.model.names = c('mod1', 'mod2', 'mod3', 'mod4'),
       label='tab:emot_mod',
       caption="results of models with outcome variable of emotional ipv. mod1 refers to a model using the same surveys used by epstein et al \\cite{epstein2020}. mod2 refers to a model using the same countries as epstein et al, but with all available surveys.  mod3 refers to a model with all african countries, and mod4 refers to a model with a global datasets of all dhs countries with ipv data.",
       longtable=t,
       use.packages=f)

#controlling violence models
texreg(l=list(cont_mod_plos, cont_mod_cty, cont_mod_afr, cont_mod_all),
       file='c://users/matt/ipv-rep-tex/tables/cont_mods.tex',
       custom.model.names = c('mod1', 'mod2', 'mod3', 'mod4'),
       label='tab:cont_mod',
       caption="results of models with outcome variable of controlling ipv. mod1 refers to a model using the same surveys used by epstein et al \\cite{epstein2020}. mod2 refers to a model using the same countries as epstein et al, but with all available surveys.  mod3 refers to a model with all african countries, and mod4 refers to a model with a global datasets of all dhs countries with ipv data.",
       longtable=t,
       use.packages=f)
