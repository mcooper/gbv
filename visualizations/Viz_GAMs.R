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
									df %>% mutate(drought_cat='extreme'),
								  df %>% mutate(drought_cat='normal'),
								  df %>% mutate(drought_cat='moderate'))
	  
	pred <- predict(mod, df, #se.fit=TRUE, 
                  type='response')
	  
  df$fit <- pred#$fit
	#df$min <- pred$fit + pred$se.fit*qnorm(0.025, 0, 1)
	#df$max <- pred$fit + pred$se.fit*qnorm(0.975, 0, 1)
			  
	df %>%
		group_by(drought_cat) %>%
		summarize(fit=mean(fit),
							#min=mean(min),
							#max=mean(max)
              ) %>%
		gather(var, val, -drought_cat) %>%
		spread(drought_cat, val) %>%
		mutate(extreme = extreme - normal,
					 severe = severe - normal,
           moderate = moderate - normal,
					 normal = NULL)
}

getLogOdds <- function(mod){
  if ('lm' %in% class(mod)){
    m <- coef(summary(mod))[, 1]
  }
  if ('gam' %in% class(mod)){
    m <- summary(mod)$p.coef
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
  m <- m[names(m) %in% c('drought_catextreme', 'drought_catmoderate', 'drought_catsevere')]
  return(m)
}

#####################################
# Read in Data
#####################################

dat <- read.csv('~/mortalityblob/gbv/GBV_sel.csv') %>%
  mutate(drought_cat=relevel(drought_cat, ref = 'normal'))


########################################
#Get AMEs
########################################
mdf <- read.csv('~/mortalityblob/gbv/moran_results.csv',
                stringsAsFactors=F) %>%
  filter(method=='gam_splines',
         scale=='code') %>%
  group_by(outcome, region) %>%
  filter(p.value==max(p.value)) %>%
  mutate(extreme=NA,
         moderate=NA,
         severe=NA,
         extreme.pval=NA,
         moderate.pval=NA,
         severe.pval=NA)

for (i in 1:nrow(mdf)){
  print(mdf$file[i])
	
	mod <- readRDS(file=mdf$file[i])
	
  if (mdf$region[i]=='afr'){
    sel <- dat %>% filter(in_afr)
  }
  if (mdf$region[i]=='asi'){
    sel <- dat %>% filter(in_asia)
  }
  if (mdf$region[i]=='lac'){
    sel <- dat %>% filter(in_lac)
  }
   
  coef <- getAME(mod, sel)
  
  mdf$extreme[i] <- coef[1, 'extreme']
  mdf$severe[i] <- coef[1, 'severe']
  mdf$moderate[i] <- coef[1, 'moderate']

	ps <- getPvals(mod)
  
  mdf$moderate.pval[i] <- ps['drought_catmoderate']
	mdf$extreme.pval[i] <- ps['drought_catextreme']
	mdf$severe.pval[i] <- ps['drought_catsevere']
  
}
mdf$extreme <- unlist(mdf$extreme)
mdf$severe <- unlist(mdf$severe)
mdf$moderate <- unlist(mdf$moderate)

write.csv(mdf, '~/mortalityblob/gbv/gams_results.csv', row.names=F)

allm <- mdf %>%
	select(-order, -method, -observed, -expected, -sd, -p.value, -scale) %>%
  gather(drought, value, -file, -outcome, -model, -region) %>%
  mutate(outcome = factor(outcome,
                          levels=c('cont', 'emot', 'phys', 'sexu'),
                          labels=c('Controlling Behaviors', 
                                          'Emotional Violence', 
                                          'Physical Violence', 
                                          'Sexual Violence')),
         region = factor(region,
                        levels=c('afr', 'asi', 'lac'),
                        labels=c("SSA", "Asia", "LAC")),
         var = ifelse(grepl('pval', drought), 'Pvalue', 'AME'), 
				 drought = gsub('.pval', '', drought)) %>%
	spread(var, value) %>%
  mutate(stars = case_when(Pvalue > 0.05 ~ '',
                           Pvalue > 0.01 ~ '*',
                           Pvalue > 0.001 ~ '**',
                           TRUE ~ '***'),
         drought = factor(drought, levels=c('moderate', 'severe', 'extreme'),
                          labels=c("Moderate", "Severe", "Extreme")))

res <- ggplot(allm) + 
  geom_bar(aes(x=drought, y=AME, fill=drought), stat='identity',
           show.legend=F) +
  #geom_errorbar(aes(x=drought, ymin=min, ymax=max)) +
  geom_text(aes(x=drought, y=AME + 0.002, label=stars)) + 
  facet_grid(outcome ~ region) + 
  scale_fill_manual(values=c('#fe9929', '#d95f0e', '#993404')) +
  scale_y_continuous(limits=c(min(allm$AME), max(allm$AME) + 0.004), sec.axis = ) + 
  theme_bw() + 
  labs(x='Drought Status', y='Average Marginal Effect (Probability)')

ggsave(plot=res, filename='~/ipv-rep-tex/img/mod_results_gams.pdf', width=6, height=6)
