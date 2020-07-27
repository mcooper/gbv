data_dir <- '/home/mattcoop/mortalityblob/gbv'
meta_dir <- '/home/mattcoop/gbv'
mod_dir <- '/home/mattcoop/mortalityblob/gbv_gams/epstein/'

library(tidyverse)
library(mgcv)

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
	  
	pred <- predict(mod, df, se.fit=TRUE, 
                  type='response')
	  
  df$fit <- pred$fit
	df$min <- pred$fit + pred$se.fit*qnorm(0.025, 0, 1)
	df$max <- pred$fit + pred$se.fit*qnorm(0.975, 0, 1)
			  
	df %>%
		group_by(drought_cat) %>%
		summarize(fit=mean(fit),
							min=mean(min),
							max=mean(max)) %>%
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

mdf <- data.frame(file=mods, stringsAsFactors=F) %>%
	mutate(outcome = substr(file, 1, 4),
				 model = "plos",
				 scale = substr(file, 6, 8),
         extreme.fit=NA,
         moderate.fit=NA,
         severe.fit=NA,
         extreme.min=NA,
         moderate.min=NA,
         severe.min=NA,
         extreme.max=NA,
         moderate.max=NA,
         severe.max=NA,
         extreme.pval=NA,
         moderate.pval=NA,
         severe.pval=NA) %>%
	data.frame

for (i in 1:nrow(mdf)){
  print(mdf$file[i])
	
	mod <- readRDS(file=file.path(mod_dir, mdf$file[i]))
	
  if (mdf$scale[i]=='afr'){
   sel <- dat %>% filter(in_afr)
  }
  if (mdf$scale[i]=='asi'){
   sel <- dat %>% filter(in_asia)
  }
  if (mdf$scale[i]=='lac'){
   sel <- dat %>% filter(in_lac)
  }
   
  coef <- getAME(mod, sel)
  
  mdf$extreme.fit[i] <- coef[1, 'extreme', drop=T]
  mdf$severe.fit[i] <- coef[1, 'severe', drop=T]
  mdf$moderate.fit[i] <- coef[1, 'moderate', drop=T]

  mdf$extreme.max[i] <- coef[2, 'extreme', drop=T]
  mdf$severe.max[i] <- coef[2, 'severe', drop=T]
  mdf$moderate.max[i] <- coef[2, 'moderate', drop=T]
  mdf$extreme.min[i] <- coef[3, 'extreme', drop=T]
  mdf$severe.min[i] <- coef[3, 'severe', drop=T]
  mdf$moderate.min[i] <- coef[3, 'moderate', drop=T]

	ps <- getPvals(mod)
  
  mdf$moderate.pval[i] <- ps['drought_catmoderate']
	mdf$extreme.pval[i] <- ps['drought_catextreme']
	mdf$severe.pval[i] <- ps['drought_catsevere']
}

bonferroni <- 12

allm <- mdf %>%
  gather(var, value, -file, -outcome, -model, -scale) %>%
  separate(var, c("drought", "var")) %>%
  spread(var, value) %>%
  mutate(outcome = factor(outcome,
                          levels=c('cont', 'emot', 'phys', 'sexu'),
                          labels=c('Controlling Behaviors', 
                                   'Emotional Violence', 
                                   'Physical Violence', 
                                   'Sexual Violence')),
         scale = factor(scale,
                        levels=c('afr', 'asi', 'lac'),
                        labels=c("SSA", "Asia", "LAC")),
         var = ifelse(grepl('pval', drought), 'pvalue', 'ame'),
         stars = case_when(pval > 0.05/bonferroni ~ '',
                           pval > 0.01/bonferroni ~ '*',
                           pval > 0.001/bonferroni ~ '**',
                           TRUE ~ '***'),
         drought = factor(drought, levels=c('moderate', 'severe', 'extreme'),
                          labels=c("Moderate", "Severe", "Extreme")))

res <- ggplot(allm) + 
  geom_pointrange(aes(x=drought, y=fit, ymax=max, ymin=min, color=drought),
           show.legend=F) +
  geom_text(aes(x=drought, y=max + 0.002, label=stars)) + 
  geom_hline(aes(yintercept=0), linetype=3) + 
  facet_grid(outcome ~ scale) + 
  scale_color_manual(values=c('#fe9929', '#d95f0e', '#993404')) +
  #scale_y_continuous(limits=c(min(allm$min), max(allm$max) + 0.004)) + 
  theme_bw() + 
  labs(x='Drought Status', y='Average Marginal Effect (Probability)')

ggsave(plot=res, filename='~/ipv-rep-tex/img/mod_results_plos.pdf', width=6, height=6)
