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

getAME <- function(mod, df, vars=c('plos_age', 'woman_literate', 'is_married', 
                                   'plos_births', 'plos_hhsize', 'plos_rural', 
                                   'husband_education_level', 'plos_husband_age')){
	  
  pred <- predict(mod, df, se=TRUE, type='terms')
  fit <- data.frame(pred$fit)
  se.fit <- data.frame(pred$se.fit)

  intercept <- mod$coefficients[1]

  resdf <- data.frame()
  for (var in vars){
    for (value in unique(df[ , var])){
      coef <- unique(fit[df[ , var] == value, var])
      se <- unique(se.fit[df[ , var] == value, var])
      
      null <- fit 
      null[ , var] <- 0
      
      diff <- fit
      diff[ , var] <- coef

      null.prob <- logit2prob(rowSums(null) + intercept)
      diff.prob <- logit2prob(rowSums(diff) + intercept)
      se.prob <- logit2prob(rowSums(diff) + intercept + se)

      ame <- mean(diff.prob - null.prob)
      ame.se <- mean(se.prob - diff.prob)

      resdf <- bind_rows(resdf,
                         data.frame(var=as.character(var), 
                                    value=as.character(value), 
                                    ame, 
                                    ame.se,
                                    z.score=coef/se,
                                    p.value=2*(pnorm(-abs(coef/se)))) %>%
                            filter(ame != 0))
    }
  }

  return(resdf)
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
         extreme=NA,
         moderate=NA,
         severe=NA,
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
   
  coef <- getAME(mod, sel, vars='drought_cat')
  
  mdf$extreme[i] <- coef$ame[coef$value == 'extreme']
  mdf$severe[i] <- coef$ame[coef$value=='severe']
  mdf$moderate[i] <- coef$ame[coef$value=='moderate']

	ps <- getPvals(mod)
  
  mdf$moderate.pval[i] <- ps['drought_catmoderate']
	mdf$extreme.pval[i] <- ps['drought_catextreme']
	mdf$severe.pval[i] <- ps['drought_catsevere']
}

allm <- mdf %>%
  gather(drought, value, -file, -outcome, -model, -scale) %>%
  mutate(outcome = factor(outcome,
                          levels=c('cont', 'emot', 'phys', 'sexu'),
                          labels=c('Controlling Behaviors', 
                                   'Emotional Violence', 
                                   'Physical Violence', 
                                   'Sexual Violence')),
                          #  scale = factor(scale,
        #                 levels=c('plos', 'cty', 'afr', 'all'),
        #                 labels=c('previous\n analysis\n(n=83,970)', 
				# 												 'previous\ncountries\nmore surveys\n(n=123,488)',
				# 												 'all\nafrican\nsurveys\n(n=194,820)', 
				# 												 'all\navailable\nsurveys\n(n=380,100)')),
         scale = factor(scale,
                        levels=c('afr', 'asi', 'lac'),
                        labels=c("SSA", "Asia", "LAC")),
         var = ifelse(grepl('pval', drought), 'pvalue', 'ame'), 
				 drought = gsub('.pval', '', drought)) %>%
	spread(var, value) %>%
  mutate(stars = case_when(pvalue > 0.05 ~ '',
                           pvalue > 0.01 ~ '*',
                           pvalue > 0.001 ~ '**',
                           TRUE ~ '***'),
         drought = factor(drought, levels=c('moderate', 'severe', 'extreme'),
                          labels=c("Moderate", "Severe", "Extreme")))

res <- ggplot(allm) + 
  geom_bar(aes(x=drought, y=ame, fill=drought), stat='identity',
           show.legend=F) +
  #geom_errorbar(aes(x=drought, ymin=min, ymax=max)) +
  geom_text(aes(x=drought, y=ame + 0.002, label=stars)) + 
  facet_grid(outcome ~ scale) + 
  scale_fill_manual(values=c('#fe9929', '#d95f0e', '#993404')) +
  scale_y_continuous(limits=c(min(allm$ame), max(allm$ame) + 0.004), sec.axis = ) + 
  theme_bw() + 
  labs(x='Drought Status', y='Average Marginal Effect (Probability)')

ggsave(plot=res, filename='~/ipv-rep-tex/img/mod_results_plos.pdf', width=6, height=6)
