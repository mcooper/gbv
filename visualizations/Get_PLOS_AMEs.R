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

getAME <- function(mod, df, vars=c('drought_cat', 'plos_age', 'woman_literate', 'is_married', 
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
mods <- list.files('~/mortalityblob/gbv_gams/epstein', full.names=T) 
mdf <- data.frame(file=mods, name=basename(mods), stringsAsFactors=F) %>%
  mutate(region=substr(name, 6, 8))
  
final <- data.frame()
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
   
  res <- getAME(mod, sel)

  res$model <- basename(mdf$file[i])
 
  final <- bind_rows(final, res)
}

write.csv(final, '~/mortalityblob/gbv/PLOS_AMEs.csv', row.names=F)
