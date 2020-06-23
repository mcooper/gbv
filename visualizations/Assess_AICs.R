library(tidyverse)
library(mgcv)
library(fastglm)

setwd('~/mortalityblob/gbv_gams')

########################################
#Get AMEs
########################################
mods <- list.files(recursive=T, full.names=T)

mdf <- data.frame(full=mods, stringsAsFactors=F) %>%
	mutate(file=basename(full),
         outcome = substr(file, 1, 4),
				 order = as.numeric(gsub('.RDS', '', substr(file, 10, 14))),
         order = ifelse(is.na(order), 0, order),
				 model = gsub('./', '', dirname(full)),
				 scale = substr(file, 6, 8)) %>%
  filter(scale %in% c('afr', 'asi', 'lac')) %>%
	arrange(outcome, model, scale, order) %>%
	group_by(outcome, model, scale) %>%
	filter(order==max(order)) %>%
  mutate(aic=NA)

for (i in 1:nrow(mdf)){
  print(mdf$file[i])
	
	mod <- readRDS(file=mdf$full[i])
  
  if ('fastglm' %in% class(mod)){
    mdf$aic[i] <- mod$aic
  } else{
    mdf$aic[i] <- AIC(mod)
  }
}

aics <- mdf %>% 
  select(-full, -file, -order) %>%
  spread(model, aic)

aics$min <- c('epstein', 'gam_splines', 'legendre2')[apply(aics[ , c('epstein', 'gam_splines', 'legendre2')], MARGIN=1, FUN=which.min)]

#PLOS never has the min AIC, sometimes its legendre or gam splines

