data_dir <- '/home/mattcoop/mortalityblob/gbv'
meta_dir <- '/home/mattcoop/gbv'
mod_dir <- '/home/mattcoop/mortalityblob/gbv_gams/legendre2/'

library(tidyverse)
library(fastglm)
library(orthopolynom)

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
	
  form <- paste0(' ~ plos_age + woman_literate + is_married + 
                          plos_births + plos_hhsize + 
                          plos_rural + husband_education_level + 
                          plos_husband_age + drought_cat',
          fe$var, collapse='')

  X <- model.matrix(as.formula(form), df)

	pred <- predict(mod, df, type='response')
	  
  df$fit <- pred#$fit
			  
	df %>%
		group_by(drought_cat) %>%
		summarize(fit=mean(fit)) %>%
		gather(var, val, -drought_cat) %>%
		spread(drought_cat, val) %>%
		mutate(extreme = extreme - normal,
					 severe = severe - normal,
           moderate = moderate - normal,
					 normal = NULL)
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
  mutate(drought_cat=relevel(drought_cat, ref = 'normal'),
         survey_code=as.character(survey_code))

dat <- cbind(dat, derive_legendre(dat$longtiude, dat$latitude, n=3))

########################################
#Get AMEs
########################################
mods <- list.files(mod_dir)

mdf <- data.frame(file=mods, stringsAsFactors=F) %>%
  filter(grepl('afr|asi|lac', file)) %>%
	mutate(outcome = substr(file, 1, 4),
				 order = as.numeric(substr(file, 10, 10)),
				 model = "cools",
				 scale = substr(file, 6, 8)) %>%
	arrange(outcome, model, scale, order) %>%
	group_by(outcome, model, scale) %>%
	filter(order==max(order)) %>%
  mutate(extreme=NA,
         moderate=NA,
         severe=NA,
         extreme.pval=NA,
         moderate.pval=NA,
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
   if (mdf$scale[i]=='lac'){
     sel <- dat %>% filter(in_lac)
   }
   if (mdf$scale[i]=='asi'){
     sel <- dat %>% filter(in_asia)
   }
   
  coef <- getAME(mod, sel)
  
  mdf$extreme[i] <- coef['drought_catextreme']
  mdf$severe[i] <- coef['drought_catsevere']
  mdf$moderate[i] <- coef['drought_catmoderate']

	ps <- getPvals(mod)
  
  mdf$moderate.pval[i] <- ps['drought_catmoderate']
	mdf$extreme.pval[i] <- ps['drought_catextreme']
	mdf$severe.pval[i] <- ps['drought_catsevere']
  #mdf$moran.pval[i] <- getMoransI(sel, mod)
  
}

allm <- mdf %>%
	select(-order) %>%
  gather(drought, value, -file, -outcome, -model, -scale) %>%
  mutate(outcome = factor(outcome,
                          levels=c('cont', 'emot', 'phys', 'sexu'),
                          labels=c('Controlling\nBehaviors', 'Emotional\nViolence', 'Physical\nViolence', 'Sexual\nViolence')),
         scale = factor(scale,
                        levels=c('afr', 'asi', 'lac'),
                        labels=c('SSA\n(n=194,820)', 
																 'Asia\n(n=100,647)',
																 'LAC\n(n=67,961)')),
         var = ifelse(grepl('pval', drought), 'Pvalue', 'AME'), 
				 drought = gsub('.pval', '', drought)) %>%
	spread(var, value) %>%
  mutate(stars = case_when(Pvalue > 0.05 ~ '',
                           Pvalue > 0.01 ~ '*',
                           Pvalue > 0.001 ~ '**',
                           TRUE ~ '***'), 
         drought = factor(drought, levels=c('moderate', 'severe', 'extreme')))

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
  facet_grid(outcome ~ scale) + 
  scale_fill_manual(values=c('#fe9929', '#d95f0e', '#993404')) +
  scale_y_continuous(limits=c(min(allm$AME), max(allm$AME) + 0.004), sec.axis = ) + 
  theme_bw() + 
  labs(x='Drought Status', y='Average Marginal Effect (Probability)')

ggsave(plot=res, filename='~/ipv-rep-tex/img/mod_results_legendre.pdf', width=6, height=6)
