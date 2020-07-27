library(tidyverse)
library(mgcv)
library(countrycode)
library(xtable)
library(ape)

dat <- read.csv('~/mortalityblob/gbv/GBV_sel.csv')

moran <- data.frame()
res <- data.frame()
for (cty in unique(dat$country)){
  print(cty)
  
  sel <- dat %>% 
    filter(country == cty) %>%
    mutate(in_drought = drought_cat != 'normal')

  form <- ' ~ plos_age + woman_literate + is_married + 
                  plos_births + plos_hhsize + 
                  plos_rural + husband_education_level + 
                  plos_husband_age + in_drought + 
                  s(latitude, longitude, bs="sos")'
  
  if (length(unique(sel$survey_code)) > 1){
    form <- paste0(form, ' + survey_code')
  }

  phys <- gam(as.formula(paste0("viol_phys", form)),
              data=sel,
              family=binomial(link = 'logit')) 
  sexu <- gam(as.formula(paste0("viol_sexu", form)), 
              data=sel,
              family=binomial(link = 'logit')) 
  cont <- gam(as.formula(paste0("viol_cont", form)), 
              data=sel,
              family=binomial(link = 'logit')) 
  emot <- gam(as.formula(paste0("viol_emot", form)), 
              data=sel,
              family=binomial(link = 'logit')) 
  
  p <- summary(phys)
  s <- summary(sexu)
  c <- summary(cont)
  e <- summary(emot)

  #Get AMES
  sel2 <- bind_rows(sel %>% mutate(in_drought=FALSE),
                    sel %>% mutate(in_drought=TRUE))

  sel2$p <- predict(phys, sel2, type='response')
  sel2$s <- predict(sexu, sel2, type='response')
  sel2$c <- predict(cont, sel2, type='response')
  sel2$e <- predict(emot, sel2, type='response')

  ame <- sel2 %>%
    group_by(in_drought) %>%
    summarize(p=mean(p),
              s=mean(s),
              c=mean(c),
              e=mean(e))
  
  ame <- ame[ame$in_drought, ] - ame[!ame$in_drought, ]

  tmp <- data.frame(Country=cty, 
                    sex.c=ame$s,
                    sex.p=s$p.pv['in_droughtTRUE'],
                    con.c=ame$c,
                    con.p=c$p.pv['in_droughtTRUE'],
                    emo.c=ame$e,
                    emo.p=e$p.pv['in_droughtTRUE'],
                    phy.c=ame$p,
                    phy.p=p$p.pv['in_droughtTRUE']) 

  res <- bind_rows(res, tmp)

  #Conduct morans I test
	#First, summarized
  midat <- sel %>%
    select(latitude, longitude, code) %>%
    mutate(phys=phys$residuals,
           sexu=sexu$residuals,
           cont=cont$residuals,
           emot=emot$residuals) %>%
    group_by(code) %>%
    summarize_all(mean)
  
  dmat <- as.matrix(dist(midat[ , c('longitude', 'latitude')]))
	dmat <- 1/dmat
	diag(dmat) <- 0
  
  mi.p <- data.frame(Moran.I(midat$phys, dmat))$p.value
  mi.s <- data.frame(Moran.I(midat$sexu, dmat))$p.value
  mi.c <- data.frame(Moran.I(midat$cont, dmat))$p.value
  mi.e <- data.frame(Moran.I(midat$emot, dmat))$p.value

  moran <- bind_rows(moran, data.frame(country=cty, p=mi.p, s=mi.s, c=mi.c, e=mi.e, level='agg'))

#  #But also, bc I'm curious, at the individual level
#  dmat <- as.matrix(dist(sel[ , c('longitude', 'latitude')]))
#	dmat <- 1/dmat
#	dmat[is.infinite(dmat)] <- 0
#
#  mi.p <- data.frame(Moran.I(phys$residuals, dmat))$p.value
#  mi.s <- data.frame(Moran.I(sexu$residuals, dmat))$p.value
#  mi.c <- data.frame(Moran.I(cont$residuals, dmat))$p.value
#  mi.e <- data.frame(Moran.I(emot$residuals, dmat))$p.value
#
#  moran <- bind_rows(moran, data.frame(country=cty, p=mi.p, s=mi.s, c=mi.c, e=mi.e, level='raw'))

}


res <- res[!is.nan(res$phy.p), ]
rownames(res) <- 1:nrow(res)
res$Country <- countrycode(res$Country, 'dhs', 'country.name')

bonferroni <- function(a, m){
  a/m
}
m <- 39

t <- res %>%
  gather(var, val, -Country) %>%
  mutate(outcome = substr(var, 1, 3),
         var = substr(var, 5, 6),
         outcome = case_when(outcome=='con' ~ "Controlling",
                             outcome=='emo' ~ "Emotional",
                             outcome=='phy' ~ "Physical",
                             outcome=='sex' ~ "Sexual")) %>%
  spread(var, val) %>%
  mutate(c = round(c, 3),
         c = case_when(p > bonferroni(0.05, m) ~ paste0(c, ''), 
                       p > bonferroni(0.01, m) ~ paste0(c, '*'),
                       p > bonferroni(0.001, m) ~ paste0(c, '**'),
                       TRUE ~ paste0(c, '***'))) %>%
  select(-p) %>%
  spread(outcome, c)


print(xtable(t,
             caption='Country-level results for the Average Marginal Effect (AME) of drought on a woman\'s probability of experiencing IPV. (*$p < 0.05$, **$p < 0.01$, ***$p<0.001$)',
             label='tab:country'),
      file='~/ipv-rep-tex/tables/country.tex',
      include.rownames=F,
      table.placement='H')
