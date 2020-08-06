library(tidyverse)
library(mgcv)
library(countrycode)
library(xtable)
library(moranfast)

dat <- read.csv('~/mortalityblob/gbv/GBV_sel.csv')

runTillNoSA <- function(form, data){
  moranp <- 0
  ks = c(50, 100, 500, 1000, 1500)
  i <- 1
  try({ 
    while (moranp < 0.05){
      cat('Running with', ks[i], 'knots\n')
      mod <- gam(as.formula(paste0(form, 
                                   ' + s(longitude, latitude, bs="sos", k=', ks[i], ')')),
                 data=data,
                 family=binomial(link = 'logit'))

      moranp <- moranfast(mod$residuals, data$longitude, data$latitude)$p.value

      i <- i + 1
    }  
  })

  return(mod)
}

res <- data.frame()
for (cty in unique(dat$country)){
  print(cty)
  
  sel <- dat %>% 
    filter(country == cty) %>%
    mutate(in_drought = drought_cat != 'normal')

  form <- ' ~ plos_age + woman_literate + is_married + 
                  plos_births + plos_hhsize + 
                  plos_rural + husband_education_level + 
                  plos_husband_age + in_drought'
  
  if (length(unique(sel$survey_code)) > 1){
    form <- paste0(form, ' + survey_code')
  }
  print('phys')
  phys <- runTillNoSA(paste0("viol_phys", form),
              data=sel) 
  print('sexu')
  sexu <- runTillNoSA(paste0("viol_sexu", form), 
              data=sel)
  print('cont')
  cont <- runTillNoSA(paste0("viol_cont", form), 
              data=sel)
  print('emot')
  emot <- runTillNoSA(paste0("viol_emot", form), 
              data=sel)
  
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
  
}

write.csv(res, '~/mortalityblob/gbv/county_results.csv', row.names=F)

res <- res[!is.nan(res$phy.p), ]
rownames(res) <- 1:nrow(res)
res$Country <- countrycode(res$Country, 'dhs', 'country.name')

bonferroni <- function(a, m){
  a/m
}
m <- 39*4

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


