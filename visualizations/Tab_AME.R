library(tidyverse)
library(xtable)

spatial <- read.csv('~/mortalityblob/gbv/AMEs.csv')
aspatial <- read.csv('~/mortalityblob/gbv/PLOS_AMEs.csv')

aics <- bind_rows(spatial, aspatial) %>%
  select(model) %>%
  unique %>%
  mutate(var = 'AIC', value='')

for (m in aics$model){
  print(m)
  
  if (grepl('0', m)){
    mod <- readRDS(file=paste0('~/mortalityblob/gbv_gams/gam_splines/', m))
  }
  if (grepl('PLOS', m)){
    mod <- readRDS(file=paste0('~/mortalityblob/gbv_gams/epstein/', m))
  }
  
  aics$ame[aics$model == m] <- AIC(mod)

}

ames <- bind_rows(spatial, aspatial, aics) %>%
  mutate(Var = gsub('TRUE', '', paste0(var, value)),
         type= case_when(grepl('PLOS', model) ~ 'aspatial',
                         TRUE ~ 'spatial'),
         ame = round(ame, 3),
         ame.se = round(ame.se, 3),
         stars = case_when(is.na(p.value) ~ '',
                           p.value > 0.1 ~ '',
                           p.value > 0.05 ~ '.',
                           p.value > 0.01 ~ '*',
                           p.value > 0.001 ~ '**',
                           TRUE ~ '***'),
        outcome = substr(model, 1, 4),
        region = substr(model, 6, 8),
        cell = paste0(ame, stars, ifelse(is.na(ame.se), '', 
                                         paste0('\\newline(', ame.se, ')')))) %>% 
  merge(read.csv('~/gbv/visualizations/labels.csv', stringsAsFactors=F) %>%
          mutate(Category = ifelse(Category == '', NA, Category)) %>%
          fill(Category) %>%
          mutate(Label = ifelse(Label == '', '', paste0(' (', Label, ')')),
                 Label = paste0(Category, Label)) %>%
          select(-Category),
        all.x=T, all.y=F) %>%
  mutate(region=paste0(region, '.', type)) %>%
  select(outcome, cell, Label, region, Order) %>%
  spread(region, cell) %>%
  arrange(Order) %>%
  select(-Order)

############################# # Write to Tables
##########################

emot <- ames %>% filter(grepl('emot', outcome)) %>% select(-outcome)
sexu <- ames %>% filter(grepl('sexu', outcome)) %>% select(-outcome)
phys <- ames %>% filter(grepl('phys', outcome)) %>% select(-outcome)
cont <- ames %>% filter(grepl('cont', outcome)) %>% select(-outcome)

new_sanitize <- function(str){
  str <- gsub('>', '$>$', str)
  str <- gsub('<', '$>$', str)
  str
}

options(xtable.sanitize.text.function=new_sanitize)

hline <- c(-1, 0)
htype <- c("\\toprule&\\multicolumn{2}{c}{SSA}&\\multicolumn{2}{c}{Asia}&\\multicolumn{2}{c}{LAC}\\\\",
"  &  \\textit{asp.} & \\textit{sp.}& \\textit{asp.} & \\textit{sp.}& \\textit{asp.} & \\textit{sp.}\\\\")

#Emotional Violence
print(xtable(emot,
             caption='Average marginal effects of covariates on emotional violence in models with (\\textit{sp.}) and without (\\textit{asp.}) spatial terms across all three continents.  (*$p < 0.05$, **$p < 0.01$, ***$p<0.001$)', 
             label='tab:ames_emot',
             align=c('r', "r", "|", "p{1.5cm}", "p{1.5cm}", "p{1.5cm}", "p{1.5cm}", "p{1.5cm}", "p{1.5cm}")), 
      file='~/ipv-rep-tex/tables/ames_emot.tex',
      include.rownames=F,
      include.colnames=F,
      add.to.row = list(pos = as.list(hline),
                        command = htype),
      scalebox=0.85)


#Sexual Violence
print(xtable(sexu,
             caption='Average marginal effects of covariates on sexual violence in models with (\\textit{sp.}) and without (\\textit{asp.}) spatial terms across all three continents.  (*$p < 0.05$, **$p < 0.01$, ***$p<0.001$)', 
             label='tab:ames_emot',
             align=c('r', "r", "|", "p{1.5cm}", "p{1.5cm}", "p{1.5cm}", "p{1.5cm}", "p{1.5cm}", "p{1.5cm}")), 
      file='~/ipv-rep-tex/tables/ames_sexu.tex',
      include.rownames=F,
      include.colnames=F,
      add.to.row = list(pos = as.list(hline),
                        command = htype),
      scalebox=0.85)


#Physical Violence
print(xtable(phys,
             caption='Average marginal effects of covariates on physical violence in models with (\\textit{sp.}) and without (\\textit{asp.}) spatial terms across all three continents.  (*$p < 0.05$, **$p < 0.01$, ***$p<0.001$)', 
             label='tab:ames_phys',
             align=c('r', "r", "|", "p{1.5cm}", "p{1.5cm}", "p{1.5cm}", "p{1.5cm}", "p{1.5cm}", "p{1.5cm}")), 
      file='~/ipv-rep-tex/tables/ames_phys.tex',
      include.rownames=F,
      include.colnames=F,
      add.to.row = list(pos = as.list(hline),
                        command = htype),
      scalebox=0.85)


#Controlling Violence
print(xtable(cont,
             caption='Average marginal effects of covariates on controlling behaviors in models with (\\textit{sp.}) and without (\\textit{asp.}) spatial terms across all three continents.  (*$p < 0.05$, **$p < 0.01$, ***$p<0.001$)', 
             label='tab:ames_cont',
             align=c('r', "r", "|", "p{1.5cm}", "p{1.5cm}", "p{1.5cm}", "p{1.5cm}", "p{1.5cm}", "p{1.5cm}")), 
      file='~/ipv-rep-tex/tables/ames_cont.tex',
      include.rownames=F,
      include.colnames=F,
      floating=TRUE,
      type='latex',
      add.to.row = list(pos = as.list(hline),
                        command = htype),
      scalebox=0.85)
