library(tidyverse)
library(xtable)

spatial <- read.csv('~/mortalityblob/gbv/AMEs.csv') %>%
  mutate(type='spatial')

aspatial <- read.csv('~/mortalityblob/gbv/PLOS_AMEs.csv') %>%
  mutate(type='aspatial')

ames <- bind_rows(spatial, aspatial) %>%
  mutate(Var = gsub('TRUE', '', paste0(var, value)),
         ame = round(ame, 3),
         ame.se = round(ame.se, 3),
         stars = case_when(p.value > 0.1 ~ '',
                           p.value > 0.05 ~ '.',
                           p.value > 0.01 ~ '*',
                           p.value > 0.001 ~ '**',
                           TRUE ~ '***'),
        outcome = substr(model, 1, 4),
        region = substr(model, 6, 8),
        cell = paste0(ame, stars, '\\newline(', ame.se, ')')) %>% 
  merge(read.csv('~/gbv/visualizations/labels.csv', stringsAsFactors=F) %>%
          mutate(Category = ifelse(Category == '', NA, Category)) %>%
          fill(Category) %>%
          mutate(Label = ifelse(Label == '', '', paste0(' (', Label, ')')),
                 Label = paste0(Category, Label)) %>%
          select(-Category),
        all.x=T, all.y=F) %>%
  arrange(Order) %>%
  mutate(region=paste0(region, '.', type)) %>%
  select(outcome, cell, Label, region) %>%
  spread(region, cell)

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

#Emotional Violence
hline <- c(-1, 0)
htype <- c("\\toprule&\\multicolumn{2}{c}{SSA}&\\multicolumn{2}{c}{Asia}&\\multicolumn{2}{c}{LAC}\\\\",
"  &  \\textit{asp.} & \\textit{sp.}& \\textit{asp.} & \\textit{sp.}& \\textit{asp.} & \\textit{sp.}\\\\")

print(xtable(emot,
             caption='Average marginal effects of individual and household-level variables on emotional violence in spatial (\\textit{sp.}) and aspatial (\\textit{asp.}) models across all three continents.  (*$p < 0.05$, **$p < 0.01$, ***$p<0.001$)', 
             label='tab:ames_emot',
             align=c('r', "r", "|", "p{1.5cm}", "p{1.5cm}", "p{1.5cm}", "p{1.5cm}", "p{1.5cm}", "p{1.5cm}")), 
      file='~/ipv-rep-tex/tables/ames_emot.tex',
      include.rownames=F,
      include.colnames=F,
      add.to.row = list(pos = as.list(hline),
                        command = htype))


#Sexual Violence
hline <- c(-1, 0)
htype <- c("\\toprule&\\multicolumn{2}{c}{SSA}&\\multicolumn{2}{c}{Asia}&\\multicolumn{2}{c}{LAC}\\\\",
"  &  \\textit{asp.} & \\textit{sp.}& \\textit{asp.} & \\textit{sp.}& \\textit{asp.} & \\textit{sp.}\\\\")

print(xtable(sexu,
             caption='Average marginal effects of individual and household-level variables on sexual violence in spatial (\\textit{sp.}) and aspatial (\\textit{asp.}) models across all three continents.  (*$p < 0.05$, **$p < 0.01$, ***$p<0.001$)', 
             label='tab:ames_emot',
             align=c('r', "r", "|", "p{1.5cm}", "p{1.5cm}", "p{1.5cm}", "p{1.5cm}", "p{1.5cm}", "p{1.5cm}")), 
      file='~/ipv-rep-tex/tables/ames_sexu.tex',
      include.rownames=F,
      include.colnames=F,
      add.to.row = list(pos = as.list(hline),
                        command = htype))


#Physical Violence
hline <- c(-1, 0)
htype <- c("\\toprule&\\multicolumn{2}{c}{SSA}&\\multicolumn{2}{c}{Asia}&\\multicolumn{2}{c}{LAC}\\\\",
"  &  \\textit{asp.} & \\textit{sp.}& \\textit{asp.} & \\textit{sp.}& \\textit{asp.} & \\textit{sp.}\\\\")

print(xtable(phys,
             caption='Average marginal effects of individual and household-level variables on physical violence in spatial (\\textit{sp.}) and aspatial (\\textit{asp.}) models across all three continents.  (*$p < 0.05$, **$p < 0.01$, ***$p<0.001$)', 
             label='tab:ames_phys',
             align=c('r', "r", "|", "p{1.5cm}", "p{1.5cm}", "p{1.5cm}", "p{1.5cm}", "p{1.5cm}", "p{1.5cm}")), 
      file='~/ipv-rep-tex/tables/ames_phys.tex',
      include.rownames=F,
      include.colnames=F,
      add.to.row = list(pos = as.list(hline),
                        command = htype))


#Controlling Violence
hline <- c(-1, 0)
htype <- c("\\toprule&\\multicolumn{2}{c}{SSA}&\\multicolumn{2}{c}{Asia}&\\multicolumn{2}{c}{LAC}\\\\",
"  &  \\textit{asp.} & \\textit{sp.}& \\textit{asp.} & \\textit{sp.}& \\textit{asp.} & \\textit{sp.}\\\\")

print(xtable(cont,
             caption='Average marginal effects of individual and household-level variables on controlling behaviors in spatial (\\textit{sp.}) and aspatial (\\textit{asp.}) models across all three continents.  (*$p < 0.05$, **$p < 0.01$, ***$p<0.001$)', 
             label='tab:ames_cont',
             align=c('r', "r", "|", "p{1.5cm}", "p{1.5cm}", "p{1.5cm}", "p{1.5cm}", "p{1.5cm}", "p{1.5cm}")), 
      file='~/ipv-rep-tex/tables/ames_cont.tex',
      include.rownames=F,
      include.colnames=F,
      add.to.row = list(pos = as.list(hline),
                        command = htype))
