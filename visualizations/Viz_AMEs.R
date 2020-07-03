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
  mutate(col=paste0(outcome, '.', type)) %>%
  select(col, cell, Label, region) %>%
  spread(col, cell)

#############################
# Write to Tables
##########################

afr1 <- ames %>% filter(region=='afr') %>% select(-region) %>% select(matches('con|emo|Lab'))
asi1 <- ames %>% filter(region=='asi') %>% select(-region) %>% select(matches('con|emo|Lab'))
lac1 <- ames %>% filter(region=='lac') %>% select(-region) %>% select(matches('con|emo|Lab'))
afr2 <- ames %>% filter(region=='afr') %>% select(-region) %>% select(matches('phy|sex|Lab'))
asi2 <- ames %>% filter(region=='asi') %>% select(-region) %>% select(matches('phy|sex|Lab'))
lac2 <- ames %>% filter(region=='lac') %>% select(-region) %>% select(matches('phy|sex|Lab'))

new_sanitize <- function(str){
  str <- gsub('>', '$>$', str)
  str <- gsub('<', '$>$', str)
  str
}

options(xtable.sanitize.text.function=new_sanitize)

#Africa
hline1 <- c(-1, 0)
htype1 <- c("\\toprule&\\multicolumn{2}{c}{Controlling}& \\multicolumn{2}{c}{Emotional}\\\\",
"  &  \\textit{asp.} & \\textit{sp.}& \\textit{asp.} & \\textit{sp.}\\\\")

print(xtable(afr1,
             caption='Average marginal effects for individual and household-level variables in spatial (\\textit{sp.}) and aspatial (\\textit{asp.}) models in Africa for controlling behaviors and emotional violence.  (*$p < 0.05$, **$p < 0.01$, ***$p<0.001$)', 
             label='tab:ames_afr1',
             align=c('r', "r", "|", "p{2cm}", "p{2cm}", "p{2cm}", "p{2cm}")), 
      file='~/ipv-rep-tex/tables/afr_res1.tex',
      include.rownames=F,
      include.colnames=F,
      add.to.row = list(pos = as.list(hline1),
                        command = htype1))

hline2 <- c(-1, 0)
htype2 <- c("\\toprule&\\multicolumn{2}{c}{Physical}& \\multicolumn{2}{c}{Sexual}\\\\",
" & \\textit{asp.} & \\textit{sp.}& \\textit{asp.} & \\textit{sp.}\\\\")

print(xtable(afr2,
             caption='Average marginal effects for individual and household-level variables in spatial (\\textit{sp.}) and aspatial (\\textit{asp.}) models in Africa for physical and sexual violence.  (*$p < 0.05$, **$p < 0.01$, ***$p<0.001$)', 
             label='tab:ames_afr2',
             align=c('r', "r", "|", "p{2cm}", "p{2cm}", "p{2cm}", "p{2cm}")), 
      file='~/ipv-rep-tex/tables/afr_res2.tex',
      include.rownames=F,
      include.colnames=F,
      add.to.row = list(pos = as.list(hline2),
                        command = htype2))


#Asia
hline1 <- c(-1, 0)
htype1 <- c("\\toprule&\\multicolumn{2}{c}{Controlling}& \\multicolumn{2}{c}{Emotional}\\\\",
"  &  \\textit{asp.} & \\textit{sp.}& \\textit{asp.} & \\textit{sp.}\\\\")

print(xtable(asi1,
             caption='Average marginal effects for individual and household-level variables in spatial (\\textit{sp.}) and aspatial (\\textit{asp.}) models in Asia for controlling behaviors and emotional violence.  (*$p < 0.05$, **$p < 0.01$, ***$p<0.001$)', 
             label='tab:ames_asi1',
             align=c('r', "r", "|", "p{2cm}", "p{2cm}", "p{2cm}", "p{2cm}")), 
      file='~/ipv-rep-tex/tables/asi_res1.tex',
      include.rownames=F,
      include.colnames=F,
      add.to.row = list(pos = as.list(hline1),
                        command = htype1))

hline2 <- c(-1, 0)
htype2 <- c("\\toprule&\\multicolumn{2}{c}{Physical}& \\multicolumn{2}{c}{Sexual}\\\\",
" & \\textit{asp.} & \\textit{sp.}& \\textit{asp.} & \\textit{sp.}\\\\")

print(xtable(asi2,
             caption='Average marginal effects for individual and household-level variables in spatial (\\textit{sp.}) and aspatial (\\textit{asp.}) models in Asia for physical and sexual violence.  (*$p < 0.05$, **$p < 0.01$, ***$p<0.001$)', 
             label='tab:ames_asi2',
             align=c('r', "r", "|", "p{2cm}", "p{2cm}", "p{2cm}", "p{2cm}")), 
      file='~/ipv-rep-tex/tables/asi_res2.tex',
      include.rownames=F,
      include.colnames=F,
      add.to.row = list(pos = as.list(hline2),
                        command = htype2))


#Latin America and the Caribbean
hline1 <- c(-1, 0)
htype1 <- c("\\toprule&\\multicolumn{2}{c}{Controlling}& \\multicolumn{2}{c}{Emotional}\\\\",
"  &  \\textit{asp.} & \\textit{sp.}& \\textit{asp.} & \\textit{sp.}\\\\")

print(xtable(lac1,
             caption='Average marginal effects for individual and household-level variables in spatial (\\textit{sp.}) and aspatial (\\textit{asp.}) models in Latin America and the Caribbean for controlling behaviors and emotional violence.  (*$p < 0.05$, **$p < 0.01$, ***$p<0.001$)', 
             label='tab:ames_lac1',
             align=c('r', "r", "|", "p{2cm}", "p{2cm}", "p{2cm}", "p{2cm}")), 
      file='~/ipv-rep-tex/tables/lac_res1.tex',
      include.rownames=F,
      include.colnames=F,
      add.to.row = list(pos = as.list(hline1),
                        command = htype1))

hline2 <- c(-1, 0)
htype2 <- c("\\toprule&\\multicolumn{2}{c}{Physical}& \\multicolumn{2}{c}{Sexual}\\\\",
" & \\textit{asp.} & \\textit{sp.}& \\textit{asp.} & \\textit{sp.}\\\\")

print(xtable(lac2,
             caption='Average marginal effects for individual and household-level variables in spatial (\\textit{sp.}) and aspatial (\\textit{asp.}) models in Latin America and the Caribbean for physical and sexual violence.  (*$p < 0.05$, **$p < 0.01$, ***$p<0.001$)', 
             label='tab:ames_lac2',
             align=c('r', "r", "|", "p{2cm}", "p{2cm}", "p{2cm}", "p{2cm}")), 
      file='~/ipv-rep-tex/tables/lac_res2.tex',
      include.rownames=F,
      include.colnames=F,
      add.to.row = list(pos = as.list(hline2),
                        command = htype2))


