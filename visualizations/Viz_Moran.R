library(xtable)
library(tidyverse)

dat <- read.csv('~/mortalityblob/gbv/moran_results.csv') %>%
  mutate(observed = round(observed, 3),
         stars = case_when(p.value > 0.1 ~ '',
                           p.value > 0.05 ~ '.',
                           p.value > 0.01 ~ '*',
                           p.value > 0.001 ~ '**',
                           TRUE ~ '***'),
         val = paste(observed, stars)) %>%
  select(-file, -model, -method, -observed, -sd, -p.value, -expected, -stars) %>%
  unique %>%
  spread(order, val) %>%
  arrange(scale, region, outcome)

n <- dat  %>%
  filter(scale == 'code') %>%
  mutate(model = paste0(outcome, region)) %>%
  select(-outcome, -region, -scale) %>%
  gather(order, moran, -model) %>%
  mutate(moran = moran,
         order = as.numeric(order)) %>%
  spread(model, moran) %>%
  merge(data.frame(order=c("0", "1", "2", "3", "50", "100", "500", '1000'),
                   lab=c("No Spatial Terms",
                         "1st-Order Legendre Polynomials",
                         "2nd-Order Legendre Polynomials",
                         "3rd-Order Legendre Polynomials",
                         "50-knot Thin Plate Splines on a Sphere",
                         "100-knot Thin Plate Splines on a Sphere",
                         "500-knot Thin Plate Splines on a Sphere",
                         "1000-knot Thin Plate Splines on a Sphere"))) %>%
  select(lab, matches('afr|asi|lac'))

hline <- c(-1, 0)
htype <- c("\\toprule & \\multicolumn{3}{c}{Controlling} & \\multicolumn{3}{c}{Emotional} & \\multicolumn{3}{c}{Physical} & \\multicolumn{3}{c}{Sexual}\\\\",
"Method  &  \\textit{SSA} & \\textit{Asia}& \\textit{LAC} &  \\textit{SSA} & \\textit{Asia}& \\textit{LAC}&  \\textit{SSA} & \\textit{Asia}& \\textit{LAC} &  \\textit{SSA} & \\textit{Asia}& \\textit{LAC} \\\\")
print(xtable(n,
             caption="Moran's I test statistic of models fit with various spatial terms.  Models were conducted across various contients - sub-Saharan Africa (SSA), Asia, and Latin America and the Caribean (LAC), as well as for various types of IPV, including controlling behaviors, emotional violence, physical violence, sexual violence. (.$p < 0.1$, *$p < 0.05$, **$p<0.01$, ***$p<0.001$)", 
             label='tab:moranps',
             align=c('r', "r", "|", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c")), 
      file='~/ipv-rep-tex/tables/moranps.tex',
      include.rownames=F,
      include.colnames=F,
      add.to.row = list(pos = as.list(hline),
                        command = htype),
      floating.environment = "sidewaystable",
      size="\\fontsize{7pt}{10pt}\\selectfont")
