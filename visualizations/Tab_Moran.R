library(xtable)
library(tidyverse)

options(scipen=100)

########################
# Gam spline models
######################
dat <- read.csv('~/mortalityblob/gbv/moran_results.csv') %>%
  filter(method != 'legendre2', order!=1250) %>%
  mutate(observed = round(observed, 4),
         stars = case_when(p.value > 0.1 ~ '',
                           p.value > 0.05 ~ '',
                           p.value > 0.01 ~ '*',
                           p.value > 0.001 ~ '**',
                           TRUE ~ '***'),
         val = paste(observed, stars)) %>%
  select(-file, -model, -method, -observed, -sd, -expected, -stars) %>%
  group_by(outcome, region) %>%
  filter(order <= min(order[p.value > 0.05])) %>%
  select(-p.value) %>%
  ungroup %>%
  spread(order, val) %>%
  arrange(region, outcome)

n <- dat  %>%
  mutate(model = paste0(outcome, region)) %>%
  select(-outcome, -region) %>%
  gather(order, moran, -model) %>%
  mutate(moran = moran,
         order = as.numeric(order)) %>%
  spread(model, moran) %>%
  merge(data.frame(order=c("0", "50", "100", "500", '1000', '1500'),
                   lab=c("No Spatial Terms",
                         "50-knot Thin Plate Splines on a Sphere",
                         "100-knot Thin Plate Splines on a Sphere",
                         "500-knot Thin Plate Splines on a Sphere",
                         "1000-knot Thin Plate Splines on a Sphere",
                         "1500-knot Thin Plate Splines on a Sphere"))) %>%
  select(lab, matches('afr|asi|lac'))

#Controlling
hline <- c(-1, 0)
htype <- c("\\toprule & \\multicolumn{3}{c}{Controlling Behaviors}  \\\\",
"  &  \\textit{SSA} & \\textit{Asia}& \\textit{LAC}\\\\")
print(xtable(n[1:4],
#             caption="Moran's I test statistic of spatial autocorrleation for models across all three continents and all four types of IPV.  The expected value of the Moran's I test under the null hypothesis of no spatial autocorrelation tends towards 0. Models were fit with increasingly complex spatial splines until there was no longer significant autocorrelation in the residuals at $\\alpha = 0.05$. (*$p < 0.05$, **$p<0.01$, ***$p<0.001$)", 
             label='tab:moran_gams_cont',
             align=c('r', "r", "|", "c", "c", "c")), 
      file='~/ipv-rep-tex/tables/moran_gams_cont.tex',
      include.rownames=F,
      include.colnames=F,
      add.to.row = list(pos = as.list(hline),
                        command = htype))

#Emotional
hline <- c(-1, 0)
htype <- c("\\toprule & \\multicolumn{3}{c}{Emotional Violence}  \\\\",
"  &  \\textit{SSA} & \\textit{Asia}& \\textit{LAC}\\\\")
print(xtable(n[c(1, 5:7)],
#             caption="Moran's I test statistic of spatial autocorrleation for models across all three continents and all four types of IPV.  The expected value of the Moran's I test under the null hypothesis of no spatial autocorrelation tends towards 0. Models were fit with increasingly complex spatial splines until there was no longer significant autocorrelation in the residuals at $\\alpha = 0.05$. (*$p < 0.05$, **$p<0.01$, ***$p<0.001$)", 
             label='tab:moran_gams_emot',
             align=c('r', "r", "|", "c", "c", "c")), 
      file='~/ipv-rep-tex/tables/moran_gams_emot.tex',
      include.rownames=F,
      include.colnames=F,
      add.to.row = list(pos = as.list(hline),
                        command = htype))

#Physical
hline <- c(-1, 0)
htype <- c("\\toprule & \\multicolumn{3}{c}{Physical Violence}  \\\\",
"  &  \\textit{SSA} & \\textit{Asia}& \\textit{LAC}\\\\")
print(xtable(n[c(1, 8:10)],
#             caption="Moran's I test statistic of spatial autocorrleation for models across all three continents and all four types of IPV.  The expected value of the Moran's I test under the null hypothesis of no spatial autocorrelation tends towards 0. Models were fit with increasingly complex spatial splines until there was no longer significant autocorrelation in the residuals at $\\alpha = 0.05$. (*$p < 0.05$, **$p<0.01$, ***$p<0.001$)", 
             label='tab:moran_gams_phys',
             align=c('r', "r", "|", "c", "c", "c")), 
      file='~/ipv-rep-tex/tables/moran_gams_phys.tex',
      include.rownames=F,
      include.colnames=F,
      add.to.row = list(pos = as.list(hline),
                        command = htype))


#Sexual
hline <- c(-1, 0)
htype <- c("\\toprule & \\multicolumn{3}{c}{Sexual Violence}  \\\\",
"  &  \\textit{SSA} & \\textit{Asia}& \\textit{LAC}\\\\")
print(xtable(n[c(1, 11:13)],
#             caption="Moran's I test statistic of spatial autocorrleation for models across all three continents and all four types of IPV.  The expected value of the Moran's I test under the null hypothesis of no spatial autocorrelation tends towards 0. Models were fit with increasingly complex spatial splines until there was no longer significant autocorrelation in the residuals at $\\alpha = 0.05$. (*$p < 0.05$, **$p<0.01$, ***$p<0.001$)", 
             label='tab:moran_gams_sexu',
             align=c('r', "r", "|", "c", "c", "c")), 
      file='~/ipv-rep-tex/tables/moran_gams_sexu.tex',
      include.rownames=F,
      include.colnames=F,
      add.to.row = list(pos = as.list(hline),
                        command = htype))


####################################
# Legendre Polynomials for appendix
#####################################

dat <- read.csv('~/mortalityblob/gbv/moran_results.csv') %>%
  filter(method == 'legendre2') %>%
  mutate(observed = round(observed, 4),
         stars = case_when(p.value > 0.1 ~ '',
                           p.value > 0.05 ~ '',
                           p.value > 0.01 ~ '*',
                           p.value > 0.001 ~ '**',
                           TRUE ~ '***'),
         val = paste(observed, stars)) %>%
  select(-file, -model, -method, -observed, -sd, -expected, -stars, -p.value) %>%
  spread(order, val) %>%
  arrange(region, outcome)

n <- dat  %>%
  mutate(model = paste0(outcome, region)) %>%
  select(-outcome, -region) %>%
  gather(order, moran, -model) %>%
  mutate(moran = moran,
         order = as.numeric(order)) %>%
  spread(model, moran) %>%
  merge(data.frame(order=c("0", "1", "2", "3"),
                   lab=c("No Spatial Terms",
                         "1st-Order Legendre Polynomials",
                         "2nd-Order Legendre Polynomials",
                         "3rd-Order Legendre Polynomials"))) %>%
  select(lab, matches('afr|asi|lac'))

hline <- c(-1, 0)
htype <- c("\\toprule & \\multicolumn{3}{c}{Controlling} & \\multicolumn{3}{c}{Emotional} & \\multicolumn{3}{c}{Physical} & \\multicolumn{3}{c}{Sexual}\\\\",
"Method  &  \\textit{SSA} & \\textit{Asia}& \\textit{LAC} &  \\textit{SSA} & \\textit{Asia}& \\textit{LAC}&  \\textit{SSA} & \\textit{Asia}& \\textit{LAC} &  \\textit{SSA} & \\textit{Asia}& \\textit{LAC} \\\\")
print(xtable(n,
             caption="Moran's I test statistic of spatial autocorrleation for models across all three continents and all four types of IPV using Legendre polynomials. (*$p < 0.05$, **$p<0.01$, ***$p<0.001$)", 
             label='tab:moran_legendre',
             align=c('r', "r", "|", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c")), 
      file='~/ipv-rep-tex/tables/moran_legendre.tex',
      include.rownames=F,
      include.colnames=F,
      add.to.row = list(pos = as.list(hline),
                        command = htype),
      scalebox=0.9)



