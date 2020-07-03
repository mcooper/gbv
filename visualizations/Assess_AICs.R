library(xtable)
library(tidyverse)
library(mgcv)
library(fastglm)

setwd('~/mortalityblob/gbv_gams')

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
	#group_by(outcome, model, scale) %>%
	#filter(order==max(order)) %>%
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
  filter(model != 'epstein') %>%
  select(-model, -full, -file) %>%
  spread(order, aic)

n <- aics %>%
  mutate(model = paste0(outcome, scale)) %>%
  select(-outcome, -scale) %>%
  gather(order, aic, -model) %>%
  mutate(aic = round(aic),
         order = as.numeric(order)) %>%
  group_by(model) %>%
  mutate(aic = ifelse(aic == min(aic), paste0('\\textbf{', aic, '}'), aic)) %>%
  spread(model, aic) %>%
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


new_sanitize <- function(str){
  str <- gsub('>', '$>$', str)
  str <- gsub('<', '$>$', str)
  str
}

options(xtable.sanitize.text.function=identity)

hline <- c(-1, 0)
htype <- c("\\toprule & \\multicolumn{3}{c}{Controlling} & \\multicolumn{3}{c}{Emotional} & \\multicolumn{3}{c}{Physical} & \\multicolumn{3}{c}{Sexual}\\\\",
"Method  &  \\textit{SSA} & \\textit{Asia}& \\textit{LAC} &  \\textit{SSA} & \\textit{Asia}& \\textit{LAC}&  \\textit{SSA} & \\textit{Asia}& \\textit{LAC} &  \\textit{SSA} & \\textit{Asia}& \\textit{LAC} \\\\")
print(xtable(n,
             caption='AIC of models fit with various spatial terms.  Models were conducted across various contients - sub-Saharan Africa (SSA), Asia, and Latin America and the Caribean (LAC), as well as for various types of IPV, including controlling behaviors, emotional violence, physical violence, sexual violence. For each outcome variable and continent, the model with the lowest AIC is given in bold.', 
             label='tab:aics',
             align=c('r', "r", "|", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c")), 
      file='~/ipv-rep-tex/tables/aics.tex',
      include.rownames=F,
      include.colnames=F,
      add.to.row = list(pos = as.list(hline),
                        command = htype),
      floating.environment = "sidewaystable")
