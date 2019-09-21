setwd('G://My Drive/DHS Processed')

library(dplyr)
library(lme4)
library(broom)
library(cowplot)
library(ggplot2)

gbv <- read.csv('GBV_all.csv', stringsAsFactors=F)

gbv$surveycode <- substr(gbv$code, 1, 6)

gbv <- gbv %>%
  filter(!is.na(wealth_factor_harmonized) & !is.na(hhsize) & !is.na(spei24))

gbv$year <- floor((gbv$date_cmc + 1)/12) + 1900

mod1 <- data.frame()
for (y in unique(gbv$year)){
  sel <- gbv %>% filter(year==y)
  
  if (length(unique(sel$country)) > 1){
    mod_spi <- glm(viol_phys_ip ~ spei24 + years_education + date_cmc + 
                     wealth_factor_harmonized + hhsize + country + 
                     mean_annual_precip + mean_annual_tmax + spei24, 
                   data=sel, family = 'binomial')
  } else{
    mod_spi <- glm(viol_phys_ip ~ spei24 + years_education + date_cmc + 
                     wealth_factor_harmonized + hhsize + 
                     mean_annual_precip + mean_annual_tmax + spei24, 
                   data=sel, family = 'binomial')
  }
  
  res <- tidy(mod_spi) %>%
    filter(term=='spei24')
  res$year <- y
  res$n <- nrow(sel)
  res$surveys <- length(unique(sel$surveycode))
  mod1 <- bind_rows(mod1, res)
  
  print(y)
}

mod1$max <- mod1$estimate + mod1$std.error*2
mod1$min <- mod1$estimate - mod1$std.error*2


convert <- function(x){
  odds <- exp(x)
  round(odds, 2)
}

Effects <- ggplot(mod1) + 
  geom_pointrange(aes(x=year, y=estimate, ymin=min, ymax=max)) + 
  geom_hline(yintercept=0, linetype=2, color='red') + 
  theme_minimal() +
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  scale_y_continuous(labels=convert) +
  xlim(1999.5, 2017.5) + 
  ylab('Change in Odds of Experiencing GBV with a 1-Unit SPEI Increase')

Hist <- ggplot(mod1) + 
  geom_bar(aes(x=year, y=n), stat='identity') + 
  theme_minimal() +
  ylab('Count') + 
  xlab('Year') + 
  xlim(1999.5, 2017.5) + 
  scale_y_continuous(breaks=c(0, 40000))


plot_grid(plotlist=list(Effects, Hist), align='v', axis='rl', ncol=1, nrow=2, rel_heights=c(1, 0.2))

setwd('C://Users/matt/gbv-tex')
ggsave('ByTime.pdf', width=5, height=8)
system("pdfcrop ByTime.pdf ByTime.pdf")
