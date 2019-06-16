setwd('G://My Drive/DHS Processed')

library(tidyverse)
library(ggplot2)

gbv <- read.csv('GBV_all.csv')

spei <- gbv %>%
  select(value=spei24, gbv_year) %>%
  mutate(metric='spei24')

temp <- gbv %>%
  select(value=temp12monthZ, gbv_year) %>%
  mutate(metric='temp12monthZ')

comb <- bind_rows(spei, temp)

comb$gbv_year <- factor(comb$gbv_year, levels = c('never', 'sometimes', 'often'))

labels <- list(
  "spei24"="24-Month Standardized\nPrecipitation-Evapotranspiration Index",
  'temp12monthZ'='12-Month Average Monthly\nHigh Temperature Z-Score'
)

labeller <- function(variable, value){
  return(labels[value])
}

ggplot(comb) + 
  geom_histogram(aes(x=value, fill=gbv_year), binwidth=0.1) + 
  facet_grid(.~metric, labeller=labeller) + 
  scale_fill_manual(labels=c('Never', 'Sometimes', 'Often'),
                    values=c('#8c96c6', '#8856a7', '#810f7c')) + 
  theme_minimal() + 
  labs(fill="Gender-Based\nViolence (GBV)") + xlab('') + ylab('')


setwd('C://Users/matt/gbv-tex')
ggsave('Histograms.pdf', width=7.5, height=3)
system("pdfcrop Histograms.pdf Histograms.pdf")

