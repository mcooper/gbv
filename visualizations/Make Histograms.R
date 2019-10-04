setwd('G://My Drive/DHS Processed')

library(tidyverse)
library(ggplot2)

gbv <- read.csv('GBV_all.csv') %>%
  filter(mean_annual_precip > 200 & builtup < 0.1) %>%
  select(spei36, `Physical Violence` = viol_phys, `Sexual Violence` = viol_sex) %>%
  gather(Type, Value, -spei36)
  
ggplot(gbv) + 
  geom_histogram(aes(x=spei36, fill=Value), binwidth=0.1) +
  scale_fill_manual(labels=c('No', 'Yes'),
                   values=c('#8c96c6', '#810f7c')) +
  theme_minimal() + 
  labs(fill="Has Experienced\nIPV in the\nPrevious Year?") + 
  xlab('36-Month Standardized Precipitation-Evapotranspiration Index') + 
  ylab('Count') + 
  facet_wrap(. ~ Type)

setwd('C://Users/matt/gbv-tex')
ggsave('Histograms.pdf', width = 8.67, height=2.81)
system("pdfcrop Histograms.pdf Histograms.pdf")

