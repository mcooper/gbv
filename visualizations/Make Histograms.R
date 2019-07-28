setwd('G://My Drive/DHS Processed')

library(tidyverse)
library(ggplot2)

gbv <- read.csv('GBV_all.csv') %>%
  mutate(gbv_year=ifelse(gbv_year=='never', "No", "Yes")) %>%
  dplyr::select(gbv_year, spei24)

ggplot(gbv) + 
  geom_histogram(aes(x=spei24, fill=gbv_year), binwidth=0.1) +
  scale_fill_manual(labels=c('No', 'Yes'),
                   values=c('#8c96c6', '#810f7c')) +
  theme_minimal() + 
  labs(fill="Has Experienced\nGBV in the\nPrevious Year?") + 
  xlab('24-Month Standardized Precipitation-Evapotranspiration Index') + 
  ylab('Count')

setwd('C://Users/matt/gbv-tex')
ggsave('Histograms.pdf', width=6, height=3)
system("pdfcrop Histograms.pdf Histograms.pdf")

