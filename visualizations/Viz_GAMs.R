library(tidyverse)

mdf <- read.csv('~/mortalityblob/gbv/AMEs.csv') %>%
  filter(var == 'drought_cat')

bonferroni <- 12

allm <- mdf %>% 
  mutate(outcome = factor(substr(model, 1, 4),
                          levels=c('cont', 'emot', 'phys', 'sexu'),
                          labels=c('Controlling Behaviors', 
                                   'Emotional Violence', 
                                   'Physical Violence', 
                                   'Sexual Violence')),
         scale = factor(substr(model, 6, 8),
                        levels=c('afr', 'asi', 'lac'),
                        labels=c("SSA", "Asia", "LAC")),
         stars = case_when(p.value > 0.05/bonferroni ~ '',
                           p.value > 0.01/bonferroni ~ '*',
                           p.value > 0.001/bonferroni ~ '**',
                           TRUE ~ '***'),
         drought = factor(value, levels=c('moderate', 'severe', 'extreme'),
                          labels=c("Moderate", "Severe", "Extreme")),
         max = ame + ame.se*2,
         min = ame - ame.se*2)

res <- ggplot(allm) + 
  geom_pointrange(aes(x=drought, y=ame, ymax=max, ymin=min, color=drought),
           show.legend=F) +
  geom_text(aes(x=drought, y=max + 0.002, label=stars)) + 
  geom_hline(aes(yintercept=0), linetype=3) + 
  facet_grid(outcome ~ scale) + 
  scale_color_manual(values=c('#fe9929', '#d95f0e', '#993404')) +
  #scale_y_continuous(limits=c(min(allm$min), max(allm$max) + 0.004)) + 
  theme_bw() + 
  labs(x='Drought Status', y='Average Marginal Effect (Probability)')

ggsave(plot=res, filename='~/ipv-rep-tex/img/mod_results_gams.pdf', width=6, height=6)
