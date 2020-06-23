library(forcats)
library(tidyverse)
library(countrycode)
library(cowplot)

dat <- read.csv('~/mortalityblob/gbv/GBV_sel.csv')

sum <- dat %>%
  filter(in_afr | in_lac | in_asia) %>%
  mutate(Region = case_when(in_afr ~ 'Africa',
                            in_lac ~ 'LAC',
                            in_asia ~ 'Asia'),
         country = countrycode(country, 'dhs', 'country.name',
                               custom_match=c("CI" = 'Côte d\'Ivoire'))) %>%
  group_by(Region, survey_code, country) %>%
  mutate(Year = min(year)) %>%
  group_by(Region, Year, country) %>%
  summarize(Normal = mean(drought_cat == 'normal'),
            Moderate = mean(drought_cat == 'moderate'),
            Severe = mean(drought_cat == 'severe'),
            Extreme = mean(drought_cat == 'extreme')) %>%
  ungroup %>%
  mutate(lab = paste0(country, ' - ', Year)) %>% 
  select(-Year, -country) %>%
  gather(Drought, Value, -Region, -lab) %>%
  mutate(Drought = factor(Drought, levels=c("Normal", "Moderate", "Severe", "Extreme")),
         lab = fct_rev(lab))

(afr <- ggplot(sum %>% filter(Region == 'Africa')) + 
  geom_bar(aes(x=lab, y=Value, fill=Drought), position='stack', stat='identity') +
  theme_minimal() + 
  scale_fill_manual(values=c('#cccccc', '#fe9929', '#d95f0e', '#993404')) +
  theme(panel.spacing = unit(0, "lines"), 
        strip.placement= 'outside',
        strip.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) + 
  scale_y_continuous(expand=c(0,0)) +
  labs(title='SSA', x='', y='') + 
  coord_flip() + 
  guides(fill=F))

(lac <- ggplot(sum %>% filter(Region == 'LAC')) + 
  geom_bar(aes(x=lab, y=Value, fill=Drought), position='stack', stat='identity') +
  theme_minimal() + 
  scale_fill_manual(values=c('#cccccc', '#fe9929', '#d95f0e', '#993404')) +
  theme(panel.spacing = unit(0, "lines"), 
        strip.placement= 'outside',
        strip.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = 'bottom') + 
  scale_y_continuous(expand=c(0,0)) +
  labs(title='LAC', x='', y='') + 
  coord_flip())

(asia <- ggplot(sum %>% filter(Region == 'Asia')) + 
  geom_bar(aes(x=lab, y=Value, fill=Drought), position='stack', stat='identity') +
  theme_minimal() + 
  scale_fill_manual(values=c('#cccccc', '#fe9929', '#d95f0e', '#993404')) +
  theme(panel.spacing = unit(0, "lines"), 
        strip.placement= 'outside',
        strip.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) + 
  scale_y_continuous(expand=c(0,0)) +
  labs(title='Asia', x='', y='') + 
  coord_flip() + 
  guides(fill=F))

plot <- plot_grid(afr, plot_grid(asia, lac, align='v', ncol=1))

ggsave(plot=plot, filename='~/ipv-rep-tex/img/drought_levels.pdf', width=8, height=8) 
