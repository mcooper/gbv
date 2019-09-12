library(mgcv)
library(parallel)
library(dplyr)
library(ggplot2)
library(ggeffects)
library(cowplot)

dat <- read.csv('G://My Drive/DHS Processed/GBV_all.csv')

dat$gbv_year <- dat$gbv_year != 'never'

dat$empowered_decisions <- as.factor(dat$empowered_decisions)
dat$empowered_gbv_notok <- as.factor(dat$empowered_gbv_notok)

dat <- dat %>%
  filter(!is.infinite(spei36) & !is.infinite(spei48))

cl <- makeCluster(4)

mod24gam <- bam(gbv_year ~ s(spei24) + mean_annual_precip + mean_annual_tmax + 
                      wealth_factor_harmonized + hhsize + date_cmc + years_education + 
                      country, data=dat, family = 'binomial', cluster=cl)

mod24lm <- glm(gbv_year ~ spei24 + mean_annual_precip + mean_annual_tmax + 
                 wealth_factor_harmonized + hhsize + date_cmc + years_education + 
                 country, data=dat, family = 'binomial')


p <- plot(mod24gam)

gamdf <- data.frame(spei24=p[[1]]$x, 
                    pred=p[[1]]$fit, 
                    se=p[[1]]$se,
                    model='Spline Model (GAM)')

s <- summary(mod24lm)

glmdf <- data.frame(spei24=p[[1]]$x, 
                    pred=s$coefficients['spei24', 'Estimate']*p[[1]]$x,
                    se=s$coefficients['spei24', 'Std. Error']*p[[1]]$x,
                    model='Linear Model (GLM)')

pltdf <- bind_rows(gamdf, glmdf) %>%
  mutate(max=pred + se*2,
         min=pred - se*2)

convert <- function(x){
  odds <- exp(x)
  prob <- odds / (1 + odds)
  return(round(prob - 0.5, 2))
}

lines <- ggplot(pltdf) + 
  geom_ribbon(aes(x=spei24, ymin=min, ymax=max, fill=model), alpha=0.25) + 
  geom_line(aes(x=spei24, y=pred, color=model)) + 
  theme_minimal() +
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) + 
  scale_y_continuous(labels=convert) +
  ylab('Change in Probability of Experiencing GBV') +
  facet_wrap(. ~ model, ncol = 1, labeller=function(x){''})

hist <- ggplot(dat) +
  geom_histogram(aes(x=spei24), binwidth=0.1) +
  theme_minimal() + 
  xlab('24-Month Standardized Precipitation-Evapotranspiration Index') +
  scale_x_continuous(breaks=c(-2, -1, 0, 1, 2)) + 
  scale_y_continuous(name = 'Count', breaks=c(0, 15000)) +
  xlim(min(pltdf$spei24), max(pltdf$spei24))

plot_grid(plotlist=list(lines, hist), align='v', ncol=1, nrow=2, rel_heights=c(1, 0.2)) +
  annotate("text", x=0.15, y=0.9, size=6, label='atop(bold("A"))', parse=T) +
  annotate("text", x=0.15, y=0.5, size=6, label='atop(bold("B"))', parse=T) +
  annotate("text", x=0.15, y=0.1, size=6, label='atop(bold("C"))', parse=T)

setwd('C://Users/matt/gbv-tex')
ggsave('Regressions.pdf', width=5, height=8)
system("pdfcrop Regressions.pdf Regressions.pdf")
