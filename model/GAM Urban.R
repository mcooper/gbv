library(mgcv)
library(parallel)
library(dplyr)
library(ggplot2)
library(ggeffects)
library(cowplot)

dat <- read.csv('G://My Drive/DHS Processed/GBV_all.csv')

dat$gbv_year <- dat$gbv_year != 'never'

dat$empowered <- ifelse(dat$empowered_gbv_notok, "Empowered", "Not Empowered")
dat$empowered_gbv_notok <- as.factor(dat$empowered_gbv_notok)

dat$urban <- dat$builtup > 0.05
dat$urban <- as.factor(dat$urban)

dat <- dat %>%
  filter(!is.infinite(spei36) & !is.infinite(spei48))

cl <- makeCluster(4)

mod24gamtp <- bam(gbv_year ~ s(spei24, bs='tp', by = urban) + mean_annual_precip + mean_annual_tmax + 
                  wealth_factor_harmonized + hhsize + date_cmc + years_education + 
                  country, data=dat, family = 'binomial', cluster=cl, method='REML')

mod24gamcr_remlg10 <- bam(gbv_year ~ s(spei24, bs='cr', by = urban) + mean_annual_precip + mean_annual_tmax + 
                  wealth_factor_harmonized + hhsize + date_cmc + years_education + 
                  country, data=dat, family = 'binomial', cluster=cl, method='REML', gamma=10)


mod24gamcr_remlg5 <- bam(gbv_year ~ s(spei24, bs='cr', by = urban) + mean_annual_precip + mean_annual_tmax + 
                            wealth_factor_harmonized + hhsize + date_cmc + years_education + 
                            country, data=dat, family = 'binomial', cluster=cl, method='REML', gamma=5)


mod24gamcr_remlg2 <- bam(gbv_year ~ s(spei24, bs='cr', by = urban) + mean_annual_precip + mean_annual_tmax + 
                            wealth_factor_harmonized + hhsize + date_cmc + years_education + 
                            country, data=dat, family = 'binomial', cluster=cl, method='REML', gamma=2)


mod24gamcr_remlg1.5 <- bam(gbv_year ~ s(spei24, bs='cr', by = urban) + mean_annual_precip + mean_annual_tmax + 
                            wealth_factor_harmonized + hhsize + date_cmc + years_education + 
                            country, data=dat, family = 'binomial', cluster=cl, method='REML', gamma=1.5)



mod24gamps <- bam(gbv_year ~ s(spei24, bs='ps', by = urban) + mean_annual_precip + mean_annual_tmax + 
                  wealth_factor_harmonized + hhsize + date_cmc + years_education + 
                  country, data=dat, family = 'binomial', cluster=cl, method='REML')

mod24gamgp <- bam(gbv_year ~ s(spei24, bs='gp', by = urban) + mean_annual_precip + mean_annual_tmax + 
                  wealth_factor_harmonized + hhsize + date_cmc + years_education + 
                  country, data=dat, family = 'binomial', cluster=cl, method='REML')


mod24glm <- glm(gbv_year ~ spei24*urban + mean_annual_precip + mean_annual_tmax + 
                  wealth_factor_harmonized + hhsize + date_cmc + years_education + 
                  country, data=dat, family = 'binomial')

library(arm)

binnedplot(fitted(mod24gamgp)[dat$urban=='FALSE'],
           residuals(mod24gamgp, type='response')[dat$urban=='FALSE'], 
           nclass = NULL, 
           xlab = "Expected Values", 
           ylab = "Average residual", 
           main = "Binned residual plot", 
           cex.pts = 0.8, 
           col.pts = 1, 
           col.int = "gray")




mod24lm <- glm(gbv_year ~ spei24*empowered_gbv_notok + mean_annual_precip + mean_annual_tmax + 
                 wealth_factor_harmonized + hhsize + date_cmc + years_education + 
                 country, data=dat, family = 'binomial')


p <- plot(mod24gam)

gamdfa <- data.frame(spei24=p[[1]]$x, 
                     pred=p[[1]]$fit, 
                     se=p[[1]]$se,
                     model='Spline Model (GAM)',
                     empowered='Not Empowered')

gamdfb <- data.frame(spei24=p[[2]]$x, 
                     pred=p[[2]]$fit, 
                     se=p[[2]]$se,
                     model='Spline Model (GAM)',
                     empowered='Empowered')

s <- summary(mod24lm)

glmdfa <- data.frame(spei24=p[[1]]$x, 
                     pred=s$coefficients['spei24', 'Estimate']*p[[1]]$x,
                     se=s$coefficients['spei24', 'Std. Error']*p[[1]]$x,
                     model='Linear Model (GLM)',
                     empowered='Not Empowered')

glmdfb <- data.frame(spei24=p[[1]]$x, 
                     pred=(s$coefficients['spei24', 'Estimate'] + s$coefficients['spei24:empowered_gbv_notokTRUE', 'Estimate'])*p[[1]]$x,
                     #https://stats.stackexchange.com/questions/3653/adding-coefficients-to-obtain-interaction-effects-what-to-do-with-ses
                     se=sqrt(s$coefficients['spei24', 'Std. Error']^2 + s$coefficients['spei24:empowered_gbv_notokTRUE', 'Std. Error']^2 + 2*s$cov.unscaled['spei24', 'spei24:empowered_gbv_notokTRUE'])*p[[1]]$x,
                     model='Linear Model (GLM)',
                     empowered='Empowered')

pltdf <- Reduce(bind_rows, list(gamdfa, gamdfb, glmdfa, glmdfb)) %>%
  mutate(max=pred + se*2,
         min=pred - se*2)

convert <- function(x){
  odds <- exp(x)
  prob <- odds / (1 + odds)
  return(round(prob - 0.5, 2))
}

lines <- ggplot(pltdf %>% filter(model=='Spline Model (GAM)')) + 
  geom_ribbon(aes(x=spei24, ymin=min, ymax=max, fill=empowered), alpha=0.25) + 
  geom_line(aes(x=spei24, y=pred, color=empowered)) + 
  theme_minimal() +
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) + 
  scale_y_continuous(labels=convert) +
  ylab('Change in Probability of Experiencing GBV') +
  facet_wrap(empowered ~ ., ncol=2)

hist <- ggplot(dat) +
  geom_histogram(aes(x=spei24), binwidth=0.1) +
  theme_minimal() + 
  xlab('24-Month Standardized Precipitation-Evapotranspiration Index') +
  scale_x_continuous(breaks=c(-2, -1, 0, 1, 2)) + 
  scale_y_continuous(name = 'Count', breaks=c(0, 9000)) +
  xlim(min(pltdf$spei24), max(pltdf$spei24)) +
  facet_wrap(empowered ~ ., ncol=2) +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )


plot_grid(plotlist=list(lines, hist), align='v', ncol=1, nrow=2, rel_heights=c(1, 0.2)) +
  annotate("text", x=0.1, y=0.9, size=6, label='atop(bold("A"))', parse=T) +
  annotate("text", x=0.55, y=0.9, size=6, label='atop(bold("B"))', parse=T) +
  annotate("text", x=0.1, y=0.1, size=6, label='atop(bold("C"))', parse=T) +
  annotate("text", x=0.55, y=0.1, size=6, label='atop(bold("D"))', parse=T)

setwd('C://Users/matt/gbv-tex')
ggsave('Empowered_Regressions.pdf', width=8, height=6)
system("pdfcrop Empowered_Regressions.pdf Empowered_Regressions.pdf")
