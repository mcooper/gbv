setwd('G://My Drive/DHS Processed')

library(dplyr)
library(ordinal)
library(texreg)

gbv <- read.csv('GBV_all.csv')

gbv$gbv_year <- factor(gbv$gbv_year, levels = c('never', 'sometimes', 'often'))

log(mean(gbv$gbv_year=='never')/mean(gbv$gbv_year!='never'))
log(mean(gbv$gbv_year %in% c('never', 'sometimes'))/mean(gbv$gbv_year!='never'))

gbv$surveycode <- substr(gbv$code, 1, 6)

gbv$survey_year <- floor(gbv$date_cmc/12) + 1900

#############################
#Temperature + SPEI
#############################
#Base level model
mod_spi <- clmm(gbv_year ~ temp12monthZ + spei24 + 
              wealth_factor_harmonized + hhsize + 
              (1|country) + (1|surveycode), data=gbv, Hess = TRUE)
summary(mod_spi)
save(mod_spi, file='mod_spi.Rdata')

#Model for empowered with decision-making
mod_spi_decisions <- clmm(gbv_year ~ temp12monthZ*empowered_decisions + spei24*empowered_decisions + 
                      wealth_factor_harmonized + hhsize + 
                        (1|country) + (1|surveycode), data=gbv, Hess = TRUE)
summary(mod_spi_decisions)
save(mod_spi_decisions, file='mod_spi_decisions.Rdata')

#Model for empowered by disbelieving in GBV being OK
mod_spi_w_gbv_notok <- clmm(gbv_year ~ temp12monthZ*empowered_gbv_notok + spei24*empowered_gbv_notok + 
                        wealth_factor_harmonized + hhsize + 
                          (1|country) + (1|surveycode), data=gbv, Hess = TRUE)
summary(mod_spi_w_gbv_notok)
save(mod_spi_w_gbv_notok, file='mod_spi_w_gbv_notok.Rdata')

#Model for empowered by WEAI
gbv$empowered_WEAI <- gbv$WEAI > 0.9
mod_spi_WEAI <- clmm(gbv_year ~ temp12monthZ*empowered_WEAI + spei24*empowered_WEAI + 
                 wealth_factor_harmonized + hhsize + 
                   (1|country) + (1|surveycode), data=gbv, Hess = TRUE)
summary(mod_spi_WEAI)
save(mod_spi_WEAI, file='mod_spi_WEAI.Rdata')

#####################################
#Format output
#####################################
setwd('G://My Drive/GBV')

load('mod_spi.Rdata')
load('mod_spi_decisions.Rdata')
load('mod_spi_w_gbv_notok.Rdata')
load('mod_spi_WEAI.Rdata')


# extension for clmm objects (ordinal package)
extract.clmm <- function(model, include.thresholds = TRUE,
                         include.loglik = TRUE, include.aic = TRUE,  include.bic = TRUE,
                         include.nobs = TRUE, include.groups = TRUE, include.variance = TRUE, ...) {
  s <- summary(model, ...)
  
  tab <- s$coefficients
  thresh <- tab[rownames(tab) %in% names(s$alpha), ]
  threshold.names <- rownames(thresh)
  threshold.coef <- thresh[, 1]
  threshold.se <- thresh[, 2]
  threshold.pval <- thresh[, 4]
  beta <- tab[rownames(tab) %in% names(s$beta), ]
  beta.names <- rownames(beta)
  beta.coef <- beta[, 1]
  beta.se <- beta[, 2]
  beta.pval <- beta[, 4]
  
  if (include.thresholds == TRUE) {
    cfnames <- c(beta.names, threshold.names)
    coef <- c(beta.coef, threshold.coef)
    se <- c(beta.se, threshold.se)
    pval <- c(beta.pval, threshold.pval)
  } else {
    cfnames <- beta.names
    coef <- beta.coef
    se <- beta.se
    pval <- beta.pval
  }
  
  gof <- numeric()
  gof.names <- character()
  gof.decimal <- logical()
  if (include.loglik == TRUE) {
    lik <- logLik(model)[1]
    gof <- c(gof, lik)
    gof.names <- c(gof.names, "Log Likelihood")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.aic == TRUE) {
    aic <- AIC(model)
    gof <- c(gof, aic)
    gof.names <- c(gof.names, "AIC")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.bic == TRUE) {
    bic <- BIC(model)
    gof <- c(gof, bic)
    gof.names <- c(gof.names, "BIC")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.nobs == TRUE) {
    n <- nobs(model)
    gof <- c(gof, n)
    gof.names <- c(gof.names, "Num.\ obs.")
    gof.decimal <- c(gof.decimal, FALSE)
  }
  if (include.groups == TRUE) {
    grp <- s$dims$nlev.gf
    grp.names <- paste0("Groups (", names(grp), ")")
    gof <- c(gof, grp)
    gof.names <- c(gof.names, grp.names)
    gof.decimal <- c(gof.decimal, rep(FALSE, length(grp)))
  }
  if (include.variance == TRUE) {
    var.names <- character()
    var.values <- numeric()
    for (i in 1:length(s$ST)) {
      variances <- diag(s$ST[[i]] %*% t(s$ST[[i]]))
      var.names <- c(var.names, paste0("Variance: ", names(s$ST)[[i]], ": ", 
                                       names(variances)))
      var.values <- c(var.values, variances)
    }
    gof <- c(gof, var.values)
    gof.names <- c(gof.names, var.names)
    gof.decimal <- c(gof.decimal, rep(TRUE, length(var.values)))
  }
  
  tr <- createTexreg(
    coef.names = cfnames, 
    coef = coef, 
    se = se, 
    pvalues = pval, 
    gof.names = gof.names, 
    gof = gof, 
    gof.decimal = gof.decimal
  )
  return(tr)
}

setMethod("extract", signature = className("clmm", "ordinal"), 
          definition = extract.clmm)

texreg(list(mod_spi, mod_spi_decisions, mod_spi_w_gbv_notok, mod_spi_WEAI), 
       single.row = TRUE,
       file='C://Users/matt/gbv-tex/Mod_Table.tex',
       custom.model.names=c('Baseline Model',
                            'Emp. in Dec.',
                            'Emp. in GBV Att.',
                            'Emp. in Ag.'),
       custom.coef.names=c('12-Month Temperature Z-Score',
                           '24-Month SPEI',
                           'Household Wealth',
                           'Household Size',
                           '``Never" - ``Sometimes"',
                           '``Sometimes" - ``Often"',
                           'Empowered in Decisions',
                           'Temp Z Score * Emp. in Desc.',
                           'SPEI * Emp. in Desc.',
                           'Empowered in GBV Attitudes',
                           'Temp Z Score * Emp. in GBV Att.',
                           'SPEI * Emp. in GBV Att.',
                           'Empowered in Agriculture',
                           'Temp Z Score * Emp. in Ag.',
                           'SPEI * Emp. in Ag.'),
       caption = 'Comparison of results of four Ordinal-Logit models',
       float.pos = 'H'
       )




stg <- stargazer(mod_spi, mod_spi_decisions, mod_spi_w_gbv_notok, mod_spi_WEAI,
                 title="Comparison of Models",
                 out = 'C://Users/matt/gbv-tex/Mod_Table.text',
                 column.labels=c("Baseline Model", "Empowerment in Decisions", 
                                 "Empowerment in GBV Perceptions", "Empowerment in Agriculture"),
                 dep.var.labels.include=FALSE,
                 dep.var.caption='',
                 add.lines=list(c("Log Likelihood", mod_spi$logLik, mod_spi_decisions$logLik, mod_spi_w_gbv_notok$logLik, mod_spi_WEAI$logLik),
                                c("n", length(mod_spi$fitted.values), length(mod_spi_decisions$fitted.values), length(mod_spi_w_gbv_notok$fitted.values), length(mod_spi_WEAI$fitted.values))),
                 covariate.labels=c("12-Month Mean High Temperature Z-Score",
                                    "24-Month SPEI", 
                                    "Harmonized Wealth Factor",
                                    "Household Size"))

#Need to make manual edits to table based on this:
#https://tex.stackexchange.com/questions/424435/help-with-long-table-from-stargazer

stg <- stg[c(1, 2, 3, 7, 5, 6, seq(8, (length(stg) - 1)))]
stg <- gsub('tabular', 'longtable', stg)
stg <- paste0(stg, collapse='\n')
cat(stg, file = 'G://My Drive/Papers/SPEI-Malnutrition/SPEI-MalnutritionTex/tables/S1.tex')

