library(INLA)
library(tidyverse)
library(sp)
library(fields)
library(geoR)

#From: https://haakonbakka.bitbucket.io/btopic108.html

data <- read.csv('~/mortalityblob/gbv/GBV_sel.csv') %>%
  filter(country=='KE')

#Create Mesh and A Matrix
max.edge <- 0.5
mesh <- inla.mesh.2d(
  loc=data[ , c('longitude', 'latitude')],
  offset=c(0.5, 1.5),
  max.edge=c(max.edge, max.edge*3),
  cutoff=max.edge/5
)

A <- inla.spde.make.A(mesh=mesh,
                      loc=data.matrix(data[ , c('longitude', 'latitude')]))

#Make the stack
Xcov <- model.matrix( ~ plos_age + woman_literate + is_married + plos_births + plos_hhsize + 
                    plos_rural + husband_education_level + plos_husband_age + drought_cat,
                  data.frame(
                  plos_age=data$plos_age,
                  woman_literate=data$woman_literate,
                  is_married=data$is_married,
                  plos_births=data$plos_births,
                  plos_hhsize=data$plos_hhsize,
                  plos_rural=data$plos_rural,
                  husband_education_level=data$husband_education_level,
                  plos_husband_age=data$plos_husband_age,
                  drought_cat=data$drought_cat
                  ))


stack <- inla.stack(tag='est',
                    data=list(y=as.numeric(data$viol_phys)),
                    effects=list(s=1:mesh$n,
                                 Xcov=Xcov),
                    A=list(A, 1))


#Make the model
prior.median.sd = 1; prior.median.range = 7
spde = inla.spde2.pcmatern(mesh, prior.range = c(prior.median.range, .5), prior.sigma = c(prior.median.sd, .5), constr = T)

formula = y ~ -1 + Xcov + f(s, model=spde)

prior.median.gaus.sd = 5.5
family = 'binomial'
control.family = list(hyper = list(prec = list(
  prior = "pc.prec", fixed = FALSE, param = c(prior.median.gaus.sd,0.5))))

res <- inla(formula, data=inla.stack.data(stack),
            control.predictor=list(A = inla.stack.A(stack), compute=T),
            # compute=T to get posterior for fitted values
            family = family,
            #control.family = control.family,
            #control.compute = list(config=T, dic=T, cpo=T, waic=T), 
            # - Model comparisons
            control.inla = list(int.strategy='eb'),
            # - faster computation
            #control.inla = list(int.strategy='grid'),
            # - More accurate integration over hyper-parameters
            verbose=F)



















