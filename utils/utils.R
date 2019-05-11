######################################
#Prep for getting asset index from PCA
######################################
PCA_assets <- function(assets, ntiles){
  #https://academic.oup.com/heapol/article/21/6/459/612115/Constructing-socio-economic-status-indices-how-to
  
  #Drop NAs
  assets_narm <- na.omit(assets)
  
  #Pivot out factor columns
  for (n in names(assets_narm)){
    
    c <- class(assets_narm[ , n, drop=TRUE])
    if (c=='factor' | c=='character'){
      
      for (f in unique(assets_narm[ , n])){
        assets_narm[ , paste0(n, '_', f)] <- assets_narm[ , n] == f
      }
      
      assets_narm[ , n] <- NULL
      
    }
    
  }
  
  #Do PCA
  res <- prcomp(assets_narm)
  sumry <- summary(res)$importance[2, 1]*100
  
  cat('PC1 explains', sumry, 'percent of the variance')
  
  #get quantiles and cut vector
  out <- quantile(res$x[ , 1], probs=seq(0, 1, 1/ntiles))
  cuts <- cut(res$x[ , 1], out)
  
  #fill back in NAs based on row.names from original df
  temp <- data.frame(cuts=as.integer(cuts), rownames=row.names(assets_narm))
  
  assets$rownames <- row.names(assets)
  
  new <- merge(temp, assets, all=T)
  
  return(new$cuts)
}


#################################
#Prep for nutrition calculations
#################################
nutritionPrep <- function(){
  
  weianthro <<- read.table("WHO Anthro reference tables/weianthro.txt", header=T)
  lenanthro <<- read.table("WHO Anthro reference tables/lenanthro.txt", header=T)
  wflanthro <<- read.table("WHO Anthro reference tables/wflanthro.txt", header=T)
  wfhanthro <<- read.table("WHO Anthro reference tables/wfhanthro.txt", header=T)
  hcanthro <<- read.table("WHO Anthro reference tables/hcanthro.txt", header=T)
  acanthro <<- read.table("WHO Anthro reference tables/acanthro.txt", header=T)
  bmianthro <<- read.table("WHO Anthro reference tables/bmianthro.txt", header=T)
  ssanthro <<- read.table("WHO Anthro reference tables/ssanthro.txt", header=T)
  tsanthro <<- read.table("WHO Anthro reference tables/tsanthro.txt", header=T)
  source('C://Git/ftf/utils/igrowup_standard.r')
  
}

######################
#Rescale Vars
#####################
# rescale_many <- function(all, resc_vars){
#   rescale <- function(x){
#     (x - mean(x, na.rm=T))/max(x, na.rm=T)
#   }
#   for (r in resc_vars){
#     if (!r %in% names(all)){
#       cat(r, ' is missing, eedjit!')
#     }
#     all[ , paste0(r, '_orig')] <- all[ , r]
#     all[ , r] <- rescale(all[ , r])
#   }
#   all
# }
# 

###########################################################
#Write results for appendix
#######################################
library(reshape2)

cleanwrite <- function(mods, labeldf='regression_labels.csv'){
  #For example:
  #write.csv(cleanwrite(list(mod), labeldf), 'ZAM_stunting.csv', row.names=F)
  #write.csv(cleanwrite(list(mod2, mod3, mod4, mod5), labeldf), 'ZAM_hhs.csv', row.names=F)
  
  labeldf <- read.csv(labeldf)

  r2 <- function(mod){
    actual <- mod$residuals + mod$fitted.values
    fitted <- mod$fitted.values
    sstot <- sum((actual - mean(actual))^2)
    ssres <- sum((actual - fitted)^2)
    rsq <- 1 - ssres/sstot
    rsq
  }
  prep <- function(num){
    as.character(signif(num, 2))
  }
  extract <- function(mod){
    if (!is.null(summary(mod)$Coef)){
      coef <- summary(mod)$Coef %>% round(5) %>% data.frame
    } else{
      coef <- summary(mod)$coefficients %>% round(5) %>% data.frame
    }
    coef <- signif(coef, digits=2)
    coef$Parameter <- row.names(coef)
    coef$Std..Error <- paste0('(', coef$Std..Error, ')')
    p <- names(coef)[4]
    coef$Estimate <- ifelse(coef[ , 4] < 0.001, paste0(coef$Estimate, '***'),
                            ifelse(coef[ , 4] < 0.01, paste0( coef$Estimate, '**'),
                                   ifelse(coef[ , 4] < 0.05, paste0(coef$Estimate, '*'), 
                                          ifelse(coef[ , 4] < 0.1, paste0(coef$Estimate, '.'),
                                                 coef$Estimate))))
    coef[ , 4] <- NULL
    coef[ , 3] <- NULL
    res <- coef %>%
      melt(id.vars=names(coef)[3]) %>%
      mutate(Parameter = ifelse(variable=='Std..Error', paste0(Parameter, ' SE'), Parameter),
             variable=NULL) %>%
      arrange(Parameter)
    
    if (class(mod)=="sarlm"){
      res <- bind_rows(res, data.frame(Parameter='Rho', value=prep(mod$rho)))
    }
    
    res <- res %>%
      bind_rows(data.frame(Parameter='AIC', value=as.character(round(AIC(mod), 2)))) %>%
      bind_rows(data.frame(Parameter='R-Squared', value=prep(r2(mod))))
    res
  }
  
  all <- Reduce(function(x, y){merge(x, y, by='Parameter', all=T)}, Map(extract, mods))
  
  all <- merge(all, labeldf, by='Parameter', all.x=T, all.y=F)
  
  all <- all[order(all$rank), ]
  
  all
}
