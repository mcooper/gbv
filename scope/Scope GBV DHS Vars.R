setwd('G://My Drive/DHS New/')

library(haven)
library(tidyverse)

vars <- read.csv('C://Users/matt/gbv/scope/gbv_codes.csv')

##############################
#Scope Available Datasets
#################################

ir <- list.files(pattern='^..(IR|ir).....(DTA|dta)$')

makeFileNameDf <- function(f){
  num <- substr(f, 5, 5)
  cc <- toupper(substr(f, 1, 2))
  subversion <- ifelse(toupper(substr(f, 6, 6)) %in% as.character(seq(0, 9)), 1,
                       ifelse(toupper(substr(f, 6, 6)) %in% LETTERS[1:8], 2, 
                              ifelse(toupper(substr(f, 6, 6)) %in% LETTERS[9:17], 3, 
                                     ifelse(toupper(substr(f, 6, 6)) %in% LETTERS[18:26], 4))))
  data.frame(num, cc, subversion, file=f)
}

ir_df <- lapply(X = ir, FUN = makeFileNameDf) %>%
  Reduce(f = bind_rows) %>%
  rename(ir=file)

for (i in 1:nrow(ir_df)){
  dat <- read_dta(ir_df$ir[i])

  print(paste0(i, ' ', round(i/nrow(ir_df), 3)*100, '% on ', ir_df$cc[i], '-', ir_df$num[i], '-', ir_df$subversion[i]))

  for (var in vars$label){
    if (var %in% names(dat)){
      ir_df[i, var] <- TRUE
    }
  }
}

#Add Geographic Data
ge <- list.files(pattern='^..(GE|eg).....(SHP|shp)$')

makeFileNameDf <- function(f){
  num <- substr(f, 5, 5)
  cc <- toupper(substr(f, 1, 2))
  subversion <- ifelse(toupper(substr(f, 6, 6)) %in% as.character(seq(0, 9)), 1,
                       ifelse(toupper(substr(f, 6, 6)) %in% LETTERS[1:8], 2, 
                              ifelse(toupper(substr(f, 6, 6)) %in% LETTERS[9:17], 3, 
                                     ifelse(toupper(substr(f, 6, 6)) %in% LETTERS[18:26], 4))))
  data.frame(num, cc, subversion, file=f)
}

ge_df <- lapply(X = ge, FUN = makeFileNameDf) %>%
  Reduce(f = bind_rows) %>%
  rename(ge=file)

all <- merge(ir_df, ge_df, all.x=T, all.y=T)

write.csv(ir_df, 'C://Users/matt/gbv/scope/scoped_vars.csv', row.names=F)
