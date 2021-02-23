library(haven)
library(tidyverse)

setwd('~/mortalityblob/dhsraw')

vars <- read.csv('../dhs/gbv_codes.csv')

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

bad <- NULL

for (i in 1:nrow(ir_df)){
  print(paste0(i, ' ', round(i/nrow(ir_df), 3)*100, '% on ', ir_df$cc[i], '-', ir_df$num[i], '-', ir_df$subversion[i]))

  tryCatch({
    dat <- read_dta(ir_df$ir[i], n_max=10)
  
    for (var in vars$label[vars$file=="IR"]){
      if (var %in% names(dat)){
        ir_df[i, var] <- TRUE
      }
    }
  }, error=function(e){
    bad <- c(bad, ir_df$ir[i])
  })
}

#Add Geographic Data
ge <- list.files(pattern='^..(GE|eg).....(SHP|shp)$')

ge_df <- lapply(X = ge, FUN = makeFileNameDf) %>%
  Reduce(f = bind_rows) %>%
  rename(ge=file)

#Combine
all <- Reduce(function(x, y){merge(x, y, all.x=T, all.y=T)},
              x=list(ir_df, ge_df))

write.csv(all, '~/gbv/scope/scoped_vars.csv', row.names=F)

system('~/telegram.sh "Done with Scoping"')


