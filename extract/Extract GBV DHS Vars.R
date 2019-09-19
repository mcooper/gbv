setwd('~/mortalityblob/dhsraw')

library(haven)
library(tidyverse)
library(foreign)

gbv_vars <- read.csv('../dhs/gbv_codes.csv', stringsAsFactors = F)

files <- read.csv('../dhs/scoped_vars.csv', stringsAsFactors = F) %>%
  filter(!is.na(ge) & !is.na(v044)) %>%
  select(num, cc, subversion, ir, ge)

ir_vars <- gbv_vars$label[gbv_vars$file=='IR']

women <- data.frame()
for (i in 1:nrow(files)){
  
  #Get Women's Data
  ir_dat <- read_dta(files$ir[i])
  
  print(paste0(round(i/nrow(files), 3)*100, '% on ', files$cc[i], '-', files$num[i], '-', files$subversion[i]))
  
  ir_dat_sel <- ir_dat[ , c(ir_vars[ir_vars %in% names(ir_dat)], "v001", "v002", "v003", "v034", "v006", "v008", "v106", "v107")]
  for (n in c(ir_vars[ir_vars %in% names(ir_dat_sel)], "v106", "v107")){
    if (class(ir_dat_sel[ , n, drop=TRUE])=='haven_labelled'){
      ir_dat_sel[ , paste0(n, '_chr')] <- as.character(as_factor(ir_dat_sel[ , n, drop=TRUE]))
      ir_dat_sel[ , paste0(n, '_int')] <- as.numeric(ir_dat_sel[ , n, drop=TRUE])
      ir_dat_sel[ , n] <- NULL
    }
  }
  ir_dat_sel$cc <- files$cc[i]
  ir_dat_sel$subversion <- files$subversion[i]
  ir_dat_sel$num <- files$num[i]
  ir_dat_sel$code <- paste0(ir_dat_sel$cc, '-', ir_dat_sel$num, '-', ir_dat_sel$subversion, '-', ir_dat_sel$v001)
  ir_dat_sel$hh_code <- paste0(ir_dat_sel$code, '-', ir_dat_sel$v002)

  #Now get spatial data
  spheadervars <- c('DHSCLUST', 'LATNUM', 'LONGNUM')
  
  spdat <- read.dbf(gsub('.shp', '.dbf', files$ge[i]), as.is=TRUE)
  if (!all(spheadervars %in% names(spdat))){
    cat(files$ge[i], 'is missing necessary column names\n')
  }else{
    spdat <- spdat[ , spheadervars]
    spdat$num <- substr(files$ge[i], 5, 5)
    spdat$cc <- toupper(substr(files$ge[i], 1, 2))
    spdat$subversion <- ifelse(toupper(substr(files$ge[i], 6, 6)) %in% as.character(seq(0, 9)), 1,
                               ifelse(toupper(substr(files$ge[i], 6, 6)) %in% LETTERS[1:8], 2,
                                      ifelse(toupper(substr(files$ge[i], 6, 6)) %in% LETTERS[9:17], 3,
                                             ifelse(toupper(substr(files$ge[i], 6, 6)) %in% LETTERS[18:26], 4, 99))))
    spdat$code <- paste(spdat$cc, spdat$num, spdat$subversion, spdat$DHSCLUST, sep='-')
    spdat <- spdat %>%
      select(latitude=LATNUM, longitude=LONGNUM, code=code)
  }
  
  initialsize <- nrow(ir_dat_sel)
  ir_dat_sel <- merge(ir_dat_sel, spdat, all.x=T, all.y=F)
  
  if (initialsize != nrow(ir_dat_sel)){
    cat("Mismatches in Spatial and Womens data.  Initial size:", initialsize, " now:", nrow(ir_sel), '\n')
  }
  
  women <- bind_rows(women, ir_dat_sel)

}

setwd('../dhs/')

women <- women %>%
  mutate(country = substr(code, 1, 2),
         v008 = ifelse(country=='NP', v008 - 681, v008))

write.csv(women, 'GBV_women_raw.csv', row.names=F)

geo <- women %>% select(latitude, longitude, code, v008) %>%
  unique

write.csv(geo, 'GBV_geo.csv', row.names=F)

system('~/telegram.sh "Done with DHS Extraction"')



