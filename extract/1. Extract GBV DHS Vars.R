if (Sys.info()['sysname']=='Linux'){
  data_dir <- '/home/mattcoop/mortalityblob/dhsraw'
  
  meta_dir <- '/home/mattcoop/gbv/'
}

library(haven)
library(tidyverse)
library(foreign)
library(data.table)
library(foreach)
library(doParallel)

setwd(meta_dir)

gbv_vars <- read.csv('scope/gbv_codes.csv', stringsAsFactors = F)

files <- read.csv('scope/scoped_vars.csv', stringsAsFactors = F) %>%
  filter(!is.na(ge) & !is.na(v044) & !is.na(d105a)) %>%
  arrange(cc, num, subversion) %>%
  filter(!duplicated(paste0(cc, num, subversion), fromLast=T)) %>% #Important: remove old versions of same survey!!
  select(num, cc, subversion, ir, ge) %>%
  mutate(fname = paste0(cc, '-', subversion, '-', num, '.csv')) %>%
  filter(!fname %in% list.files('~/mortalityblob/gbv/individual_surveys/'))

ir_vars <- gbv_vars$label[gbv_vars$file=='IR']

setwd(data_dir)

cl <- makeCluster(detectCores(), outfile = '')
registerDoParallel(cl)

foreach(i=1:nrow(files), .packages=c('haven', 'tidyverse', 'foreign')) %dopar% {
  
  #Get Women's Data
  ir_dat <- read_dta(files$ir[i])
  
  print(paste0(round(i/nrow(files), 3)*100, '% on ', files$cc[i], '-', files$num[i], '-', files$subversion[i]))
  
  ir_dat_sel <- ir_dat[ , c(ir_vars[ir_vars %in% names(ir_dat)], "caseid", "v001", "v002", "v003", "v008")]
  for (n in c(ir_vars[ir_vars %in% names(ir_dat_sel)])){
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
  
  #Keep just women with DV module
  ir_dat_sel <- ir_dat_sel %>%
    filter(v044_int == 1)
  
  fname <- paste0('../gbv/individual_surveys/', files$fname[i])
  write.csv(ir_dat_sel, fname, row.names=F)
}

women <- list.files('../gbv/individual_surveys/', full.names = T) %>%
  lapply(read.csv) 

women <- women %>%
  rbindlist(fill=TRUE)

women <- women %>%
  mutate(country = substr(code, 1, 2),
         v008 = ifelse(country=='NP', v008 - 681, v008)) %>%
  filter(!(latitude < 1 & latitude > -1 & longitude < 1 & longitude > -1))

setwd('../gbv/')

write.csv(women, 'GBV_women_raw2.csv', row.names=F)

geo <- women %>% select(latitude, longitude, code, v008) %>%
  unique

write.csv(geo, 'GBV_geo.csv', row.names=F)

system('~/telegram.sh "Done with DHS Extraction"')



