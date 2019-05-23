setwd('G://My Drive/DHS New/')

library(haven)
library(tidyverse)

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

#Follow guide here: https://dhsprogram.com/pubs/pdf/DHSG1/Guide_to_DHS_Statistics_DHS-7.pdf
#Section 17.3
gbv_vars <- c('d105a', 'd105b', 'd105c', 'd105d', 'd105e', 'd105f', 'd105g', 'd105j', 'd115y', 'd117a', 'd118y', 'd130a', 'd005')

alldata <- data.frame()
for (i in 1:nrow(ir_df)){
  dat <- read_dta(ir_df$ir[i])
  
  print(paste0(round(i/nrow(ir_df), 3)*100, '% on ', ir_df$cc[i], '-', ir_df$num[i], '-', ir_df$subversion[i]))
  
  if (!any(gbv_vars %in% names(dat))){
    next
  } else{
    print("Has Domestic Violence Module")
  }
  
  dat_sel <- dat[ , c(gbv_vars[gbv_vars %in% names(dat)], 'v044', "v001", "v000", "v002", "v009", "v006", "v007", "v008", "v005")]
  
  for (n in names(dat_sel)){
    if (class(dat_sel[ , n, drop=TRUE])=='haven_labelled'){
      dat_sel[ , paste0(n, '_chr')] <- as.character(as_factor(dat_sel[ , n, drop=TRUE]))
      dat_sel[ , paste0(n, '_int')] <- as.numeric(dat_sel[ , n, drop=TRUE])
      dat_sel[ , n] <- NULL
    }
  }
  
  dat_sel$cc <- ir_df$cc[i]
  dat_sel$subversion <- ir_df$subversion[i]
  dat_sel$num <- ir_df$num[i]
  
  alldata <- bind_rows(alldata, dat_sel)
  
}

write.csv(alldata, 'G://My Drive/GBV/gbv_dhs_extract.csv', row.names=F)

sel <- alldata %>% filter(v044_int==1)

#interview month
sel$interview_month <- rowSums(sel[ , c('v006', 'v006_int')], na.rm=T)

#interview year
sel$interview_year <- rowSums(sel[ , c('v007', 'v007_int')], na.rm=T)
sel$interview_year[which(sel$interview_year > 2020 & sel$interview_month %in% seq(1, 9))] <- sel$interview_year[which(sel$interview_year > 2020 & sel$interview_month %in% seq(1, 9))] - 57
sel$interview_year[which(sel$interview_year > 2020 & sel$interview_month %in% seq(10, 12))] <- sel$interview_year[which(sel$interview_year > 2020 & sel$interview_month %in% seq(10, 12))] - 56

#make codes
sel$surveycode <- paste0(sel$cc, '-', sel$num, '-', sel$subversion)
sel$code <- paste0(sel$surveycode, '-', sel$v001)

sel <- sel %>%
  mutate(d105a_bool= (d105a_int==1 | d105a_int==2),
         d105b_bool= (d105b_int==1 | d105b_int==2),
         d105c_bool= (d105c_int==1 | d105c_int==2),
         d105d_bool= (d105d_int==1 | d105d_int==2),
         d105e_bool= (d105e_int==1 | d105e_int==2),
         d105f_bool= (d105f_int==1 | d105f_int==2),
         d105g_bool= (d105g_int==1 | d105g_int==2),
         d105j_bool= (d105j_int==1 | d105j_int==2),
         d117a_bool= (d117a_int==1 | d117a_int==2))

sel$any_violence = rowSums(sel[ , c('d105a_bool', 'd105b_bool', 'd105c_bool', 'd105d_bool', 'd105e_bool', 
                                    'd105f_bool', 'd105g_bool', 'd105j_bool', 'd117a_bool')], na.rm=T) > 0

sel$violence_12months <- sel$d117a_bool

sel <- sel %>%
  select(country=v000, surveycode, code, interview_month, interview_year, any_violence, violence_12months)

write.csv(sel, 'G://My Drive/GBV/gbv_dhs_clean.csv', row.names=F)
