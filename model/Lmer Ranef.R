library(lme4)
library(rdhs)
library(countrycode)
library(rnaturalearth)
library(sf)
library(ggplot2)

data <- read.csv('G://My Drive/DHS Processed/GBV_all_old.csv')

mod <- lmer(viol_phys_ip ~ spei24 + 
              wealth_factor_harmonized + hhsize + date_cmc + years_education + 
              (1|country) + (spei24|country),
            data=data)
coefs <- coef(mod)$country
coefs$cc <- row.names(coefs)

sp <- st_as_sf(ne_countries())

ids <- dhs_countries(returnFields=c("CountryName", "DHS_CountryCode")) %>%
  dplyr::select(cc=DHS_CountryCode, CountryName)
ids$iso_a2 <- countrycode(sourcevar = ids$CountryName, origin='country.name', destination = 'iso2c')

ids <- merge(ids, coefs)

spm <- merge(sp, ids)

ggplot(spm) + 
  geom_sf(aes(fill=spei24)) + 
  scale_fill_gradient2()
