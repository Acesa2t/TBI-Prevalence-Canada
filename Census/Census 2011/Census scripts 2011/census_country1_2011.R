# Prior script census_extension copy
# Can I get countrycode to work?
rm(list=ls()) 

library(janitor)
library(tidyverse)
library(countrycode)

# Read in Data

census_country <- readRDS("TBI-Prevalence-Canada/Census/Census 2011/census_1930to1970.rds")
#View(census_country)
census_country%>%summarise(tot_pop = sum(as.numeric(NUMP)))

# Add iso3 with some warnings and notifications of what doesn't match (i.e. I expected the regions not to match)
census_country <- census_country%>%
       mutate(iso3 = countrycode(
         census_country$COO, 
         origin = "country.name", 
         destination = "iso3c", 
         warn = TRUE, 
         nomatch = NA))

# Checks
unique(census_country%>%filter(iso3 == "USA"))
View(unique(census_country%>%select(COO,iso3)))
cen_na <- census_country%>%filter(is.na(iso3))
unique(cen_na$COO)

View(census_country)

View(census_country%>%
       summarise(tot_pop = sum(as.numeric(NUMP))))

length(unique(census_country$iso3))
saveRDS(census_country, 
        file = "TBI-Prevalence-Canada/Census/Census 2011/census2011_iso3.rds")


# Next script census_regions.R