# Prior script census_extension copy
# Can I get countrycode to work?
rm(list=ls()) 

library(janitor)
library(tidyverse)
library(countrycode)

# Read in Data

census_country <- readRDS("2006_census_1910to1960.rds")
View(census_country%>%summarise(tot_pop = sum(as.numeric(NUMP))))

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
unique(census_country$iso3)
cen_na <- census_country%>%filter(is.na(iso3))
unique(cen_na$COO)

View(census_country)

View(census_country%>%
       summarise(tot_pop = sum(as.numeric(NUMP))))

saveRDS(census_country, 
        file = "/Users/ajordan/OneDrive - McGill University/LTBI-Aust-CEA-master/Census/Census 2006/2006_census_iso3.rds")


# Next script census_regions.R