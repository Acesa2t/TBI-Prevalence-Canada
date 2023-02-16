# Prior script census_extension

# Can I get countrycode to work?
rm(list=ls()) 
library(readxl)
library(writexl)
library(janitor)
library(tidyverse)
library(countrycode)

# Read in Data

census_country <- read_xlsx("TBI-Prevalence-Canada/Census/Census 2016/census_1930to1980.xlsx")
#View(census_country)

# Add iso3 with some warnings and notifications of what doesn't match (i.e. I expected the regions not to match)
census_country <- census_country%>%
       mutate(iso3 = countrycode(
         census_country$COO, 
         origin = "country.name", 
         destination = "iso3c", 
         warn = TRUE, 
         nomatch = NA))

View(census_country%>%filter(iso3 == "USA"))
View(unique(census_country%>%select(COO,iso3)))

write_xlsx(census_country, 
           path = "TBI-Prevalence-Canada/Census/Census 2016/census_iso3added.xlsx")


# Next script regions.R
