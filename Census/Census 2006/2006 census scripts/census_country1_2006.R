# Prior script census_dist
# Can I get countrycode to work?
rm(list=ls()) 

library(janitor)
library(tidyverse)
library(countrycode)
library(readxl)

# Read in Data

census_country <- readRDS("census_distributed_2006.rds")
census_country <- census_country%>%ungroup

unique(census_country$COO)

# Add iso3 with some warnings and notifications of what doesn't match (i.e. I expected the regions not to match)
census_country <- census_country%>%
       mutate(ISO3 = countrycode(
         census_country$COO, 
         origin = "country.name", 
         destination = "iso3c", 
         warn = TRUE, 
         nomatch = NA))

# Checks
unique(census_country%>%filter(ISO3 == "USA"))
unique(census_country$COO)
cen_na <- census_country%>%filter(is.na(ISO3))
unique(cen_na$COO)


View(census_country%>%arrange(YARP)%>%select(COO, ISO3))

View(census_country%>%
       summarise(tot_pop = sum(as.numeric(NUMP))))

saveRDS(census_country, 
        file = "/Users/ajordan/OneDrive - McGill University/LTBI-Aust-CEA-master/Census/Census 2006/census_iso3_2006.rds")


cen_tot<-census_country%>%filter(is.na(ISO3)==F)
sum(cen_tot$NUMP)

aa<-cen_tot%>%group_by(ISO3,YARP,AGEP)%>%summarise(N=n(),tot=sum(NUMP))
bb<-aa%>%group_by(ISO3,N)%>%summarise(g=first(N))


# Next script census_regions.R