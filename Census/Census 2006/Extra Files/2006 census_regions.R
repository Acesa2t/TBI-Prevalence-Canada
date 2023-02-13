# Prior script census_country1
# Now can I add the WHO regions?

library(readxl)
library(writexl)
library(janitor)
library(tidyverse)

# Read in Data

regions <- read_xlsx("jme_regional_classifications.xlsx")
regions <- regions%>%
  select("Iso3 code","WHO Region2")
regions
colnames(regions) <- c("iso3", "WHO_regions")
View(regions)

# I'm going to remove any regions in this dataset because they don't have iso3 codes
census_iso3 <- readRDS("2006_census_iso3.rds")
View(census_iso3)
census_iso3 <- census_iso3%>%
  filter(!is.na(iso3))

View(census_iso3)

# Join the datasets
unique(regions$WHO_regions)
str(regions)
str(census_iso3)

census_regions <- merge(regions, census_iso3, by = "iso3")

# Let's me know which regions are not classified. This will later be addressed in census_NC
a <- census_regions%>%
  filter(WHO_regions == "Not Classified")
View(unique(a%>%select(COO,iso3)))


View(census_regions%>%
       summarise(tot_pop = sum(as.numeric(NUMP))))

saveRDS(census_regions, 
        file = "/Users/ajordan/OneDrive - McGill University/LTBI-Aust-CEA-master/Census/Census 2006/2006_census_who.rds")

# Next script census_afr.R