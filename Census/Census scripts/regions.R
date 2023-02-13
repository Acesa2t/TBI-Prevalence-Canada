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


# I'm going to remove any regions in this dataset because they don't have iso3 codes
census_iso3 <- read_xlsx("3census_iso3added.xlsx")
census_iso3 <- census_iso3%>%
  filter(!is.na(iso3))

# Join the datasets
unique(regions$WHO_regions)
str(regions)
str(census_iso3)

census_regions <- merge(regions, census_iso3, by = "iso3")
a <- census_regions%>%
  filter(WHO_regions == "Not Classified")
View(unique(a$iso3))

write_xlsx(census_regions, path = "/Users/ajordan/OneDrive - McGill University/LTBI-Aust-CEA-master/Census/3census_whoadded.xlsx")

# Next script census_afr.R