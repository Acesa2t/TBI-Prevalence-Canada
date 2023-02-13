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
  filter(!is.na(iso3))%>%
  filter(COO != "Caribbean and Bermuda")

cen_test <- unique(census_iso3%>%select("COO", "iso3"))
View(cen_test)

# Join the datasets
unique(regions$WHO_regions)
str(regions)
str(census_iso3)

census_iso3%>%summarize(sum(NUMP))

iso3_vec <- anti_join(census_iso3, regions, by = "iso3")
iso3_vec_2 <- unique(iso3_vec%>%select(COO, iso3))
View(iso3_vec_2)



iso3_vec_2 <- iso3_vec_2%>%filter(iso3%in%c("SXM", "HKG", "MAC", "MNP"))%>%mutate(WHO_regions = ifelse(iso3 == "SXM", "AMRO", ifelse(iso3 == "HKG", "WPRO", ifelse(iso3 == "MAC", "WPRO", ifelse(iso3 == "MNP", "WPRO","Not Classified")))))%>%select(-COO)
View(iso3_vec_2)
View(regions)

# Bind this new dataframe to the WHO regions data
regions <- rbind(regions, iso3_vec_2)



census_regions <- merge(regions, census_iso3, by = "iso3")
a <- census_regions%>%
  filter(WHO_regions == "Not Classified")
View(unique(a$iso3))

census_regions%>%summarise(sum(NUMP))

write_xlsx(census_regions, path = "/Users/ajordan/OneDrive - McGill University/LTBI-Aust-CEA-master/Census/Census 2016/3census_whoadded_v2.xlsx")

# Next script census_afr.R