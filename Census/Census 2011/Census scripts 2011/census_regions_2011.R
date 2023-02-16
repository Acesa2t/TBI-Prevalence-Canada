# Prior script census_country1
# Now can I add the WHO regions?

library(readxl)
library(writexl)
library(janitor)
library(tidyverse)

# Read in Data

regions <- read_xlsx("TBI-Prevalence-Canada/Census/jme_regional_classifications.xlsx")
regions <- regions%>%
  select("Iso3 code","WHO Region2")
regions
colnames(regions) <- c("iso3", "WHO_R")
View(regions)

# I'm going to remove any regions in this dataset because they don't have iso3 codes
census_iso <- readRDS("TBI-Prevalence-Canada/Census/Census 2011/census2011_iso3.rds")
View(census_iso%>%filter(YOBP <= YARP))
census_iso3 <- census_iso%>%
  filter(!is.na(iso3))%>%
  filter(COO != "Caribbean and Bermuda")


# Make sure that ISO3s aren't duplicated
cen_test <- unique(census_iso3%>%select("COO", "iso3"))
View(table(cen_test$iso3))
View(cen_test)


View(census_iso3%>%
       summarise(tot_pop = sum(as.numeric(NUMP))))


# Which iso3s don't match up
iso3_vec <- anti_join(census_iso3, regions, by = "iso3")
iso3_vec_2 <- unique(iso3_vec%>%select(COO, iso3))
View(iso3_vec_2)

iso3_vec_2 <- iso3_vec_2%>%
  filter(iso3%in%c("SXM","SPM","TWN","FRO", "GGY", "IMN", "JEY", "HKG", "MAC"))%>%
  mutate(WHO_R = ifelse(iso3 == "SXM" | iso3 == "SPM", "AMRO", 
                               ifelse(iso3=="FRO" | iso3=="GGY" | iso3=="IMN" | iso3=="JEY",  "EURO",
                               ifelse(iso3=="TWN" | iso3=="HKG" | iso3=="MAC", "WPRO","Not Classified"))))%>%select(-COO)

View(iso3_vec_2)
View(regions)

# Bind this new dataframe to the WHO regions data
regions <- rbind(regions, iso3_vec_2)

# Join the datasets
#unique(regions$WHO_regions)
str(regions)
str(census_iso3)

census_regions <- merge(regions, census_iso3, by = "iso3")

# Let's me know which regions are not classified. This will later be addressed in census_NC
# a <- census_regions%>%
#   filter(WHO_regions == "Not Classified")
# View(unique(a%>%select(COO,iso3)))


View(census_regions%>%
       summarise(tot_pop = sum(as.numeric(NUMP))))

saveRDS(census_regions, 
        file = "TBI-Prevalence-Canada/Census/Census 2011/census2011_who.rds")

# Next script census_afr.R