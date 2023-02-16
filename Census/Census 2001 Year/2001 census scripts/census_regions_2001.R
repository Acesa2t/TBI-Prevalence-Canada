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
colnames(regions) <- c("iso3", "WHO_regions")
View(regions)

# I'm going to remove any regions in this dataset because they don't have iso3 codes
census_iso3 <- readRDS("TBI-Prevalence-Canada/Census/Census 2001 Year/census_iso3_2001.rds")
View(census_iso3)


census_iso3 <- census_iso3%>%
  filter(!is.na(ISO3))%>%
  filter(!COO%in%c("Caribbean and Bermuda", "Czech and Slovak Federal Republic, Former",  "China and Special Administrative Regions"))

#View(unique(census_iso3%>%select(COO, ISO3)))
#China and Special Administrative Regions
#Czech and Slovak Federal Republic, Former

# Make sure that ISO3s aren't duplicated
cen_test <- unique(census_iso3%>%select("COO", "ISO3"))
View(table(cen_test$ISO3))


View(census_iso3%>%
       summarise(tot_pop = sum(as.numeric(NUMP))))
#View(census_iso3)


# Prep the WHO region dataset for merging
# Data loss may occur as some ISO3s had not been recorded

unique(regions$WHO_regions)
colnames(regions) <- c("ISO3", "WHO_R")
View(regions)

# Which iso3s don't match up
iso3_vec <- anti_join(census_iso3, regions, by = "ISO3")
iso3_vec_2 <- unique(iso3_vec%>%select(COO, ISO3))
View(iso3_vec_2)

iso3_vec_2 <- iso3_vec_2%>%filter(ISO3%in%c("HKG","MAC"))%>%mutate(WHO_R = ifelse(ISO3 == "MAC", "WPRO",ifelse(ISO3 == "HKG", "WPRO","Not Classified")))%>%select(-COO)
View(iso3_vec_2)
    # Bind this new dataframe to the WHO regions data
regions <- rbind(regions, iso3_vec_2)
#View(regions)


unique(regions$WHO_R)
str(regions)
str(census_iso3)
#View(regions)



# Join the datasets
census_regions <- merge(census_iso3, regions, by = "ISO3")
#census_regions <- census_regions%>%select(-X1)
View(census_regions)


View(census_regions%>%
       summarise(tot_pop = sum(as.numeric(NUMP))))

unique(census_regions$COO)

saveRDS(census_regions, 
        file = "TBI-Prevalence-Canada/Census/Census 2001 Year/census_who_2001.rds")

# Next script census_afr.R