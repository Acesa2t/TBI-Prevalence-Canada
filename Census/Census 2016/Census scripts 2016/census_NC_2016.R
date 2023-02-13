# Prior script census_afr

library(tidyverse)
library(readxl)
library(writexl)
library(janitor)

# Taking countries that are not classified to a certain WHO region and providing them with that classifcation

census_nc <- read_xlsx("3census_afr_v2.xlsx")
head(census_nc)

country_list <- unique(census_nc%>%
              filter(WHO_regions == "Not Classified" & !COO%in%c("Anguilla",
                                                                               "Antigua and Barbuda",
                                                                               "Aruba",
                                                                               "Bermuda",
                                                                               "Cayman Islands",
                                                                               "Virgin Islands, British",
                                                                               "Puerto Rico",
                                                                               "Guadeloupe",
                                                                               "Martinique",
                                                                               "Montserrat",
                                                                               "Turks and Caicos Islands","American Samoa",
                                                                               "French Polynesia",
                                                                               "Guam",
                                                                               "New Caledonia",
                                                                               "Tokelau",
                                                                               "Wallis and Futuna"))%>%
              select(COO))


View(country_list)

dput(as.character(country_list))

census_nc%>%filter(COO == "Brazil")



census_classified <- census_nc%>%
  mutate(WHO_regions =
           ifelse(
             COO%in%
               c("Anguilla",
                 "Antigua and Barbuda",
                 "Aruba",
                 "Bermuda",
                 "Cayman Islands",
                 "Virgin Islands, British",
                 "Puerto Rico",
                 "Guadeloupe",
                 "Martinique",
                 "Montserrat",
                 "Turks and Caicos Islands",
                 "Greenland",
                 "Falkland Islands (Malvinas)",
                 "French Guiana"),
             recode(WHO_regions, "Not Classified" = "AMRO"),
             ifelse(COO%in%
                      c("American Samoa",
                        "French Polynesia",
                        "Guam",
                        "New Caledonia",
                        "Tokelau",
                        "Wallis and Futuna"),
                    recode(WHO_regions, "Not Classified" = "WPRO"),
                    ifelse(
                      COO == "West Bank and Gaza Strip (Palestine)"
                      , recode(WHO_regions, "Not Classified" = "SEARO"),
                      ifelse(
                        COO%in%
                          c("Liechtenstein", 
                            "Gibraltar"),
                        recode(WHO_regions, "Not Classified" = "EURO"),
                        recode(WHO_regions, "Not Classified" = "Not Classified")
                        
                      )
                    ))
               )
)

census_classified%>%filter(COO%in%c("Liechtenstein", "Aruba", "New Caledonia"))

# Remove Caribbean and Bermuda

census_classified <- census_classified%>%
  filter(WHO_regions != "Not Classified")

#View(census_classified%>%
#       filter(COO == "Caribbean and Bermuda"))

census_classified <- census_classified%>%
  mutate(census_year = 2016)


View(census_classified%>%
       summarise(tot_pop = sum(as.numeric(NUMP))))

#write_xlsx(census_classified, path = "/Users/ajordan/OneDrive - McGill University/A TB/Analyses/TB prev measures/Census/census_classified.xlsx")

write.csv(census_classified, file = "/Users/ajordan/OneDrive - McGill University/LTBI-Aust-CEA-master/Census/Census 2016/2016_census_classified_v2.csv", row.names = FALSE)

# Duplicate csv and use for TBI prevalence script