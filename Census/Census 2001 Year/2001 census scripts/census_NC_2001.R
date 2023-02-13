# Prior script census_afr

library(tidyverse)
library(janitor)

# Taking countries that are not classified to a certain WHO region and providing them with that classifcation

census_nc <- readRDS("census_afr_2001_v4.rds")
View(census_nc%>%
       summarise(tot_pop = sum(as.numeric(NUMP))))
head(census_nc)

country_list <- unique(census_nc%>%
              filter(WHO_R == "Not Classified" & !COO%in%c("Anguilla",
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
  mutate(WHO_R =
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
             recode(WHO_R, "Not Classified" = "AMRO"),
             ifelse(COO%in%
                      c("American Samoa",
                        "French Polynesia",
                        "Guam",
                        "New Caledonia",
                        "Tokelau",
                        "Wallis and Futuna"),
                    recode(WHO_R, "Not Classified" = "WPRO"),
                    ifelse(
                      COO == "West Bank and Gaza Strip (Palestine)"
                      , recode(WHO_R, "Not Classified" = "SEARO"),
                      ifelse(
                        COO%in%
                          c("Liechtenstein", 
                            "Gibraltar"),
                        recode(WHO_R, "Not Classified" = "EURO"),
                        recode(WHO_R, "Not Classified" = "Not Classified")
                        
                      )
                    ))
               )
)

census_classified%>%filter(COO%in%c("Liechtenstein", "Aruba", "New Caledonia"))

# Remove Caribbean and Bermuda

census_classified <- census_classified%>%
  filter(WHO_R != "Not Classified")

#View(census_classified%>%
#       filter(COO == "Caribbean and Bermuda"))

census_classified <- census_classified%>%
  mutate(census_year = 2001)

View(census_classified%>%
       summarise(tot_pop = sum(as.numeric(NUMP))))


View(census_classified)
write.csv(census_classified, file = "/Users/ajordan/OneDrive - McGill University/LTBI-Aust-CEA-master/Census/Census 2001 Year/census_classified_2001_v4.csv")

#saveRDS(census_classified, file = "/Users/ajordan/OneDrive - McGill University/LTBI-Aust-CEA-master/Census/Census 2011/2census2011_classified.rds")

# Duplicate csv and use for TBI prevalence script