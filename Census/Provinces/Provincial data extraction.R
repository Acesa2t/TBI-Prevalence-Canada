# This script extracts data for specific provinces

all_census_data <- read.csv("/Users/ajordan/Downloads/98-400-X2016184_ENG_CSV/98-400-X2016184_English_CSV_data.csv")

#View(all_census_data)

# Select strata, must select all for sex or will get three of each values and won't be able to properly pivot without creating a list
extract_big4 <- all_census_data%>%
   filter(CENSUS_YEAR=="2016" & 
            GEO_NAME%in%c("Ontario", 
                          "Alberta", 
                          "Quebec", 
                          "British Columbia") & 
            DIM..Immigrant.status.and.period.of.immigration..11. %in%c("Before 1981",
                                                                       "1981 to 1990",
                                                                       "1991 to 2000",
                                                                       "2001 to 2010",
                                                                       "2011 to 2016") &
            DIM..Age..12.%in%c("0 to 14 years",
                               "15 to 24 years",
                               "25 to 54 years",
                               "55 to 64 years",
                               "65 years and over") &
            DIM..Sex..3.=="Total - Sex")
View(extract_big4)


library(writexl)
# Big 4

write_xlsx(extract_big4, "/Users/ajordan/Library/CloudStorage/OneDrive-McGillUniversity/LTBI-Aust-CEA-master/Census/Provinces/all_provincial_strata_v2.xlsx")
 
 pre_province <- extract_big4%>%
   select(CENSUS_YEAR, GEO_NAME, DIM..Place.of.birth..272., DIM..Immigrant.status.and.period.of.immigration..11., DIM..Age..12., Dim..Citizenship..5...Member.ID...1...Total...Citizenship..Note..29.)%>%
   pivot_wider(names_from = DIM..Immigrant.status.and.period.of.immigration..11., values_from = Dim..Citizenship..5...Member.ID...1...Total...Citizenship..Note..29.)
 
View(pre_province)
 
# Alberta
 
write_xlsx(pre_province%>%filter(GEO_NAME=="Alberta"), "/Users/ajordan/Library/CloudStorage/OneDrive-McGillUniversity/LTBI-Aust-CEA-master/Census/Provinces/Alberta/alberta_strata.xlsx")
 
 
# British Columbia
write_xlsx(pre_province%>%filter(GEO_NAME=="British Columbia"), "/Users/ajordan/Library/CloudStorage/OneDrive-McGillUniversity/LTBI-Aust-CEA-master/Census/Provinces/British Columbia/bc_strata.xlsx")
 
# Ontario
write_xlsx(pre_province%>%filter(GEO_NAME=="Ontario"), "/Users/ajordan/Library/CloudStorage/OneDrive-McGillUniversity/LTBI-Aust-CEA-master/Census/Provinces/Ontario/ontario_strata.xlsx")
 
# Quebec
write_xlsx(pre_province%>%filter(GEO_NAME=="Quebec"), "/Users/ajordan/Library/CloudStorage/OneDrive-McGillUniversity/LTBI-Aust-CEA-master/Census/Provinces/Quebec/quebec_strata.xlsx")

# Other provinces
pre_other_province <- aa%>%
  filter(CENSUS_YEAR=="2016" & 
           GEO_NAME%in%c("Nova Scotia", 
                         "Prince Edward Island", 
                         "Manitoba", 
                         "New Brunswick",
                         "Saskatchewan",
                         "Newfoundland and Labrador",
                         "Northwest Territories",
                         "Yukon",
                         "Nunavut") & 
           DIM..Immigrant.status.and.period.of.immigration..11. %in%c("Before 1981",
                                                                      "1981 to 1990",
                                                                      "1991 to 2000",
                                                                      "2001 to 2010",
                                                                      "2011 to 2016") &
           DIM..Age..12.%in%c("0 to 14 years",
                              "15 to 24 years",
                              "25 to 54 years",
                              "55 to 64 years",
                              "65 years and over") &
           DIM..Sex..3.=="Total - Sex")

other_province <- pre_other_province %>%
  select(CENSUS_YEAR, DIM..Place.of.birth..272., DIM..Immigrant.status.and.period.of.immigration..11., DIM..Age..12., Dim..Citizenship..5...Member.ID...1...Total...Citizenship..Note..29.)%>%
  group_by(DIM..Place.of.birth..272., DIM..Immigrant.status.and.period.of.immigration..11., DIM..Age..12.)%>%
  summarise(Dim..Citizenship..5...Member.ID...1...Total...Citizenship..Note..29.=sum(Dim..Citizenship..5...Member.ID...1...Total...Citizenship..Note..29.))

#View(other_province)

other_province_pivot <- other_province%>%
  pivot_wider(names_from = DIM..Immigrant.status.and.period.of.immigration..11., values_from = Dim..Citizenship..5...Member.ID...1...Total...Citizenship..Note..29.)%>%
  mutate(.before=1, census_year=2016)%>%
  mutate(.after=1, province_name = "Other Provinces and Territories")


View(other_province_pivot)

write_xlsx(other_province_pivot, "/Users/ajordan/Library/CloudStorage/OneDrive-McGillUniversity/LTBI-Aust-CEA-master/Census/Provinces/Other Provinces/other provinces.xlsx")


  