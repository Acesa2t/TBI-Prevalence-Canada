# Prior script census_calc new.R

rm(list=ls()) 
# library(readxl)
# library(writexl)
library(janitor)
library(tidyverse)
library(zoo)

census_extension <- read.csv("/Users/ajordan/Library/CloudStorage/OneDrive-McGillUniversity/LTBI-Aust-CEA-master/Census/Provinces/Other Provinces/census_calced_other_provinces.csv")
#View(census_extension)


census_post1980 <- census_extension%>%filter(YARP != 1980)
View(census_post1980)

View(census_post1980%>%
       summarise(tot_pop = sum(as.numeric(NUMP))))

census_1980 <- census_extension%>%filter(YARP == 1980)
View(census_1980)

# Replicate rows
# Remove YARP then re-create

#seq(1:nrow)

census_1980$ID<-seq(1:nrow(census_1980))
census_1980$YARPv2<-census_1980$YARP-census_1980$YOBP

#min(census_1970$YOBP)
#View(census_1970$YARP)
###census_1970$NUMP<-census_1970$NUMP/(census_1970$YARPv2+1)

# census_1970t<-subset(census_1970,census_1970$COO=="Americas")
# View(census_1970t)

# Replicate rows while simultaneously creating new YARPS
# Should produce NA's that will be filled in later
# Slight change to this. AGE_GROUPS got injected a little differently this time so had to change from [h,3] to [h,4] in order to make sure that YARP column was correctly indexed. k <- as.numeric changed from [i,10] to [i,11] so that YARPv2 would be indexed. ID=unlist(census_1970t[h,9] changed to [h,10] so that ID column would be indexed

for (i in 1:nrow(census_1980)){
  k<-as.numeric(census_1980[i,11])
  h<-as.numeric(i)
  for (i in 1:k){
      census_1980<-add_row(census_1980, ID=unlist(census_1980[h,10]), YARP=unlist((census_1980[h,4])-(1*i)))
  }
}

# for (i in 1:nrow(census_1970)){
#   k<-as.numeric(census_1970[i,10])
#   print()
#   h<-as.numeric(i)
#   for (i in 1:k){
#     census_1970<-add_row(census_1970, ID=unlist(census_1970[h,9]), YARP=unlist(census_1970[h,3])-(1*i))
#   }
# }
View(census_1980)

saveRDS(census_1980, file = "/Users/ajordan/Library/CloudStorage/OneDrive-McGillUniversity/LTBI-Aust-CEA-master/Census/Provinces/Other Provinces/census_other_provinces_1980yarp.rds")


census_1980 <- readRDS("/Users/ajordan/Library/CloudStorage/OneDrive-McGillUniversity/LTBI-Aust-CEA-master/Census/Provinces/Other Provinces/census_other_provinces_1980yarp.rds")
# Fill in the NAs then delete duplicate values

census_1980 <- census_1980%>%arrange(ID)

census_1980v3 <- dplyr::distinct(na.locf(census_1980))
View(census_1980v3)

census_1980<-subset(census_1980,census_1980$YOBP<=census_1980$YARP)

nrow(census_1980v3)


# Remove anyone whose year of birth is less than or equal to YARP 


#View(census_1970v4)


# Distribute population
census_1980v4<-census_1980v3%>%
  filter(YOBP <= YARP)%>%
  group_by(COO, AGEP)%>%
  mutate(NUMPv2 = NUMP/max(row_number()))



View(census_1980v4%>%ungroup%>%
       summarise(tot_pop = sum(as.numeric(NUMPv2))))

census_1980v4 <- census_1980v4%>%ungroup
# Rename NUMP and remove extra columns

census_1980v4$NUMP<-NULL
census_1980v5 <- census_1980v4%>%
  dplyr::rename(NUMP=NUMPv2)%>%
  select(-YARPv2, -ID)


# All years-bind data frames
View(census_1980v5)
View(census_post1980)
census_allyears <- rbind(census_1980v5,census_post1980)
View(census_allyears)

View(census_allyears%>%ungroup%>%
       summarise(tot_pop = sum(as.numeric(NUMP))))

saveRDS(census_allyears, 
        file = "/Users/ajordan/Library/CloudStorage/OneDrive-McGillUniversity/LTBI-Aust-CEA-master/Census/Provinces/Other Provinces/census_1934to1980_other_provinces.rds")


# Next script is census_country1