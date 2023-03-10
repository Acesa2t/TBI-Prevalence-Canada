# Prior script census_calc new.R

rm(list=ls()) 
# library(readxl)
# library(writexl)
library(janitor)
library(tidyverse)
library(zoo)

census_extension <- read.csv("2006_census_data_clean.csv")
#View(census_extension)


census_post1960 <- census_extension%>%filter(YARP != 1960)
View(census_post1960)

View(census_post1960%>%
       summarise(tot_pop = sum(as.numeric(NUMP))))

census_1960 <- census_extension%>%filter(YARP == 1960)
View(census_1960)

# Replicate rows
# Remove YARP then re-create

#seq(1:nrow)

census_1960$ID<-seq(1:nrow(census_1960))
census_1960$YARPv2<-census_1960$YARP-census_1960$YOBP

#min(census_1960$YOBP)
#View(census_1960$YARP)
###census_1960$NUMP<-census_1960$NUMP/(census_1960$YARPv2+1)

# census_1960t<-subset(census_1960,census_1960$COO=="Americas")
# View(census_1960t)

# Replicate rows while simultaneously creating new YARPS
# Should produce NA's that will be filled in later
# Slight change to this. AGE_GROUPS got injected a little differently this time so had to change from [h,3] to [h,4] in order to make sure that YARP column was correctly indexed. k <- as.numeric changed from [i,10] to [i,11] so that YARPv2 would be indexed. ID=unlist(census_1960t[h,9] changed to [h,10] so that ID column would be indexed

for (i in 1:nrow(census_1960)){
  k<-as.numeric(census_1960[i,11])
  h<-as.numeric(i)
  for (i in 1:k){
      census_1960<-add_row(census_1960, ID=unlist(census_1960[h,10]), YARP=unlist(census_1960[h,4])-(1*i))
  }
}

# for (i in 1:nrow(census_1960)){
#   k<-as.numeric(census_1960[i,10])
#   print()
#   h<-as.numeric(i)
#   for (i in 1:k){
#     census_1960<-add_row(census_1960, ID=unlist(census_1960[h,9]), YARP=unlist(census_1960[h,3])-(1*i))
#   }
# }
View(census_1960)

saveRDS(census_1960, file = "/Users/ajordan/OneDrive - McGill University/LTBI-Aust-CEA-master/Census/Census 2006/census2006_ext.rds")

census_1960<-subset(census_1960,census_1960$YOBP<=census_1960$YARP)


#census_1960 <- readRDS("census2006_1960v3.rds")
# Fill in the NAs then delete duplicate values
census_1960 <- census_1960%>%arrange(ID)
View(census_1960)


census_1960v3 <- dplyr::distinct(na.locf(census_1960v3))

nrow(census_1960v3)


# Remove anyone whose year of birth is less than or equal to YARP 


#View(census_1960v4)


# Distribute population
census_1960v4<-census_1960v3%>%
  group_by(AGE_GROUP,YARP_group,COO)%>%
  mutate(NUMPv2 = NUMP/max(row_number()))

View(census_1960v4)

View(census_1960v4%>%ungroup%>%
       summarise(tot_pop = sum(as.numeric(NUMPv2))))

# Rename NUMP and remove extra columns

census_1960v4$NUMP<-NULL
census_1960v5 <- census_1960v4%>%
  dplyr::rename(NUMP=NUMPv2)%>%
  select(-YARPv2, -ID)


# All years-bind data frames
View(census_1960v5)
View(census_post1960)
census_allyears <- rbind(census_1960v5,census_post1960)


# Check pop total
View(census_allyears%>%ungroup%>%
       summarise(tot_pop = sum(as.numeric(NUMP))))



saveRDS(census_allyears, 
        file = "/Users/ajordan/OneDrive - McGill University/LTBI-Aust-CEA-master/Census/Census 2006/2006_census_1910to1960.rds")


# Next script is census_country1