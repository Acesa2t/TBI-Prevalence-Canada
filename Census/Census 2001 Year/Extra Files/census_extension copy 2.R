# Prior script census_calc new.R

rm(list=ls()) 
# library(readxl)
# library(writexl)
library(data.table)
library(janitor)
library(tidyverse)
library(zoo)

#census_extension <- read_xlsx("2census_data_clean.xlsx")
census_extension <- read.csv("3census_data_clean.csv")

View(census_extension)
census_post1980 <- census_extension%>%filter(YARP != 1980)
census_1980 <- census_extension%>%filter(YARP == 1980)
View(census_1980)

# Replicate rows
# Remove YARP then re-create
# 

# for (i in 1:nrow(census_1980)){
#   k<-as.numeric(census_1980[i,9])
#   h<-as.numeric(i)
#   for (i in 1:k){
#     census_1980<-add_row(census_198ID=unlist(census_1980[h,1]),Day=unlist(census_1980[h,2])+(7*i))
#   }
# }
#seq(1:nrow)

census_1980$ID<-seq(1:nrow(census_1980))
census_1980$YARPv2<-census_1980$YARP-census_1980$YOBP

###census_1980$NUMP<-census_1980$NUMP/(census_1980$YARPv2+1)

#census_1980<-subset(census_1980,census_1980$COO=="United States")



# # Replicate rows while simultaneously creating new YARPS
rep_census_rows <- function(x){ 
for (i in 1:nrow(x)){
  k<-as.numeric(x[i,10])
  h<-as.numeric(i)
  for (i in 1:k){
    x<-add_row(x, ID=unlist(x[h,9]), YARP=unlist(x[h,3])-(1*i))
  }
}}

census_1980 <- as.data.table(census_1980)
census_1980[YOBP < 1930 , YOBPgroup := 1]
census_1980[YOBP > 1929 & YOBP < 1960 , YOBPgroup := 2]
census_1980[YOBP > 1959 & YOBP < 1980 , YOBPgroup := 3]
census_1980[YOBP > 1979 & YOBP < 2000 , YOBPgroup := 4]
census_1980[YOBP > 1999 & YOBP < 2010 , YOBPgroup := 5]
census_1980[YOBP > 2009 , YOBPgroup := 6]

census.yob.split <- split(census_1980, census_1980$YOBPgroup)

census.yob.split  <- lapply(census.yob.split , rep_census_rows)

census_1980 <- do.call("rbind", census.yob.split)

saveRDS(census_1980, file = "/Users/ajordan/OneDrive - McGill University/LTBI-Aust-CEA-master/Census/4census_1980.rds")

# Fill in the NAs then delete duplicate values
census_1980 <- census_1980%>%arrange(ID)
nrow(census_1980)
census_1980v2 <- dplyr::distinct(na.locf(census_1980))

nrow(census_1980v2)
  
# NOW BACK TO 1889 =========================================================

# Remove anyone whose year of birth is less than or equal to YARP 

census_1930<-subset(census_1980v2,census_1980v2$YOBP<=census_1980v2$YARP)


# Distribute population
census_1930<-census_1930%>%
  group_by(AGEP, COO)%>%
  mutate(NUMPv2 = NUMP/max(row_number()))


# Rename NUMP and remove extra columns

census_1930$NUMP<-NULL
census_1930 <- census_1930%>%
  dplyr::rename(NUMP=NUMPv2)%>%
  select(-YARPv2, -ID)
head(census_1930)

# Bind the data frames

census_1930to1980 <- rbind(census_post1980,census_1930)
View(census_1930to1980)


#write_xlsx(census_1930to1980, 
#           path = "/Users/ajordan/OneDrive - McGill University/LTBI-Aust-CEA-master/Census/2census_1930to1980.xlsx")

saveRDS(census_1930to1980, 
        file = "/Users/ajordan/OneDrive - McGill University/LTBI-Aust-CEA-master/Census/4census_1930to1980.rds")
# Next script is census_country1