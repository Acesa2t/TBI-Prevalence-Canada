# Prior script census_rep.R
# Use this script to replicate YARP and distribute the number of people across populations

library(tidyverse)
library(reshape2)
library(readxl)
library(writexl)
library(janitor)
library(taRifx)

# Need to remove indices 273-303 from all of these datasets then try again

# Find number of excel sheets
num_sheets <- length(excel_sheets("census_rep_2011.xlsx"))

# Define empty list
list_calc <- list()

# Add each Excel sheet to the list as a data frame
for(x in 1:num_sheets){
  list_calc[[x]] <- read_xlsx("census_rep_2011.xlsx", sheet = x)
}
View(list_calc[[2]])


# Attempt to use gather to solve my problem then create a bigger problem

census_cols <- as.data.frame(census_cols)
View(census_cols)
census_colsv2 <- melt(census_cols, id.vars = c("age", "age_group","year_of_arrival"))

colnames(census_colsv2) <- c("AGEP", "AGE_GROUP","YARP", "COO", "NUMP")

# I'm not convinced that I didn't create a bigger problem but this looks like it worked so I won't complain for now, I'll just move on
View(census_colsv2)
census_colsv2 <- census_colsv2%>%mutate(YARP = ifelse(YARP == "Before 1981", 1980, as.numeric(YARP)))
table(as.numeric(census_colsv2$YARP))
census_colsv2 <- census_colsv2%>%
  mutate(census_year = 2016, YOBP = 2016-AGEP, YARP_group = ifelse(YARP == 1980, 1,
                                                                   ifelse(YARP >=1981&YARP<=1990, 2,
                                                                    ifelse(YARP>=1991&YARP<=2000,3,
                                                                    ifelse(YARP>=2001&YARP<=2010,4,5)))))

census_colsv2<-subset(census_colsv2,census_colsv2$YOBP<=census_colsv2$YARP)
View(census_colsv2)

# Use regular expression to remove some of the bracketed numbers
census_colsv2$COO <- gsub("\\[|\\]|[0-9]", "", census_colsv2$COO)
census_colsv2$NUMP<-as.numeric(census_colsv2$NUMP)
census_colsv2<-census_colsv2%>%group_by(AGE_GROUP,YARP_group,COO)%>%mutate(NUMP_v2 = NUMP/max(row_number()))
census_colsv2<-census_colsv2%>%group_by(AGE_GROUP,YARP_group,COO)%>%arrange()

census_colsv2$NUMP<-NULL
census_colsv2<-dplyr::rename(census_colsv2,NUMP=NUMP_v2)

write_xlsx(census_colsv2, path = "/Users/ajordan/OneDrive - McGill University/LTBI-Aust-CEA-master/Census/2census_data_clean.xlsx")

# Next script census_extension.R

