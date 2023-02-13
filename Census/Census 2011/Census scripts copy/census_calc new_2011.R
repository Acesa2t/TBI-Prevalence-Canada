# Prior script census_rep.R

library(tidyverse)
library(reshape2)
library(readxl)
library(writexl)
library(janitor)


# Read in file

  census_rep <- read_xlsx("census_rep_2011.xlsx")
class(census_rep)

census_cols <- census_rep%>%select(-year_of_arrival)
View(census_cols)

# Add age groups

census_cols <- census_cols%>%
  mutate(age_group = ifelse(age >= 0 & age <=14, 1,
      ifelse(age >= 15 & age<=24, 2,
        ifelse(age >= 25 & age<=54,3,                                                     ifelse(age >= 55 & age<=64,4,                                                      ifelse(age >= 65 & age<=85,5,6))))))

#View(census_cols$age_group)

# Re-format dataset

census_colsv2 <- melt(census_cols, id.vars = c("age", "age_group","yarp"))
#View(census_colsv2)

colnames(census_colsv2) <- c("AGEP", "AGE_GROUP","YARP", "COO", "NUMP")
View(census_colsv2)
# Create YARP groups, YOBP column, and census year column

census_colsv2 <- census_colsv2%>%mutate(YARP = ifelse(YARP == "Before 1971", 1970, as.numeric(YARP)))
#View(census_colsv2)

table(as.numeric(census_colsv2$YARP))

census_colsv2 <- census_colsv2%>%
  mutate(census_year = 2011, 
         YOBP = 2011-AGEP, 
         YARP_group = ifelse(YARP == 1970, 1,
                             ifelse(YARP >=1971&YARP<=1980, 2,
                                    ifelse(YARP>=1981&YARP<=1990,3,
                                           ifelse(YARP>=1991&YARP<=2000,4,
                                                  ifelse(YARP>=2001&YARP<=2011, 5, 6))))))

census_colsv2<-subset(census_colsv2,census_cols2$YOBP<=census_colsv2$YARP)

# Use regular expression to remove some of the bracketed numbers
census_colsv2$COO <- gsub("\\[|\\]|[0-9]", "", census_colsv2$COO)
census_colsv2$NUMP<-as.numeric(census_colsv2$NUMP)

census_colsv2<-census_colsv2%>%filter(YOBP<=YARP)

# Calculations
census_colsv3<-census_colsv2%>%group_by(AGE_GROUP,YARP_group,COO)%>%mutate(NUMP_v2 = NUMP/max(row_number()))
census_colsv3<-census_colsv3%>%group_by(AGE_GROUP,YARP_group,COO)%>%arrange()

census_colsv3$NUMP<-NULL
census_colsv3<-dplyr::rename(census_colsv3,NUMP=NUMP_v2)


View(census_colsv3)

na.omit(census_colsv3)%>%ungroup()%>%summarise(tot_pop = sum(NUMP))

# Safety checks
summary(census_colsv3)

unique(census_colsv3$COO)
unique(census_colsv3$AGE_GROUP)
unique(census_colsv3$YARP_group)

View(census_colsv3%>%filter(YOBP > YARP))


write.csv(census_colsv3, file = "/Users/ajordan/OneDrive - McGill University/LTBI-Aust-CEA-master/Census/Census 2011/census_calced.csv")

# Next script census_extension loop.R

