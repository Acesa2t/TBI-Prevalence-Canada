# Census melting, repping, and fumbling
# Script #1
rm(list = ls(all.names = TRUE))
library(tidyverse)
library(reshape2)
library(readxl)
library(writexl)

census_2001 <- read_xlsx("Census 2001.xlsx", skip = 1)
#View(census_2001)

# Clear out some unneeded rows and columns
census_yarp <- census_2001%>%filter(!`Immigrant Stat`%in%c("Total - Immigrant Status and Period of Immigration", "Non-immigrants", "Immigrants", "Non-permanent residents"))%>%filter(`Place of Birth` != "Total - Place of birth of respondent")%>%select(-"Total - Age groups")

colnames(census_yarp) <- c("COO", "YARP", "0-14", "15-24", "25-54", "55-64", "65+")
#View(census_yarp)

census_yarp<-census_yarp[1:7] 
# Melt the dataframe
census_melt <- melt(census_yarp, id.vars = c("COO", "YARP"))
View(census_melt)

colnames(census_melt) <- c("COO", "YARP_GROUPS", "AGE_BANDS", "NUMP")
View(census_melt)

# Rep YARP_GROUPS

census_rep_yarp <- census_melt[rep(row.names(census_melt), 
                                        ifelse(census_melt$YARP_GROUPS=="Before 1915",1,
                                               ifelse(census_melt$YARP_GROUPS=="1996 to 2001",6,5))),]

View(census_rep_yarp)


census_new_yarp <- census_rep_yarp%>%
  group_by(COO, AGE_BANDS)%>%mutate(YARP = seq(from = 1915, to = 2001))

View(census_new_yarp)

# Rep AGE_BANDS then create AGEP column
census_rep_age <- census_new_yarp[rep(row.names(census_new_yarp), 
                                        ifelse(census_new_yarp$AGE_BANDS=="0-14",15,  ifelse(census_new_yarp$AGE_BANDS == "15-24", 10, ifelse(census_new_yarp$AGE_BANDS == "25-54", 30, ifelse(census_new_yarp$AGE_BANDS == "55-64", 10,22))))),]

census_rep_age <- census_rep_age%>%arrange(COO)
View(census_rep_age)



census_new_age <- census_rep_age%>%
  group_by(COO, YARP)%>%
  mutate(
    AGEP = ifelse(AGE_BANDS == "0-14",
                         seq(from = 0, to = 14), 
                         ifelse(AGE_BANDS == "15-24",
                                seq(from = 15, to = 24),
                                ifelse(AGE_BANDS == "25-54",
                                       seq(from = 25, to = 54),
                                       ifelse(AGE_BANDS == "55-64",
                                              seq(from = 55, to = 64), 
                                              seq(from = 65, to = 86))))))

View(census_new_age)

# Add YOBP column

census_yobp <- census_new_age%>%mutate(YOBP = 2001-AGEP)%>%filter(YOBP <= YARP)
View(census_yobp)

#write.csv(census_yobp, "/Users/ajordan/OneDrive - McGill University/LTBI-Aust-CEA-master/Census/Census 2001/census_rep_2001.csv")

write_xlsx(census_yobp, "/Users/ajordan/OneDrive - McGill University/LTBI-Aust-CEA-master/Census/Census 2001 Year/census_rep_2001_v4.xlsx")

# Next script census_dist
