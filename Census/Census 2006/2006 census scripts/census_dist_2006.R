# Prior script census_fumbling_2006.R
# Use this script to replicate YARP and distribute the number of people across populations

library(tidyverse)
library(reshape2)
library(readxl)
library(writexl)
#library(janitor)
#library(taRifx)

# Renaming countries weith special symbols in Excel
census_rep <- read_xlsx("census_rep_2006.xlsx")
View(census_rep)

colnames(census_rep)

View(unique(census_rep$COO))

census_rep$NUMP<-as.numeric(census_rep$NUMP)
census_rep2<-census_rep%>%group_by(AGE_BANDS,YARP_GROUPS,COO)%>%mutate(NUMP_v2 = NUMP/max(row_number()))
census_colsv2<-census_rep2%>%group_by(AGE_BANDS,YARP_GROUPS,COO)%>%arrange()


census_colsv2$NUMP<-NULL
census_colsv2<-dplyr::rename(census_colsv2,NUMP=NUMP_v2)

census_colsv2%>%ungroup()%>%summarise(sum(NUMP))

View(census_colsv2)

#write_xlsx(census_colsv2, path = "/Users/ajordan/OneDrive - McGill University/LTBI-Aust-CEA-master/Census/Census 2006/census_distributed_v2.xlsx")

saveRDS(census_colsv2, 
        file = "/Users/ajordan/OneDrive - McGill University/LTBI-Aust-CEA-master/Census/Census 2006/census_distributed_2006.rds")
# Next script census_country1.R

