# Document used to add Age at Arrival (AARP) column, add WHO regions, eventually incidence bands will be added as well. Unnecessary columns also removed

rm(list = ls(all.names = TRUE))
gc()

library(spatstat)
library(plotly)
library(tidyverse)
library(writexl)

# Read file in
#tbi <- readRDS("census_v10.rds")
#write.csv(tbi,"/Users/ajordan/OneDrive - McGill University/LTBI-Aust-CEA-master/Data/Outputs/tbi_est.csv", row.names = FALSE)

# 2016
tbi <- readRDS("2016_census_v3.rds")
View(tbi)

tbi_AUS <- readRDS("AUScensus_2016_v12.rds")

# 2011
tbi_2011 <- readRDS("2011estimates_v3.rds")
View(tbi_2011)

tbi_AUS_2011 <- readRDS("AUScensus_v10.rds")
View(tbi_AUS_2011)

# 2006
tbi_2006 <- readRDS("2006_census_v4.rds")
View(tbi_2006)


tbi_AUS_2006 <- readRDS("AUScensus_2006_v4.rds")
View(tbi_AUS_2006)

#===================================================================================================

# CAN
tbi_graph <- na.omit(tbi_2006)%>%mutate(AARP = YARP-YOBP, TBI_prev = sum(LTBP)/sum(NUMP), YARP_split = ifelse(YARP >= 2000, "2000+", ifelse(YARP < 2000, "1999-", "No value")))%>%filter(ISO3%in%c("CHN"))
View(tbi_graph)

tbi_graph_age <- tbi_graph%>%
  mutate(AGE_BANDS = ifelse(
    AGEP < 15, "0-14", ifelse(
      AGEP >= 15 & AGEP < 35, "15-34", ifelse(
        AGEP >= 35 & AGEP < 55, "35-54",ifelse(
          AGEP >= 55 & AGEP <= 64, "55-64", ifelse(
            AGEP >= 65 & AGEP <= 200, "65+", "No value"))))))

tbi_graph_aarp <- tbi_graph_age%>%
  mutate(AARP_BANDS = ifelse(
    AARP < 15, "0-14", ifelse(
      AARP >= 15 & AARP < 35, "15-34", ifelse(
        AARP >= 35 & AARP < 55, "35-54",ifelse(
          AARP >= 55 & AARP <= 64, "55-64", ifelse(
            AARP >= 65 & AARP <= 200, "65+", "No value"))))))

tbi_graph%>%plot_ly(type = "bar", x = ~AARP, y = ~NUMP,  transforms = list(
  
  list(
    
    type = 'groupby',
    
    groups = AGE_BANDS,
    
    styles = list(
      
      list(target = 4, value = list(marker =list(color = 'blue'))),
      
      list(target = 6, value = list(marker =list(color = 'red'))),
      
      list(target = 8, value = list(marker =list(color = 'black')))
      
    )
    
  )
  
))%>%add_bars(marker = list(color = c("navy")))

View(tbi_graph%>%filter(AGEP >= 55))

# AUS
#, "CHN"
tbi_graph_AUS <- na.omit(tbi_AUS_2006)%>%mutate(AARP = YARP-YOBP,  TBI_prev = sum(LTBP)/sum(NUMP), YARP_split = ifelse(YARP >= 2000, "2000+", ifelse(YARP < 2000, "1999-", "No value")))%>%filter(ISO3%in%c("CHN") & AGEP < 86)

tbi_graph_AUS_age <- tbi_graph_AUS%>%
  mutate(AGE_BANDS = ifelse(
    AGEP < 15, "0-14", ifelse(
      AGEP >= 15 & AGEP < 35, "15-34", ifelse(
        AGEP >= 35 & AGEP < 55, "35-54",ifelse(
          AGEP >= 55 & AGEP <= 64, "55-64", ifelse(
            AGEP >= 65 & AGEP <= 200, "65+", "No value"))))))


tbi_graph_AUS_aarp <- tbi_graph_AUS_age%>%
  mutate(AARP_BANDS = ifelse(
    AARP < 15, "0-14", ifelse(
      AARP >= 15 & AARP < 35, "15-34", ifelse(
        AARP >= 35 & AARP < 55, "35-54",ifelse(
          AARP >= 55 & AARP <= 64, "55-64", ifelse(
            AARP >= 65 & AARP <= 200, "65+", "No value"))))))

tbi_graph_AUS%>%plot_ly(x = ~AARP, y = ~NUMP)%>%add_bars(marker = list(color = c("navy")))
