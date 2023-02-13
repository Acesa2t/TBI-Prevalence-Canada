# Document used to add Age at Arrival (AARP) column, add WHO regions, eventually incidence bands will be added as well. Unnecessary columns also removed

rm(list = ls(all.names = TRUE))
gc()

library(tidyverse)

# Read file in
tbi <- readRDS("2016_census_v3.rds")
#write.csv(tbi,"/Users/ajordan/OneDrive - McGill University/LTBI-Aust-CEA-master/Data/Outputs/tbi_est.csv", row.names = FALSE)
View(tbi)
na.omit(tbi)%>%group_by(ISO3, YARP,YOBP)

tbi_rep <- na.omit(tbi)%>%filter(ISO3%in%c("PHL", "IND","CHN","USA","DEU","GBR", "VNM", "HKG","MAC"))%>%group_by(ISO3, YARP, NUMP)%>%mutate(AARP = YARP - YOBP)%>%summarize(tbi_prev_CAN = sum(LTBP)/sum(NUMP), med_yarp = median(YARP), med_aarp = median(AARP), med_age = median(AGEP))
View(tbi_rep)


View(na.omit(tbi)%>%
       summarise(TBI.prev = sum(LTBP)/sum(NUMP), TBI.prev.low = sum(LTBP.low)/sum(NUMP), TBI.prev.high = sum(LTBP.high)/sum(NUMP)))

View(na.omit(tbi)%>%
       summarise(TBI.prev=sum(NUMP)))


data <- na.omit(tbi)%>%
  filter(ISO3=="CHN")%>%
  group_by(ISO3, YARP, YOBP)%>%
  summarize(NUMP = sum(NUMP),
            LTBP = sum(LTBP))

#View(data)

ggplot(data, aes(YOBP, NUMP))+geom_point()
# DATA CHECK=======================================================================================================
View(na.omit(tbi)%>%
  group_by(ISO3)%>%
  summarise(TBI.prev = sum(LTBP)/sum(NUMP), TBI.prev.low = sum(LTBP.low)/sum(NUMP), TBI.prev.high = sum(LTBP.high)/sum(NUMP)))

write.csv(by_country, file = "/Users/ajordan/OneDrive - McGill University/LTBI-Aust-CEA-master/Data/Outputs/tbi_country.csv")

View(na.omit(tbi)%>%
       filter(ISO3%in%c("PHL", "USA", "CHN", "IND", "DEU", "AUS"))%>%
       group_by(ISO3)%>%
       summarise(sum(LTBP)/sum(NUMP), sum(LTBP.low)/sum(NUMP), sum(LTBP.high)/sum(NUMP)))

View(na.omit(tbi)%>%
       filter(ISO3%in%c("PHL", "USA", "CHN", "IND", "DEU", "AUS") & YARP%in%c(1890, 1931, 1950, 1980, 1990, 2000, 2014))%>%
       group_by(ISO3,YARP)%>%
       summarise(sum(LTBP)/sum(NUMP), sum(LTBP.low)/sum(NUMP), sum(LTBP.high)/sum(NUMP)))


View(na.omit(tbi)%>%
       filter(YARP%in%c(1890, 1931, 1950, 1980, 1990, 2000, 2014))%>%
       group_by(YARP)%>%
       summarise(sum(LTBP)/sum(NUMP), sum(LTBP.low)/sum(NUMP), sum(LTBP.high)/sum(NUMP)))

cc <- tbi%>%
  group_by(ISO3)%>%
  summarise(sum(LTBP)/sum(NUMP))
View(cc)


# INCIDENCE BANDS====================================================================================================

# Merge incidence bands
tb_inc <- readRDS("incidence.rds")
colnames(tb_inc) <- c("ISO3", "WHO_Region", "e_inc_100k")
tb_inc <- tb_inc%>%select(-WHO_Region)
head(tb_inc)

tbi_data <- merge(tbi, tb_inc, by = "ISO3")


# Less bands
tbi_data_bands <- tbi_data%>%
  mutate(inc_band_per100k = ifelse(
    e_inc_100k < 50, "0-49", ifelse(
      e_inc_100k >= 50 & e_inc_100k < 100, "50-99", ifelse(
        e_inc_100k >= 100 & e_inc_100k < 200, "100-199",ifelse(
          e_inc_100k >= 200 & e_inc_100k <= 1000, "200+", "No value")))))


# Less bands
# tbi_data_bands2 <- tbi_data%>%
#   mutate(inc_band_per100k = ifelse(
#     e_inc_100k < 50, "0-49", ifelse(
#       e_inc_100k >= 50 & e_inc_100k < 100, "50-99", ifelse(
#         e_inc_100k >= 100 & e_inc_100k < 150, "100-149",ifelse(
#           e_inc_100k >= 150 & e_inc_100k <= 1000, "150+", "No value")))))

# tbi_data_bands <- tbi_data%>%
#   mutate(inc_band_per100k = ifelse(
#     e_inc_100k < 30, "0-30", ifelse(
#       e_inc_100k >= 30 & e_inc_100k < 100, "30-99", ifelse(
#         e_inc_100k >= 100 & e_inc_100k < 200, "100-199",ifelse(
#           e_inc_100k >= 200 & e_inc_100k <= 1000, "200+", "No value")))))

View(na.omit(tbi_data_bands)%>%
       filter(e_inc_100k >= 100 & e_inc_100k <=199)%>%
  group_by(inc_band_per100k, ISO3)%>%
    summarise(sum(LTBP)/sum(NUMP), 
            sum(LTBP.low)/sum(NUMP), 
            sum(LTBP.high)/sum(NUMP)))

View(na.omit(tbi_data_bands)%>%
       group_by(inc_band_per100k)%>%
       summarise(sum(LTBP)/sum(NUMP), 
                 sum(LTBP.low)/sum(NUMP), 
                 sum(LTBP.high)/sum(NUMP)))

# AGE GROUPS (AGE AT CENSUS)=====================================================================

# tbi_age_groups <- tbi_data_bands%>%
#   mutate(AGE_BANDS = ifelse(
#     AGEP < 15, "0-14", ifelse(
#       AGEP >= 15 & AGEP < 25, "15-24", ifelse(
#         AGEP >= 25 & AGEP < 55, "25-54",ifelse(
#           AGEP >= 55 & AGEP <= 64, "55-64", ifelse(
#             AGEP >= 65 & AGEP <= 200, "65+", "No value"))))))

tbi_age_groups%>%

View(tbi_age_groups)
tbi_grouping <- na.omit(na.omit(tbi_age_groups)%>%
                     group_by(ISO3, AGE_BANDS, inc_band_per100k, WHO_R)%>%
                     summarise(sum(LTBP)/sum(NUMP), 
                               sum(LTBP.low)/sum(NUMP), 
                               sum(LTBP.high)/sum(NUMP)))

View(tbi_grouping)

#write.csv(tbi_grouping, file = "/Users/ajordan/OneDrive - McGill University/LTBI-Aust-CEA-master/Census/Census 2016/tbi_age_iso3_v2.csv")
tbi_age_groups <- tbi_data_bands%>%
  mutate(AGE_BANDS = ifelse(
    AGEP < 15, "0-14", ifelse(
      AGEP >= 15 & AGEP < 35, "15-34", ifelse(
        AGEP >= 35 & AGEP < 55, "35-54",ifelse(
          AGEP >= 55 & AGEP < 75, "55-74",ifelse(
            AGEP >= 75 & AGEP <= 200, "75+", "No value"))))))

View(na.omit(tbi_age_groups)%>%
       group_by(AGE_BANDS)%>%
       summarise(sum(LTBP)/sum(NUMP), 
                 sum(LTBP.low)/sum(NUMP), 
                 sum(LTBP.high)/sum(NUMP)))
# YARP ========================================================================================

tbi_yarp <- tbi_age_groups%>%
  mutate(YARP_group = ifelse(YARP < 1980,"Before 1980", 
                             ifelse(YARP >= 1980 & YARP < 1985, "1980-1984",
                                    ifelse(YARP >= 1985 & YARP < 1990, "1985-1989",
                                           ifelse(YARP >= 1990 & YARP < 1995, "1990-1994",
                                                  ifelse(YARP >= 1995 & YARP < 2000, "1995-1999",
                                                         ifelse(YARP >= 2000 & YARP < 2005, "2000-2004",
                                                                ifelse(YARP >= 2005 & YARP < 2010, "2005-2009",
                                                                       ifelse(YARP >= 2010 & YARP <= 2016, "2010-2016","No value")))))))))

tbi_yarp$YARP_group <- factor(tbi_yarp$YARP_group, levels = c("Before 1980", 
                                                           "1980-1984", 
                                                           "1985-1989",
                                                           "1990-1994",
                                                           "1995-1999",
                                                           "2000-2004",
                                                           "2005-2009",
                                                   "2010-2016"))

View(na.omit(tbi_yarp)%>%
       group_by(YARP_group)%>%
       summarise(sum(LTBP)/sum(NUMP), 
                 sum(LTBP.low)/sum(NUMP), 
                 sum(LTBP.high)/sum(NUMP)))

tbi_pivot <- na.omit(tbi_yarp)%>%
       group_by(YARP_group, AGE_BANDS)%>%
       summarise(TBI_prev = sum(LTBP)/sum(NUMP))

tbi_pivot2 <- pivot_wider(tbi_pivot, names_from = AGE_BANDS, values_from = TBI_prev)
View(tbi_pivot2)

tbi_age_groups%>%
  filter(YARP >= 2011)%>%
  summarise(sum(NUMP))

tbi_age_groups%>%
  summarise(sum(NUMP))

1200795/7249345


View(na.omit(tbi_age_groups)%>%
       group_by(AGE_BANDS, inc_band_per100k, YARP)%>%
       summarise(sum(LTBP)/sum(NUMP), 
                 sum(LTBP.low)/sum(NUMP), 
                 sum(LTBP.high)/sum(NUMP)))

View(na.omit(tbi_age_groups)%>%
       group_by(AGE_BANDS)%>%
       summarise(sum(NUMP)))


  
  
# YARP GROUPS ============================================================================
  tbi_playground <- tbi_age_groups
  # BY YARP ========================================================
 tbi_pg_plot <- na.omit(tbi_playground)%>%
    group_by(AGE_BANDS, inc_band_per100k, YARP)%>%
    summarise(tbi_prev = sum(LTBP)/sum(NUMP))

tbi_inc_plot <- na.omit(tbi_age_groups)%>%
  group_by(inc_band_per100k, YARP)%>%
  summarise(tbi_prev = sum(LTBP)/sum(NUMP))
View(tbi_inc_plot)

# Year and Inc Band
ggplot( tbi_inc_plot, aes(YARP, tbi_prev, color = inc_band_per100k))+
  geom_smooth()+
  labs(title = "TBI Prevalence by Year of Arrival and Incidence Band",
       x = "Year of Arrival",
       y = "TBI Prevalence",
       color = "Incidence Band per 100,000 Persons")

# Age in Census Year vs Age at Arrival?

tbi_inc_plot_aarp <- na.omit(tbi_inc_plot%>%
                     mutate(AARP = YARP - YOBP))

ggplot( tbi_inc_plot, aes(YARP, tbi_prev, color = inc_band_per100k))+
  geom_smooth()+
  labs(title = "TBI Prevalence by Year of Arrival and Incidence Band",
       x = "Year of Arrival",
       y = "TBI Prevalence",
       color = "Incidence Band per 100,000 Persons")


 ggplot( tbi_pg_plot, aes(YARP, tbi_prev, color = inc_band_per100k))+
          geom_line()+
   geom_vline(xintercept = 1980, linetype = "dashed")+
   facet_wrap(.~AGE_BANDS)
 
 
 tbi_inc_plot <- na.omit(tbi_yarp)%>%
   group_by(inc_band_per100k, YARP_group)%>%
   summarise(tbi_prev = sum(LTBP)/sum(NUMP))

 View(tbi_inc_plot)
 
 ggplot( tbi_inc_plot, aes(YARP_group, tbi_prev, fill = inc_band_per100k))+
   geom_bar(stat = "identity")+
   labs(title = "TBI Prevalence by Year of Arrival and Incidence Band",
        x = "Year of Arrival",
        y = "TBI Prevalence",
        color = "Incidence Band per 100,000 Persons")
 

 # ADD AARP ===================================================================================
 
 #saveRDS(read.csv("estimates_v3.csv"), file = "/Users/ajordan/OneDrive - McGill University/LTBI-Aust-CEA-master/Data/census_v4.rds")
 
 #Adding age at arrival column
 tbi_est <- na.omit(tbi%>%
                      mutate(AARP = YARP - YOBP))
 
 
 length(unique(tbi_est$ISO3))

 # PREV VS INC PLOT =======================================================================
 tbi_group <- na.omit(tbi_age_groups)%>%
   group_by(AGE_BANDS, YARP)%>%
   summarise(tbi.prev = sum(LTBP)/sum(NUMP), 
             sum(LTBP.low)/sum(NUMP), 
             sum(LTBP.high)/sum(NUMP))
 
 
 ggplot(tbi_group, aes(YARP, tbi.prev)) + geom_line() + facet_wrap(~AGE_BANDS)
 
 tb_v_tbi <- unique(tbi_age_groups%>%group_by(ISO3)%>%summarise(tbi = sum(LTBP)/sum(NUMP), e_inc_100k = e_inc_100k))
 View(tb_v_tbi)
 
 tb_v_tbi%>%ggplot(aes(tbi, e_inc_100k, color = ISO3)) + 
   geom_point() +
   theme(legend.position = "none")
 