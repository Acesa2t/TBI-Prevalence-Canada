# Document used to add Age at Arrival (AARP) column, add WHO regions, eventually incidence bands will be added as well. Unnecessary columns also removed

rm(list = ls(all.names = TRUE))
gc()

library(tidyverse)
library(RColorBrewer)
library(data.table)
library(spatstat)

# Read file in
#tbi <- readRDS("2016_census_v3.rds")
tbi <- readRDS("2016 estimates_iso3.rds")
#write.csv(tbi,"/Users/ajordan/OneDrive - McGill University/LTBI-Aust-CEA-master/Data/Outputs/tbi_est.csv", row.names = FALSE)
tbi <- tbi%>%filter(ISO3!='CAN')
View(tbi)
Reduce("+",tbi$LTBP)

hist_graph <- tbi%>%
       mutate(AGE_BANDS_CHECK = ifelse(
  AGEP < 15, "0-14", ifelse(
    AGEP >= 15 & AGEP < 25, "15-24", ifelse(
      AGEP >= 25 & AGEP < 55, "25-54",ifelse(
        AGEP >= 55 & AGEP < 65, "55-64",ifelse(
          AGEP >= 65 & AGEP <= 200, "65+", "No value"))))))%>%
    group_by(AGE_BANDS_CHECK)

weighted.median(hist_graph$AGEP, hist_graph$NUMP) 
weighted.quantile(hist_graph$AGEP, hist_graph$NUMP, probs = c(0.25, 0.75))

ggplot(hist_graph, aes(AGEP, NUMP))+
  geom_histogram(stat = "identity")

#tbi%>%summarize(sum(LTBP)/sum(NUMP))

tbi_overall <- na.omit(tbi)%>%
  summarise(tbi_prev = median(Reduce("+", LTBP))/sum(NUMP),
            tbi_prev_iqr_low = quantile(Reduce("+",LTBP), 0.25)/sum(NUMP),
            tbi_prev_iqr_high = quantile(Reduce("+",LTBP), 0.75)/sum(NUMP),
            tbi_prev_ui_low = quantile(Reduce("+",LTBP), 0.025)/sum(NUMP),
            tbi_prev_ui_high = quantile(Reduce("+",LTBP), 0.975)/sum(NUMP))
View(tbi_overall)

tbi_iso3 <- na.omit(tbi)%>%
  group_by(ISO3)%>%
  summarise(tbi_prev = median(Reduce("+",LTBP))/sum(NUMP),
            tbi_prev_iqr_low = quantile(Reduce("+",LTBP), 0.25)/sum(NUMP),
            tbi_prev_iqr_high = quantile(Reduce("+",LTBP), 0.75)/sum(NUMP),
            tbi_prev_ui_low = quantile(Reduce("+",LTBP), 0.025)/sum(NUMP),
            tbi_prev_ui_high = quantile(Reduce("+",LTBP), 0.975)/sum(NUMP))

View(tbi_iso3)

tbi_phl <- na.omit(tbi)%>%
  filter(ISO3=="PHL")%>%
  mutate(AGE_BANDS = ifelse(
    AGEP < 15, "0-14", ifelse(
      AGEP >= 15 & AGEP < 35, "15-34", ifelse(
        AGEP >= 35 & AGEP < 55, "35-54",ifelse(
          AGEP >= 55 & AGEP < 75, "55-74",ifelse(
            AGEP >= 75 & AGEP <= 200, "75+", "No value"))))))%>%
  group_by(ISO3, AGE_BANDS)%>%
  summarise(tbi_prev = median(Reduce("+",LTBP))/sum(NUMP),
            tbi_prev_iqr_low = quantile(Reduce("+",LTBP), 0.25)/sum(NUMP),
            tbi_prev_iqr_high = quantile(Reduce("+",LTBP), 0.75)/sum(NUMP),
            tbi_prev_ui_low = quantile(Reduce("+",LTBP), 0.025)/sum(NUMP),
            tbi_prev_ui_high = quantile(Reduce("+",LTBP), 0.975)/sum(NUMP))
View(tbi_phl)

#' This finds the median of all the hazards
#' DT[,  H.med := .(mapply(median, H, SIMPLIFY = T)),]
#' DT[is.na(H.med), H.med := 0, ]
#' 
#' quantile.func <- function (x) {
#'   quantile(x, probs = c(low.percentile))
#' }
#' 
#' #' This finds the desired low percentile of the same and puts it in H.low
#' DT[,  H.low := .(mapply(quantile.func, H, SIMPLIFY = T)),]
#' DT[is.na(H.low), H.low := 0, ]
#' 
#' quantile.func <- function (x) {
#'   quantile(x, probs = c(high.percentile))
#' }
#' 
#' This finds the desired high percentile of the same and puts it in H.high
# DT[,  H.high := .(mapply(quantile.func, H, SIMPLIFY = T)),]
# DT[is.na(H.high), H.high := 0, ]

ggplot(tbi_dist, aes(YARP, LTBP))+
  geom_histogram(stat = "identity")
View(tbi_dist)


ggplot(tbi_dist, aes(YARP, tbi_prev))+
  geom_line()
View(tbi_dist)

na.omit(tbi)%>%group_by(ISO3, YARP,YOBP)%>%summarize(min(YOBP))

tbi_rep <- na.omit(tbi)%>%filter(ISO3%in%c("PHL", "IND","CHN","USA","DEU","GBR", "VNM", "HKG","MAC"))%>%group_by(ISO3, YARP, NUMP)%>%mutate(AARP = YARP - YOBP)%>%summarize(tbi_prev_CAN = sum(LTBP)/sum(NUMP), med_yarp = median(YARP), med_aarp = median(AARP), med_age = median(AGEP))
View(tbi_rep)


View(na.omit(tbi)%>%
       summarise(TBI.prev = sum(LTBP)/sum(NUMP), TBI.prev.low = sum(LTBP.low)/sum(NUMP), TBI.prev.high = sum(LTBP.high)/sum(NUMP)))

View(na.omit(tbi)%>%
       summarise(TBI.prev=sum(NUMP)))
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

tbi_age_groups
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


  tbi_age_groups$inc_band_per100k <- factor(tbi_age_groups$inc_band_per100k, 
                                            levels = c("0-49",
                                                       "50-99",
                                                       "100-199",
                                                       "200+"))
tbi_age_groups%>%
  filter(YARP == "2016")%>%
  summarise(
  prop_arrive = sum(NUMP))

tbi_age_groups%>%
  filter(YARP == "2016" & inc_band_per100k == "200+")%>%
  summarise(
    prop_arrive = sum(LTBP)/sum(NUMP))

# YARP GROUPS ============================================================================
  tbi_playground <- tbi_age_groups
  # BY YARP ========================================================
 tbi_pg_plot <- na.omit(tbi_playground)%>%
    group_by(AGE_BANDS, inc_band_per100k, YARP)%>%
    summarise(tbi_prev = sum(LTBP)/sum(NUMP))

tbi_inc_plot <- na.omit(tbi_age_groups)%>%
  group_by(inc_band_per100k, YARP)%>%
  summarise(tbi_prev = sum(LTBP)/sum(NUMP), num_pop = sum(NUMP))
View(tbi_inc_plot)

# Year and Inc Band
ggplot( tbi_inc_plot, aes(YARP, tbi_prev, color = inc_band_per100k))+
  geom_smooth()+
  labs(
       x = "Year of Immigration to Canada",
       y = "TBI Prevalence (%, 95% CI)",
       color = "TB Disease Incidence \n in Country of Origin \n per 100,000 Persons")+
  theme(text = element_text(size = 12))+
  theme(legend.title = element_text(size = 10))

ggplot( tbi_inc_plot, aes(YARP, num_pop, color = inc_band_per100k))+
  geom_smooth(se = FALSE)+
  labs(title = "Number of Arrivals in Canada by Incidence Band",
       x = "Year of Arrival in Canada",
       y = "Number of Arrivals",
       color = "TB Disease Incidence \n in Country of Origin \n per 100,000 Persons")+
  theme(text = element_text(size = 16))+
  theme(legend.title = element_text(size = 13))

tbi_inc_plot2 <- na.omit(tbi_age_groups)%>%group_by(YARP, inc_band_per100k)%>%summarise(NUMP = sum(NUMP))
View(tbi_inc_plot2)
# %>%
#   group_by(ISO3, NUMP, LTBP)%>%
#   summarise(tbi_prev = sum(LTBP)/sum(NUMP))
View(tbi_inc_plot2)
ggplot(tbi_inc_plot2, aes(x = YARP, y = NUMP, color = inc_band_per100k))+
  geom_smooth(se = FALSE)+
  labs(x = "Year of Arrival in Canada",
       y = "Number of Arrivals",
       color = "Country of Origin")+
  theme(text = element_text(size = 16))
  #theme(legend.title = element_text(size = 13))

ggplot(tbi_inc_plot2, aes(x = YARP, y = NUMP, fill= inc_band_per100k))+
  geom_histogram(stat = "identity")+
  labs(x = "Year of Arrival in Canada",
       y = "Number of Arrivals",
       color = "Country of Origin")+
  theme(text = element_text(size = 16))

# # Top 5 Countries

# 
# ggplot(tbi_top5%>%group_by(AGE_BANDS, ISO3), aes(AGE_BANDS, fill = ISO3))+
#   geom_boxplot(aes(ymin = TBI.prev.low*1.5-TBI.prev.low, 
#                    lower = TBI.prev.low, 
#                    middle = tbi_prev, 
#                    upper = TBI.prev.high, 
#                    ymax = 1.5*TBI.prev.high),
#                stat = "identity")+
#   facet_wrap(.~ISO3)+
#   labs(title = "TBI Prevalence by Time Since Arrival and TB Disease Incidence in Country of Origin",
#        x = "Time Since Arrival",
#        y = "TBI Prevalence")+
#   theme(legend.position="none")

# Arrivals in last 2 years

tbi_yarp_2years <- na.omit(tbi_age_groups)%>%
  mutate(YARP_group = ifelse(YARP > 2014, "<2",
                             ifelse(YARP > 2011 & YARP <= 2014, "2-5",
                                    ifelse(YARP > 2006 & YARP <= 2011,"5-10",
                                           ifelse(YARP < 2007,">10", "No value")))))

tbi_inc_plot2 <- na.omit(tbi_yarp_2years)%>%
  group_by(inc_band_per100k, YARP_group, AGE_BANDS)%>%
  summarise(tbi_prev = sum(LTBP)/sum(NUMP),
            TBI.prev.low = sum(LTBP.low)/sum(NUMP), 
            TBI.prev.high = sum(LTBP.high)/sum(NUMP))
View(tbi_inc_plot2)



tbi_inc_plot2$YARP_group <- factor(tbi_inc_plot2$YARP_group, 
levels = c("<2", "2-5","5-10", ">10"))

View(tbi_inc_plot2)


# Boxplot by Inc
ggplot(tbi_inc_plot2, aes(YARP_group, fill = AGE_BANDS))+
  geom_boxplot(aes(ymin = TBI.prev.low*1.5-TBI.prev.low, 
                   lower = TBI.prev.low, 
                   middle = tbi_prev, 
                   upper = TBI.prev.high, 
                   ymax = ifelse(1.5*TBI.prev.high > 1, 1, 1.5*TBI.prev.high)),
               stat = "identity")+
  facet_wrap(.~inc_band_per100k)+
  labs(
       x = "Time Since Arrival (Years)",
       y = "TBI Prevalence ( %, IQR)",
       fill = "TB Disease Incidence \n in Country of Origin \n per 100,000 Persons")+
  scale_fill_brewer(palette = "Set3")+
  theme(text = element_text(size = 16))+
  theme(legend.text = element_text(size = 12))+
  theme(axis.text = element_text(size = 12))
  

# Boxplot by ISO3
tbi_top5 <- na.omit(tbi_age_groups)%>%
  group_by(ISO3, inc_band_per100k)%>%
  summarise(tbi_prev = sum(LTBP)/sum(NUMP), 
            TBI.prev.low = sum(LTBP.low)/sum(NUMP), 
            TBI.prev.high = sum(LTBP.high)/sum(NUMP), 
            NUMP_v2 = sum(NUMP))%>%
  filter(NUMP_v2 > 100000)%>%
  arrange(NUMP_v2, tbi_prev)


tbi_top5 <- top_n(tbi_top5, 10)
tbi_top5 <- tbi_top5%>%arrange(desc(NUMP_v2))
tbi_top5$ISO3 <- factor(tbi_top5$ISO3, levels = c("IND", "CHN","PHL", "GBR","USA","ITA","HKG","PAK","VNM","IRN"))
View(tbi_top5)

ggplot(tbi_top5, aes(ISO3, fill = ISO3))+
  geom_boxplot(aes(ymin = TBI.prev.low*1.5-TBI.prev.low, 
                   lower = TBI.prev.low, 
                   middle = tbi_prev, 
                   upper = TBI.prev.high, 
                   ymax = 1.5*TBI.prev.high),
               stat = "identity")+
  labs(x= "Country of Origin",
       y = "TBI Prevalence (%, IQR)",
       color = "Age Group")+
  theme(legend.position="none")+
  theme(text = element_text(size = 16))+
  theme(legend.text = element_text(size = 12))+
  theme(axis.text = element_text(size = 12))

#============LINE GRAPH================
View(na.omit(tbi_age_groups))

tbi_top10 <- na.omit(tbi_age_groups)%>%
  filter(ISO3%in%c("IND", "CHN","PHL", "GBR","USA","ITA","HKG","PAK","VNM","IRN"))%>%
  mutate(tbi_prev = LTBP/NUMP)

View(tbi_top10)

ggplot(na.omit(tbi_top10), aes(x = YARP, y = tbi_prev, color = ISO3))+
  geom_smooth()+
  labs(x= "Year of Immigration to Canada",
       y = "TBI Prevalence (%, IQR)",
       color = "Country of Origin \n(ISO3)")+
  theme(text = element_text(size = 12))+
  theme(legend.text = element_text(size = 10))+
  theme(axis.text = element_text(size = 10))

#Top 5
tbi_top5 <- na.omit(tbi_age_groups)%>%group_by(ISO3)%>%summarise(tbi_prev = sum(LTBP)/sum(NUMP), TBI.prev.low = sum(LTBP.low)/sum(NUMP), TBI.prev.high = sum(LTBP.high)/sum(NUMP), NUMP_v2 = sum(NUMP))%>%filter(NUMP_v2 > 100000 & tbi_prev > 0.28)%>%arrange(NUMP_v2, tbi_prev)

ggplot(tbi_top5, aes(ISO3, fill = ISO3))+
  geom_boxplot(aes(ymin = TBI.prev.low*1.5-TBI.prev.low, 
                   lower = TBI.prev.low, 
                   middle = tbi_prev, 
                   upper = TBI.prev.high, 
                   ymax = 1.5*TBI.prev.high),
               stat = "identity")+
  labs(x = "Country of Origin",
       y = "TBI Prevalence (%,IQR)")+
  theme(legend.position="none")+
  theme(text = element_text(size = 16))+
  theme(legend.text = element_text(size = 12))+
  theme(axis.text = element_text(size = 12))

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
 