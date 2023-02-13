# Document used to add Age at Arrival (AARP) column, add WHO regions, eventually incidence bands will be added as well. Unnecessary columns also removed

rm(list = ls(all.names = TRUE))
gc()

library(spatstat)
library(tidyverse)

# Read file in
tbi <- readRDS("2011estimates_v3.rds")
#View(tbi)
#write.csv(tbi,"/Users/ajordan/OneDrive - McGill University/LTBI-Aust-CEA-master/Data/Outputs/tbi_est.csv", row.names = FALSE)

tbi <- tbi%>%
  filter(ISO3=='AND' & AGEP > 0 & AGEP < 15)

Reduce("+",tbi$LTBP)/tbi$NUMP

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


View(na.omit(tbi)%>%summarise(sum(LTBP)/sum(NUMP), TBI.prev.low = sum(LTBP.low)/sum(NUMP), TBI.prev.high = sum(LTBP.high)/sum(NUMP)))
View(na.omit(tbi)%>%summarise(sum(NUMP)))
# DATA CHECK=======================================================================================================

tbi <- na.omit(tbi)%>%mutate(AARP = YARP-YOBP)

vector_test <- c("PHL", "IND","CHN","USA","DEU","GBR", "VNM", "HKG","MAC")

tbi.func.test <- function(df, vec){
  df3 <- data.frame(ISO3 = character(), med = numeric())
  
  for(i in 1:length(vec)){
    df3[i,1] <- vec[i]
    df2 <- subset(df,df$ISO3==vec[i])
    df3[i,2]<-weighted.median(df2$AARP, df2$NUMP)
  }    
  print(df3)
}


View(tbi.func.test(df=tbi, vec=vector_test))


by_country <- na.omit(tbi)%>%
  group_by(ISO3)%>%
  summarise(TBI.prev = sum(LTBP)/sum(NUMP), TBI.prev.low = sum(LTBP.low)/sum(NUMP), TBI.prev.high = sum(LTBP.high)/sum(NUMP))


View(na.omit(tbi)%>%group_by(ISO3)%>%filter(ISO3%in%c("PHL", "IND","CHN","USA","DEU","GBR", "VNM", "HKG","MAC"))%>%mutate(AARP = YARP - YOBP)%>%summarize(tbi_prev_AUS = sum(LTBP)/sum(NUMP), med_yarp = median(YARP), med_aarp = median(AARP), med_age = median(AGEP)))


View(na.omit(tbi)%>%group_by(ISO3)%>%filter(ISO3%in%c("PHL", "IND","CHN","USA","DEU","GBR", "VNM", "HKG","MAC"))%>%summarize(sum(LTBP)/sum(NUMP)))


View(by_country)


View(na.omit(tbi)%>%
       filter(ISO3%in%c("PHL", "USA", "CHN", "IND", "DEU", "AUS"))%>%
       group_by(ISO3)%>%
       summarise(sum(LTBP)/sum(NUMP), sum(LTBP.low)/sum(NUMP), sum(LTBP.high)/sum(NUMP)))

View(na.omit(tbi)%>%
       filter(ISO3%in%c("PHL", "USA", "CHN", "IND", "DEU", "AUS") & YARP%in%c(1890, 1931, 1950, 1980, 1990, 2000, 2014))%>%
       group_by(ISO3,YARP)%>%
       summarise(sum(LTBP)/sum(NUMP), sum(LTBP.low)/sum(NUMP), sum(LTBP.high)/sum(NUMP)))

tbi_est <- tbi

# INCIDENCE BANDS====================================================================================================

# Merge incidence bands
tb_import <- read.csv("TB burden country copy.csv")
tb_inc  <- tb_import%>%filter(year == 2016)%>%select(iso3,  e_inc_100k)
colnames(tb_inc) <- c("ISO3", "e_inc_100k")
View(tb_inc)

tbi_data <- merge(tbi_est, tb_inc, by = "ISO3")
#View(tbi_data)

# Less bands
tbi_data_bands <- tbi_data%>%
  mutate(inc_band_per100k = ifelse(
    e_inc_100k < 50, "0-49", ifelse(
      e_inc_100k >= 50 & e_inc_100k < 100, "50-99", ifelse(
        e_inc_100k >= 100 & e_inc_100k < 200, "100-199",ifelse(
          e_inc_100k >= 200 & e_inc_100k <= 10000, "200+", "No value")))))


View(na.omit(tbi_data_bands)%>%
       group_by(inc_band_per100k)%>%
       summarise(sum(LTBP)/sum(NUMP), sum(LTBP.low)/sum(NUMP), sum(LTBP.high)/sum(NUMP)))


inc_bands_narrow <- na.omit(tbi_data_bands)%>%
       filter(inc_band_per100k == "50-99" | inc_band_per100k == "100-199")%>%
       group_by(ISO3, inc_band_per100k,YARP)%>%
       summarise(tbi_prev = sum(LTBP)/sum(NUMP), sum(LTBP.low)/sum(NUMP), sum(LTBP.high)/sum(NUMP))
View(inc_bands_narrow)

aa<- na.omit(tbi_data_bands)%>%
  filter(!ISO3%in%c("CHN", "ROU"))%>%
  group_by(inc_band_per100k)%>%
  summarise(tbi_prev = sum(LTBP)/sum(NUMP), sum(LTBP.low)/sum(NUMP), sum(LTBP.high)/sum(NUMP))
View(aa)

View(unique(na.omit(tbi_data_bands)%>%
       filter(e_inc_100k >= 70 & e_inc_100k < 100)%>%
  select(ISO3,e_inc_100k)))


# AGE GROUPS (AGE AT CENSUS)=====================================================================

tbi_age_groups <- tbi_data_bands%>%
  mutate(AGE_BANDS = ifelse(
    AGEP < 15, "0-14", ifelse(
      AGEP >= 15 & AGEP < 35, "15-34", ifelse(
        AGEP >= 35 & AGEP < 55, "35-54",ifelse(
          AGEP >= 55 & AGEP <= 75, "55-74", ifelse(
            AGEP >= 75 & AGEP <= 200, "75+", "No value"))))))


# tbi_age_groups <- tbi_data_bands%>%
#   mutate(AGE_BANDS = ifelse(
#     AGEP < 15, "0-14", ifelse(
#       AGEP >= 15 & AGEP < 35, "15-34", ifelse(
#         AGEP >= 35 & AGEP < 65, "35-65",ifelse(
#           AGEP >= 65 & AGEP <= 200, "65+", "No value")))))

View(na.omit(tbi_age_groups)%>%
       group_by(AGE_BANDS)%>%
       summarise(sum(LTBP)/sum(NUMP), 
                 sum(LTBP.low)/sum(NUMP), 
                 sum(LTBP.high)/sum(NUMP)))


#View(tbi_age_groups)%>%
 #      filter(AGE_BANDS== "No value"))


# YARP ========================================================================================

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
       group_by(AGE_BANDS, YARP)%>%
       filter("PRT")%>%
       summarise(sum(LTBP)/sum(NUMP), 
                 sum(LTBP.low)/sum(NUMP), 
                 sum(LTBP.high)/sum(NUMP)))




# PREV VS INC PLOT =======================================================================
View(tbi_age_groups)
tb_v_tbi <- unique(na.omit(tbi_age_groups)%>%
                     group_by(ISO3)%>%
                     summarize(tbi_prev = sum(LTBP)/sum(NUMP),
                               e_inc_100k = e_inc_100k))
  View(tb_v_tbi)
  
  tb_v_tbi%>%ggplot(aes(tbi_prev, e_inc_100k, color = ISO3)) + 
  geom_point() +
  theme(legend.position = "none")
  
  unique(tbi_age_groups%>%filter(e_inc_100k > 1000)%>%select(ISO3))
# YARP GROUPS ============================================================================
  tbi_yarp <- tbi_age_groups%>%
    mutate(YARP_group = ifelse(YARP <= 1970, "<= 1970",
                               ifelse(YARP >=1971&YARP<=1980, "1971-1980",
                                      ifelse(YARP>=1981&YARP<=1990,"1981-1990",
                                             ifelse(YARP>=1991&YARP<=2000,"1991-2000",
                                                    ifelse(YARP>=2001&YARP<=2011, "2001-2011", "No value"))))))
  
  View(na.omit(tbi_yarp)%>%
         group_by(YARP_group)%>%
         summarise(sum(LTBP)/sum(NUMP), 
                   sum(LTBP.low)/sum(NUMP), 
                   sum(LTBP.high)/sum(NUMP)))
  # BY YARP ========================================================
 tbi_pg_plot <- tbi_playground%>%
    filter(inc_band_per100k%in%c("100-199", "200+"))%>%
    group_by(AGE_BANDS, inc_band_per100k, YARP)%>%
    summarise(tbi_prev = sum(LTBP)/sum(NUMP))

 
 View(tbi_pg_plot)
 
 ggplot(tbi_pg_plot, aes(YARP, tbi_prev, color = inc_band_per100k))+
          geom_line()+
   geom_vline(xintercept = 1980, linetype = "dashed")+
   facet_wrap(.~AGE_BANDS)
        
 ggplot(tbi_pg_plot, aes(YARP, tbi_prev, color = AGE_BANDS))+
   geom_line()+
   geom_vline(xintercept = 1980)+
   facet_wrap(.~inc_band_per100k)



# saveRDS(tbi_playground, file = "/Users/ajordan/OneDrive - McGill University/LTBI-Aust-CEA-master/Data/Outputs/tbi_playground.rds") 
 
 # CAN vs AUS=====================================================================
 aus_tbi <- read.csv("/Users/ajordan/OneDrive - McGill University/LTBI-Aust-CEA-master/Data/Outputs/AUSestimates_10.csv")
 
 
 View(aus_tbi%>%summarise(sum(NUMP)))
 aus_tbi_sum <- na.omit(aus_tbi%>%select(-poptb, -year))%>%
   summarise(TBI.prev = sum(LTBP), TBI.prev.low = sum(LTBP.low)/sum(NUMP), TBI.prev.high = sum(LTBP.high)/sum(NUMP))
 
 View(aus_tbi_sum)
 
 #TBI.prev.x = CAN, TBI.prev.y = AUS
 tbi_tog <- merge(by_country, aus_tbi_sum, by = "ISO3")
 View(tbi_tog%>%select(ISO3, TBI.prev.x, TBI.prev.y)%>%
        mutate(abs.diff =TBI.prev.x- TBI.prev.y))
 
 # ADD AARP AND INCIDENCE BAND SHIZZLE===================================================================================
 
 #saveRDS(read.csv("estimates_v3.csv"), file = "/Users/ajordan/OneDrive - McGill University/LTBI-Aust-CEA-master/Data/census_v4.rds")
 
 #Adding age at arrival column
 tbi_est <- na.omit(tbi%>%
                      mutate(AARP = YARP - YOBP))
 
 
 length(unique(tbi_est$ISO3))
 
 
 
 #View(tbi_data_bands%>%filter(inc_band_per100k == "No value"))
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
 
 
 # tbi_data_bands <- tbi_data%>%
 #   mutate(inc_band_per100k = ifelse(
 #     e_inc_100k < 30, "0-30", ifelse(
 #       e_inc_100k >= 30 & e_inc_100k < 90, "30-89", ifelse(
 #         e_inc_100k >= 90 & e_inc_100k < 200, "90-199",ifelse(
 #           e_inc_100k >= 200 & e_inc_100k <= 10000, "200+", "No value")))))
 
 
 # View(na.omit(tbi_data_bands)%>%
 #        filter(e_inc_100k >= 150 & e_inc_100k <=200)%>%
 #        group_by(inc_band_per100k, ISO3)%>%
 #        summarise(sum(LTBP)/sum(NUMP), 
 #                  sum(LTBP.low)/sum(NUMP), 
 #                  sum(LTBP.high)/sum(NUMP)))
 
 #==================================================================================
 #write.csv(by_country, file = "/Users/ajordan/OneDrive - McGill University/LTBI-Aust-CEA-master/Data/Outputs/tbi_country.csv")