# Working directory will be /Data
library(tidyverse)
tbi_2001 <- readRDS("estimates_2001_v4.rds")
View(tbi_2001)

hist_graph <- tbi_2001%>%
  mutate(AGE_BANDS_CHECK = ifelse(
    AGEP < 15, "0-14", ifelse(
      AGEP >= 15 & AGEP < 25, "15-24", ifelse(
        AGEP >= 25 & AGEP < 55, "25-54",ifelse(
          AGEP >= 55 & AGEP < 65, "55-64",ifelse(
            AGEP >= 65 & AGEP <= 200, "65+", "No value"))))))%>%
  group_by(AGE_BANDS_CHECK)

weighted.median(hist_graph$AGEP, hist_graph$NUMP) 
weighted.quantile(hist_graph$AGEP, hist_graph$NUMP, probs = c(0.25, 0.75))

# Find total TBI prev among 2001 arrivals
na.omit(tbi_2001)%>%summarise(tbi_tot = sum(NUMP))

View(na.omit(tbi_2001%>%group_by(AGEP,YARP,ISO3)%>%filter(ISO3%in%c("PRT","RUS","RWA"))%>%summarise(tbi_tot = max(sum(LTBP)/sum(NUMP)))))

View(na.omit(tbi_2001)%>%group_by(ISO3, )%>%summarise(tbi_tot = median(LTBP)/median(NUMP)))

View(na.omit(tbi_2001)%>%group_by(ISO3, WHO_R)%>%summarise(tbi_tot = median(NUMP)))

tbi_dist <- na.omit(tbi_2001)%>%filter(ISO3 == "IND")%>%arrange(YARP)
ggplot(tbi_dist, aes(YARP, LTBP))+
  geom_histogram(stat = "identity")

tbi_dist <- na.omit(tbi_2001)%>%
  filter(ISO3 == "IND")%>%
  group_by(YARP)%>%
  summarise(tbi_prev = sum(LTBP)/sum(NUMP))

ggplot(tbi_dist, aes(YARP, tbi_prev))+
  geom_line()

View(tbi_dist)


tbi_est <- tbi_2001

# Merge incidence bands based on 2016 incidence
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
          AGEP >= 55 & AGEP <= 74, "55-74", ifelse(
            AGEP >= 75 & AGEP <= 200, "75+", "No value"))))))


# tbi_age_groups <- tbi_data_bands%>%
#   mutate(AGE_BANDS = ifelse(
#     AGEP < 15, "0-14", ifelse(
#       AGEP >= 15 & AGEP < 35, "15-34", ifelse(
#         AGEP >= 35 & AGEP < 65, "35-65",ifelse(
#           AGEP >= 65 & AGEP <= 200, "65+", "No value")))))

View(na.omit(tbi_age_groups)%>%
       group_by(AGE_BANDS, YARP)%>%
       filter(ISO3=="PRT"))%>%
       summarise(sum(LTBP)/sum(NUMP), 
                 sum(LTBP.low)/sum(NUMP), 
                 sum(LTBP.high)/sum(NUMP)))


#View(tbi_age_groups)%>%
#      filter(AGE_BANDS== "No value"))
View(na.omit(tbi_age_groups)%>%group_by(ISO3, AGE_BANDS, YARP)%>%summarise(med_tbi_tot = median(LTBP)/median(NUMP))%>%filter(ISO3%in%c("PRT","RUS")& AGE_BANDS == "0-14"))
View(na.omit(tbi_age_groups)%>%group_by(ISO3, AGE_BANDS, YARP)%>%summarise(sum_tbi_tot = sum(LTBP)/sum(NUMP))%>%filter(ISO3%in%c("PRT","RUS") & AGE_BANDS == "75+"))
View(na.omit(tbi_age_groups)%>%group_by(ISO3, AGE_BANDS)%>%summarise(sum_tbi_tot = mean(LTBP)/mean(NUMP))%>%filter(ISO3%in%c("PRT","RUS") & AGE_BANDS == "75+"))



View(na.omit(tbi_age_groups)%>%group_by(WHO_R)%>%summarise(med_tbi_tot = median(LTBP)/median(NUMP)))
View(na.omit(tbi_age_groups)%>%group_by(WHO_R)%>%summarise(sum_tbi_tot = sum(LTBP)/sum(NUMP)))
# YARP ========================================================================================

tbi_age_groups%>%
  filter(YARP >= 2011)%>%
  summarise(sum(NUMP))

tbi_age_groups%>%
  summarise(sum(NUMP))

1200795/7249345


View(na.omit(tbi_age_groups)%>%
       group_by(AGE_BANDS, YARP,ISO3)%>%
       filter(ISO3%in%c("PRT","RUS","RWA"))%>%
       summarise(sum(LTBP)/sum(NUMP), 
                 sum(NUMP)))

View(na.omit(tbi_age_groups)%>%
       group_by(AGE_BANDS, YARP)%>%
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


