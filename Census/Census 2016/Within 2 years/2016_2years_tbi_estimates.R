# Document used to add Age at Arrival (AARP) column, add WHO regions, eventually incidence bands will be added as well. Unnecessary columns also removed

rm(list = ls(all.names = TRUE))
gc()

library(spatstat)
library(tidyverse)
library(zoo)
library(data.table)
# Population excludes people from countries that were not within the 168 that had ARI trajectories
# Alberta total population: 839439.5, median LTBP: 202993, UR 2.5%: 172818, UR 97.5%: 231094, overall TBI-prev: 24% (21-28)

# BC population: 1252290, median LTBP: 299373, UR 2.5%: 243768, UR 97.5%: 368858, overall TBI prev: 24 (20-29)

# Ontario population 3828025, median LTBP: 826883, UR 2.5%: 699872, UR 97.5%: 976312, overall TBI prev: 22 (18-26)

# Quebec population 1086015, median LTBP: 210645.7, UR 2.5%: 176295.6, UR 97.5%: 258565.3, overall TBI prev: 19 (16-24)

# Other provinces

# Read file in
tbi <- readRDS("/Users/ajordan/Library/CloudStorage/OneDrive-McGillUniversity/LTBI-Aust-CEA-master/Data/2016-2 years estimates_v4.rds")
#View(tbi)

#write.csv(tbi,"/Users/ajordan/OneDrive - McGill University/LTBI-Aust-CEA-master/Data/Outputs/tbi_est.csv", row.names = FALSE)
# tbi <- tbi%>%filter(NUMP > 0 | ISO3 != "CAN")%>%filter(YARP >= YOBP)
# 
#View(tbi%>%
#       filter(YARP%in%c(2015,2016)))

# tbi_prob_null <- tbi%>%
#        select(-CNSY)%>%
#        filter(YARP%in%c(2015,2016))%>%
# group_by(ISO3, AGEP, YARP)%>%
#   mutate(PROB.med = ifelse(YOBP%in%c(2015,2016), PROB.med, NA),
#          PROB.low = ifelse(YOBP%in%c(2015,2016), PROB.low, NA),
#          PROB.high = ifelse(YOBP%in%c(2015,2016), PROB.high, NA))%>%
#   arrange(YARP)
# 
# tbi_prob_null$PROB.med <- na.locf(tbi_prob_null$PROB.med, fromLast = T)
# tbi_prob_null$PROB.low <- na.locf(tbi_prob_null$PROB.low, fromLast = T)
# tbi_prob_null$PROB.high <- na.locf(tbi_prob_null$PROB.high, fromLast = T)
# 
# View(tbi_prob_null)

tbi_ltbp <- tbi%>%
  mutate(LTBP.med = NUMP*PROB.med,
         LTBP.low = NUMP*PROB.low,
         LTBP.high = NUMP*PROB.high)%>%
  mutate(AGE_BANDS = ifelse(
    AGEP < 15, "0-14", ifelse(
      AGEP >= 15 & AGEP < 35, "15-34", ifelse(
        AGEP >= 35 & AGEP < 55, "35-54",ifelse(
          AGEP >= 55 & AGEP < 75, "55-74",ifelse(
            AGEP >= 75 & AGEP <= 200, "75+", "No value"))))),
    AARP = YARP-YOBP)%>%
  mutate(AARP_BANDS = ifelse(
    AARP < 15, "AARP 0-14", ifelse(
      AARP >= 15 & AARP < 35, "AARP 15-34", ifelse(
        AARP >= 35 & AARP < 55, "AARP 35-54",ifelse(
          AARP >= 55 & AARP <= 74, "AARP 55-74", ifelse(
            AARP >= 75 & AARP <= 200, "AARP 75+", "No value"))))))

View(tbi_ltbp)

tb_inc <- readRDS("/Users/ajordan/Library/CloudStorage/OneDrive-McGillUniversity/LTBI-Aust-CEA-master/Data/incidence.rds")

colnames(tb_inc) <- c("ISO3", "WHO_Region", "e_inc_100k")
tb_inc <- tb_inc%>%select(-WHO_Region)
head(tb_inc)

tbi_data <- merge(tbi_ltbp, tb_inc, by = "ISO3")

tbi_prev_aarp_2years <- tbi_data%>%
  filter(YARP%in%c(2015, 2016))%>%
  filter(NUMP > 0 & !is.na(NUMP))%>%
  filter(ISO3!="CAN")%>%
  mutate(inc_band_per100k = ifelse(
    e_inc_100k < 10, "0-9", ifelse(
      e_inc_100k >= 10 & e_inc_100k < 50, "10-49", ifelse(
        e_inc_100k >= 50 & e_inc_100k < 100, "50-99", ifelse(
          e_inc_100k >= 100 & e_inc_100k < 200, "100-199",ifelse(
            e_inc_100k >= 200 & e_inc_100k <= 1000, "200+", "No value"))))))%>%
  group_by(inc_band_per100k, AARP_BANDS)%>%
  summarise(tbi_prev_median = sum(LTBP.med)/sum(NUMP),
            tbi_prev_low = sum(LTBP.low)/sum(NUMP),
            tbi_prev_high = sum(LTBP.high)/sum(NUMP))


View(tbi_prev_aarp_2years)
# %>%
#   mutate(inc_band_per100k = ifelse(
#     e_inc_100k < 10, "0-9 per 100,000 persons", ifelse(
#       e_inc_100k >= 10 & e_inc_100k < 50, "10-49 per 100,000 persons", ifelse(
#         e_inc_100k >= 50 & e_inc_100k < 100, "50-99 per 100,000 persons", ifelse(
#           e_inc_100k >= 100 & e_inc_100k < 200, "100-199 per 100,000 persons",ifelse(
#             e_inc_100k >= 200 & e_inc_100k <= 1000, "200+ per 100,000 persons", "No value"))))))
  
tbi_prev_aarp_2years$inc_band_per100k <- factor(tbi_prev_aarp_2years$inc_band_per100k,
                                            levels = c("0-9",
                                                       "10-49",
                                                       "50-99",
                                                       "100-199",
                                                       "200+"))


