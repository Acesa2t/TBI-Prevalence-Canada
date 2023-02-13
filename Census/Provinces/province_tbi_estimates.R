# Document used to add Age at Arrival (AARP) column, add WHO regions, eventually incidence bands will be added as well. Unnecessary columns also removed

rm(list = ls(all.names = TRUE))
gc()

library(spatstat)
library(tidyverse)
# Population excludes people from countries that were not within the 168 that had ARI trajectories
# Alberta total population: 839439.5, median LTBP: 202993, UR 2.5%: 172818, UR 97.5%: 231094, overall TBI-prev: 24% (21-28)

# BC population: 1252290, median LTBP: 299373, UR 2.5%: 243768, UR 97.5%: 368858, overall TBI prev: 24 (20-29)

# Ontario population 3828025, median LTBP: 826883, UR 2.5%: 699872, UR 97.5%: 976312, overall TBI prev: 22 (18-26)

# Quebec population 1086015, median LTBP: 210645.7, UR 2.5%: 176295.6, UR 97.5%: 258565.3, overall TBI prev: 19 (16-24)

# Other provinces

# Read file in
tbi <- readRDS("/Users/ajordan/Library/CloudStorage/OneDrive-McGillUniversity/LTBI-Aust-CEA-master/Data/census_other_provinces_v1.rds")
#View(tbi)

#write.csv(tbi,"/Users/ajordan/OneDrive - McGill University/LTBI-Aust-CEA-master/Data/Outputs/tbi_est.csv", row.names = FALSE)
# tbi <- tbi%>%filter(NUMP > 0 | ISO3 != "CAN")%>%filter(YARP >= YOBP)
# 
# 
# View(tbi%>%
# group_by(ISO3)%>%
#   summarise(NUMP_coo = sum(NUMP)))
# 
# ltbp_set <- do.call(rbind.data.frame, tbi$LTBP)
# colnames(ltbp_set) <- c(1:200)
# #View(ltbp_set)
# 
# ltbp_summary <- data.frame(t(ltbp_set%>%summarise(across(everything(), sum))))
# colnames(ltbp_summary) <- c("LTBP_tot")
# #class(ltbp_summary)
# median(ltbp_summary$LTBP_tot)/437340
# quantile(ltbp_summary$LTBP_tot, 0.025)/437340
# quantile(ltbp_summary$LTBP_tot, 0.975)/437340
# tbi%>%summarize(sum(NUMP))

# Age groups
  
tbi_age_groupings <- tbi%>%
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

# weighted.median(tbi_age_groupings$AGEP, tbi_age_groupings$NUMP)
# 
# weighted.quantile(tbi_age_groupings$AGEP, tbi_age_groupings$NUMP, probs = c(0.25, 0.75))
# 
# weighted.median(tbi_age_groupings$AARP, tbi_age_groupings$NUMP)
# 
# weighted.quantile(tbi_age_groupings$AARP, tbi_age_groupings$NUMP, probs = c(0.25, 0.75))

aarp_vec <- c("AARP 0-14", "AARP 15-34", "AARP 35-54", "AARP 55-74", "AARP 75+")
  
for(i in 1:5){
  
  
  tbi_age_groups <- tbi_age_groupings%>%
    filter(NUMP > 0 | ISO3 != "CAN")%>%
    filter(YARP >= YOBP)
  
  tbi_age_groups <- subset(tbi_age_groups, AARP_BANDS==aarp_vec[i])
  
  #View(tbi_age_groups)
  
  ltbp_set <- do.call(rbind.data.frame, tbi_age_groups$LTBP)
  colnames(ltbp_set) <- c(1:200)
  View(ltbp_set)
  
  ltbp_summary <- data.frame(t(ltbp_set%>%summarise(across(everything(), sum))))
  colnames(ltbp_summary) <- c("LTBP_tot")
  View(ltbp_summary)
  # class(ltbp_summary)
  # median(ltbp_summary$LTBP_tot)
  # quantile(ltbp_summary$LTBP_tot, 0.025)
  # quantile(ltbp_summary$LTBP_tot, 0.975)
  # tbi%>%summarize(sum(NUMP))
  
  print(str_c("Age group", aarp_vec[i]))
  print(median(ltbp_summary$LTBP_tot)/tbi_age_groups%>%summarize(sum(NUMP)))
  print(quantile(ltbp_summary$LTBP_tot, 0.025)/ tbi_age_groups%>%summarize(sum(NUMP)))
  print(quantile(ltbp_summary$LTBP_tot, 0.975)/ tbi_age_groups%>%summarize(sum(NUMP)))
  View(tbi_age_groups%>%
         group_by(ISO3)%>%
         summarise(NUMP_coo = sum(NUMP))%>%
         arrange(desc(NUMP_coo)))
  
}
