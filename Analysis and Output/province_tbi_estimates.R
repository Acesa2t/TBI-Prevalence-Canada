# Analyzing provincial level prevalence datta

rm(list = ls(all.names = TRUE))
gc()

library(spatstat)
library(tidyverse)

# Read desired provincial file in
tbi <- readRDS("Analysis and Output/census_other_provinces_v1.rds")
#View(tbi)

# Ensure that no extra rows made it into analysis
tbi <- tbi%>%filter(NUMP > 0 | ISO3 != "CAN")%>%filter(YARP >= YOBP)

# View(tbi%>%
# group_by(ISO3)%>%
#   summarise(NUMP_coo = sum(NUMP)))

ltbp_set <- do.call(rbind.data.frame, tbi$LTBP)
colnames(ltbp_set) <- c(1:200)
#View(ltbp_set)
 
 ltbp_summary <- data.frame(t(ltbp_set%>%summarise(across(everything(), sum))))
 colnames(ltbp_summary) <- c("LTBP_tot")
 class(ltbp_summary)
 median(ltbp_summary$LTBP_tot)/437340
 quantile(ltbp_summary$LTBP_tot, 0.025)/437340
 quantile(ltbp_summary$LTBP_tot, 0.975)/437340
 tbi%>%summarize(sum(NUMP))

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

# Median age during census year and at immigration
weighted.median(tbi_age_groupings$AGEP, tbi_age_groupings$NUMP)

weighted.quantile(tbi_age_groupings$AGEP, tbi_age_groupings$NUMP, probs = c(0.25, 0.75))

weighted.median(tbi_age_groupings$AARP, tbi_age_groupings$NUMP)
 
weighted.quantile(tbi_age_groupings$AARP, tbi_age_groupings$NUMP, probs = c(0.25, 0.75))

aarp_vec <- c("AARP 0-14", "AARP 15-34", "AARP 35-54", "AARP 55-74", "AARP 75+")
  
# Print TBI estimates for every age group
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
  
  print(str_c("Age group", aarp_vec[i]))
  print(median(ltbp_summary$LTBP_tot)/tbi_age_groups%>%summarize(sum(NUMP)))
  print(quantile(ltbp_summary$LTBP_tot, 0.025)/ tbi_age_groups%>%summarize(sum(NUMP)))
  print(quantile(ltbp_summary$LTBP_tot, 0.975)/ tbi_age_groups%>%summarize(sum(NUMP)))
  
}
