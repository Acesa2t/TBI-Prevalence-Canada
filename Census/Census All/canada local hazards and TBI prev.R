# Canada local hazards and TBI prevalence


# HAZARDS ==============================================================
# 2001 all hazards
prob_2001year <- readRDS("2001.prob.Inf_v4.rds")
prob_2001year_can <- prob_2001year%>%filter(ISO3 == "CAN")%>%select(YOBP, PROB.med, PROB.low, PROB.high)

# 2006 all hazards
prob_2006year <- readRDS("2006.prob.Inf_v4.rds")
prob_2006year_can <- prob_2006year%>%filter(ISO3 == "CAN")%>%select(YOBP, PROB.med, PROB.low, PROB.high)

# 2011 all hazards
prob_2011 <- readRDS("2011.prob.Inf_v3.rds")
prob_2011_can <- prob_2011%>%filter(ISO3 == "CAN")%>%select(YOBP, PROB.med, PROB.low, PROB.high)
prob_2011_can
# 2016 all hazards
prob_2016 <- readRDS("2016.prob.Inf_v3.rds")
prob_2016_can <- prob_2016%>%filter(ISO3 == "CAN")%>%select(YOBP, PROB.med, PROB.low, PROB.high)
prob_2016_can

# CANADA POPULATION ========================================================
# Will need to be by age groups, then mutated into YOBP


rep_dist_age <- function(df, census_year){
  df_v2 <- df[rep(row.names(df), 
                  ifelse(df$AGE_BANDS=="0-14",15,
                         ifelse(df$AGE_BANDS == "15-24", 10,
                                ifelse(df$AGE_BANDS == "25-54", 30, 
                                       ifelse(df$AGE_BANDS == "55-64", 10,  ifelse(df$AGE_BANDS == "65+", 21, 0                                                                                                )))))),]
  
  View(df_v2)
  
  # Add sequence of age groups
  cborn_dist_age <- df_v2%>%mutate(
    AGEP = ifelse(AGE_BANDS == "0-14",
                  seq(from = 0, to = 14), 
                  ifelse(AGE_BANDS == "15-24",
                         seq(from = 15, to = 24),
                         ifelse(AGE_BANDS == "25-54",
                                seq(from = 25, to = 54),
                                ifelse(AGE_BANDS == "55-64",
                                       seq(from = 55, to = 64), 
                                       ifelse(AGE_BANDS == "65+",
                                              seq(from = 65, to = 85),
                                              AGE_BANDS
                                              
                                       )))
                  )))
  
  cborn_dist_age$AGEP <- as.numeric(cborn_dist_age$AGEP)
  View(cborn_dist_age)
  
  nrow(cborn_dist_age)
  cborn_dist_age_v2 <- cborn_dist_age%>%group_by(AGE_BANDS)%>%
    mutate(NUMP = `Canadian citizens only`/max(row_number()))%>%
    mutate(YOBP = census_year - AGEP)%>%
    select(-`Canadian citizens only`)
  
  cborn_dist_age_v2
  
}




# FUNCTION===============
# 2001
cborn_2001year <- read_csv("2001 total canada.CSV", skip = 2)

cborn_2001year_age_restrict <-cborn_2001year%>%
  filter(`Age (122)`%in%c("0-4","5-9", 
                                "10-14", "15-19", 
                                "20-24", "25-29", 
                                "30-34","35-39",
                                "40-44", "45-49",
                                "50-54","55-59",
                                "60-64","65-69",
                                "70-74","75-79",
                                "80-84", "85-89",
                                "90-94", "95-99",
                                "100+"))%>%
  select(`Age (122)`, `Total - Sex`)

colnames(cborn_2001year_age_restrict) <- c("AGE_BANDS_v1", "NUMP_v1")

cborn_2001_collapsed <- cborn_2001year_age_restrict%>%
  mutate(AGE_BANDS = ifelse(AGE_BANDS_v1%in%c("0-4","5-9","10-14"), "0-14", 
                            ifelse(AGE_BANDS_v1%in%c("15-19","20-24"), "15-24", 
                                   ifelse(AGE_BANDS_v1%in%c("25-29", "30-34", 
                                                            "35-39", "40-44", 
                                                            "45-49","50-54"), "25-54", 
                                          ifelse(AGE_BANDS_v1%in%c("55-59", "60-64"), "55-64",
                                                 ifelse(AGE_BANDS_v1%in%c("65-69", "70-74", 
                                                                          "75-79","80-84",
                                                                          "85-89", "90-94",
                                                                          "95-99","100+"), "65+", "No value"))))))

cborn_2001_post_collapse <- unique(cborn_2001_collapsed%>%
              group_by(AGE_BANDS)%>%
              mutate(NUMP_v2 = sum(NUMP_v1))%>%
              select(AGE_BANDS, NUMP_v2))
     
fb_2001year <- data.frame(foreign_born = c(343645, 516230,2954475, 779390, 1033295))

cborn_2001_wfb <- cbind(cborn_2001_post_collapse, fb_2001year)                           
cborn_only_2001 <- cborn_2001_wfb%>%mutate(`Canadian citizens only` = NUMP_v2-foreign_born)


      # Rep age
cborn_2001_agerep <- rep_dist_age(cborn_only_2001, 2001)

cborn_2001_pre_hazard <- cborn_2001_agerep%>%select(-NUMP_v2, -foreign_born)

      # Combine hazards

pre_cborn_hazards_2001 <- merge(cborn_2001_pre_hazard, prob_2001year_can, by = "YOBP")
head(pre_cborn_hazards_2001)
cborn_tbi_2001 <- pre_cborn_hazards_2001%>%mutate(LTBP.med = NUMP*PROB.med,
                                        LTBP.low = NUMP*PROB.low,
                                        LTBP.high = NUMP*PROB.high)

    #
    # TBI PREVALENCE 2001
    #


# 2006
cborn_2006year <-read_csv("2006 canada total pop.CSV", skip = 2)
View(cborn_2006year)

cborn_2006year_age_restrict <-cborn_2006year%>%
  filter(`Age (123)`%in%c("0 to 4 years","5 to 9 years", 
                          "10 to 14 years", "15 to 19 years", 
                          "20 to 24 years", "25 to 29 years", 
                          "30 to 34 years","35 to 39 years",
                          "40 to 44 years", "45 to 49 years",
                          "50 to 54 years","55 to 59 years",
                          "60 to 64 years","65 to 69 years",
                          "70 to 74 years","75 to 79 years",
                          "80 to 84 years", "85 to 89 years",
                          "90 to 94 years", "95 to 99 years",
                          "100 years and over"))%>%
  select(`Age (123)`, `Total - Sex`)

colnames(cborn_2006year_age_restrict) <- c("AGE_BANDS_v1", "NUMP_v1")

cborn_2006_collapsed <- cborn_2006year_age_restrict%>%
  mutate(AGE_BANDS = ifelse(AGE_BANDS_v1%in%c("0 to 4 years",
                                              "5 to 9 years",
                                              "10 to 14 years"), "0-14", 
                            ifelse(AGE_BANDS_v1%in%c("15 to 19 years",
                                                     "20 to 24 years"), "15-24", 
                                   ifelse(AGE_BANDS_v1%in%c("25 to 29 years", 
                                                            "30 to 34 years", 
                                                            "35 to 39 years", 
                                                            "40 to 44 years", 
                                                            "45 to 49 years",
                                                            "50 to 54 years"), "25-54", 
                                          ifelse(AGE_BANDS_v1%in%c("55 to 59 years", 
                                                                   "60 to 64 years"), "55-64",
                                                 ifelse(AGE_BANDS_v1%in%c("65 to 69 years", 
                                                                          "70 to 74 years", 
                                                                          "75 to 79 years",
                                                                          "80 to 84 years",
                                                                          "85 to 89 years", 
                                                                          "90 to 94 years",
                                                                          "95 to 99 years",
                                                                          "100 years and over"), "65+", "No value"))))))

cborn_2006_post_collapse <- unique(cborn_2006_collapsed%>%
                                     group_by(AGE_BANDS)%>%
                                     mutate(NUMP_v2 = sum(NUMP_v1))%>%
                                     select(AGE_BANDS, NUMP_v2))


fb_2006year <- data.frame(foreign_born = c(379890,609875,3235090, 984085, 1213535))

cborn_2006_wfb <- cbind(cborn_2006_post_collapse, fb_2006year)                           
cborn_only_2006 <- cborn_2006_wfb%>%mutate(`Canadian citizens only` = NUMP_v2-foreign_born)

      # Rep age
cborn_2006_agerep <- rep_dist_age(cborn_only_2006, 2006)

cborn_2006_pre_hazard <- cborn_2006_agerep%>%select(-NUMP_v2, -foreign_born)

      # Combine hazards

pre_cborn_hazards_2006 <- merge(cborn_2006_pre_hazard, prob_2006year_can, by = "YOBP")
head(pre_cborn_hazards_2006)
cborn_tbi_2006 <- pre_cborn_hazards_2006%>%mutate(LTBP.med = NUMP*PROB.med,
                                                  LTBP.low = NUMP*PROB.low,
                                                  LTBP.high = NUMP*PROB.high)

#
# TBI PREVALENCE 2006
#
cborn_tbi_2006%>%summarise(tbi_prev = sum(LTBP.med)/sum(NUMP))

# 2011
cborn_2011_0to14 <- read_csv("2011 all canada age 0-14.CSV", skip = 6)%>%
  filter(`Place of birth (236)` == "Born in Canada")%>%
  select(`Canadian citizens only`)%>%
  mutate(AGE_BANDS = "0-14")

cborn_2011_15to24 <- read_csv("2011 all canada ages 15-24.CSV", skip = 6)%>%
  filter(`Place of birth (236)` == "Born in Canada")%>%
  select(`Canadian citizens only`)%>%
  mutate(AGE_BANDS = "15-24")

cborn_2011_25to54 <-read_csv("2011 all canada ages 25-54.CSV", skip = 6)%>%
  filter(`Place of birth (236)` == "Born in Canada")%>%
  select(`Canadian citizens only`)%>%
  mutate(AGE_BANDS = "25-54")

cborn_2011_55to64 <-read_csv("2011 all canada ages 55-64.CSV", skip = 6)%>%
  filter(`Place of birth (236)` == "Born in Canada")%>%
  select(`Canadian citizens only`)%>%
  mutate(AGE_BANDS = "55-64")

cborn_2011_65up <-read_csv("2011 all canada ages 65up.CSV", skip = 6)%>%
       filter(`Place of birth (236)` == "Born in Canada")%>%
  select(`Canadian citizens only`)%>%
  mutate(AGE_BANDS = "65+")

cborn_all_ages_2011 <- rbind(cborn_2011_0to14, cborn_2011_15to24, cborn_2011_25to54, cborn_2011_55to64, cborn_2011_65up)

View(cborn_all_ages_2011)

    # Distribute population...again...good thing this doesn't get old
cborn_all_ages_2011_v2 <- cborn_all_ages_2011[rep(row.names(cborn_all_ages_2011), 
                                                ifelse(cborn_all_ages_2011$AGE_BANDS=="0-14",15,
                                                       ifelse(cborn_all_ages_2011$AGE_BANDS == "15-24", 10,
                                                                ifelse(cborn_all_ages_2011$AGE_BANDS == "25-54", 30, 
                                                              ifelse(cborn_all_ages_2011$AGE_BANDS == "55-64", 10,  ifelse(cborn_all_ages_2011$AGE_BANDS == "65+", 21, 0                                                                                                )))))),]

View(cborn_all_ages_2011_v2)

    # Add sequence of age groups
cborn_dist_age_2011 <- cborn_all_ages_2011_v2%>%mutate(
  AGEP = ifelse(AGE_BANDS == "0-14",
                seq(from = 0, to = 14), 
                ifelse(AGE_BANDS == "15-24",
                       seq(from = 15, to = 24),
                       ifelse(AGE_BANDS == "25-54",
                              seq(from = 25, to = 54),
                              ifelse(AGE_BANDS == "55-64",
                                     seq(from = 55, to = 64), 
                                     ifelse(AGE_BANDS == "65+",
                                            seq(from = 65, to = 85),
                                            AGE_BANDS
                                            
                                     )))
                )))

cborn_dist_age$AGEP <- as.numeric(cborn_dist_age$AGEP)
View(cborn_dist_age)

nrow(cborn_dist_age)
cborn_dist_age_v2 <- cborn_dist_age%>%group_by(AGE_BANDS)%>%
  mutate(NUMP = `Canadian citizens only`/max(row_number()))%>%
  mutate(YOBP = 2011 - AGEP)%>%
  select(-`Canadian citizens only`)


View(cborn_dist_age_v2)

pre_cborn_hazards <- merge(cborn_dist_age_v2, prob_2011_can, by = "YOBP")
head(pre_cborn_hazards)
cborn_tbi <- pre_cborn_hazards%>%mutate(LTBP.med = NUMP*PROB.med,
                                LTBP.low = NUMP*PROB.low,
                                LTBP.high = NUMP*PROB.high)

cborn_tbi%>%summarise(tbi_prev = sum(LTBP.med)/sum(NUMP))



# 2016 ==============================================================================
cborn_2016_0to14 <- read_csv("2016 all canada ages 0-14.CSV", skip = 6)%>%
  filter(`Place of birth (272)` == "Born in Canada")%>%
  select(`Canadian citizens only`)%>%
  mutate(AGE_BANDS = "0-14")

cborn_2016_15to24 <-read_csv("2016 all canada ages 15-24.CSV", skip = 6)%>%
  filter(`Place of birth (272)` == "Born in Canada")%>%
  select(`Canadian citizens only`)%>%
  mutate(AGE_BANDS = "15-24")

cborn_2016_25to54 <-read_csv("2016 all canada 25-54.CSV", skip = 6)%>%
  filter(`Place of birth (272)` == "Born in Canada")%>%
  select(`Canadian citizens only`)%>%
  mutate(AGE_BANDS = "25-54")


cborn_2016_55to64 <- read_csv("2016 all canada 55-64.CSV", skip = 6)%>%
  filter(`Place of birth (272)` == "Born in Canada")%>%
  select(`Canadian citizens only`)%>%
  mutate(AGE_BANDS = "55-64")


cborn_2016_65up <-read_csv("2016 all canada 65up.CSV", skip = 6)%>%
  filter(`Place of birth (272)` == "Born in Canada")%>%
  select(`Canadian citizens only`)%>%
  mutate(AGE_BANDS = "65+")

cborn_all_ages_2016 <- rbind(cborn_2016_0to14, cborn_2016_15to24, cborn_2016_25to54, cborn_2016_55to64, cborn_2016_65up)

View(cborn_all_ages_2016)


    # Distribute population...again...good thing this doesn't get old
cborn_all_ages_2016_v2 <- cborn_all_ages_2016[rep(row.names(cborn_all_ages_2016), 
                                                  ifelse(cborn_all_ages_2016$AGE_BANDS=="0-14",15,
                                                         ifelse(cborn_all_ages_2016$AGE_BANDS == "15-24", 10,
                                                                ifelse(cborn_all_ages_2016$AGE_BANDS == "25-54", 30, 
                                                                       ifelse(cborn_all_ages_2016$AGE_BANDS == "55-64", 10,  ifelse(cborn_all_ages_2016$AGE_BANDS == "65+", 21, 0                                                                                                )))))),]

View(cborn_all_ages_2016_v2)

    # Add sequence of age groups
cborn_dist_age_2016 <- cborn_all_ages_2016_v2%>%mutate(
  AGEP = ifelse(AGE_BANDS == "0-14",
                seq(from = 0, to = 14), 
                ifelse(AGE_BANDS == "15-24",
                       seq(from = 15, to = 24),
                       ifelse(AGE_BANDS == "25-54",
                              seq(from = 25, to = 54),
                              ifelse(AGE_BANDS == "55-64",
                                     seq(from = 55, to = 64), 
                                     ifelse(AGE_BANDS == "65+",
                                            seq(from = 65, to = 85),
                                            AGE_BANDS
                                            
                                     )))
                )))

cborn_dist_age_2016$AGEP <- as.numeric(cborn_dist_age_2016$AGEP)
View(cborn_dist_age_2016)

nrow(cborn_dist_age_2016)
cborn_dist_age_2016_v2 <- cborn_dist_age_2016%>%group_by(AGE_BANDS)%>%
  mutate(NUMP = `Canadian citizens only`/max(row_number()))%>%
  mutate(YOBP = 2016 - AGEP)%>%
  select(-`Canadian citizens only`)


View(cborn_dist_age_2016_v2)

pre_cborn_hazards_2016 <- merge(cborn_dist_age_2016_v2, prob_2016_can, by = "YOBP")
head(pre_cborn_hazards_2016)
cborn_tbi_2016 <- pre_cborn_hazards_2016%>%mutate(LTBP.med = NUMP*PROB.med,
                                        LTBP.low = NUMP*PROB.low,
                                        LTBP.high = NUMP*PROB.high)


# CANADA PREV ========================================================
c_2001year <- cborn_tbi_2001%>%summarise(year = "2001", 
                                         tbi_prev = sum(LTBP.med)/sum(NUMP), 
                           tbi_prev_low = sum(LTBP.low)/sum(NUMP),
                           tbi_prev_high = sum(LTBP.high)/sum(NUMP))

c_2006 <- cborn_tbi_2006%>%summarise(year = "2006",
                                     tbi_prev = sum(LTBP.med)/sum(NUMP), 
                           tbi_prev_low = sum(LTBP.low)/sum(NUMP),
                           tbi_prev_high = sum(LTBP.high)/sum(NUMP))

c_2011 <- cborn_tbi%>%summarise(year = "2011",
  tbi_prev = sum(LTBP.med)/sum(NUMP), 
                      tbi_prev_low = sum(LTBP.low)/sum(NUMP),
                      tbi_prev_high = sum(LTBP.high)/sum(NUMP))

c_2016 <- cborn_tbi_2016%>%summarise(year = "2016",
                                     tbi_prev = sum(LTBP.med)/sum(NUMP), 
                           tbi_prev_low = sum(LTBP.low)/sum(NUMP),
                           tbi_prev_high = sum(LTBP.high)/sum(NUMP))

cborn_prev <- rbind(c_2001year, c_2006, c_2011, c_2016)
cborn_prev

cborn_num_tbi <- rbind(cborn_tbi_2001, cborn_tbi_2006, cborn_tbi, cborn_tbi_2016)

#saveRDS(cborn_num_tbi, file = "/Users/ajordan/OneDrive - McGill University/LTBI-Aust-CEA-master/Data/canadian_born_num_tbi_v2.rds")
#saveRDS(cborn_prev, file = "/Users/ajordan/OneDrive - McGill University/LTBI-Aust-CEA-master/Data/canadian_born_tbiprev_v2.rds")

ggplot(cborn_prev, aes(year))+
  geom_boxplot(aes(ymin = tbi_prev_low*1.5-tbi_prev_low, 
                   lower = tbi_prev_low, 
                   middle = tbi_prev, 
                   upper = tbi_prev_high, 
                   ymax = ifelse(1.5*tbi_prev_high > 1, 1, 1.5*tbi_prev_high)),
               stat = "identity", 
               fill = "burlywood1")+
  labs(x = "Census Year",
       y = "TBI Prevalence (%,IQR)")+
  theme(legend.position="none")+
  theme(text = element_text(size = 16))+
  theme(legend.text = element_text(size = 12))+
  theme(axis.text = element_text(size = 12))+
  ylim(0,1)

