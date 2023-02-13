# Census immigration trends

# Compare census estimates between all years
rm(list = ls(all.names = TRUE))
gc()
library(tidyverse)
library(plotly)
library(readxl)
library(modelbased)

# 2001
tbi_2001 <- readRDS("estimates_2001_v4.rds")
#View(tbi_2001)



# Find total TBI prev among 2001 arrivals
# a <- na.omit(tbi_2001)%>%
#   mutate(AGE_BANDS = ifelse(
#     AGEP < 15, "0-14", ifelse(
#       AGEP >= 15 & AGEP < 35, "15-34", ifelse(
#         AGEP >= 35 & AGEP < 55, "35-54",ifelse(
#           AGEP >= 55 & AGEP <= 64, "55-64", ifelse(
#             AGEP >= 65 & AGEP <= 200, "65+", "No value"))))))
# a_2001 <- a%>%group_by(ISO3, AGE_BANDS)%>%summarise(TBI.prev = sum(LTBP)/sum(NUMP))

# 2006
tbi_2006 <- readRDS("2006_census_v4.rds")
View(tbi_2006)

# b <- na.omit(tbi_2006)%>%
#   mutate(AGE_BANDS = ifelse(
#     AGEP < 15, "0-14", ifelse(
#       AGEP >= 15 & AGEP < 35, "15-34", ifelse(
#         AGEP >= 35 & AGEP < 55, "35-54",ifelse(
#           AGEP >= 55 & AGEP <= 64, "55-64", ifelse(
#             AGEP >= 65 & AGEP <= 200, "65+", "No value"))))))
# 
# b_2006 <- b%>%group_by(ISO3, AGE_BANDS)%>%summarise(TBI.prev = sum(LTBP)/sum(NUMP))

# 2011
tbi_2011 <- readRDS("2011estimates_v3.rds")
View(tbi_2011)

# c <- na.omit(tbi_2011)%>%
#   mutate(AGE_BANDS = ifelse(
#     AGEP < 15, "0-14", ifelse(
#       AGEP >= 15 & AGEP < 35, "15-34", ifelse(
#         AGEP >= 35 & AGEP < 55, "35-54",ifelse(
#           AGEP >= 55 & AGEP <= 64, "55-64", ifelse(
#             AGEP >= 65 & AGEP <= 200, "65+", "No value"))))))
# 
# c_2011 <- c%>%group_by(ISO3, AGE_BANDS)%>%summarise(TBI.prev = sum(LTBP)/sum(NUMP))

# 2016
tbi_2016 <- readRDS("2016_census_v3.rds")

# AGE GROUP FUNCTION===============================================

# age_group_func <- function(df){
#   
#   df2 <- na.omit(df)%>%
#     mutate(AGE_BANDS = ifelse(
#       AGEP < 15, "0-14", ifelse(
#         AGEP >= 15 & AGEP < 35, "15-34", ifelse(
#           AGEP >= 35 & AGEP < 55, "35-54",ifelse(
#             AGEP >= 55 & AGEP <= 74, "55-74", ifelse(
#               AGEP >= 75 & AGEP <= 200, "75+", "No value"))))))
#   
#   
#   df2%>%
#     dplyr::group_by(AGE_BANDS)%>%
#     dplyr::summarise(tot_pop = sum(NUMP))%>%
#     mutate(sum_pop = sum(tot_pop),
#            percent_age = tot_pop/sum_pop*100)%>%
#     select(AGE_BANDS, percent_age)
# 
# }



# AARP FUNCTION===============================================

aarp_func <- function(df){

  df2 <- na.omit(df)%>%
    mutate(AARP = YARP-YOBP)%>%
    mutate(AARP_BANDS = ifelse(
      AARP < 15, "0-14", ifelse(
        AARP >= 15 & AARP < 35, "15-34", ifelse(
          AARP >= 35 & AARP < 55, "35-54",ifelse(
            AARP >= 55 & AARP <= 74, "55-74", ifelse(
              AARP >= 75 & AARP <= 200, "75+", "No value"))))))

  df2%>%
    dplyr::group_by(AARP_BANDS)%>%
    dplyr::summarise(tot_pop = sum(NUMP))%>%
    mutate(sum_pop = sum(tot_pop),
           percent_AARP = tot_pop/sum_pop*100)%>%
    select(AARP_BANDS, percent_AARP)

}

tbi_2001_v2 <- aarp_func (tbi_2001)
View(tbi_2001_v2)

tbi_2006_v2 <- aarp_func (tbi_2006)
View(tbi_2006_v2)

tbi_2011_v2 <-  aarp_func (tbi_2011)
View(tbi_2011_v2)

tbi_2016_v2 <- aarp_func (tbi_2016)
View(tbi_2016_v2)


cc <- cbind(tbi_2001_v2, tbi_2006_v2, tbi_2011_v2, tbi_2016_v2)
View(cc)

# WRITING AGE GROUP DEMOGRAPHIC DATA TO EXCEL
# library(writexl)
# write_xlsx(cc, "/Users/ajordan/OneDrive - McGill University/LTBI-Aust-CEA-master/Census/Census All/age_group_immigration")


# YARP BY TOP 10 ARRIVALS
View(tbi_2016)

View(tbi_2016%>%
       filter(YARP == 2016)%>%
       group_by(ISO3)%>%
       summarize(NUMP = sum(NUMP)))


tbi_2016_top_10 <- tbi_2016%>%filter(ISO3%in%c("CHN", 
                                               "VNM",
                                               "PAK",
                                               "IND",
                                               "PHL",
                                               "GBR",
                                               "USA",
                                               "ITA",
                                               "IRN",
                                               "HKG"))

# 
# View(tbi_2016_top_10%>%group_by(ISO3)%>%summarize(sum(LTBP)/sum(NUMP),
#                                                   sum(LTBP.low)/sum(NUMP),
#                                                   sum(LTBP.high)/sum(NUMP)))

tbi_2016_top_10_grouped <- tbi_2016_top_10%>%group_by(ISO3,YARP)%>%summarize(NUMP = sum(NUMP))
tbi_2016_top_10_iso3 <- tbi_2016_top_10%>%group_by(ISO3)%>%summarize(NUMP = sum(NUMP))
View(tbi_2016_top_10_iso3)

# TOP TEN NUMBER OF ARRIVALS
ggplot(tbi_2016_top_10_grouped, aes(x = YARP, y = NUMP, color = ISO3))+
  geom_smooth(se = FALSE,
              method = "gam")+
  labs(x = "Year of Immigration to Canada",
       y = "Permanent Residents and Foreign-Born \nCitizens Living in Canada",
       color = "Country of Origin (ISO3)")+
  scale_x_continuous(breaks = round(seq(1940, 2010, by = 10),1))+
  scale_y_continuous(breaks = round(seq(0, 60000, by = 5000),1))+
  theme_bw()+
  theme(text = element_text(size = 12))


ggplot(tbi_2016_top_10_grouped, aes(x = YARP, y = NUMP, color = ISO3))+
  geom_smooth(method = lm, formula = y ~ splines::bs(x, 10), se = FALSE)+
  labs(x = "Year of Immigration to Canada",
       y = "Permanent Residents and Foreign-Born \nCitizens Living in Canada",
       color = "Country of Origin\n (ISO3)")+
  scale_x_continuous(breaks = round(seq(1940, 2010, by = 10),1))+
  scale_y_continuous(breaks = round(seq(0, 60000, by = 5000),1))+
  theme_bw()+
  theme(text = element_text(size = 13, face = "bold"))+
  theme(axis.text = element_text(size = 12))


  ggplot(tbi_2016_top_10_grouped, aes(x = YARP, y = NUMP, color = ISO3))+
    geom_line()+
    labs(x = "Year of Immigration to Canada",
         y = "Permanent Residents and Foreign-Born \nCitizens Living in Canada",
         color = "Country of Origin (ISO3)")+
    scale_x_continuous(breaks = round(seq(1940, 2010, by = 10),1))+
    scale_y_continuous(breaks = round(seq(0, 60000, by = 10000),1))+
  theme_light()+
    theme(text = element_text(size = 12)) 
  
  
  
  # INC BAND AND YARP
View(tbi_2016)
tbi_inc_immigration <- tbi_2016%>%group_by(ISO3)%>%summarize(sum(LTBP)/sum(NUMP),
                                                    sum(LTBP.low)/sum(NUMP),
                                                    sum(LTBP.high)/sum(NUMP)))
  
  tbi_2016_top_10_grouped <- tbi_2016_top_10%>%group_by(ISO3,YARP)%>%summarize(NUMP = sum(NUMP))
  tbi_2016_top_10_iso3 <- tbi_2016_top_10%>%group_by(ISO3)%>%summarize(NUMP = sum(NUMP))
  View(tbi_2016_top_10_iso3)
  
  # TOP TEN NUMBER OF ARRIVALS
  ggplot(tbi_2016_top_10_grouped, aes(x = YARP, y = NUMP, color = ISO3))+
    geom_smooth(se = FALSE,
                method = "gam")+
    labs(x = "Year of Immigration to Canada",
         y = "Permanent Residents and Foreign-Born \nCitizens Living in Canada",
         color = "Country of Origin (ISO3)")+
    scale_x_continuous(breaks = round(seq(1940, 2010, by = 10),1))+
    scale_y_continuous(breaks = round(seq(0, 60000, by = 5000),1))+
    theme_bw()+
    theme(text = element_text(size = 12))
  
  
  ggplot(tbi_2016_top_10_grouped, aes(x = YARP, y = NUMP, color = ISO3))+
    geom_smooth(method = lm, formula = y ~ splines::bs(x, 10), se = FALSE)+
    labs(x = "Year of Immigration to Canada",
         y = "Permanent Residents and Foreign-Born \nCitizens Living in Canada",
         color = "Country of Origin\n (ISO3)")+
    scale_x_continuous(breaks = round(seq(1940, 2010, by = 10),1))+
    scale_y_continuous(breaks = round(seq(0, 60000, by = 5000),1))+
    theme_bw()+
    theme(text = element_text(size = 13, face = "bold"))+
    theme(axis.text = element_text(size = 12))
  
  
  ggplot(tbi_2016_top_10_grouped, aes(x = YARP, y = NUMP, color = ISO3))+
    geom_line()+
    labs(x = "Year of Immigration to Canada",
         y = "Permanent Residents and Foreign-Born \nCitizens Living in Canada",
         color = "Country of Origin (ISO3)")+
    scale_x_continuous(breaks = round(seq(1940, 2010, by = 10),1))+
    scale_y_continuous(breaks = round(seq(0, 60000, by = 10000),1))+
    theme_light()+
    theme(text = element_text(size = 12)) 
  
  
  
  
  
  
  tbi_2016_top_10_grouped_prev <- tbi_2016_top_10%>%
    group_by(ISO3, YARP)%>%
    summarize(tbi_prev = sum(LTBP)/sum(NUMP),
              tbi_prev.low = sum(LTBP.low)/sum(NUMP),
              tbi_prev.high = sum(LTBP.high)/sum(NUMP))
  
  View(  tbi_2016_top_10_grouped_prev)
  
  top10_p1 <- ggplot(tbi_2016_top_10_grouped_prev, aes(x = YARP, y = tbi_prev*100, color = ISO3))+
    geom_smooth(method = lm, formula = y ~ splines::bs(x, 4))+
    labs(x = "Year of Immigration to Canada",
         y = "TBI Prevalence (%, 95% CI)",
         color = "Country of Origin\n (ISO3)")+
    scale_x_continuous(breaks = round(seq(1940, 2010, by = 10),1))+
    scale_y_continuous(breaks = round(seq(0, 100, by = 5),1))+
    theme_bw()+
    theme(text = element_text(size = 13, face = "bold"))+
    theme(axis.text = element_text(size = 12))
  
  
  top10_p1
  
  gg_build <- ggplot_build(top10_p1)
  
  View(data.frame(gg_build$data))
  
  
  
  
  

  ggplot(tbi_2016_top_10_grouped_prev, aes(x = YARP, y = tbi_prev*100, color = ISO3))+
    geom_smooth(method = "gam")+
    labs(x = "Year of Immigration to Canada",
         y = "TBI Prevalence (%, 95% CI)",
         color = "Country of Origin (ISO3)")+
    scale_x_continuous(breaks = round(seq(1940, 2010, by = 10),1))+
    scale_y_continuous(breaks = round(seq(0, 100, by = 5),1))+
    theme_bw()+
    theme(text = element_text(size = 12))
  
  
  ggplot(tbi_2016_top_10_grouped_prev, aes(x = YARP, y = tbi_prev*100, color = ISO3))+
    geom_line()+
    labs(x = "Year of Immigration to Canada",
         y = "TBI Prevalence (%, 95% CI)",
         color = "Country of Origin (ISO3)")+
    scale_x_continuous(breaks = round(seq(1940, 2010, by = 10),1))+
    scale_y_continuous(breaks = round(seq(0, 100, by = 5),1))+
    theme_bw()+
    theme(text = element_text(size = 12))
  
  

  