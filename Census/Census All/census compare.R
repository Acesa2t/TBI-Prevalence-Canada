# Compare census estimates between all years
rm(list = ls(all.names = TRUE))
gc()
library(tidyverse)
library(plotly)
library(readxl)

# 2001
tbi_2001 <- readRDS("estimates_2001_v4.rds")
View(tbi_2001)

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
View(tbi_2016%>%filter(ISO3%in%c("DEU","PHL")))

# d <- na.omit(tbi_2016)%>%
#   mutate(AGE_BANDS = ifelse(
#     AGEP < 15, "0-14", ifelse(
#       AGEP >= 15 & AGEP < 35, "15-34", ifelse(
#         AGEP >= 35 & AGEP < 55, "35-54",ifelse(
#           AGEP >= 55 & AGEP <= 64, "55-64", ifelse(
#             AGEP >= 65 & AGEP <= 200, "65+", "No value"))))))
# 
# 
# d_2016 <- d%>%group_by(ISO3, AGE_BANDS)%>%summarise(TBI.prev = sum(LTBP)/sum(NUMP))


# tbi_2001 <- tbi_2001%>%
#   group_by(ISO3)%>%summarise(TBI.prev = sum(LTBP)/sum(NUMP))
# 
# tbi_2006 <- tbi_2006%>%
#   group_by(ISO3)%>%summarise(TBI.prev = sum(LTBP)/sum(NUMP))
# 
# tbi_2011 <- tbi_2011%>%
#   group_by(ISO3)%>%summarise(TBI.prev = sum(LTBP)/sum(NUMP))
# 
# tbi_2016 <- tbi_2016%>%
#   group_by(ISO3)%>%summarise(TBI.prev = sum(LTBP)/sum(NUMP))



tbi_2001_v2 <- tbi_2001%>%summarise(
  year = "2001",
  TBI.prev = sum(LTBP)/sum(NUMP),
  TBI.prev.high = sum(LTBP.high)/sum(NUMP),
  TBI.prev.low = sum(LTBP.low)/sum(NUMP))

tbi_2006_v2 <- tbi_2006%>%summarise(
  year = "2006",
  TBI.prev = sum(LTBP)/sum(NUMP),
  TBI.prev.high = sum(LTBP.high)/sum(NUMP),
  TBI.prev.low = sum(LTBP.low)/sum(NUMP))

tbi_2011_v2 <- tbi_2011%>%summarise(
  year = "2011",
  TBI.prev = sum(LTBP)/sum(NUMP),
  TBI.prev.high = sum(LTBP.high)/sum(NUMP),
  TBI.prev.low = sum(LTBP.low)/sum(NUMP))

tbi_2016_v2 <- na.omit(tbi_2016)%>%summarise(
  year = "2016",
  TBI.prev = sum(LTBP)/sum(NUMP),
  TBI.prev.high = sum(LTBP.high)/sum(NUMP),
  TBI.prev.low = sum(LTBP.low)/sum(NUMP))


cc <- rbind(tbi_2001_v2, tbi_2006_v2, tbi_2011_v2, tbi_2016_v2)

# aa <- full_join(tbi_2001, tbi_2006 , by = c("ISO3"))
# bb <- full_join(aa, tbi_2011, by = c("ISO3"))
# cc <- full_join(bb, tbi_2016, by = c("ISO3"))
# View(cc)
#"AGE GROUPS",
colnames(cc) <- c("ISO3", "TBI.prev.2001", "TBI.prev.2006", "TBI.prev.2011", "TBI.prev.2016")
View(cc)
cc
#cc$year <- as.factor(cc$year)

# Graphs-OVERALL COMPARISON
ggplot(cc, aes(year, fill = "red"))+
  geom_boxplot(aes(ymin = TBI.prev.low*1.5-TBI.prev.low, 
                   lower = TBI.prev.low, 
                   middle = TBI.prev, 
                   upper = TBI.prev.high, 
                   ymax = 1.5*TBI.prev.high),
               stat = "identity")+
  labs(title = "TBI Prevalence by Census Year",
       x = "Census Year",
       y = "TBI Prevalence (%,IQR)")+
  theme(legend.position="none")+
  theme(text = element_text(size = 16))+
  theme(legend.text = element_text(size = 12))+
  theme(axis.text = element_text(size = 12))



# AGE TRENDS  =========================================================

# 2001
tbi_2001 <- readRDS("estimates_2001_v4.rds")
View(tbi_2001)

#Find total TBI prev among 2001 arrivals
a <- na.omit(tbi_2001)%>%
  mutate(AGE_BANDS = ifelse(
    AGEP < 15, "0-14", ifelse(
      AGEP >= 15 & AGEP < 35, "15-34", ifelse(
        AGEP >= 35 & AGEP < 55, "35-54",ifelse(
          AGEP >= 55 & AGEP <= 64, "55-64", ifelse(
            AGEP >= 65 & AGEP <= 200, "65+", "No value"))))))
a_2001 <- a%>%group_by(AGE_BANDS)%>%summarise(year = "Census 2001",
                                              TBI.prev = sum(LTBP)/sum(NUMP),
                                              TBI.prev.high = sum(LTBP.high)/sum(NUMP),
                                              TBI.prev.low = sum(LTBP.low)/sum(NUMP))

# 2006
tbi_2006 <- readRDS("2006_census_v4.rds")
View(tbi_2006)

b <- na.omit(tbi_2006)%>%
  mutate(AGE_BANDS = ifelse(
    AGEP < 15, "0-14", ifelse(
      AGEP >= 15 & AGEP < 35, "15-34", ifelse(
        AGEP >= 35 & AGEP < 55, "35-54",ifelse(
          AGEP >= 55 & AGEP <= 64, "55-64", ifelse(
            AGEP >= 65 & AGEP <= 200, "65+", "No value"))))))

b_2006 <- b%>%group_by(AGE_BANDS)%>%summarise(year = "Census 2006",
                                              TBI.prev = sum(LTBP)/sum(NUMP),
                                              TBI.prev.high = sum(LTBP.high)/sum(NUMP),
                                              TBI.prev.low = sum(LTBP.low)/sum(NUMP))

# 2011
tbi_2011 <- readRDS("2011estimates_v3.rds")
View(tbi_2011)

c <- na.omit(tbi_2011)%>%
  mutate(AGE_BANDS = ifelse(
    AGEP < 15, "0-14", ifelse(
      AGEP >= 15 & AGEP < 35, "15-34", ifelse(
        AGEP >= 35 & AGEP < 55, "35-54",ifelse(
          AGEP >= 55 & AGEP <= 64, "55-64", ifelse(
            AGEP >= 65 & AGEP <= 200, "65+", "No value"))))))

c_2011 <- c%>%group_by(AGE_BANDS)%>%summarise(year = "Census 2011",
                                              TBI.prev = sum(LTBP)/sum(NUMP),
                                              TBI.prev.high = sum(LTBP.high)/sum(NUMP),
                                              TBI.prev.low = sum(LTBP.low)/sum(NUMP))


# 2016
tbi_2016 <- readRDS("2016_census_v3.rds")
View(tbi_2016)

d <- na.omit(tbi_2016)%>%
  mutate(AGE_BANDS = ifelse(
    AGEP < 15, "0-14", ifelse(
      AGEP >= 15 & AGEP < 35, "15-34", ifelse(
        AGEP >= 35 & AGEP < 55, "35-54",ifelse(
          AGEP >= 55 & AGEP <= 64, "55-64", ifelse(
            AGEP >= 65 & AGEP <= 200, "65+", "No value"))))))


d_2016 <- d%>%group_by(AGE_BANDS)%>%summarise(year = "Census 2016",
                                              TBI.prev = sum(LTBP)/sum(NUMP),
                                              TBI.prev.high = sum(LTBP.high)/sum(NUMP),
                                              TBI.prev.low = sum(LTBP.low)/sum(NUMP))


cc_age <- rbind(a_2001, b_2006,c_2011,d_2016)

#View(pivot_longer(cc_age, cols = c("TBI.prev.2001", "TBI.prev.2006", "TBI.prev.2011", "TBI.prev.2016"), names_to =  c("census_year", "census_year_high", "census_year_low")))
#View(cc_age)


# Graphs-OVERALL AGE COMPARISON
ggplot(cc_age, aes(AGE_BANDS, fill = AGE_BANDS))+
  geom_boxplot(aes(ymin = TBI.prev.low*1.5-TBI.prev.low, 
                   lower = TBI.prev.low, 
                   middle = TBI.prev, 
                   upper = TBI.prev.high, 
                   ymax = 1.5*TBI.prev.high),
               stat = "identity")+
  labs(title = "TBI Prevalence by Census Year",
       x = "Age Group",
       y = "TBI Prevalence (%,IQR)")+
  facet_wrap(~year, ncol = 4)+
  theme(legend.position="none")+
  theme(text = element_text(size = 16))+
  theme(legend.text = element_text(size = 12))+
  theme(axis.text = element_text(size = 12))

# INC BAND TRENDS  =========================================================
# Merge incidence bands
tb_inc <- readRDS("incidence.rds")
colnames(tb_inc) <- c("ISO3", "WHO_Region", "e_inc_100k")
tb_inc <- tb_inc%>%select(-WHO_Region)
head(tb_inc)


# 2001
tbi_2001 <- readRDS("estimates_2001_v4.rds")
View(tbi_2001)

tb_inc <- readRDS("incidence.rds")
colnames(tb_inc) <- c("ISO3", "WHO_Region", "e_inc_100k")
tb_inc <- tb_inc%>%select(-WHO_Region)
head(tb_inc)

tbi_data <- merge(tbi_2001, tb_inc, by = "ISO3")


# More bands
tbi_inc_2001 <- tbi_data%>%
  mutate(inc_band_per100k = ifelse(
    e_inc_100k < 50, "0-49", ifelse(
          e_inc_100k >= 50 & e_inc_100k < 100, "50-99", ifelse(
            e_inc_100k >= 100 & e_inc_100k < 200, "100-199",ifelse(
              e_inc_100k >= 200 & e_inc_100k < 100000, "200+", "No value")))))

a <- na.omit(tbi_inc_2001)%>%
  mutate(AGE_BANDS = ifelse(
    AGEP < 15, "0-14", ifelse(
      AGEP >= 15 & AGEP < 35, "15-34", ifelse(
        AGEP >= 35 & AGEP < 55, "35-54",ifelse(
          AGEP >= 55 & AGEP <= 74, "55-74", ifelse(
            AGEP >= 75 & AGEP <= 200, "75+", "No value"))))))

a_2001 <- a%>%group_by(inc_band_per100k, AGE_BANDS)%>%
  summarise(year = "Census 2001",
            TBI.prev = sum(LTBP)/sum(NUMP),
            TBI.prev.high = sum(LTBP.high)/sum(NUMP),
            TBI.prev.low = sum(LTBP.low)/sum(NUMP))

# 2006
tbi_2006 <- readRDS("2006_census_v4.rds")
View(tbi_2006)


tb_inc <- readRDS("incidence.rds")
colnames(tb_inc) <- c("ISO3", "WHO_Region", "e_inc_100k")
tb_inc <- tb_inc%>%select(-WHO_Region)
head(tb_inc)

tbi_data <- merge(tbi_2006, tb_inc, by = "ISO3")


# More bands
tbi_inc_2006 <- tbi_data%>%
  mutate(inc_band_per100k = ifelse(
    e_inc_100k < 50, "0-49", ifelse(
      e_inc_100k >= 50 & e_inc_100k < 100, "50-99", ifelse(
        e_inc_100k >= 100 & e_inc_100k < 200, "100-199",ifelse(
          e_inc_100k >= 200 & e_inc_100k < 100000, "200+", "No value")))))

b <- na.omit(tbi_inc_2011)%>%
  mutate(AGE_BANDS = ifelse(
    AGEP < 15, "0-14", ifelse(
      AGEP >= 15 & AGEP < 35, "15-34", ifelse(
        AGEP >= 35 & AGEP < 55, "35-54",ifelse(
          AGEP >= 55 & AGEP <= 74, "55-74", ifelse(
            AGEP >= 75 & AGEP <= 200, "75+", "No value"))))))

b_2006 <- b%>%group_by(inc_band_per100k, AGE_BANDS)%>%
  summarise(year = "Census 2006",
            TBI.prev = sum(LTBP)/sum(NUMP),
            TBI.prev.high = sum(LTBP.high)/sum(NUMP),
            TBI.prev.low = sum(LTBP.low)/sum(NUMP))

# 2011
tbi_2011 <- readRDS("2011estimates_v3.rds")
View(tbi_2011)

tb_inc <- readRDS("incidence.rds")
colnames(tb_inc) <- c("ISO3", "WHO_Region", "e_inc_100k")
tb_inc <- tb_inc%>%select(-WHO_Region)
head(tb_inc)

tbi_data <- merge(tbi_2011, tb_inc, by = "ISO3")

# More bands
tbi_inc_2011 <- tbi_data%>%
  mutate(inc_band_per100k = ifelse(
    e_inc_100k < 50, "0-49", ifelse(
      e_inc_100k >= 50 & e_inc_100k < 100, "50-99", ifelse(
        e_inc_100k >= 100 & e_inc_100k < 200, "100-199",ifelse(
          e_inc_100k >= 200 & e_inc_100k < 100000, "200+", "No value")))))


c <- na.omit(tbi_inc_2011)%>%
  mutate(AGE_BANDS = ifelse(
    AGEP < 15, "0-14", ifelse(
      AGEP >= 15 & AGEP < 35, "15-34", ifelse(
        AGEP >= 35 & AGEP < 55, "35-54",ifelse(
          AGEP >= 55 & AGEP <= 74, "55-74", ifelse(
            AGEP >= 75 & AGEP <= 200, "75+", "No value"))))))

c_2011 <- c%>%group_by(inc_band_per100k, AGE_BANDS)%>%
  summarise(year = "Census 2011",
            TBI.prev = sum(LTBP)/sum(NUMP),
            TBI.prev.high = sum(LTBP.high)/sum(NUMP),
            TBI.prev.low = sum(LTBP.low)/sum(NUMP))

# 2016
# More bands
tbi_2016 <- readRDS("2016_census_v3.rds")
View(tbi_2016)

tb_inc <- readRDS("incidence.rds")
colnames(tb_inc) <- c("ISO3", "WHO_Region", "e_inc_100k")
tb_inc <- tb_inc%>%select(-WHO_Region)
head(tb_inc)

tbi_data <- merge(tbi_2016, tb_inc, by = "ISO3")


tbi_inc_2016 <- tbi_data%>%
  mutate(inc_band_per100k = ifelse(
    e_inc_100k < 50, "0-49", ifelse(
      e_inc_100k >= 50 & e_inc_100k < 100, "50-99", ifelse(
        e_inc_100k >= 100 & e_inc_100k < 200, "100-199",ifelse(
          e_inc_100k >= 200 & e_inc_100k < 100000, "200+", "No value")))))



d <- na.omit(tbi_inc_2016)%>%
  mutate(AGE_BANDS = ifelse(
    AGEP < 15, "0-14", ifelse(
      AGEP >= 15 & AGEP < 35, "15-34", ifelse(
        AGEP >= 35 & AGEP < 55, "35-54",ifelse(
          AGEP >= 55 & AGEP <= 74, "55-74", ifelse(
            AGEP >= 75 & AGEP <= 200, "75+", "No value"))))))

d_2016 <- na.omit(d)%>%group_by(inc_band_per100k, AGE_BANDS)%>%
  summarise(year = "Census 2016",
            TBI.prev = sum(LTBP)/sum(NUMP),
            TBI.prev.high = sum(LTBP.high)/sum(NUMP),
            TBI.prev.low = sum(LTBP.low)/sum(NUMP))


    # MEDIAN TBI PREVALENCE ALL YEARS

med_prev_function <- function(df, year_char){
  prevalence <- na.omit(df)%>%
    filter(NUMP>0)%>%
    group_by(ISO3,inc_band_per100k, AGE_BANDS)%>%
    summarise(year = year_char,
              TBI.prev = median(LTBP)/median(NUMP),
              TBI.prev.high = median(LTBP.high)/median(NUMP),
              TBI.prev.low = median(LTBP.low)/median(NUMP))
  prevalence
}

View(med_prev_function(df = a, year_char = "Census 2001"))


cc <- rbind(a_2001, b_2006, c_2011, d_2016)

View(cc)
cc$inc_band_per100k <- factor(cc$inc_band_per100k, levels = c("0-49", "50-99", "100-199", "200+"))
#cc$year <- as.factor(cc$year)

# Graphs-OVERALL INC BAND COMPARISON
ggplot(cc, aes(year, color = inc_band_per100k))+
  geom_boxplot(aes(ymin = TBI.prev.low*1.5-TBI.prev.low, 
                lower = TBI.prev.low, 
                middle = TBI.prev, 
                upper = TBI.prev.high, 
                ymax = ifelse(1.5*TBI.prev.high > 1, 1, 1.5*TBI.prev.high)),
            stat = "identity")+
  labs(title = "TBI Prevalence by Census Year",
       x = "Census Year",
       y = "TBI Prevalence (%,IQR)",
       color = "TB Disease Incidence in\n Country of Origin \n(per 100,000 persons)")+
  facet_wrap(~AGE_BANDS, nrow = 5)+
  theme(text = element_text(size = 12))+
  theme(legend.text = element_text(size = 12))+
  theme(axis.text = element_text(size = 12))

# Graphs-2016 ONLY INC BAND COMPARISON

cc_2016_only <- cc%>%filter(year == "Census 2016")
cc_2016_only$inc_band_per100k <- gsub("$"," TB Disease Cases per 100,000 Persons",cc_2016_only$inc_band_per100k) 
cc_2016_only$inc_band_per100k <- factor(
  cc_2016_only$inc_band_per100k,
  levels = c("0-49 TB Disease Cases per 100,000 Persons",
             "50-99 TB Disease Cases per 100,000 Persons" ,
             "100-199 TB Disease Cases per 100,000 Persons",
             "200+ TB Disease Cases per 100,000 Persons"))
                                        
ggplot(cc_2016_only, aes(AGE_BANDS, color = AGE_BANDS))+
  geom_boxplot(aes(ymin = TBI.prev.low*1.5-TBI.prev.low, 
                   lower = TBI.prev.low, 
                   middle = TBI.prev, 
                   upper = TBI.prev.high, 
                   ymax = ifelse(1.5*TBI.prev.high > 1, 1, 1.5*TBI.prev.high)),
               stat = "identity")+
  labs(title = "TBI Prevalence by Census Year",
       x = "Age Groups",
       y = "TBI Prevalence (%,IQR)",
       color = "Age groups")+
  facet_wrap(~inc_band_per100k, nrow = 4)+
  theme(text = element_text(size = 12))+
  theme(legend.text = element_text(size = 12))+
  theme(axis.text = element_text(size = 12))+
  theme(legend.position="none")

# Top 10

tbi_top10_2016 <- na.omit(tbi_2016)%>%group_by(ISO3)%>%summarise(tbi_prev = sum(LTBP)/sum(NUMP), TBI.prev.low = sum(LTBP.low)/sum(NUMP), TBI.prev.high = sum(LTBP.high)/sum(NUMP), NUMP_v2 = sum(NUMP))%>%filter(NUMP_v2 > 100000 & tbi_prev > 0.10)%>%arrange(NUMP_v2, tbi_prev)

View(tbi_top10_2016)

tbi_top10_2011 <- na.omit(tbi_2011)%>%group_by(ISO3)%>%summarise(tbi_prev = sum(LTBP)/sum(NUMP), TBI.prev.low = sum(LTBP.low)/sum(NUMP), TBI.prev.high = sum(LTBP.high)/sum(NUMP), NUMP_v2 = sum(NUMP))%>%filter(NUMP_v2 > 100000 & tbi_prev > 0.10)%>%arrange(NUMP_v2, tbi_prev)

View(tbi_top10_2011)


tbi_top10_2006 <- na.omit(tbi_2006)%>%group_by(ISO3)%>%summarise(tbi_prev = sum(LTBP)/sum(NUMP), TBI.prev.low = sum(LTBP.low)/sum(NUMP), TBI.prev.high = sum(LTBP.high)/sum(NUMP), NUMP_v2 = sum(NUMP))%>%filter(NUMP_v2 > 100000 & tbi_prev > 0.10)%>%arrange(NUMP_v2, tbi_prev)

View(tbi_top10_2006)

ggplot(tbi_top5, aes(ISO3, fill = ISO3))+
  geom_boxplot(aes(ymin = TBI.prev.low*1.5-TBI.prev.low, 
                   lower = TBI.prev.low, 
                   middle = tbi_prev, 
                   upper = TBI.prev.high, 
                   ymax = ifelse(1.5*TBI.prev.high > 1, 1, 1.5*TBI.prev.high)),
               stat = "identity")+
  labs(title = "TBI Prevalence by Years Post-Arrival and Incidence Band",
       x = "Country of Origin",
       y = "TBI Prevalence (%,IQR)")+
  theme(legend.position="none")+
  theme(text = element_text(size = 16))+
  theme(legend.text = element_text(size = 12))+
  theme(axis.text = element_text(size = 12))



# OLD CODE =============================
# , "AGE GROUPS"
write.csv(cc, "/Users/ajordan/OneDrive - McGill University/LTBI-Aust-CEA-master/Census/Census All/tbi_prev_all_years_age2.csv")


e <- rbind(a, b, c, d)
e
f <- data.frame(year = c(2001,2006,2011, 2016))

g <- cbind(f,e)

g%>%plot_ly(x = ~year, y = ~TBI.prev)%>%add_bars(marker = list(color = c("navy")))

e$CNSY <- as.factor(e$CNSY)
fig <- plot_ly(e, y = ~TBI.prev, color = ~CNSY, type = "box")
fig


# ISO3, Houben and Dodd, 2016 incidence
# 2001
tbi_2001 <- readRDS("estimates_2001_v3.rds")
View(tbi_2001)

# Find total TBI prev among 2001 arrivals
a2 <- na.omit(tbi_2001)%>%group_by(ISO3)%>%summarise(TBI.prev.2001 = sum(LTBP)/sum(NUMP))


# 2006
tbi_2006 <- readRDS("2006_census_v4.rds")
View(tbi_2006)

b2 <- na.omit(tbi_2006)%>%group_by(ISO3)%>%summarise(TBI.prev.2006 = sum(LTBP)/sum(NUMP))



# 2011
tbi_2011 <- readRDS("2011estimates_v3.rds")
View(tbi_2011)

c2 <- na.omit(tbi_2011)%>%group_by(ISO3)%>%summarise(TBI.prev.2011 = sum(LTBP)/sum(NUMP))


# 2016
tbi_2016 <- readRDS("2016_census_v3.rds")
View(tbi_2016)

d2 <- na.omit(tbi_2016)%>%group_by(ISO3)%>%summarise(TBI.prev.2016 = sum(LTBP)/sum(NUMP))

e2 <-list(a2, b2, c2, d2)
f2 <- e2%>%reduce(full_join, by = "ISO3")
f2

        # Houben and Dodd
        hd_data <- read_csv("pmed.1002152.s007-2.csv", skip = 1)%>%
          select(iso3, `All LTBI`)
        
        
        hd_data$`All LTBI` <- gsub("\\s*\\[[^\\]+\\]", "", hd_data$`All LTBI`)
        hd_data$`All LTBI` <- gsub(",", "", hd_data$`All LTBI`)
        hd_data$`All LTBI` <- as.numeric(hd_data$`All LTBI`)
        colnames(hd_data) <- c("ISO3", "TBI.All")
        hd_data
        
        
        
        # Global population
        colnames(read_csv("global population.csv"))
        global_pop <- read_csv("global population.csv")%>%
              filter(`Series Code` == "SP.POP.TOTL")%>%
                select(`Country Name`, `Country Code`, `2014 [YR2014]`)
        
        
        colnames(global_pop) <- c("Country", "ISO3", "Pop.2014")
        View(global_pop)
        
        #Merge the dataset
        tbi_plus_pop <- merge(global_pop, hd_data, by = "ISO3")

        # Divide by global population to get prevalence
        tbi_plus_pop$TBI.All <- as.numeric(tbi_plus_pop$TBI.All)
        tbi_plus_pop$Pop.2014 <- as.numeric(tbi_plus_pop$Pop.2014)
        tbi_plus_pop$Pop.2014[46] <- 5100000 
        View(tbi_plus_pop)
        tbi_plus_pop2 <- tbi_plus_pop%>%mutate(tbi.global.2014 = TBI.All/Pop.2014)
        
        
# Global prev
tbi_global <- tbi_plus_pop2%>%select(ISO3, Country, tbi.global.2014)

View(merge(f2, tbi_global, by = "ISO3"))
tbi_compare <- merge(f2, tbi_global, by = "ISO3")
View(tbi_compare%>%relocate(Country,ISO3))


