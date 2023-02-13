# Working directory will be /Data

tbi_2006 <- readRDS("2006_census_v4.rds")
View(tbi_2006)

hist_graph <- tbi_2006%>%
  mutate(AGE_BANDS_CHECK = ifelse(
    AGEP < 15, "0-14", ifelse(
      AGEP >= 15 & AGEP < 25, "15-24", ifelse(
        AGEP >= 25 & AGEP < 55, "25-54",ifelse(
          AGEP >= 55 & AGEP < 65, "55-64",ifelse(
            AGEP >= 65 & AGEP <= 200, "65+", "No value"))))))%>%
  group_by(AGE_BANDS_CHECK)

weighted.median(hist_graph$AGEP, hist_graph$NUMP) 
weighted.quantile(hist_graph$AGEP, hist_graph$NUMP, probs = c(0.25, 0.75))

View(na.omit(tbi_2006)%>%group_by(YARP)%>%filter(ISO3 == "PRT")%>%summarise(sum(LTBP)/sum(NUMP)))
# Find total TBI prev among 2006 arrivals

na.omit(tbi_2006)%>%summarise(tbi_tot = sum(NUMP))
View(na.omit(tbi_2006)%>%group_by(ISO3)%>%summarise(tbi_tot = sum(LTBP)/sum(NUMP)))

# Median for Canada
tbi_2006 <- na.omit(tbi_2006)%>%mutate(AARP = YARP-YOBP)
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


View(tbi.func.test(df=tbi_2006, vec=vector_test))


# COO
View(na.omit(tbi_2006%>%group_by(ISO3)%>%summarise(tbi_tot = sum(LTBP)/sum(NUMP))))

class(tbi_2006$YOBP)
class(tbi_2006$YARP)
View(na.omit(tbi_2006)%>%group_by(ISO3)%>%filter(ISO3%in%c("PHL", "IND","CHN","USA","DEU","GBR", "VNM", "HKG","MAC"))%>%mutate(AARP = YARP - YOBP)%>%summarize(tbi_prev_CAN = sum(LTBP)/sum(NUMP), med_yarp = median(YARP), med_aarp = median(AARP), med_age = median(AGEP)))

tbi_est <- tbi_2006

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
       filter(ISO3=="PRT")%>%
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
