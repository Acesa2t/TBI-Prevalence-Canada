# Working directory will be /Data
library(tidyverse)
library(readxl)
library(writexl)
tbi_2001 <- readRDS("estimates_2001_v4.rds")
View(tbi_2001)

# Find total TBI prev among 2001 arrivals
na.omit(tbi_2001)%>%summarise(tbi_tot = sum(LTBP)/sum(NUMP))
na.omit(tbi_2001)%>%summarise(tbi_tot = sum(NUMP))


View(na.omit(tbi_2001%>%group_by(ISO3)%>%summarise(tbi_tot = sum(LTBP)/sum(NUMP))))


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
    e_inc_100k >= 0 & e_inc_100k < 10, "0-9", ifelse(
      e_inc_100k >= 10 & e_inc_100k < 30, "10-29", ifelse(
        e_inc_100k >= 30 & e_inc_100k < 50, "30-49", ifelse(
          e_inc_100k >= 50 & e_inc_100k < 100, "50-99", ifelse(
            e_inc_100k >= 100 & e_inc_100k < 200, "100-199",ifelse(
              e_inc_100k >= 200 & e_inc_100k <= 10000, "200+", "No value")))))))


View(na.omit(tbi_data_bands)%>%
       group_by(inc_band_per100k)%>%
       summarise(sum(LTBP)/sum(NUMP), sum(LTBP.low)/sum(NUMP), sum(LTBP.high)/sum(NUMP)))


# AGE GROUPS (AGE AT CENSUS)=====================================================================

tbi_age_groups <- tbi_data_bands%>%
  mutate(AGE_BANDS = ifelse(
    AGEP < 15, "0-14", ifelse(
      AGEP >= 15 & AGEP < 35, "15-34", ifelse(
        AGEP >= 35 & AGEP < 55, "35-54",ifelse(
          AGEP >= 55 & AGEP <= 64, "55-64", ifelse(
            AGEP >= 65 & AGEP <= 200, "65+", "No value"))))))


# tbi_age_groups <- tbi_data_bands%>%
#   mutate(AGE_BANDS = ifelse(
#     AGEP < 15, "0-14", ifelse(
#       AGEP >= 15 & AGEP < 35, "15-34", ifelse(
#         AGEP >= 35 & AGEP < 65, "35-65",ifelse(
#           AGEP >= 65 & AGEP <= 200, "65+", "No value")))))

tbi_pop <- na.omit(tbi_age_groups)%>%
       group_by(AGE_BANDS, ISO3,inc_band_per100k)%>%
       summarise(NUMP = sum(NUMP))

# Transpose so that age groups will become new columns
View(tbi_pop)

tbi_pop_wider <- tbi_pop%>%pivot_wider(id_cols = c(ISO3, inc_band_per100k), names_from = AGE_BANDS, values_from = NUMP)

View(tbi_pop_wider)

tab <- read_xlsx("tables set up2 copy 2.xlsx", skip = 1, sheet = 2)
colnames(tab)[3] <- "ISO3"
tab2 <- tab%>%select(`Country ID`, `Country of origin`, ISO3, `INC BAND`, `WHO region`)
str(tab2)

anti_join(tab2, tbi_pop_wider, by = "ISO3")

table_merge <- full_join(tab2, tbi_pop_wider, by = "ISO3")
table_merge2 <- table_merge%>%filter(!`Country of origin`%in%c("Born in Canada", "Born outside Canada"))%>%filter(!ISO3%in%c("ASM","FSM","NIU","PLW","SMR","TKL","TUV"))%>%select(-`INC BAND`)
View(table_merge2)

write_xlsx(table_merge2, path = "/Users/ajordan/OneDrive - McGill University/LTBI-Aust-CEA-master/Census/Census 2001 Year/2001 census scripts/Requests/table set up2 pop table.xlsx")


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


