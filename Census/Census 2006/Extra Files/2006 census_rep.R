# Prior script census fumbling.R

# Census rep
# Use this script to replicate age and make YARP groups
rm(list=ls()) 

library(tidyverse)
library(readxl)
library(writexl)
library(janitor)

# Find number of excel sheets
num_sheets <- length(excel_sheets("census_t.xlsx"))

# Define empty list
list_census_p2 <- list()

# Add each Excel sheet to the list as a data frame
for(x in 1:num_sheets){
  list_census_p2[[x]] <- read_xlsx("census_t.xlsx", sheet = x)
}


list_census_len <- list()
x_age <- list(c(0:14), c(15:24), c(25:54), c(55:64), c(65:85))

# Replicate age
for(i in 1:length(x_age)){
  
  list_census_len[[i]] <- list_census_p2[[i]]%>%
    slice(rep(1:n(), each = length(x_age[[i]])))%>%
    mutate(age = rep(x_age[[i]], times = nrow(list_census_p2[[i]])/length(x_age[i])))%>%
    select(age, everything())
}


census_cols_df1 <-  list_census_len[[1]]
census_cols_df2 <-  list_census_len[[2]]
census_cols_df3 <-  list_census_len[[3]]
census_cols_df4 <-  list_census_len[[4]]
census_cols_df5 <-  list_census_len[[5]]

census_cols <- rbind(census_cols_df1, census_cols_df2, census_cols_df3, census_cols_df4, census_cols_df5)
View(census_cols)
unique(census_cols$year_of_arrival)

# Replicate year_of_arrival

# if(){}
# else if(){}
# else

  # Replicate rows to get number of desired rows to fill in
census_cols_expanded <- census_cols[rep(row.names(census_cols), 
                                        ifelse(census_cols$year_of_arrival=="Before 1961",1,
                                               ifelse(census_cols$year_of_arrival == "1961-1970" | census_cols$year_of_arrival == "1971-1980" | census_cols$year_of_arrival == "1981-1990" | census_cols$year_of_arrival == "1991-2000", 10, ifelse(census_cols$year_of_arrival == "2001-2006", 6, 1) ))),]


  # Fill in conditionally-will include people who were not alive
#rep seq
c_rep_year<- census_cols_expanded%>%mutate(
  yarp = ifelse(year_of_arrival == "Before 1961",
                "Before 1961",
                ifelse(year_of_arrival == "1961-1970",
                       seq(from = 1961, to = 1970), 
                       ifelse(year_of_arrival == "1971-1980",
                              seq(from = 1971, to = 1980),
                              ifelse(year_of_arrival == "1981-1990",
                                     seq(from = 1981, to = 1990),
                                     ifelse(year_of_arrival == "1991-2000",
                                            seq(from = 1991, to = 2000),
                                            ifelse(year_of_arrival == "2001-2006"),
                                          seq(from = 2001, to = 2006),
                                            year_of_arrival
                              
                       )))
)))

View(c_rep_year%>%select(age, year_of_arrival, yarp, Americas))


write_xlsx(c_rep_year, path = "/Users/ajordan/OneDrive - McGill University/LTBI-Aust-CEA-master/Census/Census 2006/census_rep_2006.xlsx")


# Next script census_calc new.R
