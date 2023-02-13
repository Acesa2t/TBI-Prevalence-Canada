library(tidyverse)
library(readxl)
library(janitor)
library(writexl)

# Read in the provincial data of choice
province_census <- read_xlsx("/Users/ajordan/Library/CloudStorage/OneDrive-McGillUniversity/LTBI-Aust-CEA-master/Census/Provinces/Other Provinces/other provinces.xlsx")



colnames(province_census) <- c("census_year", 'province_name', "COO", "AGE_BANDS", "Before 1981", "1981 to 1990", "1991 to 2000", "2001 to 2010", "2011 to 2016")

province_remove_colnames <- province_census%>%
  select(-census_year, -province_name)
View(province_census)

# Define empty list
lc2 <- list()

# Add age groups to list
age_band_vector <- c("0 to 14 years",
                     "15 to 24 years",
                     "25 to 54 years",
                     "55 to 64 years",
                     "65 years and over")

for(i in 1:5){
  
  
  lc2[[i]] <- province_remove_colnames%>%filter(AGE_BANDS==age_band_vector[i])%>%select(-AGE_BANDS)
}

lc2[[5]]

# Transpose the rows and columns

l1 <- map(lc2, as.matrix)
l2 <- map(l1, t)
lc2[[1]]
list_census_t  <- map(l2, as.data.frame)
View(list_census_t[[1]])

# Make the names of the first row the column names
# Keep copy of current transposed data frame in case of edits later
list_census_t2 <- map(list_census_t, row_to_names, row_number = 1)
View(list_census_t2[[1]])

# Delete unnecessary rows and columns

lc3 <- map(list_census_t2,
           ~select(.,-"Total - Place of birth", -"Born in Canada",-"Born outside Canada"))

lc4 <- map(lc3, rownames_to_column, "year_of_arrival")
View(lc4[[1]])



#Export back into Excel
#aberta-, british columbia, ontario, quebec
write_xlsx(lc4, path = "/Users/ajordan/Library/CloudStorage/OneDrive-McGillUniversity/LTBI-Aust-CEA-master/Census/Provinces/Other Provinces/census_t_other_provinces.xlsx")

# Next script census_rep