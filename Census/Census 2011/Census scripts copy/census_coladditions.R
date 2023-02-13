# This document will be for re-formatting the countries into one column called country_of_origin and adding a column called "census_year" which in this case is 2016 

library(readxl)
library(writexl)
library(janitor)
library(tidyverse)

# Read in Data

census_cols <- read_xlsx("2census_data_clean.xlsx")

# Remove unnecessary columns and clean up the others
census_cols <- census_cols%>%
  select(-total_placeofbirth, -`Born outside Canada`)

names(census_cols)[names(census_cols) == "Born in Canada"] <- 'Canada'

  # To get the indices of columns at the end of the dataset that need to be deleted
  # data.frame(colnames(census))

# Attempt to use gather to solve my problem then create a bigger problem

census_cols <- gather(census_cols, key = "country_of_origin", value = "num_persons", 3:271)
View(census_cols)
# Use regular expression to remove some of the bracketed numbers
census_cols$country_of_origin <- gsub("\\[|\\]|[0-9]", "", census_cols$country_of_origin)

# I'm not convinced that I didn't create a bigger problem but this looks like it worked so I won't complain for now, I'll just move on

census_cols <- census_cols%>%
  mutate(census_year = 2016)

#View(census_cols%>%filter(age <15))

# Write to Excel again
write_xlsx(census_cols, path = "/Users/ajordan/OneDrive - McGill University/A TB/Analyses/TB prev measures/Census/2census_additions.xlsx")

# Remaining problems: How to assign year_of_birth as a column, this same procedure will likely have to be edited and re-done for other census years, the country names may not be compatible with the countrycode function used within cleanseCensus
