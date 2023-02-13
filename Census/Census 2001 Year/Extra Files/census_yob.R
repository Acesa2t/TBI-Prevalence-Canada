
#========SCRIPT NOW USELESS
# This script will be used to convert age groups into a set of year of births, or *median year*?

library(readxl)
library(writexl)
library(janitor)
library(tidyverse)

# Read in Data

census_yob <- read_xlsx("2census_additions.xlsx")

# Will use mutate to populate the column year_of_birth

census_yob2 <- census_yob%>%
  mutate(year_of_birth = 2016 - age)

View(census_yob2)

# Write to Xcel to preserve formatting before I move to next major step
write_xlsx(census_yob2, path = "/Users/ajordan/OneDrive - McGill University/A TB/Analyses/TB prev measures/Census/2census_yearofbirth.xlsx")



# After this, will need to see if the file will run within cleanseCensus or if, based on Katie's census data, it will even be necessary
# May only need to use countrycode to add ISO3 and use that other function that will "fix" countries that didn't convert properly...whatever that may mean