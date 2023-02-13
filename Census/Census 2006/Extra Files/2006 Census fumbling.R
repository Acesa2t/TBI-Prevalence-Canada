# Script used for removing unnecessary information and beginning to re-format 2006 census data

library(tidyverse)
library(readxl)
library(janitor)
library(writexl)

# Find number of excel sheets
nsheets <- length( excel_sheets( "All Ages 2006 Census.xlsx") )

# Read in each page, save inside a list?

# Define empty list
list_census <- list()

# Add each Excel sheet to the list as a data frame
for(x in 1:nsheets){
  list_census[[x]] <- read_xlsx("All Ages 2006 Census.xlsx", sheet = x, range = "A7:K243")
}

class(list_census)


# This time, corrections to columns need to be made before transposing

list_census[[1]] <- list_census[[1]]%>%select(-"...9", -"...10",-"...11")
View(list_census[[1]])


list_census[[2]] <- list_census[[2]]%>%select( -"...10",-"...11")
View(list_census[[2]])

lc2 <- map(list_census,
           ~select(.,-"Total - Citizenship [4]", -"Canadian citizens", -"Canadian citizens only", -"Citizens of Canada and at least one other country", -"Not Canadian citizens [5]"))
View(lc2[[1]])

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
           ~select(.,-"Total - Place of birth of respondent", -"Born in Canada",-"Born outside Canada"))

lc4 <- map(lc3, rownames_to_column, "year_of_arrival")
View(lc4[[1]])



#Export back into Excel
write_xlsx(lc4, path = "/Users/ajordan/OneDrive - McGill University/LTBI-Aust-CEA-master/Census/Census 2006/census_t.xlsx")

# Next script census_rep