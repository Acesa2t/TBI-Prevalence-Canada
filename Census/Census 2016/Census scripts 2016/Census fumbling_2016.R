library(tidyverse)
library(readxl)
library(janitor)
library(writexl)

# Find number of excel sheets
nsheets <- length( excel_sheets( "2016 census stratified.xlsx") )

# Read in each page, save inside a list
# aa <- read_xlsx("2016 census stratified.xlsx", sheet = 3)
# View(aa)
# Making sure that Excel sheet range is correct
# bb <- read_xlsx("2016 census stratified.xlsx", sheet = 3, range = "A6:L278")
# View(bb)
# Define empty list
list_census <- list()

# Add each Excel sheet to the list as a data frame
for(x in 1:nsheets){
  list_census[[x]] <- read_xlsx("2016 census stratified.xlsx", sheet = x, range = "A6:L278")
}

class(list_census)

# Transpose the rows and columns

l1 <- map(list_census, as.matrix)
l2 <- map(l1, t)
list_census_t  <- map(l2, as.data.frame)
list_census_t

# Make the names of the first row the column names
# Keep copy of current transposed data frame in case of edits later
list_census_t2 <- map(list_census_t, row_to_names, row_number = 1)
View(list_census_t2[[3]])

# Delete unnecessary rows and columns

lc2 <- map(list_census_t2,
  ~filter(.x, !`Total - Place of birth [7]` %in% 
           c("Total - Immigrant status and period of immigration [2]", 
             "Non-immigrants [3]", "Immigrants [4]", 
             "Non-permanent residents [6]"))%>%
  select(-"Geography = Canada [1]", -"Global non-response rate (GNR) =   5.1 %"))

lc2[[1]]
# Delete first 3 rows, fix row name, delete unnecessary columns

for(i in 1:nsheets){
  lc2[[i]][7,3] <- "2011 to 2016"
}


#Export back into Excel
write_xlsx(lc2, path = "/Users/ajordan/OneDrive - McGill University/A TB/Analyses/TB prev measures/census_data_fumble_2016.xlsx")

# Next script census_rep
