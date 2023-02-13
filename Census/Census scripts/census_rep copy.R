# Prior script census fumbling.R

# Census rep

library(tidyverse)
library(readxl)
library(writexl)
library(janitor)
library(taRifx)

# Find number of excel sheets
num_sheets <- length(excel_sheets("census_total.xlsx"))

# Define empty list
list_census_p2 <- list()

# Add each Excel sheet to the list as a data frame
for(x in 1:num_sheets){
  list_census_p2[[x]] <- read_xlsx("census_total.xlsx", sheet = x)
}

#View(list_census_p2[[1]])

# Extended to 127=============================================================
x_age <- list(c(0:14), c(15:24), c(25:54), c(55:64), c(65:127))

list_census_len <- list()

for(i in 1:length(x_age)){
  
  list_census_len[[i]] <- list_census_p2[[i]]%>%
    slice(rep(1:n(), each = length(x_age[[i]])))%>%
    mutate(age = rep(x_age[[i]], times = nrow(list_census_p2[[2]])/length(x_age[2])))%>%
    select(age, everything())
}

write_xlsx(list_census_len, path = "/Users/ajordan/OneDrive - McGill University/LTBI-Aust-CEA-master/Census/census_rep.xlsx")


# Next script census_calc new.R
