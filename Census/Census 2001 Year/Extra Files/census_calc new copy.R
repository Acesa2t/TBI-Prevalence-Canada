library(tidyverse)
library(reshape2)
library(readxl)
library(writexl)
library(janitor)
library(taRifx)

# Need to remove indices 273-303 from all of these datasets then try again

# Find number of excel sheets
num_sheets <- length(excel_sheets("census_rep.xlsx"))

# Define empty list
list_calc <- list()

# Add each Excel sheet to the list as a data frame
for(x in 1:num_sheets){
  list_calc[[x]] <- read_xlsx("census_rep.xlsx", sheet = x)
}
View(list_calc[[2]])

# Remove unnecessary columns and clean up the others

census_cols <- map(list_calc, ~select(.x, -total_placeofbirth, -`Born outside Canada`))
View(census_cols[[1]])

# To get the indices of columns at the end of the dataset that need to be deleted
# data.frame(colnames(census))

census_cols_df1 <- census_cols[[1]]
census_cols_df2 <- census_cols[[2]]
census_cols_df3 <- census_cols[[3]]
census_cols_df4 <- census_cols[[4]]
census_cols_df5 <- census_cols[[5]]

census_cols <- rbind(census_cols_df1, census_cols_df2, census_cols_df3, census_cols_df4, census_cols_df5)

census_cols <- census_cols[,c(1, 4:271)]
head(census_cols)


# Attempt to use gather to solve my problem then create a bigger problem

#census_cols <- gather(census_cols, key = "country_of_origin", value = "num_persons", c(1, 4:271))

census_cols <- as.data.frame(census_cols)
census_colsv2 <- melt(census_cols, id.vars = c("age", "age_group","year_of_arrival"))

colnames(census_colsv2) <- c("AGEP", "YARP", "COO", "NUMP")

# I'm not convinced that I didn't create a bigger problem but this looks like it worked so I won't complain for now, I'll just move on

census_colsv2 <- census_colsv2%>%
  mutate(census_year = 2016, YOBP = 2016-AGEP)

census_colsv2<-subset(census_colsv2,census_colsv2$YOBP<=census_colsv2$YARP)
View(census_colsv2)

# Use regular expression to remove some of the bracketed numbers
census_colsv2$COO <- gsub("\\[|\\]|[0-9]", "", census_colsv2$COO)

census_colsv2%>%group_by("age_group", "year_of_arrival_group","")%>%mutate(NUMP_v2 = NUMP/max(row_number()))

write_xlsx(census_colsv2, path = "/Users/ajordan/OneDrive - McGill University/LTBI-Aust-CEA-master/Census/2census_data_clean.xlsx")

