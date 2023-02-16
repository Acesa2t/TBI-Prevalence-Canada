library(tidyverse)
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


# Split the data frame into two separate data frames
df1 <- map(list_calc, ~select(.x, age, year_of_arrival))
View(df1[[2]])

df2 <- map(list_calc, ~select(.x, -age_group, -age, -sex, -year_of_arrival))
ncol(df2[[2]])

for(x in 1:num_sheets){
# Convert all columns within every data frame to numeric

  df2[[x]] <- sapply(df2[[x]], as.numeric)
}



View(df2[[1]])
# technically speaking could use some combo of assign and paste in order to make new data frames, but only 5 dataframes in the list so I'll just do this manually

# Divide second data frame by 10 or 6
df.divide1 <- df2[[1]]
df.divide2 <- df2[[2]]
df.divide3 <- df2[[3]]
df.divide4 <- df2[[4]]
df.divide5 <- df2[[5]]
View(df.divide1)

#count instances for division
table(list_calc[[1]][[5]])
table(list_calc[[2]][[5]])
table(list_calc[[3]][[5]])
table(list_calc[[4]][[5]])
table(list_calc[[5]][[5]])
View(list_calc[[5]])
#------------------------------------------------------------------------------
# AGES 0-14
#View(df.divide1[1:4,273:303])

df.divide1[1:315,1:303] <- df.divide1[1:315,1:303]/315
df.divide1[316:465,1:303] <- df.divide1[316:465,1:303]/150
df.divide1[466:555,1:303] <- df.divide1[466:555,1:303]/90

#View(df.divide1)

# AGES 15-24
df.divide2[1:110,1:303] <- df.divide2[1:110,1:303]/110
df.divide2[111:210,1:303] <- df.divide2[111:210,1:303]/100
df.divide2[211:310,1:303] <- df.divide2[211:310,1:303]/100
df.divide2[311:370,1:303] <- df.divide2[311:370,1:303]/60
#View(df.divide2)

# AGES 25-54
df.divide3[1:30,1:303] <- df.divide3[1:30,1:303]/30
df.divide3[31:330,1:303] <- df.divide3[31:330,1:303]/300
df.divide3[331:630,1:303] <- df.divide3[331:630,1:303]/300
df.divide3[631:930,1:303] <- df.divide3[631:930,1:303]/300
df.divide3[931:1110,1:303] <- df.divide3[931:1110,1:303]/180

# AGES 55-64
df.divide4[1:10,1:303] <- df.divide4[1:10,1:303]/10
df.divide4[11:110,1:303] <- df.divide4[11:110,1:303]/100
df.divide4[111:210,1:303] <- df.divide4[111:210,1:303]/100
df.divide4[211:310,1:303] <- df.divide4[211:310,1:303]/100
df.divide4[311:370,1:303] <- df.divide4[311:370,1:303]/60

# AGES 65+
df.divide5[1:21,1:303] <- df.divide5[1:21,1:303]/21
df.divide5[22:231,1:303] <- df.divide5[22:231,1:303]/210
df.divide5[232:441,1:303] <- df.divide5[232:441,1:303]/210
df.divide5[442:651,1:303] <- df.divide5[442:651,1:303]/210
df.divide5[652:777,1:303] <- df.divide5[652:777,1:303]/126
View(df.divide5)
# Back into list
#z <- df1[[1]][3]

# Trying something else
#calc_bind <- rbind(df.divide1, df.divide2, df.divide3, df.divide4, df.divide5)




re_list <- list(df.divide1, df.divide2, df.divide3, df.divide4, df.divide5)
View(re_list[[1]])
#re_list <- map(re_list, ~mutate(.x, year_of_arrival = z$year_of_arrival))
View(re_list)

list_calc2 <- list()
# Re-merge data frames-apparently the hard way. I'm not spending 4 hours troubleshooting this

new_df1 <- cbind(df1[[1]], re_list[[1]])
View(new_df1)
new_df2 <- cbind(df1[[2]], re_list[[2]])
new_df3 <- cbind(df1[[3]], re_list[[3]])
new_df4 <- cbind(df1[[4]], re_list[[4]])
new_df5 <- cbind(df1[[5]], re_list[[5]])

census_data_clean <- bind_rows(new_df1,new_df2,new_df3,new_df4,new_df5)

write_xlsx(census_data_clean, path = "/Users/ajordan/OneDrive - McGill University/A TB/Analyses/TB prev measures/2census_data_clean.xlsx")

View(census_data_clean)
