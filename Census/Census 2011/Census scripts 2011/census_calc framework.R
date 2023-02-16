library(tidyverse)
library(readxl)
library(writexl)
library(janitor)

# Read in the census data
census_calc <- read_xlsx("census_total.xlsx", sheet = 2)
View(census_calc)

# Rep the columns so that they go from age 0-14. 
# Get the number of columns for division later.
x_age <- c(0:14)
census_calc <- census_calc%>%
  slice(rep(1:n(), each = 15))%>%
  mutate(age = rep(x_age, times = 37))%>%
  select(age, everything())



# Split the data frame into two separate data frames
df1 <- census_calc %>%
  select(age_group, sex, year_of_arrival)

df2 <- census_calc %>%
  select(-age_group, -sex, -year_of_arrival)

# Number of columns even though can literally just look at global environment
ncol(df2)
i <- 1:303
      
      # Function from Joachim Schork on Statistics Globe, thanks 

df2[ , i] <- apply(df2[ , i], 2,            
                    function(x) as.numeric(as.character(x)))

df2 <- sapply( df2, as.numeric )
df2

#Double check that the class has changed
sapply(df2, class)

# Divide second data frame by 10
df2[2:31,1:303] <- df2[2:31,1:303]/10
df2[32:37,1:303] <- df2[32:37,1:303]/6

View(df2)

# Re-merge data frames
df1$year_of_arrival <- factor(df1$year_of_arrival)

census_calc2 <- distinct(merge(df1,df2))
census_calc2 <- cbind(df1,df2)
View(census_calc2)
# When using a full list, will use cbind to merge the separate pages




x_age <- list(c(0:14), c(15:24), c(25:54), c(55:64), c(65:85))

list_census_len <- list()

for(i in 1:length(x_age)){
  
  list_census_len[[i]] <- list_census_p2[[i]]%>%
    slice(rep(1:n(), each = length(x_age[[i]])))%>%
    mutate(age = rep(x_age[[i]], times = nrow(list_census_p2[[2]])/length(x_age[2])))%>%
    select(age, everything())
}
