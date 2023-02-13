# Prior script regions.R
#----------------------
# Later problem: Will some of these islands that fall under 'not classified' need their populations added to the larger country that they are a part of? (i.e. American Samoa)
#----------------------
library(tidyverse)

census_afr <- readRDS("2006_census_who.rds")
head(census_afr)

# AFRO will need to be split into HIV-high and HIV-low
# Source: http://stoptb.org/global/plan/funding/region/african.asp

afr_high <- c("Botswana", "Burundi", "Cameroon", "Central African Republic", "Congo", "Cte d'Ivoire", "Democratic Republic of Congo", "Ethiopia", "Gabon", "Kenya", "Malawi", "Mozambique", "Namibia", "Nigeria", "Lesotho", "Rwanda", "South Africa", "Swaziland", "Uganda", "United Republic of Tanzania", "Zambia", "Zimbabwe")
afr_high

afr_low <- as.character(quote(c("Algeria", "Angola", Benin, "Burkina Faso", "Cape Verde", Chad, Comoros, "Equatorial Guinea", Eritrea, Gambia, Ghana, Guinea, "Guinea-Bissau", Liberia, Madagascar, Mali, Mauritania, Mauritius, Niger, "Sao Tome & Principe", Senegal, Seychelles, "Sierra Leone", Togo)))[-1]
afr_low


afr <- census_afr%>%filter(WHO_regions == "AFRO")

setdiff(afr_high, afr$COO)
setdiff(afr_low, afr$COO)

# Africa High: "CÙte d'Ivoire", "Congo, Republic of the" (Congo), "Congo, Democratic Republic of the", "South Africa, Republic of", "Tanzania"

# Africa-Low: "Cabo Verde", "Sao Tome and Principe"

# Change the names of places so that they match

afr_high <- recode(afr_high, 
                   "Congo" = "Congo, Republic of the", 
                   "Cte d'Ivoire" = "CÙte d'Ivoire", 
                   "Democratic Republic of Congo" = "Congo, Democratic Republic of the", 
                   "South Africa" = "South Africa, Republic of", 
                   "United Republic of Tanzania" =  "Tanzania")

dput(as.character(afr_high))

afr_low <- recode(afr_low, 
                  "Cape Verde" = "Cabo Verde", 
                  "Sao Tome & Principe" = "Sao Tome and Principe")

paste("'",as.character(afr_low),"'",collapse=", ",sep="")


match(afr_high, census_afr$COO)
match(afr_low, census_afr$COO)

# Separate AFRO into AFR-High and AFR-Low

census_afr_new <- census_afr%>%
  mutate(WHO_regions =
  ifelse(
    COO%in%
         c("Botswana", 
           "Burundi", 
           "Cameroon", 
           "Central African Republic",
            "Congo, Republic of the", 
           "CÙte d'Ivoire", 
           "Congo, Democratic Republic of the", 
                                         "Ethiopia", "Gabon", "Kenya", "Malawi", "Mozambique", "Namibia", 
                                         "Nigeria", "Lesotho", "Rwanda", "South Africa, Republic of", 
                                         "Swaziland", "Uganda", "Tanzania", "Zambia", "Zimbabwe"),

recode(WHO_regions, "AFRO" = "AFR-High"),

recode(WHO_regions, "AFRO" = "AFR-Low"))
)

unique(census_afr_new$WHO_regions)

saveRDS(census_afr_new, 
        file = "/Users/ajordan/OneDrive - McGill University/LTBI-Aust-CEA-master/Census/Census 2006/2006_census_afr.rds")

# Next script census_NC.R
