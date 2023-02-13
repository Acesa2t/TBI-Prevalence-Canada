# Census 2006 Outputs

tbi_2006 <- readRDS("estimates_2006_v2.rds")
tbi_2006

tbi_2006%>%summarise(tbi_tot = sum(LTBP)/sum(NUMP))
