
WUDR_github<-"F:/My Drive/WUDR/WUDR_Github/WUDR"
setwd(WUDR_github)

library(tidyverse) 
library(rgdal)
library(DescTools)
options(scipen = 9999)

# Load VA Shapefile to be used later
VA_counties<-readOGR(paste0(WUDR_github, "/VA_counties_sp"), layer="VA_counties")

county.codes <- read_csv(paste0(WUDR_github, "/csv_files/county_codes_census.csv"))

load(paste0(WUDR_github,"/dat_load/PRECIP_JUNE_AUGUST_ALL_YEARS.RData")) # PRISM PPT data

load(paste0(WUDR_github,"/dat_load/PRISM_county_codes.RData")) # County Codes for PRISM

PPT <- lapply(ppt_list_yearly, function(x)
  mutate(x, Irrigation = round(508 - PPT,0)))
PPT <- bind_rows(PPT, .id = "Year")

PPT <- left_join(PPT,PRISM_county_codes, by = "name" )
PPT <- PPT[,c(1,5,2,3,4)]
colnames(PPT)[c(1:2,4)] <- c("YEAR", "COUNTYFP","Precip")
PPT$YEAR <- as.numeric(PPT$YEAR)

Dry_Year <- PPT %>% group_by(COUNTYFP) %>% slice(which.max(Irrigation))
Wet_Year <- PPT %>% group_by(COUNTYFP) %>% slice(which.min(Irrigation))
Avg_Year <- PPT %>% 
  group_by(COUNTYFP) %>%
  summarise(name = first(name),
    Precip= mean(Precip),
    Irrigation = mean(Irrigation))

Ag_census <- function(Year){
source(paste0(WUDR_github,"/Small_farmers_coeff/Functions/fn_Coefficent1.R"))
load(paste0(WUDR_github, "/dat_load/DEQ_data_total_nd_irr.Rdata"))
IRR_DEQ_withdarwals <<- Irri_deq_county %>% 
    filter(YEAR == Year) %>% 
    filter(Facility_withdrawal_mg >0)

  QS_data(Year) 
  fn_Area_TH(10, Year)
  
  Non.Reported_Coefficient1 <- Non.Reported
  
  Non.Reported_Coefficient1 <- left_join(Non.Reported_Coefficient1, county.codes, by = c("County" = "County_Name"))
  return(Non.Reported_Coefficient1)
  }

Ag_census_2017 <- Ag_census(2017)

Dry_Year <- left_join(Dry_Year, Ag_census_2017[,c(2,7,8)], by = c("COUNTYFP" = "County_Code"))
Dry_Year <- Dry_Year %>% 
  mutate(Irrigation_demand = round(Total.Irri.Area * (Irrigation  /25.5)*27154/1000000,2))

sum(Dry_Year$Irrigation_demand, na.rm = TRUE)         

Wet_Year <- left_join(Wet_Year, Ag_census_2017[,c(2,7,8)], by = c("COUNTYFP" = "County_Code"))
Wet_Year <- Wet_Year %>% 
  mutate(Irrigation_demand = round(Total.Irri.Area * (Irrigation  /25.5)*27154/1000000,2))

sum(Wet_Year$Irrigation_demand, na.rm = TRUE)  

Avg_Year <- left_join(Avg_Year, Ag_census_2017[,c(2,7,8)], by = c("COUNTYFP" = "County_Code"))
Avg_Year <- Avg_Year %>% 
  mutate(Irrigation_demand = round(Total.Irri.Area * (Irrigation  /25.5)*27154/1000000,2))

sum(Avg_Year$Irrigation_demand, na.rm = TRUE) 

sum(IRR_DEQ_withdarwals$Facility_withdrawal_mg, na.rm = TRUE)
