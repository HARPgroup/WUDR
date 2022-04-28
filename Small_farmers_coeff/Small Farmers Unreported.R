WUDR_github<-"F:/My Drive/WUDR/WUDR_Github/WUDR"
setwd(WUDR_github)

library(tidyverse)
library(tmap)
library(rgdal)
library(stringr)
options(scipen = 9999)


VA_counties<-readOGR(paste0(WUDR_github, "/VA_counties_sp"), layer="VA_counties")
county.codes <- read_csv(paste0(WUDR_github, "/csv_files/county_codes_census.csv")) 
county.codes_deq <- VA_counties@data[,c(1,3)]

#########################################################################
#Load funtions and Census data
source(paste0(WUDR_github,"/Small_farmers_coeff/Functions/fn_Coefficent1.R"))
rm(absent, Year)

# QS_data function uses binned data and county summary to fill the D values. The intermediate steps are detailed in the function.

# fn_Area_TH calculates teh area under threshold.



Year = 2002
QS_data(Year) 
fn_Area_TH(10, Year)

rm(bin.char, binned_irrigated_area, binned_operations,nass_binned,nass.ag.data, nass.ops.data, opeartions_list) # Not required for further calculations.

##################################################################################################################################
# Load DEQ data   

load(paste0(WUDR_github, "/dat_load/DEQ_data_total_nd_irr.Rdata"))

# Total_deq_counties are the sum of  NON-POWER withdrawals for each census year at county level
# Irri_deq_counties are the sum of  Irrigtaion withdrawals for each census year at county level



Non.Reported_Coefficient1 <- Non.Reported

Non.Reported_Coefficient1 <- merge.data.frame(Non.Reported_Coefficient1, county.codes, by.x = "County" , by.y = "County_Name")

DEQ_withdarwals <- Irri_deq_county %>% 
  filter(Year == Year) %>% 
  filter(Facility_withdrawal_mg >0)

Non.Reported_Coefficient1 <- left_join(DEQ_withdarwals[, c(2,4)], Non.Reported_Coefficient1, by = c("COUNTYFP"= "County_Code"))

Non.Reported_Coefficient1$Method1_Unreported <- ((Non.Reported_Coefficient1$Facility_withdrawal_mg*100)/
                                                   (100-Non.Reported_Coefficient1$Pct.under.TH.of.total.Irr.area))-Non.Reported_Coefficient1$Facility_withdrawal_mg

Non.Reported_Coefficient1$Method1_Unreported<- ifelse(Non.Reported_Coefficient1$Pct.under.TH.of.total.Irr.area == 100,Non.Reported_Coefficient1$Facility_withdrawal_mg,
                                                      Non.Reported_Coefficient1$Method1_Unreported )

