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

# fn_Area_TH calculates the area under threshold.



small_counties_coefficient <- function(Year){
QS_data(Year) 
fn_Area_TH(10, Year)

# rm(bin.char, binned_irrigated_area, binned_operations,nass_binned,nass.ag.data, nass.ops.data, opeartions_list) # Not required for further calculations.


Non.Reported_Coefficient1 <- Non.Reported

Non.Reported_Coefficient1 <- left_join(Non.Reported_Coefficient1, county.codes, by = c("County" = "County_Name"))

##################################################################################################################################
# Load DEQ data   

load(paste0(WUDR_github, "/dat_load/DEQ_data_total_nd_irr.Rdata"))

# Total_deq_counties are the sum of  NON-POWER withdrawals for each census year at county level
# Irri_deq_counties are the sum of  Irrigation withdrawals for each census year at county level


IRR_DEQ_withdarwals <- Irri_deq_county %>% 
  filter(YEAR == Year) %>% 
  filter(Facility_withdrawal_mg >0)

Total_DEQ_withdarwals <- Total_deq_county %>% 
  filter(YEAR == Year) %>% 
  filter(Facility_withdrawal_mg >0)

###############################################################################
# Withdrawals as function of total withdrawals

Fn_of_TOTAL_withdrawals <- left_join(Non.Reported_Coefficient1,Total_DEQ_withdarwals[, c("COUNTYFP","Facility_withdrawal_mg")], by = c("County_Code"= "COUNTYFP"))

Fn_of_TOTAL_withdrawals <- Fn_of_TOTAL_withdrawals %>%
  drop_na(Facility_withdrawal_mg)

Fn_of_TOTAL_withdrawals$Method1_Unreported <- round(((Fn_of_TOTAL_withdrawals$Facility_withdrawal_mg*100)/
                                                   (100-Fn_of_TOTAL_withdrawals$Pct.under.TH.of.total.Irr.area))-Fn_of_TOTAL_withdrawals$Facility_withdrawal_mg ,2)

Fn_of_TOTAL_withdrawals$C_tot = round (Fn_of_TOTAL_withdrawals$Method1_Unreported / Fn_of_TOTAL_withdrawals$Facility_withdrawal_mg ,2)

write.csv(Fn_of_TOTAL_withdrawals, paste0(WUDR_github,"/Output_Tables/",Year, "Coefficient1.csv"))

plotdat<-sp::merge(VA_counties,Fn_of_TOTAL_withdrawals, by.x = "COUNTYFP", by.y = "County_Code")

# Check if merge was correct
sum(Fn_of_TOTAL_withdrawals$Irr.Area.above.TH, na.rm = TRUE)
sum(plotdat@data$Irr.Area.above.TH, na.rm=TRUE)

p1<-tm_shape(plotdat)+
  tm_polygons("C_tot", title = "Unreported Coefficient",
              breaks = c(0,0.05,0.1,0.2,0.5,0.75,Inf),
              # n=5,style="jenks",
              id="NAMELSAD")+
  tm_layout(main.title = paste0(Year," Small farmers unreported coefficient"),
            legend.outside = FALSE,
            legend.title.size = 1.2,
            legend.text.size = 0.8,
            legend.position = c("left","top"),
            legend.bg.alpha = 1)
p1

tmap_save(p1, paste0(WUDR_github,"/plots/Coefficient1/",Year, "Irr.Area.Under.TH.png"),  width = 8, height = 5, units = 'in')
}

small_counties_coefficient(2002)
small_counties_coefficient(2007)
small_counties_coefficient(2012)
small_counties_coefficient(2017)
