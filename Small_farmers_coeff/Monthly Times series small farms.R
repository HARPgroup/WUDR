WUDR_github<-"F:/My Drive/WUDR/WUDR_Github/WUDR"
setwd(WUDR_github)

library(tidyverse) 
library(rgdal)
options(scipen = 9999)
# Load VA Shapefile to be used later
VA_counties<-readOGR(paste0(WUDR_github, "/VA_counties_sp"), layer="VA_counties")

county.codes <- read.csv(paste0(WUDR_github, "/csv_files/county_codes_census.csv")) 

Monthly_Pct  <- read.table("http://deq1.bse.vt.edu:81/files/vahydro/irr_frac_fips.txt", header = TRUE, sep = "", dec = ".")

Check <- Monthly_Pct %>% 
  group_by(fips_hydrocode) %>% 
  summarise(Pct = sum(mo_frac))

rm(Check)


load(paste0(WUDR_github,"/dat_load/SF_times_series_Median.RData"))
rm(SF_Unreported_Median_Irr_Coeff)

SF_Unreported_Median_UnderTh <- left_join(SF_Unreported_Median_UnderTh,VA_counties@data[,c(1,3)], by = c("County_Code" = "COUNTYFP"))

Monthly_timeseries_SF_unreported <-data.frame( Date = seq(as.Date("2002/1/1"), as.Date("2017/12/31"), "month" ))
Monthly_timeseries_SF_unreported$Year = lubridate::year(Monthly_timeseries_SF_unreported$Date)   
Monthly_timeseries_SF_unreported$Month = lubridate::month(Monthly_timeseries_SF_unreported$Date)   
Monthly_timeseries_SF_unreported <- left_join(SF_Unreported_Median_UnderTh[,c(1,2,3,8,7)],Monthly_timeseries_SF_unreported, by = "Year" )                                       
Monthly_timeseries_SF_unreported$Date = NULL                                      

Monthly_timeseries_SF_unreported$GEOID <- as.numeric(Monthly_timeseries_SF_unreported$GEOID)
Monthly_timeseries_SF_unreported <- left_join(Monthly_timeseries_SF_unreported, Monthly_Pct, by= c("GEOID"= "fips_hydrocode", "Month" = "mo_num"))                                       



Default_distribution <- data.frame(Month = 1:12, mo_frac = c(0,0,0,0,0,0.25,0.25,0.25,0.25,0,0,0))
Counties_Default_distribution<- Monthly_timeseries_SF_unreported[is.na(Monthly_timeseries_SF_unreported$mo_frac),]
Counties_Default_distribution$mo_frac <- ifelse(between(Counties_Default_distribution$Month, 6,9),0.25,0)

Monthly_timeseries_SF_unreported<- Monthly_timeseries_SF_unreported[!is.na(Monthly_timeseries_SF_unreported$mo_frac),]
Monthly_timeseries_SF_unreported <- rbind.data.frame(Monthly_timeseries_SF_unreported, Counties_Default_distribution)

rm(Counties_Default_distribution)

Monthly_timeseries_SF_unreported <- Monthly_timeseries_SF_unreported %>% 
  mutate(Unreported_Monthly = Unreported_Irrigation_based * mo_frac)



Check <- Monthly_timeseries_SF_unreported %>% 
  group_by(Year,County_Code) %>% 
  summarise(Yearly_sum = round(first(Unreported_Irrigation_based,0)),
            Monthly_Sum = round(sum(Unreported_Monthly),0))

Check$Test <- ifelse(Check$Yearly_sum == Check$Monthly_Sum, "Pass", "Need Check")

Final_Timeseries <- Monthly_timeseries_SF_unreported[,c(1,2,3,4,6,8)]

colnames(Final_Timeseries) <- c("Year", "County Code", "County Name", "GEOID", "Month", "Small Farm Unreported (mg)")

Final_Timeseries$`Small Farm Unreported (mg)` <- ifelse(Final_Timeseries$`Small Farm Unreported (mg)` <0 , 0 ,Final_Timeseries$`Small Farm Unreported (mg)`)

write.csv(Final_Timeseries, paste0(WUDR_github, "/Monthly_timeseries/SmallFarm_unreported_Monthly_timeseries.csv"), row.names = FALSE)

####################################################
# Large Farm Monthly Time series

load(paste0(WUDR_github,"/dat_load/LF_All_times_series.RData")) # Large Farm time series
rm(TS_LF_Coeff_Unreported_median)

TS_LF_Unreported_Median_Area <- left_join(TS_LF_Unreported_Median_Area,VA_counties@data[,c(1,3)], by = c("County_Code" = "COUNTYFP"))

Monthly_timeseries_LF_unreported <-data.frame( Date = seq(as.Date("2002/1/1"), as.Date("2017/12/31"), "month" ))
Monthly_timeseries_LF_unreported$Year = lubridate::year(Monthly_timeseries_LF_unreported$Date)   
Monthly_timeseries_LF_unreported$Month = lubridate::month(Monthly_timeseries_LF_unreported$Date)   
Monthly_timeseries_LF_unreported <- left_join(TS_LF_Unreported_Median_Area[,c(1,2,3,13,11)],Monthly_timeseries_LF_unreported, by = "Year" )                                       
Monthly_timeseries_LF_unreported$Date = NULL                                      

Monthly_timeseries_LF_unreported$GEOID <- as.numeric(Monthly_timeseries_LF_unreported$GEOID)
Monthly_timeseries_LF_unreported <- left_join(Monthly_timeseries_LF_unreported, Monthly_Pct, by= c("GEOID"= "fips_hydrocode", "Month" = "mo_num"))                                       



Default_distribution <- data.frame(Month = 1:12, mo_frac = c(0,0,0,0,0,0.25,0.25,0.25,0.25,0,0,0))
Counties_Default_distribution<- Monthly_timeseries_LF_unreported[is.na(Monthly_timeseries_LF_unreported$mo_frac),]
Counties_Default_distribution$mo_frac <- ifelse(between(Counties_Default_distribution$Month, 6,9),0.25,0)

Monthly_timeseries_LF_unreported<- Monthly_timeseries_LF_unreported[!is.na(Monthly_timeseries_LF_unreported$mo_frac),]
Monthly_timeseries_LF_unreported <- rbind.data.frame(Monthly_timeseries_LF_unreported, Counties_Default_distribution)

rm(Counties_Default_distribution)

Monthly_timeseries_LF_unreported <- Monthly_timeseries_LF_unreported %>% 
  mutate(Unreported_Monthly = Unreported_Deficit_Irr_based * mo_frac)

Final_Timeseries_LF <- Monthly_timeseries_LF_unreported[,c(1,2,3,4,6,8)]
colnames(Final_Timeseries_LF) <- c("Year", "County Code", "County Name", "GEOID", "Month", "Large Farm Unreported (mg)")

Final_Timeseries_LF$`Large Farm Unreported (mg)` <- ifelse(Final_Timeseries_LF$`Large Farm Unreported (mg)` <0 , 0 ,Final_Timeseries_LF$`Large Farm Unreported (mg)`)
# Remove negative values fro unreported withdrawals


write.csv(Final_Timeseries_LF, paste0(WUDR_github, "/Monthly_timeseries/Large_Farm_unreported_Monthly_timeseries.csv"), row.names = FALSE)


Check <- Monthly_timeseries_LF_unreported %>% 
  group_by(Year,County_Code) %>% 
  summarise(Yearly_sum = round(first(Unreported_Deficit_Irr_based,0)),
            Monthly_Sum = round(sum(Unreported_Monthly),0))

Check$Test <- ifelse(Check$Yearly_sum == Check$Monthly_Sum, "Pass", "Need Check")

head(Final_Timeseries)

