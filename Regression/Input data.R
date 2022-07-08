WUDR_github<-"F:/My Drive/WUDR/WUDR_Github/WUDR"
setwd(WUDR_github)

library(tidyverse) 

options(scipen = 9999)



############################################################################
WUDR_github<-"F:/My Drive/WUDR/WUDR_Github/WUDR"
setwd(WUDR_github)
pacman::p_load(tidyverse,rgdal)
options(scipen=999)


load(paste0(WUDR_github,"/dat_load/All_DEQ_dat.Rdata"))

# Load VA Shapefile to be used later
VA_counties<-readOGR(paste0(WUDR_github, "/VA_counties_sp"), layer="VA_counties")



deq_dat <- All_DEQ_dat %>% 
  dplyr::filter(between(Year,2002,2017))


# DEQ available data for irrigation

deq_dat <- deq_dat %>% 
  dplyr::filter(Use.Type =="irrigation")



# DEQ total non power withdrawals
fac_power <- c("hydropower", "nuclearpower" , "fossilpower")

deq_dat <- deq_dat %>% 
  dplyr::filter(!Use.Type %in% fac_power) 

# Sum of withdrawals for all intakes a facilties  
summary_multiple_intakes<-deq_dat%>%
  dplyr::group_by(Year,Facility,Month,FIPS.Code)%>%
  dplyr::summarise(F_HydroID=first(Facility_hydroid),
                   Facility_withdrawal_mgm=sum(Water.Use.MGM))

# Sum of withdrawals for facility over a year
Monthly_DEQ_withdrawals<-summary_multiple_intakes%>%
  dplyr::group_by(Year,Month,FIPS.Code)%>%
  dplyr::summarise( Facility_withdrawal_mg=sum(Facility_withdrawal_mgm))


Monthly_DEQ_withdrawals$FIPS.Code <- as.character(Monthly_DEQ_withdrawals$FIPS.Code)

Monthly_DEQ_withdrawals <- left_join(Monthly_DEQ_withdrawals, VA_counties@data[,c(1,3,10)], by = c("FIPS.Code" = "GEOID"))

Monthly_DEQ_withdrawals <- Monthly_DEQ_withdrawals[,c(5,6,1:4)]
colnames(Monthly_DEQ_withdrawals)[5] <- "GEOID"
colnames(Monthly_DEQ_withdrawals)[3] <- "YEAR"

Monthly_DEQ_withdrawals<- Monthly_DEQ_withdrawals %>% 
  group_by(COUNTYFP,YEAR) %>% 
  mutate( Pct_Rep = round(Facility_withdrawal_mg/sum(Facility_withdrawal_mg, na.rm = TRUE),2))


save(Monthly_DEQ_withdrawals, file=paste0(WUDR_github,"/dat_load/DEQ_Monthly_Data.Rdata"))

load(paste0(WUDR_github,"/dat_load/SF_times_series_Median.RData"))


#########################################################################################
# SMALL FARM UNREPORTED MONTHLY DATA ####################################################
#########################################################################################


### For counties with Irrigation data
Irri_Counties_SF_Monthly_Unreported <- left_join(Monthly_DEQ_withdrawals[,-c(2,5)],SF_Unreported_Median_Irr_Coeff[,c(1,2,3,6)], by = c("YEAR", "COUNTYFP"))

Irri_Counties_SF_Monthly_Unreported <- Irri_Counties_SF_Monthly_Unreported %>% 
  filter(Unreported_Coeff_Based >0)
 
Irri_Counties_SF_Monthly_Unreported <- Irri_Counties_SF_Monthly_Unreported[,c(1,3,2,6,4,5,7)]

Irri_Counties_SF_Monthly_Unreported$Unreported_monthly <- Irri_Counties_SF_Monthly_Unreported$Unreported_Coeff_Based*Irri_Counties_SF_Monthly_Unreported$Pct_Rep


### For irrigation data Missing Counties

All_Counties_SF_Monthly_Unreported <- left_join(Monthly_DEQ_withdrawals[,-c(2,5)],SF_Unreported_Median_UnderTh[,c(1,2,3,7)], by = c("YEAR"="Year", "COUNTYFP"="County_Code"))
 All_Counties_SF_Monthly_Unreported <- All_Counties_SF_Monthly_Unreported %>% 
  filter(Unreported_Irrigation_based >0)
 

All_Counties_SF_Monthly_Unreported <- All_Counties_SF_Monthly_Unreported[,c(1,3,2,6,4,5,7)]

All_Counties_SF_Monthly_Unreported$Unreported_monthly <- All_Counties_SF_Monthly_Unreported$Unreported_Irrigation_based*All_Counties_SF_Monthly_Unreported$Pct_Rep

All_Counties_SF_Monthly_Unreported$Unreported_monthly<- ifelse(is.na(All_Counties_SF_Monthly_Unreported$Unreported_monthly), All_Counties_SF_Monthly_Unreported$Unreported_Irrigation_based/12,All_Counties_SF_Monthly_Unreported$Unreported_monthly)



#########################################################################################
# LARGE FARM UNREPORTED MONTHLY DATA ####################################################
#########################################################################################


load(paste0(WUDR_github,"/dat_load/LF_All_times_series.RData"))

### For counties with Irrigation data
Irri_Counties_LF_Monthly_Unreported <- left_join(Monthly_DEQ_withdrawals[,-c(2,5)],TS_LF_Coeff_Unreported_median[,c(1,2,3,6)], by = c("YEAR", "COUNTYFP"))

 Irri_Counties_LF_Monthly_Unreported <- Irri_Counties_LF_Monthly_Unreported %>% 
  filter(Unreported_Coeff_Based >0)
 
Irri_Counties_LF_Monthly_Unreported <- Irri_Counties_LF_Monthly_Unreported[,c(1,3,2,6,4,5,7)]

Irri_Counties_LF_Monthly_Unreported$Unreported_monthly <- Irri_Counties_LF_Monthly_Unreported$Unreported_Coeff_Based*Irri_Counties_LF_Monthly_Unreported$Pct_Rep


### For irrigation data Missing Counties

All_Counties_LF_Monthly_Unreported <- left_join(Monthly_DEQ_withdrawals[,-c(2,5)],TS_LF_Unreported_Median_Area[,c(1,2,3,11)], by = c("YEAR"="Year", "COUNTYFP"="County_Code"))

 All_Counties_LF_Monthly_Unreported <- All_Counties_LF_Monthly_Unreported %>% 
   filter(Unreported_Deficit_Irr_based >0)
 
All_Counties_LF_Monthly_Unreported <- All_Counties_LF_Monthly_Unreported[,c(1,3,2,6,4,5,7)]

All_Counties_LF_Monthly_Unreported$Unreported_monthly <- All_Counties_LF_Monthly_Unreported$Unreported_Deficit_Irr_based*All_Counties_LF_Monthly_Unreported$Pct_Rep

All_Counties_LF_Monthly_Unreported$Unreported_monthly<- ifelse(is.na(All_Counties_LF_Monthly_Unreported$Unreported_monthly), All_Counties_LF_Monthly_Unreported$Unreported_Deficit_Irr_based/12,All_Counties_LF_Monthly_Unreported$Unreported_monthly)

#########################################################################################
# Precip data                        ####################################################
#########################################################################################
load(paste0(WUDR_github,"/dat_load/PRECIP_2002-2019_daily.RData"))

PPT_Monthly <- PPT_VA[,-3] %>% 
  group_by(Month,Year) %>% 
  summarise_all(sum)

PPT_Monthly <- pivot_longer(PPT_Monthly,c(3:135), names_to = "County", values_to = "PPT")

PPT_Monthly$County<-gsub(".", " ", PPT_Monthly$County, fixed=TRUE)
PPT_Monthly$County<-gsub("1", "city", PPT_Monthly$County, fixed=TRUE)


PPT_Monthly <- left_join(PPT_Monthly, VA_counties@data[,c(3,6)], by = c("County"= "NAME"))
colnames(PPT_Monthly)[2] <- "YEAR"

Monthly_dat <- purrr::reduce(list(PPT_Monthly[,c(-3)],Irri_Counties_SF_Monthly_Unreported[,c(1,2,3,8)],All_Counties_SF_Monthly_Unreported[,c(1,2,3,8)],
                              Irri_Counties_LF_Monthly_Unreported[,c(1,2,3,8)],All_Counties_LF_Monthly_Unreported[,c(1,2,3,8)],Monthly_DEQ_withdrawals[,c(1,3,4,6)]),
                         dplyr::full_join, by = c("COUNTYFP","YEAR", "Month"))

colnames(Monthly_dat)[5:9] <- c("SF_Unreported_Coeff_method_Deq_irri_counties","SF_Unreported_Eff_precip_method_all_counties",
                                "LF_Unreported_Coeff_method_Deq_irri_counties","LF_Unreported_Eff_precip_method_all_counties",
                                "DEQ_Irrigation_withdrawals")

save(Monthly_dat, file=paste0(WUDR_github,"/dat_load/Explantory_var_Monthly_dat.Rdata"))

load(paste0(WUDR_github,"/DEQ_data/Prism_temp_data.Rdata"))



###################################################################
# Non rainfall days between June and August
# No of days with Mean temp above 30











