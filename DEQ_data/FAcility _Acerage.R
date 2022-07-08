WUDR_github<-"F:/My Drive/WUDR/WUDR_Github/WUDR"
setwd(WUDR_github)
pacman::p_load(tidyverse,rgdal)
options(scipen=999)


load(paste0(WUDR_github,"/dat_load/All_DEQ_dat.Rdata"))

# Load VA Shapefile to be used later
VA_counties<-readOGR(paste0(WUDR_github, "/VA_counties_sp"), layer="VA_counties")



deq_dat <- dplyr::filter(All_DEQ_dat, between(Year,2002,2017))


# DEQ avaliable data for irrigation

DEQ_irrigation <- deq_dat %>% 
  dplyr::filter(Use.Type =="irrigation")


# Sum of withdrawals for all intakes a facilties  
summary_multiple_intakes<-DEQ_irrigation%>%
  dplyr::group_by(Year,Facility,Month,FIPS.Code)%>%
  dplyr::summarise(F_HydroID=first(Facility_hydroid),
                   Facility_withdrawal_mgm=sum(Water.Use.MGM))

# Sum of withdrawals for facility over a year
summary_facilities<-summary_multiple_intakes%>%
  dplyr::group_by(Year,Facility,FIPS.Code)%>%
  dplyr::summarise(F_HydroID=first(F_HydroID),
                   Facility_withdrawal_mg=sum(Facility_withdrawal_mgm))

summary_facilities$FIPS.Code <- as.character(summary_facilities$FIPS.Code)

summary_facilities <- left_join(summary_facilities, VA_counties@data[,c(1,3,10)], by = c("FIPS.Code" = "GEOID"))

summary_facilities <- summary_facilities[,c(6,7,1:5)]
colnames(summary_facilities)[5] <- "GEOID"
colnames(summary_facilities)[3] <- "YEAR"


load(paste0(WUDR_github,"/dat_load/PRECIP_JUNE_AUGUST_ALL_YEARS.RData")) # PRISM PPT data

load(paste0(WUDR_github,"/dat_load/PRISM_county_codes.RData")) # County Codes for PRISM

PPT <- lapply(ppt_list_yearly, function(x)
  mutate(x, Irrigation = round(508 - PPT,0)))
PPT <- bind_rows(PPT, .id = "Year")

PPT <- left_join(PPT,PRISM_county_codes, by = "name" )
PPT <- PPT[,c(1,5,2,3,4)]
colnames(PPT)[1:2] <- c("YEAR", "COUNTYFP")
PPT$YEAR <- as.numeric(PPT$YEAR)


summary_facilities <- left_join(summary_facilities, PPT[c(-3)], by =c("YEAR", "COUNTYFP"))

Irrigation_Max <- summary_facilities %>% 
  group_by(F_HydroID) %>% 
  summarise(Max_demand_in = max(Irrigation)/25.4)

summary_facilities <- left_join(summary_facilities, Irrigation_Max, by =c("F_HydroID"))

summary_facilities$Irrigated_Acres<- summary_facilities$Facility_withdrawal_mg/(summary_facilities$Max_demand_in *27154/1000000)
















