WUDR_github<-"F:/My Drive/WUDR/WUDR_Github/WUDR"
setwd(WUDR_github)
pacman::p_load(tidyverse,rgdal)
options(scipen=999)


load(paste0(WUDR_github,"/dat_load/All_DEQ_dat.Rdata"))

# Load VA Shapefile to be used later
VA_counties<-readOGR(paste0(WUDR_github, "/VA_counties_sp"), layer="VA_counties")



deq_dat <- dplyr::filter(All_DEQ_dat, Year >2001)
Census_years <- c(2002, 2007, 2012, 2017)

# DEQ avaliable data for irrigation

DEQ_irrigation <- deq_dat %>% 
  dplyr::filter(Use.Type =="irrigation")%>% 
  filter(Year %in% Census_years)



# DEQ total non power withdrawals
fac_power <- c("hydropower", "nuclearpower" , "fossilpower")

DEQ_total <- deq_dat %>% 
  dplyr::filter(!Use.Type %in% fac_power) %>% 
  filter(Year %in% Census_years)


DEQ_summary <- function(dat){
# Sum of withdrawals for all intakes a facilties  
summary_multiple_intakes<<-dat%>%
  dplyr::group_by(Year,Facility,Month,FIPS.Code)%>%
  dplyr::summarise(F_HydroID=first(Facility_hydroid),
                   Facility_withdrawal_mgm=sum(Water.Use.MGM))

# Sum of withdrawals for facility over a year
summary_facilities<-summary_multiple_intakes%>%
  dplyr::group_by(Year,Facility,FIPS.Code)%>%
  dplyr::summarise(F_HydroID=first(F_HydroID),
                   Facility_withdrawal_mg=sum(Facility_withdrawal_mgm))

# Sum of withdrawals in a county  
summary_counties <-summary_facilities%>%
  dplyr::group_by(Year,FIPS.Code)%>%
  dplyr::summarise(F_HydroID=first(F_HydroID),
                   Count=n(),
                   Facility_withdrawal_mg=sum(Facility_withdrawal_mg))

summary_counties$FIPS.Code <- as.character(summary_counties$FIPS.Code)

summary_counties <- left_join(summary_counties, VA_counties@data[,c(1,3,10)], by = c("FIPS.Code" = "GEOID"))

summary_counties <- summary_counties[,c(6,7,1:5)]
colnames(summary_counties)[4] <- "GEOID"
return(summary_counties)
}

Total_deq_county <- DEQ_summary(DEQ_total)

Irri_deq_county <- DEQ_summary(DEQ_irrigation)

save(Total_deq_county,Irri_deq_county, file="DEQ_data_total_nd_irr.Rdata")
