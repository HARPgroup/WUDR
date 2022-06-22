WUDR_github<-"F:/My Drive/WUDR/WUDR_Github/WUDR"
setwd(WUDR_github)
pacman::p_load(tidyverse,rgdal)
options(scipen=999)

#Load funtions and Census data
source(paste0(WUDR_github,"/Small_farmers_coeff/Functions/fn_Coefficent1.R"))
load(paste0(WUDR_github,"/dat_load/All_DEQ_dat.Rdata"))
nass_binned <- filter(nass_binned, Year > 2000)
# Load VA Shapefile to be used later
VA_counties<-readOGR(paste0(WUDR_github, "/VA_counties_sp"), layer="VA_counties")
county.codes <- read_csv(paste0(WUDR_github, "/csv_files/county_codes_census.csv")) 
county.codes_deq <- VA_counties@data[,c(1,3)]

load(paste0(WUDR_github,"/dat_load/PRECIP_JUNE_AUGUST_ALL_YEARS.RData"))
PPT <- lapply(ppt_list_yearly, function(x)
  mutate(x, Irrigation = round(508 - PPT,0)))

PPT <- bind_rows(PPT, .id = "Year")
PPT <- left_join(PPT,county.codes, by = c("name"= "County_Name"))

deq_dat <- dplyr::filter(All_DEQ_dat, Year >2001)


# DEQ avaliable data for irrigation

DEQ_irrigation <- deq_dat %>% 
  dplyr::filter(Use.Type =="irrigation") %>% 
  filter(Year <2018)

rm(ppt_list_yearly,deq_dat)

dat <- DEQ_irrigation
  # Sum of withdrawals for all intakes a facilties  
  summary_multiple_intakes<-dat%>%
    dplyr::group_by(Year,Facility,Month,FIPS.Code)%>%
    dplyr::summarise(F_HydroID=first(Facility_hydroid),
                     Facility_withdrawal_mgm=sum(Water.Use.MGM))
  
  # Sum of withdrawals for facility over a year
  summary_facilities<-summary_multiple_intakes%>%
    dplyr::group_by(Year,Facility,FIPS.Code)%>%
    dplyr::summarise(F_HydroID=first(F_HydroID),
                     Facility_withdrawal_mg=sum(Facility_withdrawal_mgm))
  
  summary_facilities$FIPS.Code <- as.character(summary_facilities$FIPS.Code)
  summary_facilities$Year <- as.character(summary_facilities$Year)
  
  summary_facilities <- left_join(summary_facilities, VA_counties@data[,c(1,3,10)], by = c("FIPS.Code" = "GEOID"))
  
  summary_facilities <- summary_facilities[,c(6,7,1:5)]

  summary_facilities <- left_join(summary_facilities,PPT, by= c("Year"="Year", "COUNTYFP" = "County_Code"))
  
  summary_facilities <- summary_facilities %>% 
    filter(Irrigation >0)
  
  summary_facilities$Irrigation_IN <- summary_facilities$Irrigation/25.4
  summary_facilities$Irrigation <- NULL
  summary_facilities$PPT <- NULL
  summary_facilities$Facility_withdrawal_ACRE_IN <- round(summary_facilities$Facility_withdrawal_mg*1000000/27154,2)
  summary_facilities$Acres_Irrigtated <- round(summary_facilities$Facility_withdrawal_ACRE_IN/summary_facilities$Irrigation_IN,1)
  
  
  Farms_Greater_than_2000<- summary_facilities %>% 
    filter(Acres_Irrigtated >= 2000) %>% 
    group_by(Year,Facility,name) %>% 
      summarise(Acres_Irrigtated = Acres_Irrigtated,
                Irrigation_IN = Irrigation_IN)
  
  Farms_between_1000_1999 <- summary_facilities %>% 
    filter(between(Acres_Irrigtated,1000,1999)) %>% 
    group_by(Year,Facility,name) %>% 
    summarise(Acres_Irrigtated = Acres_Irrigtated,
              Irrigation_IN = Irrigation_IN)

  
  Farms_between_500_999 <- summary_facilities %>% 
    filter(between(Acres_Irrigtated,500,999)) %>% 
    group_by(Year,Facility,name) %>% 
    summarise(Acres_Irrigtated = Acres_Irrigtated,
              Irrigation_IN = Irrigation_IN)
  
  Farms_between_1000_1999 <- summary_facilities %>% 
    filter(between(Acres_Irrigtated,1000,1999)) %>% 
    group_by(Year,Facility,name) %>% 
    summarise(Acres_Irrigtated = Acres_Irrigtated,
              Irrigation_IN = Irrigation_IN)
  
  
  Farms_between_500_999 <- summary_facilities %>% 
    filter(between(Acres_Irrigtated,500,999)) %>% 
    group_by(Year,Facility,name) %>% 
    summarise(Acres_Irrigtated = Acres_Irrigtated,
              Irrigation_IN = Irrigation_IN)
  
  Farms_between_260_499 <- summary_facilities %>% 
    filter(between(Acres_Irrigtated,260,499)) %>% 
    group_by(Year,Facility,name) %>% 
    summarise(Acres_Irrigtated = Acres_Irrigtated,
              Irrigation_IN = Irrigation_IN)
  
  
  Farms_between_220_259 <- summary_facilities %>% 
    filter(between(Acres_Irrigtated,220,259)) %>% 
    group_by(Year,Facility,name) %>% 
    summarise(Acres_Irrigtated = Acres_Irrigtated,
              Irrigation_IN = Irrigation_IN)
  
  Farms_between_180_219 <- summary_facilities %>% 
    filter(between(Acres_Irrigtated,180,219)) %>% 
    group_by(Year,Facility,name) %>% 
    summarise(Acres_Irrigtated = Acres_Irrigtated,
              Irrigation_IN = Irrigation_IN)
  
  Farms_between_140_170 <- summary_facilities %>% 
    filter(between(Acres_Irrigtated,140,170)) %>% 
    group_by(Year,Facility,name) %>% 
    summarise(Acres_Irrigtated = Acres_Irrigtated,
              Irrigation_IN = Irrigation_IN)
  
  
  Farms_between_100_139 <- summary_facilities %>% 
    filter(between(Acres_Irrigtated,100,139)) %>% 
    group_by(Year,Facility,name) %>% 
    summarise(Acres_Irrigtated = Acres_Irrigtated,
              Irrigation_IN = Irrigation_IN)
  
  Farms_between_70_99 <- summary_facilities %>% 
    filter(between(Acres_Irrigtated,70,99)) %>% 
    group_by(Year,Facility,name) %>% 
    summarise(Acres_Irrigtated = Acres_Irrigtated,
              Irrigation_IN = Irrigation_IN)
  
  Farms_between_50_69 <- summary_facilities %>% 
    filter(between(Acres_Irrigtated,50,69)) %>% 
    group_by(Year,Facility,name) %>% 
    summarise(Acres_Irrigtated = Acres_Irrigtated,
              Irrigation_IN = Irrigation_IN)
  
  
  Farms_between_10_49 <- summary_facilities %>% 
    filter(between(Acres_Irrigtated,10,49)) %>% 
    group_by(Year,Facility,name) %>% 
    summarise(Acres_Irrigtated = Acres_Irrigtated,
              Irrigation_IN = Irrigation_IN)
  
  
  Farms_between_1_9 <- summary_facilities %>% 
    filter(between(Acres_Irrigtated,1,9)) %>% 
    group_by(Year,Facility,name) %>% 
    summarise(Acres_Irrigtated = Acres_Irrigtated,
              Irrigation_IN = Irrigation_IN)
  
  
  Binned_summary <- Farms_Greater_than_2000 %>% 
    group_by(Year) %>% 
    summarise(No.of.facilities = n())
    
  Binned_dat <- c("Farms_Greater_than_2000","Farms_between_1000_1999","Farms_between_500_999","Farms_between_260_499","Farms_between_220_259","Farms_between_180_219","Farms_between_140_170",
                  "Farms_between_100_139","Farms_between_70_99","Farms_between_50_69","Farms_between_10_49","Farms_between_1_9")
  
  binned_list <- list(Farms_Greater_than_2000,Farms_between_1000_1999,Farms_between_500_999,Farms_between_260_499,Farms_between_220_259,Farms_between_180_219,Farms_between_140_170,
                      Farms_between_100_139,Farms_between_70_99,Farms_between_50_69,Farms_between_10_49,Farms_between_1_9)
  
  names(binned_list) <- Binned_dat
  
  Binned_summary  <- list()
  Binned_summary <- lapply(binned_list, function(x) x %>% 
                             group_by(Year) %>% 
                             summarise(No.of.facilities = n()))
    
  names(Binned_summary) <- Binned_dat

  
  # # Sum of withdrawals in a county  
  # summary_counties <-summary_facilities%>%
  #   dplyr::group_by(Year,FIPS.Code)%>%
  #   dplyr::summarise(F_HydroID=first(F_HydroID),
  #                    Count=n(),
  #                    Facility_withdrawal_mg=sum(Facility_withdrawal_mg))
  # 
  # summary_counties$FIPS.Code <- as.character(summary_counties$FIPS.Code)
  # 
  # summary_counties <- left_join(summary_counties, VA_counties@data[,c(1,3,10)], by = c("FIPS.Code" = "GEOID"))
  # 
  # summary_counties <- summary_counties[,c(6,7,1:5)]
  # 