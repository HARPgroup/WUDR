# This code is used to generate the unreported withdrawals and corresponding coefficients for small farmers.
#  1. Firstly the deficit irrigation is used to calculate the irrigation amount for area under the threshold.
#  2. Only variation is in calculation of coefficient.


WUDR_github<-"F:/My Drive/WUDR/WUDR_Github/WUDR"
setwd(WUDR_github)

library(tidyverse) 
library(tmap)
library(rgdal)
library(stringr)
library(Kendall)
library(plotly)
library("gridExtra")
options(scipen = 9999)


VA_counties<-readOGR(paste0(WUDR_github, "/VA_counties_sp"), layer="VA_counties")
county.codes <- read.csv(paste0(WUDR_github, "/csv_files/county_codes_census.csv")) 
county.codes_deq <- VA_counties@data[,c(1,3)]


#Load funtions and Census data
source(paste0(WUDR_github,"/Small_farmers_coeff/Functions/fn_Coefficent1.R"))
rm(absent, Year)

# QS_data function uses binned data and county summary to fill the D values. The intermediate steps are detailed in the function.

# fn_Area_TH calculates the area under threshold.


#1 Counties with DEQ Reported Irrigation withdrawals
##############################################################################
# NA in Facility_withdrawal_mg column indicates no Irrigation withdrawals reported.
# Here we calculate (C_irr) i.e.  FUNCTION of Irrigation withdrawals

  Year = 2012
small_counties_coefficient <- function(Year){

    QS_data(Year) 
  fn_Area_TH(10, Year)
  
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
  
  
  #1 Counties with DEQ Reported Irrigation withdrawals
  ##############################################################################
  # NA in Facility_withdrawal_mg column indicates no Irrigation withdrawals reported.
  # Here we calculate (C_irr) i.e.  FUNCTION of Irrigation withdrawals
  
  Fn_of_Irri_withdrawals <- left_join(Non.Reported_Coefficient1,IRR_DEQ_withdarwals[, c("COUNTYFP","Facility_withdrawal_mg")], by = c("County_Code"= "COUNTYFP"))
  
  
  Fn_of_Irri_withdrawals <- Fn_of_Irri_withdrawals %>%
    drop_na(Facility_withdrawal_mg)
  
  
  # Fn_of_Irri_withdrawals$C_irr2 <- round((((Fn_of_Irri_withdrawals$Facility_withdrawal_mg*100)/
  #                                                    (100-Fn_of_Irri_withdrawals$Pct.under.TH.of.total.Irr.area))-Fn_of_Irri_withdrawals$Facility_withdrawal_mg)/ Fn_of_Irri_withdrawals$Facility_withdrawal_mg,2)
  load(paste0(WUDR_github,"/dat_load/June_August_Effective precipitation.RData"))
  
  if (Year == 2002) {
    i = 1}else if (Year == 2007){
      i = 2} else if (Year == 2012){
        i = 3} else if (Year == 2017){
          i =4
        }
  
  
  Irr_deficit <- ppt_list_yearly[[i]][,c(2,3)]
  Irr_deficit$Irrigation <- round(508 - Irr_deficit$PPT,0) # considering 20 inches as crop water demand
  
  Fn_of_Irri_withdrawals <- left_join(Fn_of_Irri_withdrawals, Irr_deficit, by = c("County"= "name"))
  
  Fn_of_Irri_withdrawals$Vol_Unreported <- round(Fn_of_Irri_withdrawals$Irr.Area.Under.TH*(Fn_of_Irri_withdrawals$Irrigation/25.5)*27154/1000000,2)
  
  
  Fn_of_Irri_withdrawals$C_irr = 100*Fn_of_Irri_withdrawals$Vol_Unreported / Fn_of_Irri_withdrawals$Facility_withdrawal_mg 
  
 Fn_of_Irri_withdrawals$Method1_Unreported <- Fn_of_Irri_withdrawals$C_irr * Fn_of_Irri_withdrawals$Facility_withdrawal_mg

  
  plotdat<-sp::merge(VA_counties,Fn_of_Irri_withdrawals, by.x = "COUNTYFP", by.y = "County_Code")
  
  # Check if merge was correct
  # sum(Fn_of_Irri_withdrawals$Irr.Area.above.TH, na.rm = TRUE)
  # sum(plotdat@data$Irr.Area.above.TH, na.rm=TRUE)
  
  # Plot for Unreported Coefficient
  p1<-tm_shape(plotdat)+
    tm_polygons("C_irr", title = "Unreported Coefficient",
                breaks = c(0,5,10,20,50,75,Inf),
                # n=5,style="jenks",
                textNA = "Missing DEQ Irrigation Withdrawals / No Census data",
                id="NAMELSAD")+
    # tm_text("NAME", size = 0.3)+
    tm_layout(main.title = paste0(Year," Small farm unreported\n(as a percentage of VDEQ Irrigation withdrawal)"),
              legend.outside = FALSE,
              legend.title.size = 1.2,
              legend.text.size = 0.8,
              legend.position = c("left","top"),
              legend.bg.alpha = 1)
  p1
  
  
  Fn_of_Irri_withdrawals <- Fn_of_Irri_withdrawals[,c(1,8,2,4,9,10:14)]
   tmap_save(p1, paste0(WUDR_github,"/plots/Coefficient1/",Year, "_SF_Coeff_DEQ_Avalaible_counties.png"),  width = 8.5, height = 5, units = 'in')
  
  ##### Plot for unreported Volume
  
  p2<-tm_shape(plotdat)+
    tm_polygons("Method1_Unreported", title = "Unreported volume  (MG)",
                breaks = c(0,5,10,20,40,80,100),
                # n=5,style="jenks",
                textNA = "Missing DEQ Irrigation Withdrawals / No Census data",
                id="NAMELSAD")+
    # tm_text("NAME", size = 0.3)+
    tm_layout(main.title = paste0(Year," Small farm unreported withdrawals in counties with DEQ data"),
              legend.outside = FALSE,
              legend.title.size = 1.2,
              legend.text.size = 0.8,
              legend.position = c("left","top"),
              legend.bg.alpha = 1)
  p2
  
  
  # tmap_save(p2, paste0(WUDR_github,"/plots/Coefficient1/",Year, "_SF_Vol_DEQ_Avalaible_counties.png"),  width = 8.5, height = 5, units = 'in')
  
  # 
  return(Fn_of_Irri_withdrawals)
}

DEQ_2002 <- small_counties_coefficient(2002)
DEQ_2007 <- small_counties_coefficient(2007)
DEQ_2012 <- small_counties_coefficient(2012)
DEQ_2017 <- small_counties_coefficient(2017)

#2 Counties with NO DEQ Irrigation withdrawals
###############################################################################
# C_tot Coefficient as function of total withdrawals
# Crop water demand (20 inches)

small_counties_coefficient3 <- function(Year){
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
  Unreported_Under_TH <- left_join(Non.Reported_Coefficient1,Total_DEQ_withdarwals[, c("COUNTYFP","Facility_withdrawal_mg")], by = c("County_Code"= "COUNTYFP"))
  
  Unreported_Under_TH <- Unreported_Under_TH %>%
    drop_na(Facility_withdrawal_mg)
  
  
  load(paste0(WUDR_github,"/dat_load/June_August_Effective precipitation.RData"))
  
  if (Year == 2002) {
    i = 1}else if (Year == 2007){
      i = 2} else if (Year == 2012){
        i = 3} else if (Year == 2017){
          i =4
        }
  
  
  Irr_deficit <- ppt_list_yearly[[i]][,c(2,3)]
  Irr_deficit$Irrigation <- round(508 - Irr_deficit$PPT,0) # considering 20 inches as crop water demand
  
  Unreported_Under_TH <- left_join(Unreported_Under_TH, Irr_deficit, by = c("County"= "name"))
  
  Unreported_Under_TH$Vol_Unreported <- round(Unreported_Under_TH$Irr.Area.Under.TH*(Unreported_Under_TH$Irrigation/25.5)*27154/1000000,2)
  
  Unreported_Under_TH$C_tot = Unreported_Under_TH$Vol_Unreported / Unreported_Under_TH$Facility_withdrawal_mg 
  
  Unreported_Under_TH$Method1_Unreported <- Unreported_Under_TH$C_tot * Unreported_Under_TH$Facility_withdrawal_mg
  
  # Unreported_Under_TH <- Unreported_Under_TH %>% 
  #   filter(C_tot >= 0)
  
  # Get the DEQ irrigation withdrawals missing counties
  
  MISSING_withdrawals <- left_join(Non.Reported_Coefficient1,IRR_DEQ_withdarwals[, c("COUNTYFP","Facility_withdrawal_mg")], by = c("County_Code"= "COUNTYFP"))
  
  MISSING_withdrawals <- MISSING_withdrawals[is.na(MISSING_withdrawals$Facility_withdrawal_mg),]
  
  MISSING_withdrawals <- MISSING_withdrawals[,c(1,8)]
  
  # Unreported_Under_TH <- left_join(MISSING_withdrawals, Unreported_Under_TH, by = c("County_Code"))
  
  
  Unreported_Under_TH_Irrigation_Missing <- Unreported_Under_TH %>% 
    filter(County_Code %in% MISSING_withdrawals$County_Code)
  
  plotdat<-sp::merge(VA_counties,Unreported_Under_TH, by.x = "COUNTYFP", by.y = "County_Code")
  
  # Check if merge was correct
  sum(Unreported_Under_TH$Irr.Area.above.TH, na.rm = TRUE)
  sum(plotdat@data$Irr.Area.above.TH, na.rm=TRUE)
  
  p3<-tm_shape(plotdat)+
    tm_polygons("C_tot", title = "Unreported Coefficient",
                breaks = c(0,0.05,0.1,0.2,0.5,0.75,Inf),
                # n=5,style="jenks",
                textNA = "Missing Census data / No Irrigation deficit",
                id="NAMELSAD")+
    tm_layout(main.title = paste0(Year," Small farm unreported\n(as a percentage of VDEQ total nonenergy withdrawal)"),
              legend.outside = FALSE,
              legend.title.size = 1.2,
              legend.text.size = 0.8,
              legend.position = c("left","top"),
              legend.bg.alpha = 1)
  p3
  
  # tmap_save(p3, paste0(WUDR_github,"/plots/Coefficient1/",Year, "_SF_Coeff_DEQ_Missing_counties.png"),  width = 8, height = 5, units = 'in')
  
  ##### Plot for unreported Volume
  
  p4<-tm_shape(plotdat)+
    tm_polygons("Method1_Unreported", title = "Unreported volume  (MG)",
                breaks = c(0,5,10,20,40,80,100),
                # n=5,style="jenks",
                textNA = "Missing DEQ Irrigation Withdrawals / No Census data",
                id="NAMELSAD")+
    # tm_text("NAME", size = 0.3)+
    tm_layout(main.title = paste0(Year," Small farm unreported withdrawals in all counties"),
              legend.outside = FALSE,
              legend.title.size = 1.2,
              legend.text.size = 0.8,
              legend.position = c("left","top"),
              legend.bg.alpha = 1)
  p4
  
  
   tmap_save(p4, paste0(WUDR_github,"/plots/Coefficient1/",Year, "_SF_Vol_DEQ_Missing_counties.png"),  width = 8.5, height = 5, units = 'in')
  
  
  Unreported_Under_TH <- Unreported_Under_TH[,c(1,8,2,4,9,10:14)]
  # write.csv(Unreported_Under_TH, paste0(WUDR_github,"/Output_Tables/",Year, "_SF_Coeff_DEQ_Missing_counties.csv"))
  return(Unreported_Under_TH)
}

Tdeq_coef_2002 <- small_counties_coefficient3(2002)
Tdeq_coef_2007 <- small_counties_coefficient3(2007)
Tdeq_coef_2012 <- small_counties_coefficient3(2012)
Tdeq_coef_2017 <- small_counties_coefficient3(2017)

save(DEQ_2002,DEQ_2007,DEQ_2012,DEQ_2017,Tdeq_coef_2002,Tdeq_coef_2007,Tdeq_coef_2012,Tdeq_coef_2017,file= paste0(WUDR_github,"/dat_load/SF_Coeff.RData"))


###3Summary of Coefficients
Fn_merge_coeff <- function(x,y){
  CF <- full_join(x[,c(2,9)],y[,c(1,2,9)], by = "County_Code")
  return(CF)
}

CF_2002 <- Fn_merge_coeff(DEQ_2002,Tdeq_coef_2002)
CF_2007 <- Fn_merge_coeff(DEQ_2007,Tdeq_coef_2007)
CF_2012 <- Fn_merge_coeff(DEQ_2012,Tdeq_coef_2012)
CF_2017 <- Fn_merge_coeff(DEQ_2017,Tdeq_coef_2017)

Coeff_Summary <- list()
Coeff_list <- list(CF_2002,CF_2007,CF_2012,CF_2017)


for (i in 1:4) {
  Coeff_Summary[[i]] <- Coeff_list[[i]] %>% 
    summarise(Min = min(C_irr, na.rm = TRUE),
              Median = median(C_irr, na.rm = TRUE),
              Mean = mean(C_irr, na.rm = TRUE),
              Max = max(C_irr, na.rm = TRUE))
}


names(Coeff_Summary) <- c(seq(2002,2017,5))
Coeff_Summary <- bind_rows(Coeff_Summary, .id = "Year")



# Check the name carefully
 write.csv(Coeff_Summary, paste0(WUDR_github,"/Output_Tables/SF_Summary of (DEQ MISSING) Total Coefficients.csv"), row.names = FALSE) # use C_tot in loop
 write.csv(Coeff_Summary, paste0(WUDR_github,"/Output_Tables/SF_Summary of (DEQ Avaliable) Irrigation Coefficients.csv"), row.names = FALSE) # use C_irr in loop























