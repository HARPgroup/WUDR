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
county.codes <- read_csv(paste0(WUDR_github, "/csv_files/county_codes_census.csv")) 
county.codes_deq <- VA_counties@data[,c(1,3)]


#Load funtions and Census data
source(paste0(WUDR_github,"/Small_farmers_coeff/Functions/fn_Coefficent1.R"))
rm(absent, Year)
  
DEQ_Ag_census_data <- function(Year){
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
  
  return(Fn_of_Irri_withdrawals)
  
  Fn_of_Irri_withdrawals <- Fn_of_Irri_withdrawals[,c(2,8)]
  Fn_of_Irri_withdrawals$Year <- Year
}

dat_2017 <-   DEQ_Ag_census_data(2017)
dat_2017$Year = 2017
dat_2012 <-   DEQ_Ag_census_data(2012)

dat_2012$Year = 2012
dat_2007 <-   DEQ_Ag_census_data(2007)
dat_2007$Year = 2007

dat_2002 <-   DEQ_Ag_census_data(2002)
dat_2002$Year = 2002

dat_census <- rbind(dat_2017[,c(2,8,10)],dat_2012[,c(2,8,10)],dat_2007[,c(2,8,10)],dat_2002[,c(2,8,10)])


load(paste0(WUDR_github, "/dat_load/All_Years_DEQ_data_Total_nd_irr.Rdata")) 

  dat_UTH<- purrr::reduce(list(Large_DEQ_Tot_area[[1]][,c(1,2,3)],Large_DEQ_Tot_area[[2]][,c(2,3)],
                               Large_DEQ_Tot_area[[3]][,c(2,3)],Large_DEQ_Tot_area[[4]][,c(2,3)]), dplyr::full_join, by = 'County_Code')
  
  
  dat_UTH <- left_join(dat_UTH , county.codes , by = "County_Code")
  dat_UTH <- dat_UTH[-1]
  dat_UTH <- dat_UTH[,c(1,6,2:5)]
  dat_UTH <- dat_UTH[order(dat_UTH$County_Name),]
  
  # Specify the coefficient to be selected
  Un_TH = median
  
  Coef_val<- apply(dat_UTH[,3:6], 1, Un_TH, na.rm=TRUE)
  
  dat_UTH$parm_Tot_Area <- Coef_val
  
  colnames(dat_UTH)[3:6] <- seq(2002,2017,5)


var_res_2 <- left_join(Var_res, dat_census, by = c("YEAR" = "Year", "COUNTYFP" = "County_Code" ))
var_res_2 <- left_join(var_res_2, dat_UTH[,c(1,7)], by = c("COUNTYFP" = "County_Code" ))
var_res_2$Normalised_AC <- var_res_2$Total.Irri.Area/var_res_2$parm_Tot_Area


M3 <- lm(Normalised_AC ~  YEAR + Irrigation, data = var_res_2)
summary(M3)

var_res_2$LF_Unreported_Eff_precip_method_all_counties



var_res_2$Normalised_Unreported <- var_res_2$LF_Unreported_Eff_precip_method_all_counties/var_res_2$DEQ_Irrigation_withdrawals
var_res_2 <- subset(var_res_2, Normalised_Unreported != "Inf")
var_res_2 <- subset(var_res_2, Normalised_Unreported != "-Inf")
M3 <- lm(Normalised_Unreported ~  YEAR, data = var_res_2)
summary(M3)

save(var_res_2, file = paste0(WUDR_github, "/dat_load/Regression.Rdata"))

M3 <- lm(Normalised_Unreported ~  Irrigation+YEAR, data = var_res_2)
summary(M3)
