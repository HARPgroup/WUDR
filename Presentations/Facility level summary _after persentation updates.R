# Facility level summary for 2012-2018

WUDR_github<-"F:/My Drive/WUDR/WUDR_Github/WUDR_local"
setwd(WUDR_github)

pacman::p_load(dplyr)

all_dat <-read.csv(paste0(WUDR_github,"/csv_files/all_2012_2018_withdrawal_monthly.csv"))
dat_01_11 <- read.csv(paste0(WUDR_github,"/csv_files/2001-2011withdrawal_monthly.csv"))
datT <- rbind.data.frame(dat_01_11, all_dat)

# YEAR <- 2017

facility_summary <- function(YEAR){
dat_year <- filter(datT , Year == YEAR)
 

#Get the withdrawal dat for use type of irrigation only
dat_year<-dplyr::filter(dat_year, Use.Type =="irrigation")

# winter_dat<-dplyr::filter(dat_year, Month <=4)
# sum(winter_dat$Water.Use.MGM)
# unique(winter_dat$Facility)

summary_multiple_intakes<-dat_year%>%
  dplyr::group_by(Facility,Month,FIPS.Code)%>%
  dplyr::summarise(F_HydroID=first(Facility_hydroid),
                   Facility_withdrawal_mgm=sum(Water.Use.MGM))

summary_facilities<<-summary_multiple_intakes%>%
  dplyr::group_by(Facility,FIPS.Code)%>%
  dplyr::summarise(F_HydroID=first(F_HydroID),
                   Facility_withdrawal_mg=sum(Facility_withdrawal_mgm))
}

fsummary_2018<-facility_summary(2018)
fsummary_2017<- facility_summary(2017)
fsummary_2016 <- facility_summary(2016)
fsummary_2015 <- facility_summary(2015)
fsummary_2014 <- facility_summary(2014)
fsummary_2013 <- facility_summary(2013)
fsummary_2012 <- facility_summary(2012)
fsummary_2011<- facility_summary(2011)
fsummary_2010<- facility_summary(2010)
fsummary_2009 <- facility_summary(2009)
fsummary_2008 <- facility_summary(2008)
fsummary_2007 <- facility_summary(2007)
fsummary_2006 <- facility_summary(2006)
fsummary_2005 <- facility_summary(2005)
fsummary_2004 <- facility_summary(2004)
fsummary_2003 <- facility_summary(2003)
fsummary_2002 <- facility_summary(2002)
fsummary_2001 <- facility_summary(2001)

########################################################################

# Difference in facilties

# Change the file accordingly

# Facilties missing in 2017 
dat_diff_16_17 <- anti_join(fsummary_2016, fsummary_2017, by = c("F_HydroID" = "F_HydroID"))

#New facilties added in 2017
dat_diff_17_16 <- anti_join(fsummary_2017, fsummary_2016, by = c("F_HydroID" = "F_HydroID"))

write.csv(dat_diff_16_17, "facilties missing in 2017 from 2016.csv" )
