
WUDR_github<-"F:/My Drive/WUDR/WUDR_Github/WUDR"
setwd(WUDR_github)

library(tidyverse) 
library(rgdal)
library(DescTools)
options(scipen = 9999)

# Load VA Shapefile to be used later
VA_counties<-readOGR(paste0(WUDR_github, "/VA_counties_sp"), layer="VA_counties")

county.codes <- read_csv(paste0(WUDR_github, "/csv_files/county_codes_census.csv"))

load(paste0(WUDR_github,"/dat_load/PRECIP_JUNE_AUGUST_ALL_YEARS.RData")) # PRISM PPT data

load(paste0(WUDR_github,"/dat_load/PRISM_county_codes.RData")) # County Codes for PRISM


load(paste0(WUDR_github, "/dat_load/All_Years_DEQ_data_Total_nd_irr.Rdata")) 


#########################################################################
# DEQ reported Witdharwals

DEQ_Withdrawals <- Irri_deq_county %>% group_by(YEAR) %>% 
  summarise(DEQ_Withdarwals = sum(Facility_withdrawal_mg))

DEQ_withdrawals_County <- Irri_deq_county %>% 
  group_by(YEAR,COUNTYFP) %>% 
  summarise(DEQ_Withdarwals = sum(Facility_withdrawal_mg))


DEQ_withdrawals_Dry_Year <- DEQ_withdrawals_County %>% 
  filter(DEQ_Withdarwals >0) %>% 
  group_by(COUNTYFP) %>% 
  slice(which.max(DEQ_Withdarwals))

p1 <- as.data.frame(DEQ_withdrawals_Dry_Year %>%
  group_by(YEAR) %>%
  summarise(counts = n()) )

p1 <- ggplot(p1, aes(x=YEAR,y=counts))+
  geom_col(color='black',fill='lightblue')+
 labs(title= "Dry year for Counties based on DEQ reported between 2002-2017",
     x="YEAR", y = "Count")+
   scale_x_continuous(limits = c(2001.5, 2017.5),  breaks = seq(2002, 2017, by = 1))+
  scale_y_continuous( breaks = seq(0, 12, by = 3))
p1<- p1 + theme_bw()
  p1 <- p1+theme(axis.text.x=element_text(angle = 0, hjust = 0),
                 legend.position="None", 
                 legend.title=element_blank(),
                 legend.box = "horizontal", 
                 legend.background = element_rect(fill="white",
                                                  size=0.5, linetype="solid", 
                                                  colour ="white"),
                 legend.text=element_text(size=12),
                 axis.text=element_text(size=12, colour="black"),
                 axis.title=element_text(size=14, colour="black"),
                 axis.line = element_line(colour = "black", 
                                          size = 0.5, linetype = "solid"),
                 axis.ticks = element_line(colour="black"),
                 panel.grid.major=element_line(colour = "light grey"), 
                 panel.grid.minor=element_blank())
  p1

  ggsave(paste0(WUDR_github,"/plots/Dry_Wet_year", "DEQ_reported_dry.png"), plot = p1, width = 9.5, height = 6, units = "in")

  
#########################################################
  # Based on Effective precip approch
PPT <- lapply(ppt_list_yearly, function(x)
  mutate(x, Irrigation = round(508 - PPT,0)))
PPT <- bind_rows(PPT, .id = "Year")

PPT <- left_join(PPT,PRISM_county_codes, by = "name" )
PPT <- PPT[,c(1,5,2,3,4)]
colnames(PPT)[c(1:2,4)] <- c("YEAR", "COUNTYFP","Precip")
PPT$YEAR <- as.numeric(PPT$YEAR)

Dry_Year <- PPT %>% group_by(COUNTYFP) %>% slice(which.max(Irrigation))
Wet_Year <- PPT %>% group_by(COUNTYFP) %>% slice(which.min(Irrigation))
Avg_Year <- PPT %>% 
  group_by(COUNTYFP) %>%
  summarise(name = first(name),
    Precip= mean(Precip),
    Irrigation = mean(Irrigation))

Ag_census <- function(Year){
source(paste0(WUDR_github,"/Small_farmers_coeff/Functions/fn_Coefficent1.R"))
load(paste0(WUDR_github, "/dat_load/DEQ_data_total_nd_irr.Rdata"))
IRR_DEQ_withdarwals <<- Irri_deq_county %>% 
    filter(YEAR == Year) %>% 
    filter(Facility_withdrawal_mg >0)

  QS_data(Year) 
  fn_Area_TH(10, Year)
  
  Non.Reported_Coefficient1 <- Non.Reported
  
  Non.Reported_Coefficient1 <- left_join(Non.Reported_Coefficient1, county.codes, by = c("County" = "County_Name"))
  return(Non.Reported_Coefficient1)
  }

Ag_census_2017 <- Ag_census(2017)

Dry_Year <- left_join(Dry_Year, Ag_census_2017[,c(2,7,8)], by = c("COUNTYFP" = "County_Code"))
Dry_Year <- Dry_Year %>% 
  mutate(Irrigation_demand = round(Total.Irri.Area * (Irrigation  /25.5)*27154/1000000,2))


sum(Dry_Year$Irrigation_demand, na.rm = TRUE)         

Wet_Year <- left_join(Wet_Year, Ag_census_2017[,c(2,7,8)], by = c("COUNTYFP" = "County_Code"))
Wet_Year <- Wet_Year %>% 
  mutate(Irrigation_demand = round(Total.Irri.Area * (Irrigation  /25.5)*27154/1000000,2))

sum(Wet_Year$Irrigation_demand, na.rm = TRUE)  

Avg_Year <- left_join(Avg_Year, Ag_census_2017[,c(2,7,8)], by = c("COUNTYFP" = "County_Code"))
Avg_Year <- Avg_Year %>% 
  mutate(Irrigation_demand = round(Total.Irri.Area * (Irrigation  /25.5)*27154/1000000,2))

sum(Avg_Year$Irrigation_demand, na.rm = TRUE) 

sum(IRR_DEQ_withdarwals$Facility_withdrawal_mg, na.rm = TRUE)





p1 <- Dry_Year %>%
  group_by(YEAR) %>%
  summarise(counts = n()) %>% 
ggplot(aes(x=factor(YEAR),y=counts))+
  geom_col(color='black',fill='lightblue')+
 labs(title= "Driest Year for Counties based on calcualted Irrigation demand between 2002-2017",
     x="YEAR", y = "Count") 
  # +
p1<- p1 + theme_bw()
  p1 <- p1+theme(axis.text.x=element_text(angle = 0, hjust = 0),
                 legend.position="None", 
                 legend.title=element_blank(),
                 legend.box = "horizontal", 
                 legend.background = element_rect(fill="white",
                                                  size=0.5, linetype="solid", 
                                                  colour ="white"),
                 legend.text=element_text(size=12),
                 axis.text=element_text(size=12, colour="black"),
                 axis.title=element_text(size=14, colour="black"),
                 axis.line = element_line(colour = "black", 
                                          size = 0.5, linetype = "solid"),
                 axis.ticks = element_line(colour="black"),
                 panel.grid.major=element_line(colour = "light grey"), 
                 panel.grid.minor=element_blank())
  p1

  
ggsave(paste0(WUDR_github,"/plots/Dry_Wet_year/", "Calculated_dry.png"), plot = p1, width = 9.5, height = 6, units = "in")

  
  
  p1 <- Wet_Year %>%
  group_by(YEAR) %>%
  summarise(counts = n()) %>% 
ggplot(aes(x=factor(YEAR),y=counts))+
  geom_col(color='black',fill='cyan3')+
 labs(title= "Wet Years for Counties from 2002-2017",
     x="YEAR", y = "Count") 
  # +
p1<- p1 + theme_bw()
  p1 <- p1+theme(axis.text.x=element_text(angle = 0, hjust = 0),
                 legend.position="None", 
                 legend.title=element_blank(),
                 legend.box = "horizontal", 
                 legend.background = element_rect(fill="white",
                                                  size=0.5, linetype="solid", 
                                                  colour ="white"),
                 legend.text=element_text(size=12),
                 axis.text=element_text(size=12, colour="black"),
                 axis.title=element_text(size=14, colour="black"),
                 axis.line = element_line(colour = "black", 
                                          size = 0.5, linetype = "solid"),
                 axis.ticks = element_line(colour="black"),
                 panel.grid.major=element_line(colour = "light grey"), 
                 panel.grid.minor=element_blank())
  p1
  