WUDR_github<-"F:/My Drive/WUDR/WUDR_Github/WUDR"
setwd(WUDR_github)

library(tidyverse)
library(tmap)
library(rgdal)
library(stringr)
library(Kendall)
library(plotly)
library("gridExtra")
library(ggpubr)
options(scipen = 9999)


VA_counties<-readOGR(paste0(WUDR_github, "/VA_counties_sp"), layer="VA_counties")
county.codes <- read_csv(paste0(WUDR_github, "/csv_files/county_codes_census.csv")) 
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

#  Year = 2002
# PPT_start_month = 6 #June
# PPT_end_month = 8 #august
# Crop_Demand = 762  # 30 inches

Senstivity <- function(Year,PPT_start_month,PPT_end_month,Crop_Demand){
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
  
  load(paste0(WUDR_github,"/dat_load/PPT_foundational_datset.RData"))
  
  
  PPT_Growing_Season <- PPT_VA %>% 
  filter(between(Month, PPT_start_month,PPT_end_month))

PPT_Growing_Season <- PPT_Growing_Season[, colSums(is.na(PPT_Growing_Season)) != nrow(PPT_Growing_Season)]

# Build loop for this 
ppt_df <- matrix(NA, nrow(PPT_Growing_Season), ncol(PPT_Growing_Season))
colnames(ppt_df) <- colnames(PPT_Growing_Season)


 for (i in 3:ncol(ppt_df)) {
   c <-  as.data.frame(PPT_Growing_Season[,i]) %>%
     mutate(tot = lag(PPT_Growing_Season[,i], 1, default = 0) + PPT_Growing_Season[,i],
            runoff = case_when(tot >= 38.5 ~  tot-38.5,
                               tot <  38.5 ~ 0))
   c$runoff2 <- ifelse(c$`PPT_Growing_Season[, i]` == 0, 0, c$runoff)
   
   c$ppt <- c$`PPT_Growing_Season[, i]`-c$runoff2
   ppt <- ifelse(c$ppt < 0, 0, c$ppt)
   
   ppt_df[,i] <- ppt
   rm(c)
 }

ppt_df[,1] <- PPT_Growing_Season[,1]
ppt_df[,2] <- PPT_Growing_Season[,2]

ppt_df <- as.data.frame(ppt_df)

Monthly_PPT_Growing_Season <- ppt_df %>% 
  group_by(Year, Month) %>% 
  summarise_each(funs(sum))


Yearly_PPT_Growing_Season <- Monthly_PPT_Growing_Season %>% 
  subset(select = - Month) %>% 
  group_by(Year) %>% 
  summarise_each(funs(sum))

Yearly_PPT_Growing_Season <- Yearly_PPT_Growing_Season %>% 
 filter(Year == c("2002", "2007","2012","2017"))

 # Yearly_PPT_Growing_Season <- Yearly_PPT_Growing_Season[,-c(1)]

ldata <-  pivot_longer(Yearly_PPT_Growing_Season, cols = (2:ncol(Yearly_PPT_Growing_Season)))
# ldata <- ldata[,-c(1)]
ldata <- ldata[complete.cases(ldata), ]
ldata$name <- toupper(ldata$name)
ldata$name <-gsub(".", " ", ldata$name , fixed=TRUE)
ldata$name <-gsub("1", "", ldata$name , fixed=TRUE)
ldata$name[ldata$name == "SUFFOLK"] <- "SUFFOLK CITY"
ldata$name[ldata$name == "VIRGINIA BEACH"] <- "VIRGINIA BEACH CITY"
ldata$name[ldata$name == "CHESAPEAKE"] <- "CHESAPEAKE CITY"
ldata$name[ldata$name == "FAIRFAX "] <- "FAIRFAX"
colnames(ldata)[3] <- "PPT"
ldata$PPT <- round(ldata$PPT, 2)
ppt_list_yearly <- split( ldata , f = ldata$Year)



  if (Year == 2002) {
    i = 1}else if (Year == 2007){
      i = 2} else if (Year == 2012){
        i = 3} else if (Year == 2017){
          i =4
        }
  
Irr_deficit <- ppt_list_yearly[[i]][,c(2,3)]

# Crop_Demand <- 762 #30 inches
Irr_deficit$Irrigation <- round(Crop_Demand - Irr_deficit$PPT,0) # considering 30 inches as crop water demand
  
  Fn_of_Irri_withdrawals <- left_join(Fn_of_Irri_withdrawals, Irr_deficit, by = c("County"= "name"))
  
  Fn_of_Irri_withdrawals$Vol_Unreported <- round(Fn_of_Irri_withdrawals$Irr.Area.Under.TH*(Fn_of_Irri_withdrawals$Irrigation/25.5)*27154/1000000,2)
  
  
  Fn_of_Irri_withdrawals$C_irr = Fn_of_Irri_withdrawals$Vol_Unreported / Fn_of_Irri_withdrawals$Facility_withdrawal_mg
  
  Fn_of_Irri_withdrawals$Method1_Unreported <- Fn_of_Irri_withdrawals$C_irr * Fn_of_Irri_withdrawals$Facility_withdrawal_mg
  
  Fn_of_Irri_withdrawals <- Fn_of_Irri_withdrawals[,c(1,8,2,4,5,9,10:13)]

  
Fn_of_Irri_withdrawals$Pct_Under_TH <- (100*Fn_of_Irri_withdrawals$Irr.Area.Under.TH/Fn_of_Irri_withdrawals$Total.Irri.Area)
Fn_of_Irri_withdrawals$Unreported_DEQ_irrigtaion <- round(((Fn_of_Irri_withdrawals$Facility_withdrawal_mg*100)/
                                                   (100-Fn_of_Irri_withdrawals$Pct_Under_TH))-Fn_of_Irri_withdrawals$Facility_withdrawal_mg ,2)
return(Fn_of_Irri_withdrawals)
}


#########################################################################
Small_farm_unreported_dat <- list()

plot_list <- list()
C_year <- c(2002, 2007,2012,2017)

###########################################################################

# ********MAKE CHANGES HERE SO THAT PLOT TITLES ARE UPDATED ****************
# The currently used is 6, 8, 762 respectively
# 1.
PPT_start_month = 6 #June
PPT_end_month = 8 #august
Crop_Demand = 762 #(mm) # 30 inches

#3.
PPT_start_month = 6 #June
PPT_end_month = 8 #august
Crop_Demand = 635 #(mm)   #635 = 25inches. #508 = 20inches

#2.
PPT_start_month = 6 #June
PPT_end_month = 8 #august
Crop_Demand = 508 #(mm)   #635 = 25inches. #508 = 20inches

#4.
PPT_start_month = 5 #June
PPT_end_month = 9 #august
Crop_Demand = 762 #(mm)   #635 = 25inches. #508 = 20inches
###############################################################################

for (y in 1:4) {
  Small_farm_unreported_dat[[y]] <- Senstivity(C_year[y],PPT_start_month,PPT_end_month,Crop_Demand)
} 

###########################################################################



for (i in 1:4) {
  
plot_list[[i]] <- ggplot(Small_farm_unreported_dat[[i]], aes(x=log(Unreported_DEQ_irrigtaion), y=log(Vol_Unreported)))+
  geom_point()+
  geom_smooth(method=lm)+
  labs(title= C_year[i],
       x="Unreported based on \n DEQ Irrigation Withdrawals", y = "Unreported based \n on Deficit Irrigation") +
  scale_y_continuous(limits = c(-1, 5),  breaks = seq(-1, 5, by = 1))+
  scale_x_continuous(limits = c(-5, 5),  breaks = seq(-5, 5, by = 2))

plot_list[[i]]<- plot_list[[i]] + theme_bw()
plot_list[[i]] <- plot_list[[i]]+theme(axis.text.x=element_text(angle = 0, hjust = 0),
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

}

nam_start_month <- month.abb[PPT_start_month]
nam_end_month <- month.abb[PPT_end_month]
p <- grid.arrange(plot_list[[1]] , plot_list[[2]] , plot_list[[3]] ,plot_list[[4]] , ncol = 2, nrow = 2)
p <- annotate_figure(p, top = text_grob(paste0("Irrigation Start Month: ", nam_start_month , "      Irrigation End Month: ",nam_end_month , "               Crop demand (mm): ", Crop_Demand),
                     face = "bold", size = 14))

pnam <- paste0("Scatter-", nam_start_month, "-",nam_end_month, "-", Crop_Demand,"mm.png" )
ggsave(paste0(WUDR_github,"/plots/Coefficient1/Scatter plots Senstivity/",pnam), plot = p, width = 12, height = 8, units = "in")



















