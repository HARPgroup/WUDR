
WUDR_github<-"F:/My Drive/WUDR/WUDR_Github/WUDR"
setwd(WUDR_github)

library(tidyverse) 
library(tmap)
library(rgdal)
library(stringr)
library(Kendall)
library(purrr)
library("gridExtra")
options(scipen = 9999)

####################################################################
# Load the data

load(paste0(WUDR_github,"/dat_load/Large_DEQ_IRR_Coeff.RData"))
load(paste0(WUDR_github, "/dat_load/All_Years_DEQ_data_Total_nd_irr.Rdata")) 


county.codes <- read.csv(paste0(WUDR_github,"/csv_files/county_codes_census.csv"))

#########################################


TS_Coeff_fn <- function(parm){
  dat<- purrr::reduce(list(Large_DEQ_IRR_Coeff[[1]][,c(1,2,11)],
                                Large_DEQ_IRR_Coeff[[2]][,c(2,11)],Large_DEQ_IRR_Coeff[[3]][,c(2,11)],
                                Large_DEQ_IRR_Coeff[[4]][,c(2,11)]), dplyr::full_join, by = 'County_Code') # Irrigation coefficient for counties with data
  
  
  dat <- left_join(dat , county.codes , by = "County_Code")
  dat <- dat[-1]
  dat <- dat[,c(1,6,2:5)]
  dat <- dat[order(dat$County_Name),]
  
  colnames(dat)[3:6] <- seq(2002,2017,5)
  # Specify the coefficient to be selected
  Coef_type = parm
  
  Coef_val<- apply(dat[,3:6], 1, Coef_type, na.rm=TRUE)
  
  dat$parm_Coeff <- Coef_val
  
  colnames(dat)[3:6] <- seq(2002,2017,5)
  
  
  Irri_DEQ_withdrawals <- Irri_deq_county %>% 
    filter(YEAR < 2018) %>% 
    filter(Facility_withdrawal_mg >0)
  
  # rm(Irri_deq_county)
  
  LF_Coeff_Unreported_parm <- left_join(Irri_DEQ_withdrawals, dat[,c(1,2,7)], by = c("COUNTYFP" = "County_Code"))
  
  LF_Coeff_Unreported_parm <- LF_Coeff_Unreported_parm[complete.cases(LF_Coeff_Unreported_parm), ]
  
  LF_Coeff_Unreported_parm$Unreported_Coeff_Based <- round(LF_Coeff_Unreported_parm$Facility_withdrawal_mg*LF_Coeff_Unreported_parm$parm_Coeff,2)
  
  LF_Coeff_Unreported_parm <- LF_Coeff_Unreported_parm[,c(1,3,8,7,9,10)]
  
  return(LF_Coeff_Unreported_parm)
}

TS_LF_Coeff_Unreported_mean <- TS_Coeff_fn(mean)
TS_LF_Coeff_Unreported_median <- TS_Coeff_fn(median)
 # write.csv(TS_LF_Coeff_Unreported_mean, paste0(WUDR_github,"/Output_Tables/", "TS_LF_Coeff_Unreported_mean.csv"), row.names= FALSE)
 # write.csv(TS_LF_Coeff_Unreported_median, paste0(WUDR_github,"/Output_Tables/", "TS_LF_Coeff_Unreported_median.csv"), row.names= FALSE)


#############################################
#### PLOTS    ###############################
#############################################

plot_dat_mean <- split( TS_LF_Coeff_Unreported_mean , f = TS_LF_Coeff_Unreported_mean$County_Name)
plot_dat_median <- split( TS_LF_Coeff_Unreported_median , f = TS_LF_Coeff_Unreported_median$County_Name)

for (i in 1:length(plot_dat_mean)) {

   p1 <- ggplot(plot_dat_mean[[i]], aes(x=YEAR, y=`Unreported_Coeff_Based`, group = 1))+
    geom_line()+
    geom_point()+
    labs(title= paste0("Mean Coeff ", plot_dat_mean[[i]][1,3] ),
         x="Year", y = "Large Farms \n Unreported Withdrawals (MG)")+
    scale_x_continuous(limits = c(2002, 2017),  breaks = seq(2002, 2017, by = 2))

  p1<- p1 + theme_bw()
  p1 <- p1+theme(axis.text.x=element_text(angle = 0, hjust = 0),
                 legend.position="top",
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

  nam = paste0( plot_dat_mean[[i]][1,3],"_Mean_Coeff")
 ggsave(paste0(WUDR_github,"/plots/Coefficient1/timeseries/Large Farmers/Mean_Coeff/", nam,".png"), plot = p1, width = 9.5, height = 6, units = "in")

}


for (i in 1:length(plot_dat_median)) {
  
  p1 <- ggplot(plot_dat_median[[i]], aes(x=YEAR, y=`Unreported_Coeff_Based`, group = 1))+
    geom_line()+
    geom_point()+
    labs(title= paste0("Median Coeff ", plot_dat_median[[i]][1,3]) ,
         x="Year", y = "Large Farms \n Unreported Withdrawals (MG)")+
    scale_x_continuous(limits = c(2002, 2017),  breaks = seq(2002, 2017, by = 2))
  
  p1<- p1 + theme_bw()
  p1 <- p1+theme(axis.text.x=element_text(angle = 0, hjust = 0),
                 legend.position="top",
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
  
  nam = paste0( plot_dat_median[[i]][1,3],"_Median_Coeff")
   ggsave(paste0(WUDR_github,"/plots/Coefficient1/timeseries/Large Farmers/Median_Coeff/", nam,".png"), plot = p1, width = 9.5, height = 6, units = "in")
  
}


##############################################################
# Large farm time series using the  Total area
# Time series all counties

load(paste0(WUDR_github,"/dat_load/All_times_series.RData")) # Small farm time series to subtract

TS_LF_Under_TH_fn <- function(parm,SM_dat){
  
  
  load(paste0(WUDR_github,"/dat_load/Large_DEQ_Under_TH_Coeff.RData"))
  
  
  dat_UTH<- purrr::reduce(list(Large_DEQ_Under_TH[[1]][,c(1,2,3)],Large_DEQ_Under_TH[[2]][,c(2,3)],
                               Large_DEQ_Under_TH[[3]][,c(2,3)],Large_DEQ_Under_TH[[4]][,c(2,3)]), dplyr::full_join, by = 'County_Code')
  
  
  dat_UTH <- left_join(dat_UTH , county.codes , by = "County_Code")
  dat_UTH <- dat_UTH[-1]
  dat_UTH <- dat_UTH[,c(1,6,2:5)]
  dat_UTH <- dat_UTH[order(dat_UTH$County_Name),]
  
  # Specify the coefficient to be selected
  Un_TH = parm
  
  Coef_val<- apply(dat_UTH[,3:6], 1, Un_TH, na.rm=TRUE)
  
  dat_UTH$Max_Tot_Area <- Coef_val
  
  colnames(dat_UTH)[3:6] <- seq(2002,2017,5)
  
  ###########################
  load(paste0(WUDR_github,"/dat_load/PRECIP_JUNE_AUGUST_ALL_YEARS.RData"))
  
  
  ppt_list_yearly <- lapply(ppt_list_yearly, function(x)
    mutate(x, Irrigation = round(762 - PPT,0)))
  
  ppt_list_yearly <- bind_rows(ppt_list_yearly, .id = "Year")
  
  ppt_list_yearly <- left_join(ppt_list_yearly, dat_UTH[,c(1,2,7)], c("name"= "County_Name"))
  
  ppt_list_yearly <- ppt_list_yearly[complete.cases(ppt_list_yearly), ]
  
  ppt_list_yearly$All_Irrigation_mg = round(ppt_list_yearly$Max_Tot_Area*(ppt_list_yearly$Irrigation/25.5)*27154/1000000,2)
  
  ppt_list_yearly<- ppt_list_yearly[,c(1,5,2,3,4,6,7)]
  
  
  ppt_list_yearly$Year <- as.numeric(ppt_list_yearly$Year)
  
  load(paste0(WUDR_github, "/dat_load/All_Years_DEQ_data_Total_nd_irr.Rdata")) 
  Irri_DEQ_withdrawals <- Irri_deq_county %>% 
    filter(YEAR < 2018) %>% 
    filter(Facility_withdrawal_mg >0)
  
  rm(Irri_deq_county)
  
  # Merge Small farm withdrawals
  
  Large_farm_timeseries <- left_join(ppt_list_yearly, SM_dat[,c(1,2,7)], c("Year" ,"County_Code" ))
  
  # Merge DEQ total and Irrigation withdrawals
  Large_farm_timeseries <- left_join(Large_farm_timeseries, Total_deq_county[,c(1,3,7)], c("Year" = "YEAR" ,"County_Code" = "COUNTYFP" ))
  Large_farm_timeseries <- left_join(Large_farm_timeseries, Irri_DEQ_withdrawals[,c(1,3,7)], c("Year" = "YEAR" ,"County_Code" = "COUNTYFP" ))
  
  colnames(Large_farm_timeseries)[c(8:10)] <- c("Small_Farm_Unreported", "TOT_DEQ_Withdrawals", "IRR_DEQ_withdrawals")
  
  Large_farm_timeseries$IRR_DEQ_withdrawals[is.na(Large_farm_timeseries$IRR_DEQ_withdrawals)] <- 0
  
  
  # Large_farm_timeseries$Large_Farm_unreported <- round((Large_farm_timeseries$All_Irrigation_mg - Large_farm_timeseries$IRR_DEQ_withdrawals-Large_farm_timeseries$Small_Farm_Unreported),5)
  
  Large_farm_timeseries$Large_Farm_unreported <- NA
  Large_farm_timeseries[,11] <- round((Large_farm_timeseries[,7] - Large_farm_timeseries[,10]-Large_farm_timeseries[,8]),5)
  
  Large_farm_timeseries$C_tot_lg <- Large_farm_timeseries$Large_Farm_unreported / Large_farm_timeseries$TOT_DEQ_Withdrawals
  
  return(Large_farm_timeseries)
}

TS_LF_Unreported_Median_Area <- TS_LF_Under_TH_fn(median, SF_Unreported_Median_UnderTh)
TS_LF_Unreported_MAX_Area <- TS_LF_Under_TH_fn(max, SF_Unreported_MAX_UnderTh)


write.csv(Large_farm_timeseries, paste0(WUDR_github,"/Output_Tables/", "Timeseries_Tot_large_farms.csv"), row.names= FALSE)
# write.csv(TS_LF_Unreported_Median_Area, paste0(WUDR_github,"/Output_Tables/", "TS_LF_Unreported_Median_Area.csv"), row.names= FALSE)
# write.csv(TS_LF_Unreported_MAX_Area, paste0(WUDR_github,"/Output_Tables/", "TS_LF_Unreported_MAX_Area.csv"), row.names= FALSE)


plot_dat <- split( TS_LF_Unreported_MAX_Area , f = TS_LF_Unreported_MAX_Area$name)

 for (i in 1:length(plot_dat)) {

  p1 <- ggplot(plot_dat[[i]], aes(x=Year, y=`Large_Farm_unreported`, group = 1))+
    geom_line()+
    geom_point()+
    labs(title= plot_dat[[i]][1,3] ,
         x="Year", y = "Large Farm \n Unreported Withdrawals (MG)")+
    scale_x_continuous(limits = c(2002, 2017),  breaks = seq(2002, 2017, by = 2))

  p1<- p1 + theme_bw()
  p1 <- p1+theme(axis.text.x=element_text(angle = 0, hjust = 0),
                 legend.position="top",
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
  # p1

  nam = paste0( plot_dat[[i]][1,3],"Max Area")
  ggsave(paste0(WUDR_github,"/plots/Coefficient1/timeseries/Large Farmers/Max Acerage/", nam,".png"), plot = p1, width = 9.5, height = 6, units = "in")
}

plot_dat <- split( TS_LF_Unreported_Median_Area , f = TS_LF_Unreported_Median_Area$name)

for (i in 1:length(plot_dat)) {
  
  p1 <- ggplot(plot_dat[[i]], aes(x=Year, y=`Large_Farm_unreported`, group = 1))+
    geom_line()+
    geom_point()+
    labs(title= plot_dat[[i]][1,3] ,
         x="Year", y = "Large Farm \n Unreported Withdrawals (MG)")+
    scale_x_continuous(limits = c(2002, 2017),  breaks = seq(2002, 2017, by = 2))
  
  p1<- p1 + theme_bw()
  p1 <- p1+theme(axis.text.x=element_text(angle = 0, hjust = 0),
                 legend.position="top",
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
  # p1
  
  nam = paste0( plot_dat[[i]][1,3],"Median Area")
  ggsave(paste0(WUDR_github,"/plots/Coefficient1/timeseries/Large Farmers/Median Acerage/", nam,".png"), plot = p1, width = 9.5, height = 6, units = "in")
}

# COMPARISON PLOT 

Compare_LF__Mean_Coeff_Max_UnderTH <- left_join(TS_LF_Coeff_Unreported_mean[,c(1,2,3,6)],TS_LF_Unreported_MAX_Area[,c(1:2,11)], c("COUNTYFP" = "County_Code", "YEAR"= "Year"))

colnames(Compare_LF__Mean_Coeff_Max_UnderTH)[4] <- c("(b) Mean Coeff")

colnames(Compare_LF__Mean_Coeff_Max_UnderTH)[5] <- c("(a) Max Area")

plot_dat <- pivot_longer(Compare_LF__Mean_Coeff_Max_UnderTH, cols = c(4,5), names_to = "Type" ,values_to = "Unreported Wth")

plot_dat <- split( plot_dat , f = plot_dat$County_Name)


for (i in 1:length(plot_dat)) {
  p1 <- ggplot(plot_dat[[i]], aes(x=YEAR, y=`Unreported Wth`, group = Type ))+
    geom_line(aes(color=Type))+
    geom_point(aes(color=Type))+
    labs(title= plot_dat[[i]][1,3] ,
         x="Year", y = "Unreported Withdrawals (MG)")+
    scale_x_continuous(limits = c(2002, 2017),  breaks = seq(2002, 2017, by = 2))

  p1<- p1 + theme_bw()
  p1 <- p1+theme(axis.text.x=element_text(angle = 0, hjust = 0),
                 legend.position="top",
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

  nam = plot_dat[[i]][1,3]
  ggsave(paste0(WUDR_github,"/plots/Coefficient1/timeseries/Large Farmers/Comparison_large_Farm_Mean_Max/", nam,".png"), plot = p1, width = 9.5, height = 6, units = "in")

}



## Comparison plot 2
Compare_LF_median_Coeff_Area <- left_join(TS_LF_Coeff_Unreported_median[,c(1,2,3,6)],TS_LF_Unreported_Median_Area[,c(1:2,11)], c("COUNTYFP" = "County_Code", "YEAR"= "Year"))

colnames(Compare_LF_median_Coeff_Area)[4] <- c("(b) Median Coeff")

colnames(Compare_LF_median_Coeff_Area)[5] <- c("(a) Median Area")

plot_dat <- pivot_longer(Compare_LF_median_Coeff_Area, cols = c(4,5), names_to = "Type" ,values_to = "Unreported Wth")

plot_dat <- split( plot_dat , f = plot_dat$County_Name)


for (i in 1:length(plot_dat)) {
  p1 <- ggplot(plot_dat[[i]], aes(x=YEAR, y=`Unreported Wth`, group = Type ))+
    geom_line(aes(color=Type))+
    geom_point(aes(color=Type))+
    labs(title= plot_dat[[i]][1,3] ,
         x="Year", y = "Unreported Withdrawals (MG)")+
    scale_x_continuous(limits = c(2002, 2017),  breaks = seq(2002, 2017, by = 2))
  
  p1<- p1 + theme_bw()
  p1 <- p1+theme(axis.text.x=element_text(angle = 0, hjust = 0),
                 legend.position="top",
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
  
  nam = plot_dat[[i]][1,3]
  ggsave(paste0(WUDR_github,"/plots/Coefficient1/timeseries/Large Farmers/Comparison_large_Farm Median/", nam,".png"), plot = p1, width = 9.5, height = 6, units = "in")
  
}


state_summary <- Large_farm_timeseries %>% 
  group_by(Year,name) %>% 
  summarise(DEQ_irr = sum(IRR_DEQ_withdrawals),
            Deficit_irrigation = sum(All_Irrigation_mg))

state_summary2 <- m_dat %>% 
  group_by(YEAR) %>% 
  summarise(Coeff_based = sum(Unreported_Coeff_Based),
            Deficit_irrigation = sum(Large_Farm_unreported))



plot_dat <- pivot_longer(state_summary2, cols = c(2,3), names_to = "Type" ,values_to = "Unreported Wth")

p1 <- ggplot(plot_dat, aes(x=YEAR, y=`Unreported Wth`, group = Type ))+
  geom_line(aes(color=Type))+
  scale_y_continuous(limits = c(0, 120000),  breaks = seq(0, 120000, by = 10000))+labs(title= plot_dat[[i]][1,3] ,
                                                                                       x="Year", y = "Unreported Withdrawals (MG)")+
  scale_x_continuous(limits = c(2002, 2017),  breaks = seq(2002, 2017, by = 2))

p1<- p1 + theme_bw()
p1 <- p1+theme(axis.text.x=element_text(angle = 0, hjust = 0),
               legend.position="top",
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

