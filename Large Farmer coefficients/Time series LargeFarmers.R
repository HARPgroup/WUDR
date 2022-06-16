
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

load(paste0(WUDR_github,"/dat_load/LF_Coeff_both.RData"))
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
  
  # Coef_val <- ifelse(Coef_val <0,0,Coef_val)
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


TS_LF_Coeff_Unreported_median <- TS_Coeff_fn(median)
  # write.csv(TS_LF_Coeff_Unreported_median, paste0(WUDR_github,"/Output_Tables/", "TS_LF_Coeff_Unreported_median.csv"), row.names= FALSE)


#############################################
#### PLOTS    ###############################
#############################################

plot_dat_median <- split( TS_LF_Coeff_Unreported_median , f = TS_LF_Coeff_Unreported_median$County_Name)



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
 # ggsave(paste0(WUDR_github,"/plots/Coefficient1/timeseries/Large Farmers/Median_Coeff/", nam,".png"), plot = p1, width = 9.5, height = 6, units = "in")

}


##############################################################
# Large farm time series using the  Total area
# Time series all counties

load(paste0(WUDR_github,"/dat_load/SF_times_series_Median.RData")) # Small farm time series to subtract

TS_LF_Tot_area_fn <- function(parm,SM_dat){
  
  
  dat_UTH<- purrr::reduce(list(Large_DEQ_Tot_area[[1]][,c(1,2,3)],Large_DEQ_Tot_area[[2]][,c(2,3)],
                               Large_DEQ_Tot_area[[3]][,c(2,3)],Large_DEQ_Tot_area[[4]][,c(2,3)]), dplyr::full_join, by = 'County_Code')
  
  
  dat_UTH <- left_join(dat_UTH , county.codes , by = "County_Code")
  dat_UTH <- dat_UTH[-1]
  dat_UTH <- dat_UTH[,c(1,6,2:5)]
  dat_UTH <- dat_UTH[order(dat_UTH$County_Name),]
  
  # Specify the coefficient to be selected
  Un_TH = parm
  
  Coef_val<- apply(dat_UTH[,3:6], 1, Un_TH, na.rm=TRUE)
  
  dat_UTH$parm_Tot_Area <- Coef_val
  
  colnames(dat_UTH)[3:6] <- seq(2002,2017,5)
  
  ###########################
  load(paste0(WUDR_github,"/dat_load/PRECIP_JUNE_AUGUST_ALL_YEARS.RData"))
  
  
  ppt_list_yearly <- lapply(ppt_list_yearly, function(x)
    mutate(x, Irrigation = round(508 - PPT,0)))
  
  ppt_list_yearly <- bind_rows(ppt_list_yearly, .id = "Year")
  
  ppt_list_yearly <- left_join(ppt_list_yearly, dat_UTH[,c(1,2,7)], c("name"= "County_Name"))
  
  ppt_list_yearly <- ppt_list_yearly[complete.cases(ppt_list_yearly), ]
  
  ppt_list_yearly$All_Irrigation_mg = round(ppt_list_yearly$parm_Tot_Area*(ppt_list_yearly$Irrigation/25.5)*27154/1000000,2)
  
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
  
  
  # Large_farm_timeseries$Unreported_Deficit_Irr_based <- round((Large_farm_timeseries$All_Irrigation_mg - Large_farm_timeseries$IRR_DEQ_withdrawals-Large_farm_timeseries$Small_Farm_Unreported),5)
  
  Large_farm_timeseries$Unreported_Deficit_Irr_based <- NA
  Large_farm_timeseries[,11] <- round((Large_farm_timeseries[,7] - Large_farm_timeseries[,10]-Large_farm_timeseries[,8]),5)
  
  Large_farm_timeseries$C_tot_lg <- Large_farm_timeseries$Unreported_Deficit_Irr_based / Large_farm_timeseries$TOT_DEQ_Withdrawals
  
  return(Large_farm_timeseries)
}

TS_LF_Unreported_Median_Area <- TS_LF_Tot_area_fn(median, SF_Unreported_Median_UnderTh)


 # write.csv(TS_LF_Unreported_Median_Area, paste0(WUDR_github,"/Output_Tables/", "TS_LF_Unreported_Median_Area.csv"), row.names= FALSE)


# save(TS_LF_Coeff_Unreported_median,TS_LF_Unreported_Median_Area, file=paste0(WUDR_github,"/dat_load/LF_All_times_series.RData"))



plot_dat <- split( TS_LF_Unreported_Median_Area , f = TS_LF_Unreported_Median_Area$name)

for (i in 1:length(plot_dat)) {
  
  p1 <- ggplot(plot_dat[[i]], aes(x=Year, y=`Unreported_Deficit_Irr_based`, group = 1))+
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

#################################################################
# Plot unreported withdrawals based on both methods in counties with Irrigation data
# In time series check the unreported volumes 
load(paste0(WUDR_github,"/dat_load/LF_All_times_series.RData"))

Common_counties <- left_join(TS_LF_Coeff_Unreported_median[,c(1,2,3,6)],TS_LF_Unreported_Median_Area[,c(1,2,11)], by= c("COUNTYFP" = "County_Code", "YEAR"= "Year"))
p1 <- ggplot(Common_counties, aes(x=log10(Unreported_Coeff_Based), y=log10(Unreported_Deficit_Irr_based)))+
  geom_point()+
  geom_smooth(method=lm)+
  labs(title = "Unreported Large Farm withdrawals 2002-2017",
       x="Unreported median Irrigation Coefficient (log)", y = "Unreported median Total Acreage (log)")+
  scale_y_continuous(limits = c(-2, 5),  breaks = seq(-2, 5, by = 1))+
  scale_x_continuous(limits = c(-2, 5),  breaks = seq(-2, 5, by = 1))

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

ggsave(paste0(WUDR_github,"/plots/Coefficient1/scatterLF.png"), plot = p1, width = 9.5, height = 6, units = "in")

###################################################################
# Yearly summary for counties with DEQ data 
# For comparison between methods

Yearly_Summary <- Common_counties %>% 
  group_by(YEAR) %>% 
  summarise(`Unreported Coeff Based` = sum(Unreported_Coeff_Based[which(Unreported_Coeff_Based>0)]),
            `Unreported Deficit Irrigation` = sum(Unreported_Deficit_Irr_based[which(Unreported_Deficit_Irr_based>0)]))
DEQ_Irr<- TS_LF_Coeff_Unreported_median %>% 
  group_by(YEAR) %>% 
  summarise(`DEQ Irrigation` = sum(Facility_withdrawal_mg))

plot_dat <- cbind.data.frame(Yearly_Summary,DEQ_Irr)

plot_dat <- plot_dat[,c(-4)]
plot_dat <- pivot_longer(plot_dat, cols = c(2,3,4), names_to = "Type" ,values_to = "Unreported_Withdrawals")

p1 <- ggplot(plot_dat, aes(x=YEAR, y=Unreported_Withdrawals, color = Type ))+
  # geom_bar(stat="identity", color="black",width=0.75,   position=position_dodge(0.75)) +
  geom_line(aes(color=Type))+
  geom_point(aes(color=Type))+
  labs(title= "Unreported withdrawals common counties 2002-2017",
       x="Year", y = "Unreported Withdrawals (MG)")+
  scale_x_continuous(limits = c(2001.5, 2017.5),  breaks = seq(2002, 2017, by = 1))+
  scale_y_continuous(limits = c(0, 20000),  breaks = seq(0, 20000, by = 1500))

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

ggsave(paste0(WUDR_github,"/plots/Coefficient1/Line_LF_Comparison_methods.png"), plot = p1, width = 9.5, height = 6, units = "in")


####################################################################
####################################################################
# Yearly summary for all counties in both methods 
DEQ_Irr<- TS_LF_Coeff_Unreported_median %>% 
  group_by(YEAR) %>% 
  summarise(`DEQ Irrigation` = sum(Facility_withdrawal_mg))


Yearly_Summary_Coeff <- TS_LF_Coeff_Unreported_median %>% 
  group_by(YEAR) %>% 
  summarise(`Unreported Coeff Based` = sum(Unreported_Coeff_Based[which(Unreported_Coeff_Based>0)]))


Yearly_Summary_Area <- TS_LF_Unreported_Median_Area %>% 
  group_by(Year) %>% 
  summarise(`Unreported Deficit Irrigation` = sum(Unreported_Deficit_Irr_based[which(Unreported_Deficit_Irr_based>0)]))

# plot_dat <- cbind.data.frame(Yearly_Summary_Coeff,DEQ_Irr,Yearly_Summary_Area)
# plot_dat <- plot_dat[,-c(3,5)]
# plot_dat <- pivot_longer(plot_dat, cols = c(2:4), names_to = "Type" ,values_to = "Unreported_Withdrawals")

plot_dat <- cbind.data.frame(DEQ_Irr,Yearly_Summary_Area)
plot_dat <- plot_dat[,-c(3)]
plot_dat <- pivot_longer(plot_dat, cols = c(2:3), names_to = "Type" ,values_to = "Unreported_Withdrawals")

p1 <- ggplot(plot_dat, aes(x=YEAR, y=Unreported_Withdrawals, color = Type ))+
# geom_bar(stat="identity", color="black",width=0.75,   position=position_dodge(0.75)) +
 geom_line(aes(color=Type))+
  geom_point(aes(color=Type))+
  labs(title= "Large Farm Unreported withdrawals all counties 2002-2017",
       x="Year", y = "Unreported Withdrawals (MG)")+
  scale_x_continuous(limits = c(2001.5, 2017.5),  breaks = seq(2002, 2017, by = 1))+
  scale_y_continuous(limits = c(0, 20000),  breaks = seq(0, 20000, by = 1500))

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

ggsave(paste0(WUDR_github,"/plots/Coefficient1/Line_LF_Total_Unreproted_withdarwals.png"), plot = p1, width = 12, height = 6, units = "in")




