####STATE SUMMARY USING BOTH METHODS
# MAX

# GET DEQ WITHDRAWALS

WUDR_github<-"F:/My Drive/WUDR/WUDR_Github/WUDR"
setwd(WUDR_github)

library(tidyverse) 

###LOAD BOTH TIME SERIES

load(paste0(WUDR_github,"/dat_load/All_times_series.RData"))
load(paste0(WUDR_github,"/dat_load/LF_All_times_series.RData"))

DEQ_Irr<- TS_LF_Coeff_Unreported_median %>% 
  group_by(YEAR) %>% 
  summarise(`DEQ Irrigation` = sum(Facility_withdrawal_mg))

##LARGE FARMS

# State summary with Median

Unreported_median_Area <- TS_LF_Unreported_Median_Area %>% 
  filter(Large_Farm_unreported >0 ) %>% 
  group_by(Year) %>% 
  summarise(LF_unreported_median_area = sum(Large_Farm_unreported))

Unreported_median_coeff <- TS_LF_Coeff_Unreported_median %>% 
  filter(Unreported_Coeff_Based >0 ) %>% 
  group_by(YEAR) %>% 
  summarise(LF_unreported_median_coeff = sum(Unreported_Coeff_Based))

plot_dat_median <- cbind.data.frame(DEQ_Irr,Unreported_median_Area,Unreported_median_coeff)
plot_dat_median <- plot_dat_median[,c(1,2,4,6)]

plot_dat <- pivot_longer(plot_dat_median, cols = c(2:4), names_to = "Type" ,values_to = "Unreported Wth")

p1 <- ggplot(plot_dat, aes(x=YEAR, y=`Unreported Wth`, group = Type ))+
  geom_line(aes(color=Type))+
  labs(title= "Large Farmer Unreported Withdrawals (Both Median)" ,
       x="Year", y = " Unreported Withdrawals (MG)")+
  scale_y_continuous(limits = c(0, 120000),  breaks = seq(0, 120000, by = 10000))+
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

ggsave(paste0(WUDR_github,"/plots/Coefficient1/timeseries/","State_Summary_Large Farm Median.png"), plot = p1, width = 9.5, height = 6, units = "in")

################################################

# State summary with Mean Coeff and MAX Area

Unreported_max_Area <- TS_LF_Unreported_MAX_Area %>% 
  filter(Large_Farm_unreported >0 ) %>% 
  group_by(Year) %>% 
  summarise(LF_unreported_max_Area = sum(Large_Farm_unreported))

Unreported_mean_coeff <- TS_LF_Coeff_Unreported_mean %>% 
  filter(Unreported_Coeff_Based >0 ) %>% 
  group_by(YEAR) %>% 
  summarise(LF_unreported_mean_coeff = sum(Unreported_Coeff_Based))

plot_dat_median <- cbind.data.frame(DEQ_Irr,Unreported_max_Area,Unreported_mean_coeff)
plot_dat_median <- plot_dat_median[,c(1,2,4,6)]

plot_dat <- pivot_longer(plot_dat_median, cols = c(2:4), names_to = "Type" ,values_to = "Unreported Wth")

p1 <- ggplot(plot_dat, aes(x=YEAR, y=`Unreported Wth`, group = Type ))+
  geom_line(aes(color=Type))+
  labs(title= "Large Farmer Unreported Withdrawals (Mean Coeff and Max Acerage)" ,
       x="Year", y = " Unreported Withdrawals (MG)")+
  scale_y_continuous(limits = c(0, 120000),  breaks = seq(0, 120000, by = 10000))+
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

ggsave(paste0(WUDR_github,"/plots/Coefficient1/timeseries/","State_Summary_Large Farm max mean.png"), plot = p1, width = 9.5, height = 6, units = "in")

################################################
# ####STATE SUMMARY SMALL FARMS




Unreported_max_Area <- SF_Unreported_MAX_UnderTh %>% 
  filter(Unreported_Irrigation_based >0 ) %>% 
  group_by(Year) %>% 
  summarise(SF_Unreported_max_UnderTH = sum(Unreported_Irrigation_based))

Unreported_mean_coeff <- SF_Unreported_Mean_Irr_Coeff %>% 
  filter(Unreported_Coeff_Based >0 ) %>% 
  group_by(YEAR) %>% 
  summarise(SF_Unreported_mean_coeff = sum(Unreported_Coeff_Based))

plot_dat_mean <- cbind.data.frame(DEQ_Irr,Unreported_max_Area,Unreported_mean_coeff)
plot_dat_mean <- plot_dat_mean[,c(1,2,4,6)]

plot_dat <- pivot_longer(plot_dat_mean, cols = c(2:4), names_to = "Type" ,values_to = "Unreported Wth")


p1 <- ggplot(plot_dat, aes(x=YEAR, y=`Unreported Wth`, group = Type ))+
  geom_line(aes(color=Type))+
  labs(title= "Small Farmer Unreported Withdrawals (Mean Coeff and Max Under TH Acerage)" ,
       x="Year", y = " Unreported Withdrawals (MG)")+
  scale_y_continuous(limits = c(0, 12000),  breaks = seq(0, 12000, by = 1000))+
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

ggsave(paste0(WUDR_github,"/plots/Coefficient1/timeseries/Small Farmers/","State_Summary_SMALL_FARM_MeanCoeff_MaxUnderTh.png"), plot = p1, width = 9.5, height = 6, units = "in")

################################################
Unreported_median_Area <- SF_Unreported_Median_UnderTh %>% 
  filter(Unreported_Irrigation_based >0 ) %>% 
  group_by(Year) %>% 
  summarise(SF_Unreported_Median_UnderTH = sum(Unreported_Irrigation_based))

Unreported_median_coeff <- SF_Unreported_Median_Irr_Coeff %>% 
  filter(Unreported_Coeff_Based >0 ) %>% 
  group_by(YEAR) %>% 
  summarise(SF_Unreported_Median_coeff = sum(Unreported_Coeff_Based))

plot_dat_median <- cbind.data.frame(DEQ_Irr,Unreported_median_Area,Unreported_median_coeff)
plot_dat_median <- plot_dat_median[,c(1,2,4,6)]

plot_dat <- pivot_longer(plot_dat_median, cols = c(2:4), names_to = "Type" ,values_to = "Unreported Wth")



p2 <- ggplot(plot_dat, aes(x=YEAR, y=`Unreported Wth`, group = Type ))+
  geom_line(aes(color=Type))+
  labs(title= "Small Farmer Unreported Withdrawals (Median values)" ,
       x="Year", y = " Unreported Withdrawals (MG)")+
  scale_y_continuous(limits = c(0, 12000),  breaks = seq(0, 12000, by = 1000))+
  scale_x_continuous(limits = c(2002, 2017),  breaks = seq(2002, 2017, by = 2))

p2<- p2 + theme_bw()
p2 <- p2+theme(axis.text.x=element_text(angle = 0, hjust = 0),
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
p2

ggsave(paste0(WUDR_github,"/plots/Coefficient1/timeseries/Small Farmers/","State_Summary_MedianBoth.png"), plot = p2, width = 9.5, height = 6, units = "in")



#####################
Yearly_max_demand <- TS_LF_Unreported_Median_Area %>% 
  group_by(Year) %>% 
  summarise(Irri_demand = max(All_Irrigation_mg))

TS_LF_Coeff_Unreported_median <- left_join(TS_LF_Coeff_Unreported_median,Yearly_max_demand, by = c("YEAR"= "Year"))
TS_LF_Coeff_Unreported_median$Unreported_updated <- TS_LF_Coeff_Unreported_median$Unreported_Coeff_Based

TS_LF_Coeff_Unreported_median$Unreported_updated <- ifelse(TS_LF_Coeff_Unreported_median$Unreported_Coeff_Based > TS_LF_Coeff_Unreported_median$Irri_demand, TS_LF_Coeff_Unreported_median$Irri_demand,TS_LF_Coeff_Unreported_median$Unreported_updated)


TS_LF_Coeff_Unreported_median$Coeff_updated <- TS_LF_Coeff_Unreported_median$parm_Coeff

TS_LF_Coeff_Unreported_median$Coeff_updated <- TS_LF_Coeff_Unreported_median$Unreported_updated/TS_LF_Coeff_Unreported_median$Facility_withdrawal_mg

sum(TS_LF_Coeff_Unreported_median$Unreported_Coeff_Based)


###################################################################
# Updated state summary chart

Unreported_median_Area <- TS_LF_Unreported_Median_Area %>% 
  filter(Large_Farm_unreported >0 ) %>% 
  group_by(Year) %>% 
  summarise(LF_unreported_median_area = sum(Large_Farm_unreported))

Unreported_median_coeff <- TS_LF_Coeff_Unreported_median %>% 
  filter(Unreported_updated >0 ) %>% 
  group_by(YEAR) %>% 
  summarise(LF_unreported_median_coeff = sum(Unreported_updated))

plot_dat_median <- cbind.data.frame(DEQ_Irr,Unreported_median_Area,Unreported_median_coeff)
plot_dat_median <- plot_dat_median[,c(1,2,4,6)]

plot_dat <- pivot_longer(plot_dat_median, cols = c(2:4), names_to = "Type" ,values_to = "Unreported Wth")

p1 <- ggplot(plot_dat, aes(x=YEAR, y=`Unreported Wth`, group = Type ))+
  geom_line(aes(color=Type))+
  labs(title= "Large Farmer Unreported Withdrawals (Both Median)" ,
       x="Year", y = " Unreported Withdrawals (MG)")+
  scale_y_continuous(limits = c(0, 120000),  breaks = seq(0, 120000, by = 10000))+
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

ggsave(paste0(WUDR_github,"/plots/Coefficient1/timeseries/","State_Summary_Large Farm Median Coeff limit.png"), plot = p1, width = 9.5, height = 6, units = "in")
