WUDR_github<-"F:/My Drive/WUDR/WUDR_Github/WUDR"
setwd(WUDR_github)

load(paste0(WUDR_github,"/dat_load/All_times_series.RData"))
load(paste0(WUDR_github,"/dat_load/LF_All_times_series.RData"))
load(paste0(WUDR_github, "/dat_load/All_Years_DEQ_data_Total_nd_irr.Rdata")) 

load(paste0(WUDR_github, "/dat_load/All_Years_DEQ_data_Total_nd_irr.Rdata")) 
Irri_DEQ_withdrawals <- Irri_deq_county %>% 
  filter(YEAR < 2018) %>% 
  filter(Facility_withdrawal_mg >0)

DEQ_Irri_withdrawals <- Irri_DEQ_withdrawals %>% 
  group_by(YEAR) %>% 
  summarise(DEQ_wth = sum(Facility_withdrawal_mg))

Irr_demand_median_Area <- TS_LF_Unreported_Median_Area %>% 
  group_by(Year) %>% 
  summarise(Irrigation_Demand_median_area = sum(All_Irrigation_mg))


Irr_demand_max_Area <- TS_LF_Unreported_MAX_Area %>% 
  group_by(Year) %>% 
  summarise(Irrigation_Demand_max_area = sum(All_Irrigation_mg))

####SURVEY
fn_Survey <- function(Year){
if (Year == 2018) {
  Irri_Amt = 0.3                    #Estimated Quantity of Water Applied By Source
  #Average acre-feet per acre Table 4
  Irri_Area = 63433
  Irr_vol_survey = round(Irri_Amt * Irri_Area *325851 /1000000, 0)           #million gallons

  
} else if (Year == 2013){
  Irri_Amt = 0.5
  Irri_Area = 68651 
  Irr_vol_survey = round(Irri_Amt * Irri_Area *325851 /1000000, 0)
  
  
} else if (Year == 2008){
  Irri_Amt = 0.6
  Irri_Area = 82187 
  Irr_vol_survey = round(Irri_Amt * Irri_Area *325851 /1000000, 0)
  
  
  
} else if (Year == 2003){
  Irri_Amt = 0.4 #2003
  Irri_Area = 98913 # 2002 census
  Irr_vol_survey= round(Irri_Amt * Irri_Area *325851 /1000000, 0)
  
}
return(Irr_vol_survey)
}

irr_2003 <- fn_Survey(2003)
irr_2008 <- fn_Survey(2008)
irr_2013 <- fn_Survey(2013)
irr_2018 <- fn_Survey(2018)

survey_dat <- Irr_demand_max_Area
survey_dat$survey_irr <- 0 
survey_dat$Irrigation_Demand_max_area <- NULL 
survey_dat[2,2] <-irr_2003
survey_dat[7,2] <-irr_2008
survey_dat[12,2] <-irr_2013
survey_dat[16,2] <-irr_2018

plot_dat <- cbind.data.frame(DEQ_Irri_withdrawals,Irr_demand_median_Area,Irr_demand_max_Area,survey_dat)
plot_dat <- plot_dat[,c(1,2,4,6,8)]

plot_dat <- pivot_longer(plot_dat, cols = c(2:5), names_to = "Type" ,values_to = "Irrigation_dat")
p1 <- ggplot(plot_dat, aes(x=YEAR, y=Irrigation_dat, fill = Type ))+
  geom_bar(stat="identity", color="black", position=position_dodge())+
# +
  # geom_line(aes(color=Type))+
  labs(title= "Irrigtaion demand and DEQ withdrawals" ,
       x="Year", y = " Irrigation (MG)")+
  scale_y_continuous(limits = c(0, 70000),  breaks = seq(0, 70000, by = 10000))+
  scale_x_continuous(limits = c(2001.5, 2017.5),  breaks = seq(2002, 2017, by = 2))

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
ggsave(paste0(WUDR_github,"/plots/Coefficient1/timeseries/","Barplot.png"), plot = p1, width = 9.5, height = 6, units = "in")


