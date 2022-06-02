# Create acreage Data set
# Should get the maximum unreported acreage ** that is what is being used for calculation Irri acreage is not of any help. * Check with Dr.Shortridge



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
load(paste0(WUDR_github,"/dat_load/IrrCoeff.RData"))

# Use function to create the time series based on coefficients parm = mean or median 
TS_SF_Coeff_fn <- function(parm){
  
  dat_main<- purrr::reduce(list(DEQ_2002[,c(1,2,9)],DEQ_2007[,c(2,9)],DEQ_2012[,c(2,9)],DEQ_2017[,c(2,9)]), dplyr::full_join, by = 'County_Code')
  
  
  dat_main <- left_join(dat_main , county.codes , by = "County_Code")
  dat_main <- dat_main[-1]
  dat_main <- dat_main[,c(1,6,2:5)]
  dat_main <- dat_main[order(dat_main$County_Name),]
  
  dat <- dat_main
  
  #######################################################################################
  # Use parm coeffcient to generate the timeseries.
  
  # Specify the coefficient to be selected
  Coef_type = parm
  
  Coef_val<- apply(dat[,3:6], 1, Coef_type, na.rm=TRUE)
  
  # 1.
  ###parm for rest of the years and actual coefficients for census years
  
  dat$parm_Coeff <- Coef_val
  
  colnames(dat)[3:6] <- seq(2002,2017,5)
  
  load(paste0(WUDR_github, "/dat_load/All_Years_DEQ_data_Total_nd_irr.Rdata")) 
  
  SF_Unreported_parm_Irr_Coeff <- Irri_deq_county %>% 
    filter(YEAR < 2018) %>% 
    filter(Facility_withdrawal_mg >0)
  
  rm(Irri_deq_county)
  
  SF_Unreported_parm_Irr_Coeff <- left_join(SF_Unreported_parm_Irr_Coeff, dat[,c(1,2,7)], by = c("COUNTYFP" = "County_Code"))
  
  SF_Unreported_parm_Irr_Coeff <- SF_Unreported_parm_Irr_Coeff[complete.cases(SF_Unreported_parm_Irr_Coeff), ]
  
  SF_Unreported_parm_Irr_Coeff$Unreported_Coeff_Based <- round(SF_Unreported_parm_Irr_Coeff$Facility_withdrawal_mg*SF_Unreported_parm_Irr_Coeff$parm_Coeff,2)
  
  SF_Unreported_parm_Irr_Coeff <- SF_Unreported_parm_Irr_Coeff[,c(1,3,8,7,9,10)]
  
  return(SF_Unreported_parm_Irr_Coeff)
}

SF_Unreported_Mean_Irr_Coeff <- TS_SF_Coeff_fn(mean)
SF_Unreported_Median_Irr_Coeff <- TS_SF_Coeff_fn(median)


plot_dat <- split( SF_Unreported_Mean_Irr_Coeff , f = SF_Unreported_Mean_Irr_Coeff$County_Name)


  for (i in 1:length(plot_dat)) {

  p1 <- ggplot(plot_dat[[i]], aes(x=YEAR, y=`Unreported_Coeff_Based`, group = 1))+
    geom_line()+
    geom_point()+
    labs(title=paste0("Mean Coeff " , plot_dat[[i]][1,3]) ,
         x="Year", y = "Small Farmer \n Unreported Withdrawals (MG)")+
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

  nam = paste0( plot_dat[[i]][1,3],"_Mean_Coeff")
#ggsave(paste0(WUDR_github,"/plots/Coefficient1/timeseries/Small Farmers/Mean_Coeff/", nam,".png"), plot = p1, width = 9.5, height = 6, units = "in")

  }




plot_dat <- split( SF_Unreported_Median_Irr_Coeff , f = SF_Unreported_Median_Irr_Coeff$County_Name)

 for (i in 1:length(plot_dat)) {

  p1 <- ggplot(plot_dat[[i]], aes(x=YEAR, y=`Unreported_Coeff_Based`, group = 1))+
    geom_line()+
    geom_point()+
    labs(title=paste0("Median Coeff ", plot_dat[[i]][1,3]) ,
         x="Year", y = "Small Farmer \n Unreported Withdrawals (MG)")+
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

  nam = paste0( plot_dat[[i]][1,3],"_Median_Coeff")
#ggsave(paste0(WUDR_github,"/plots/Coefficient1/timeseries/Small Farmers/Median Coeff/", nam,".png"), plot = p1, width = 9.5, height = 6, units = "in")

 }

##############################################################
# Time series all counties using Under TH acerage

TS_SF_Under_TH_fn <- function(parm){
  
  dat_UTH<- purrr::reduce(list(Tdeq_coef_2002[,c(1,2,4)],Tdeq_coef_2007[,c(2,4)],Tdeq_coef_2012[,c(2,4)],Tdeq_coef_2017[,c(2,4)]), dplyr::full_join, by = 'County_Code')
  
  
  dat_UTH <- left_join(dat_UTH , county.codes , by = "County_Code")
  dat_UTH <- dat_UTH[-1]
  dat_UTH <- dat_UTH[,c(1,6,2:5)]
  dat_UTH <- dat_UTH[order(dat_UTH$County_Name),]
  
  
  # Specify the coefficient to be selected
  Un_TH = parm
  
  Coef_val<- apply(dat_UTH[,3:6], 1, Un_TH, na.rm=TRUE)
  
  dat_UTH$parm_UnderTh <- Coef_val
  
  colnames(dat_UTH)[3:6] <- seq(2002,2017,5)
  
  ###########################
  load(paste0(WUDR_github,"/dat_load/PRECIP_JUNE_AUGUST_ALL_YEARS.RData"))
  
  
  SF_Unreported_parm_UnderTh <- lapply(ppt_list_yearly, function(x)
    mutate(x, Irrigation = round(762 - PPT,0)))
  
  SF_Unreported_parm_UnderTh <- bind_rows(SF_Unreported_parm_UnderTh, .id = "Year")
  
  SF_Unreported_parm_UnderTh <- left_join(SF_Unreported_parm_UnderTh, dat_UTH[,c(1,2,7)], c("name"= "County_Name"))
  
  SF_Unreported_parm_UnderTh <- SF_Unreported_parm_UnderTh[complete.cases(SF_Unreported_parm_UnderTh), ]
  
  SF_Unreported_parm_UnderTh$Unreported_Irrigation_based = round(SF_Unreported_parm_UnderTh$parm_UnderTh*(SF_Unreported_parm_UnderTh$Irrigation/25.5)*27154/1000000,2)
  
  SF_Unreported_parm_UnderTh<- SF_Unreported_parm_UnderTh[,c(1,5,2,3,4,6,7)]
  
  #######################
  SF_Unreported_parm_UnderTh$Year <- as.numeric(SF_Unreported_parm_UnderTh$Year)
  
  return(SF_Unreported_parm_UnderTh)
}

SF_Unreported_MAX_UnderTh<- TS_SF_Under_TH_fn(max)
SF_Unreported_Median_UnderTh <- TS_SF_Under_TH_fn(median)

# write.csv(SF_Unreported_MAX_UnderTh, paste0(WUDR_github,"/Output_Tables/", "MAX_Timeseries_UnderTh_Deficit_Irr.csv"), row.names= FALSE)
# write.csv(SF_Unreported_Median_UnderTh, paste0(WUDR_github,"/Output_Tables/", "MEDIAN_Timeseries_Median_UnderTh_Deficit_Irr.csv"), row.names= FALSE)


plot_dat <- split( SF_Unreported_MAX_UnderTh , f = SF_Unreported_MAX_UnderTh$name)

 for (i in 1:length(plot_dat)) {

  p1 <- ggplot(plot_dat[[i]], aes(x=Year, y=`Unreported_Irrigation_based`, group = 1))+
    geom_line()+
    geom_point()+
    labs(title= paste0("Max Under Threshold Acerage ",plot_dat[[i]][1,3]) ,
         x="Year", y = "Small Farmer \n Unreported Withdrawals (MG)")+
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

  nam = paste0( plot_dat[[i]][1,3],"_MaxUnderTH")
  #ggsave(paste0(WUDR_github,"/plots/Coefficient1/timeseries/Small Farmers/Max_UnderTH/", nam,".png"), plot = p1, width = 9.5, height = 6, units = "in")
}


# Median Plot
plot_dat <- split( SF_Unreported_Median_UnderTh , f = SF_Unreported_Median_UnderTh$name)

 for (i in 1:length(plot_dat)) {

  p1 <- ggplot(plot_dat[[i]], aes(x=Year, y=`Unreported_Irrigation_based`, group = 1))+
    geom_line()+
    geom_point()+
    labs(title= paste0("Median Under Threshold Acerage ",plot_dat[[i]][1,3]) ,
         x="Year", y = "Small Farmer \n Unreported Withdrawals (MG)")+
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

  nam = paste0( plot_dat[[i]][1,3],"_MedianUnderTH")
  #ggsave(paste0(WUDR_github,"/plots/Coefficient1/timeseries/Small Farmers/Median UnderTH/", nam,".png"), plot = p1, width = 9.5, height = 6, units = "in")
}

#########################################################################################################
#########################################################################################################
# COMPARISON PLOTS

DEQ_Irr<- SF_Unreported_Median_Irr_Coeff %>% 
  group_by(YEAR) %>% 
  summarise(`DEQ Irrigation` = sum(Facility_withdrawal_mg))

Compare_Mean_Coeff_Max_UnderTH <- left_join(SF_Unreported_Mean_Irr_Coeff[,c(1,2,3,6)],SF_Unreported_MAX_UnderTh[,c(1:2,7)], c("COUNTYFP" = "County_Code", "YEAR"= "Year"))

colnames(Compare_Mean_Coeff_Max_UnderTH)[5] <- c("(a) Max Under Th")

colnames(Compare_Mean_Coeff_Max_UnderTH)[4] <- c("(b) Mean Coeff")

plot_dat <- pivot_longer(Compare_Mean_Coeff_Max_UnderTH, cols = c(4,5), names_to = "Type" ,values_to = "Unreported Wth")

plot_dat <- split( plot_dat , f = plot_dat$County_Name)


 for (i in 1:length(plot_dat)) {
  p1 <- ggplot(plot_dat[[i]], aes(x=YEAR, y=`Unreported Wth`, group = Type ))+
    geom_line(aes(color=Type))+
    geom_point(aes(color=Type))+
    labs(title= plot_dat[[i]][1,3] ,
         x="Year", y = "Small Farmer \n Unreported Withdrawals (MG)")+
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

  nam = plot_dat[[i]][1,3]
  ggsave(paste0(WUDR_github,"/plots/Coefficient1/timeseries/Small Farmers/Max_Mean_Comparison time series/", nam,".png"), plot = p1, width = 9.5, height = 6, units = "in")

 }


# COMPARISON PLOT 2 

Compare_Median_Coeff_UnderTH <- left_join(SF_Unreported_Median_Irr_Coeff[,c(1,2,3,6)],SF_Unreported_Median_UnderTh[,c(1:2,7)], c("COUNTYFP" = "County_Code", "YEAR"= "Year"))

colnames(Compare_Median_Coeff_UnderTH)[4] <- c("(b) Median Coeff")
colnames(Compare_Median_Coeff_UnderTH)[5] <- c("(a) Median Under Th")



plot_dat <- pivot_longer(Compare_Median_Coeff_UnderTH, cols = c(4,5), names_to = "Type" ,values_to = "Unreported Wth")

plot_dat <- split( plot_dat , f = plot_dat$County_Name)


for (i in 1:length(plot_dat)) {
  p1 <- ggplot(plot_dat[[i]], aes(x=YEAR, y=`Unreported Wth`, group = Type ))+
    geom_line(aes(color=Type))+
    geom_point(aes(color=Type))+
    labs(title= plot_dat[[i]][1,3] ,
         x="Year", y = "Small Farmer Unreported Withdrawals (MG)")+
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
  #ggsave(paste0(WUDR_github,"/plots/Coefficient1/timeseries/Small Farmers/Median_Median_Comparison time series/", nam,".png"), plot = p1, width = 9.5, height = 6, units = "in")

}

sum(SF_Unreported_Mean_Irr_Coeff$Unreported_Coeff_Based)
sum(SF_Unreported_Median_Irr_Coeff$Unreported_Coeff_Based)
sum(SF_Unreported_MAX_UnderTh$Unreported_Irrigation_based)
sum(SF_Unreported_Median_UnderTh$Unreported_Irrigation_based)

save(SF_Unreported_Mean_Irr_Coeff,SF_Unreported_Median_Irr_Coeff,SF_Unreported_Median_UnderTh,SF_Unreported_MAX_UnderTh, file=paste0(WUDR_github,"/dat_load/All_times_series.RData"))

