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
county.codes <- read_csv(paste0(WUDR_github, "/csv_files/county_codes_census.csv")) 
####################################################################
load(paste0(WUDR_github,"/dat_load/SF_Coeff.RData"))

# Use function to create the time series based on coefficients parm = mean or median 

############################################################################################################
#############################################################################################################
### AFTER ANALYIS IT WAS FOUND THAT MEDIAN PROVIED MOST ACCURATE RESULTS, HENCE MEAN VALUES ARE NO LONGER CONSIDERED FOR FURTUR ANALYSIS##
### REFER TO ARCHIVE Time series Small Farmers_Median_Max_Comparisons  CODE FOR DETAILS ######################################
##############################################################################################################################

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

SF_Unreported_Median_Irr_Coeff <- TS_SF_Coeff_fn(median)

# write.csv(SF_Unreported_Median_Irr_Coeff, paste0(WUDR_github,"/Output_Tables/", "SF_Timeseries_Median_Coeff.csv"), row.names= FALSE)

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
# ggsave(paste0(WUDR_github,"/plots/Coefficient1/timeseries/Small Farmers/Median Coeff/", nam,".png"), plot = p1, width = 9.5, height = 6, units = "in")

 }

##############################################################
# Time series all counties WITH MISSING DEQ IRRIGTAION DATA.
# Use median Under TH acreage and Effective precip approach  


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
    mutate(x, Irrigation = round(508 - PPT,0)))
  
  SF_Unreported_parm_UnderTh <- bind_rows(SF_Unreported_parm_UnderTh, .id = "Year")
  
  SF_Unreported_parm_UnderTh <- left_join(SF_Unreported_parm_UnderTh, dat_UTH[,c(1,2,7)], c("name"= "County_Name"))
  
  SF_Unreported_parm_UnderTh <- SF_Unreported_parm_UnderTh[complete.cases(SF_Unreported_parm_UnderTh), ]
  
  SF_Unreported_parm_UnderTh$Unreported_Irrigation_based = round(SF_Unreported_parm_UnderTh$parm_UnderTh*(SF_Unreported_parm_UnderTh$Irrigation/25.5)*27154/1000000,2)
  
  SF_Unreported_parm_UnderTh<- SF_Unreported_parm_UnderTh[,c(1,5,2,3,4,6,7)]
  
  #######################
  SF_Unreported_parm_UnderTh$Year <- as.numeric(SF_Unreported_parm_UnderTh$Year)
  
  return(SF_Unreported_parm_UnderTh)
}


SF_Unreported_Median_UnderTh <- TS_SF_Under_TH_fn(median)

# write.csv(SF_Unreported_Median_UnderTh, paste0(WUDR_github,"/Output_Tables/", "SF_Timeseries_Median_UnderTh_Deficit_Irr.csv"), row.names= FALSE)


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
 # ggsave(paste0(WUDR_github,"/plots/Coefficient1/timeseries/Small Farmers/Median UnderTH/", nam,".png"), plot = p1, width = 9.5, height = 6, units = "in")
 }


# save(SF_Unreported_Median_Irr_Coeff,SF_Unreported_Median_UnderTh, file=paste0(WUDR_github,"/dat_load/SF_times_series_Median.RData"))

#################################################################
# Plot unreported withdrawals based on both methods in counties with Irrigation data
# In time series check the unreported volumes 


load(paste0(WUDR_github,"/dat_load/SF_times_series_Median.RData")) 
load(paste0(WUDR_github, "/dat_load/All_Years_DEQ_data_Total_nd_irr.Rdata")) 

Common_counties <- left_join(SF_Unreported_Median_Irr_Coeff[,c(1,2,3,6)],SF_Unreported_Median_UnderTh[,c(1,2,7)], by= c("COUNTYFP" = "County_Code", "YEAR"= "Year"))

p1 <- ggplot(Common_counties, aes(x=log10(Unreported_Coeff_Based), y=log10(Unreported_Irrigation_based)))+
  geom_point()+
  geom_smooth(method=lm)+
  labs(title = "Unreported withdrawals 2002-2017",
    x="Unreported median Irrigation Coefficient (log)", y = "Unreported median Under TH Acreage (log)")+
  scale_y_continuous(limits = c(-2, 4),  breaks = seq(-2, 4, by = 1))+
  scale_x_continuous(limits = c(-2, 4),  breaks = seq(-2, 4, by = 1))

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

ggsave(paste0(WUDR_github,"/plots/Coefficient1/scatter.png"), plot = p1, width = 9.5, height = 6, units = "in")


###################################################################
# Yearly summary for counties with DEQ data 
# For comparison between methods
load(paste0(WUDR_github,"/dat_load/SF_times_series_Median.RData"))

DEQ_Irr<- Irri_deq_county %>% 
  filter(YEAR <2018) %>% 
  group_by(YEAR) %>% 
  summarise(`DEQ Irrigation` = sum(Facility_withdrawal_mg))

Yearly_Summary <- Common_counties %>% 
  group_by(YEAR) %>% 
  summarise(`Unreported Coeff Based` = sum(Unreported_Coeff_Based),
            `Unreported Deficit Irrigation` = sum(Unreported_Irrigation_based[which(Unreported_Irrigation_based>0)]))

plot_dat <- cbind.data.frame(Yearly_Summary,DEQ_Irr)
plot_dat <- plot_dat[,-c(4)]
plot_dat <- pivot_longer(plot_dat, cols = c(2:4), names_to = "Type" ,values_to = "Unreported_Withdrawals")

p1 <- ggplot(plot_dat, aes(x=YEAR, y=Unreported_Withdrawals, group = Type ))+
  geom_line(aes(color=Type))+
  geom_point(aes(color=Type))+
  labs(title= "Unreported withdrawals common counties 2002-2017",
       x="Year", y = "Unreported Withdrawals (MG)")+
  scale_x_continuous(limits = c(2002, 2017),  breaks = seq(2002, 2017, by = 2))+
  scale_y_continuous(limits = c(0, 12000),  breaks = seq(0, 12000, by = 1000))

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

ggsave(paste0(WUDR_github,"/plots/Coefficient1/Chnaged_SF_Comparison_methods.png"), plot = p1, width = 9.5, height = 6, units = "in")

####################################################################
####################################################################
# Yearly summary for all counties in both methods 

Yearly_Summary_Coeff <- SF_Unreported_Median_Irr_Coeff %>% 
  group_by(YEAR) %>% 
  summarise(`Unreported Coeff Based` = sum(Unreported_Coeff_Based))


Yearly_Summary_Under_Th <- SF_Unreported_Median_UnderTh %>% 
  group_by(Year) %>% 
  summarise(`Unreported Deficit Irrigation` = sum(Unreported_Irrigation_based[which(Unreported_Irrigation_based>0)]))

# plot_dat <- cbind.data.frame(Yearly_Summary_Coeff,DEQ_Irr,Yearly_Summary_Under_Th)
# plot_dat <- plot_dat[,-c(3,5)]
# plot_dat <- pivot_longer(plot_dat, cols = c(2:4), names_to = "Type" ,values_to = "Unreported_Withdrawals")


plot_dat <- left_join(DEQ_Irr,Yearly_Summary_Under_Th, by = c("YEAR"= "Year"))
plot_dat <- pivot_longer(plot_dat, cols = c(2,3), names_to = "Type" ,values_to = "Unreported_Withdrawals")

p1 <- ggplot(plot_dat, aes(x=YEAR, y=Unreported_Withdrawals, group = Type ))+
  geom_line(aes(color=Type))+
  geom_point(aes(color=Type))+
  labs(title= "Unreported withdrawals all counties 2002-2017",
       x="Year", y = "Unreported Withdrawals (MG)")+
  scale_x_continuous(limits = c(2002, 2017),  breaks = seq(2002, 2017, by = 2))+
  scale_y_continuous(limits = c(0, 12000),  breaks = seq(0, 12000, by = 1000))

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

ggsave(paste0(WUDR_github,"/plots/Coefficient1/updatedTotal_Unreproted_withdarwals.png"), plot = p1, width = 9.5, height = 6, units = "in")
