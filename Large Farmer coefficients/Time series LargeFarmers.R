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
load(paste0(WUDR_github,"/dat_load/Large_DEQ_IRR_Coeff.RData"))
county.codes <- read.csv(paste0(WUDR_github,"/csv_files/county_codes_census.csv"))
# Merge coefficients
dat<- purrr::reduce(list(Large_DEQ_IRR[[1]][,c(1,2,11)],Large_DEQ_IRR[[2]][,c(2,11)],Large_DEQ_IRR[[3]][,c(2,11)],Large_DEQ_IRR[[4]][,c(2,11)]), dplyr::full_join, by = 'County_Code')


dat <- left_join(dat , county.codes , by = "County_Code")
dat <- dat[-1]
dat <- dat[,c(1,6,2:5)]
dat <- dat[order(dat$County_Name),]


colnames(dat)[3:6] <- seq(2002,2017,5)
# Specify the coefficient to be selected
Coef_type = mean

Coef_val<- apply(dat[,3:6], 1, Coef_type, na.rm=TRUE)

# 1.
###Mean for rest of the years and actual coefficients for census years

dat$Mean_Coeff <- Coef_val

colnames(dat)[3:6] <- seq(2002,2017,5)



####################################################################
# load(paste0(WUDR_github,"/dat_load/PRECIP_JUNE_AUGUST_ALL_YEARS.RData"))
#  
# 
# ppt_list_yearly <- lapply(ppt_list_yearly, function(x)
#                              mutate(x, Irrigation = round(762 - PPT,0)))
# 
# 
# for (i in 1:length(dat_list_yearly)) {
#   dat_list_yearly[[i]] <- left_join(dat_list_yearly[[i]],ppt_list_yearly[[i]][,c(2:4)], by = c("County_Name" = "name"))
# }

load(paste0(WUDR_github, "/dat_load/All_Years_DEQ_data_Total_nd_irr.Rdata")) 

Irri_DEQ_withdrawals <- Irri_deq_county %>% 
  filter(YEAR < 2018) %>% 
    filter(Facility_withdrawal_mg >0)

# rm(Irri_deq_county)

Irri_DEQ_withdrawals <- left_join(Irri_DEQ_withdrawals, dat[,c(1,2,7)], by = c("COUNTYFP" = "County_Code"))

Irri_DEQ_withdrawals <- Irri_DEQ_withdrawals[complete.cases(Irri_DEQ_withdrawals), ]

Irri_DEQ_withdrawals$Unreported_Coeff_Based <- round(Irri_DEQ_withdrawals$Facility_withdrawal_mg*Irri_DEQ_withdrawals$Mean_Coeff,2)

Irri_DEQ_withdrawals <- Irri_DEQ_withdrawals[,c(1,3,8,7,9,10)]

Timeseries_IRR_large_farmers <- Irri_DEQ_withdrawals

plot_dat <- split( Timeseries_IRR_large_farmers , f = Timeseries_IRR_large_farmers$County_Name)

for (i in 1:length(plot_dat)) {

   p1 <- ggplot(plot_dat[[i]], aes(x=YEAR, y=`Unreported_Coeff_Based`, group = 1))+
    geom_line()+
    geom_point()+
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

  nam = paste0( plot_dat[[i]][1,3],"_Avg_Coeff")
ggsave(paste0(WUDR_github,"/plots/Coefficient1/timeseries/Large Farmers/Irr Withdrawals (Coeff)/", nam,".png"), plot = p1, width = 9.5, height = 6, units = "in")

}

##############################################################
# Time series all counties
# Use Outer join 
# USE UN_th = max 
load(paste0(WUDR_github,"/dat_load/Large_DEQ_TOT_Coeff.RData"))


dat_UTH<- purrr::reduce(list(Large_DEQ_TOT[[1]][,c(1,2,3)],Large_DEQ_TOT[[2]][,c(2,3)],Large_DEQ_TOT[[3]][,c(2,3)],Large_DEQ_TOT[[4]][,c(2,3)]), dplyr::full_join, by = 'County_Code')


dat_UTH <- left_join(dat_UTH , county.codes , by = "County_Code")
dat_UTH <- dat_UTH[-1]
dat_UTH <- dat_UTH[,c(1,6,2:5)]
dat_UTH <- dat_UTH[order(dat_UTH$County_Name),]

# Specify the coefficient to be selected
Un_TH = max

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

ppt_list_yearly$All_Irrigation = round(ppt_list_yearly$Max_Tot_Area*(ppt_list_yearly$Irrigation/25.5)*27154/1000000,2)

ppt_list_yearly<- ppt_list_yearly[,c(1,5,2,3,4,6,7)]


ppt_list_yearly$Year <- as.numeric(ppt_list_yearly$Year)

#######################
load(paste0(WUDR_github,"/dat_load/Small_farmer_TOT_timeseries.RData"))

Irri_DEQ_withdrawals <- Irri_deq_county %>% 
  filter(YEAR < 2018) %>% 
  filter(Facility_withdrawal_mg >0)

rm(Irri_deq_county)

Large_farm_timeseries <- left_join(ppt_list_yearly, Small_farmer_TOT_timeseries[,c(1,2,7)], c("Year" ,"County_Code" ))



Large_farm_timeseries <- left_join(Large_farm_timeseries, Total_deq_county[,c(1,3,7)], c("Year" = "YEAR" ,"County_Code" = "COUNTYFP" ))
Large_farm_timeseries <- left_join(Large_farm_timeseries, Irri_DEQ_withdrawals[,c(1,3,7)], c("Year" = "YEAR" ,"County_Code" = "COUNTYFP" ))

colnames(Large_farm_timeseries)[c(8:10)] <- c("Small_Farm_Unreported", "TOT_DEQ_Withdrawals", "IRR_DEQ_withdrawals")

Large_farm_timeseries$IRR_DEQ_withdrawals[is.na(Large_farm_timeseries$IRR_DEQ_withdrawals)] <- 0


Large_farm_timeseries$Large_Farm_unreported <- round((Large_farm_timeseries$All_Irrigation - Large_farm_timeseries$IRR_DEQ_withdrawals-Large_farm_timeseries$Small_Farm_Unreported),5)

Large_farm_timeseries$C_tot_lg <- Large_farm_timeseries$Large_Farm_unreported / Large_farm_timeseries$TOT_DEQ_Withdrawals



write.csv(Large_farm_timeseries, paste0(WUDR_github,"/Output_Tables/", "Timeseries_Tot_large_farms.csv"), row.names= FALSE)


plot_dat <- split( Large_farm_timeseries , f = Large_farm_timeseries$name)

 for (i in 1:length(plot_dat)) {

  p1 <- ggplot(plot_dat[[i]], aes(x=Year, y=`Large_Farm_unreported`, group = 1))+
    geom_line()+
    geom_point()+
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

  nam = paste0( plot_dat[[i]][1,3],"_TOT")
  ggsave(paste0(WUDR_github,"/plots/Coefficient1/timeseries/Large Farmers/TOT withdrawals (Acerage)/", nam,".png"), plot = p1, width = 9.5, height = 6, units = "in")
}


# COMPARISON PLOT 

Compare_dat <- left_join(Timeseries_IRR_large_farmers[,c(1,2,3,6)],Large_farm_timeseries[,c(1:2,11)], c("COUNTYFP" = "County_Code", "YEAR"= "Year"))

colnames(Compare_dat)[5] <- c("Unreported Large Farm withdrawals \nbased on Irrigation deficit (Tot Irriigation)")

colnames(Compare_dat)[4] <- c("Unreported Large Farm withdrawals \nbased on Coefficient (Irr Withdrawals")

plot_dat <- pivot_longer(Compare_dat, cols = c(4,5), names_to = "Type" ,values_to = "Unreported Wth")

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
  ggsave(paste0(WUDR_github,"/plots/Coefficient1/timeseries/Large Farmers/Comparison_large_Farm/", nam,".png"), plot = p1, width = 9.5, height = 6, units = "in")

}




