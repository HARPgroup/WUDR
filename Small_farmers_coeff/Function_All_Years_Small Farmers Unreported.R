WUDR_github<-"F:/My Drive/WUDR/WUDR_Github/WUDR"
setwd(WUDR_github)

library(tidyverse)
library(tmap)
library(rgdal)
library(stringr)
library(Kendall)
library(plotly)
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

# Year = 2002
small_counties_coefficient <- function(Year){
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
  
  
  # Fn_of_Irri_withdrawals$C_irr2 <- round((((Fn_of_Irri_withdrawals$Facility_withdrawal_mg*100)/
  #                                                    (100-Fn_of_Irri_withdrawals$Pct.under.TH.of.total.Irr.area))-Fn_of_Irri_withdrawals$Facility_withdrawal_mg)/ Fn_of_Irri_withdrawals$Facility_withdrawal_mg,2)
  load(paste0(WUDR_github,"/dat_load/June_August_Effective precipitation.RData"))
  
  if (Year == 2002) {
    i = 1}else if (Year == 2007){
      i = 2} else if (Year == 2012){
        i = 3} else if (Year == 2017){
          i =4
        }
  
  
  Irr_deficit <- ppt_list_yearly[[i]][,c(2,3)]
  Irr_deficit$Irrigation <- round(762 - Irr_deficit$PPT,0) # considering 30 inches as crop water demand
  
  Fn_of_Irri_withdrawals <- left_join(Fn_of_Irri_withdrawals, Irr_deficit, by = c("County"= "name"))
  
  Fn_of_Irri_withdrawals$Vol_Unreported <- round(Fn_of_Irri_withdrawals$Irr.Area.Under.TH*(Fn_of_Irri_withdrawals$Irrigation/25.5)*27154/1000000,2)
  
  
  Fn_of_Irri_withdrawals$C_irr = round (Fn_of_Irri_withdrawals$Vol_Unreported / Fn_of_Irri_withdrawals$Facility_withdrawal_mg ,2)
  
  Fn_of_Irri_withdrawals$Method1_Unreported <- Fn_of_Irri_withdrawals$C_irr * Fn_of_Irri_withdrawals$Facility_withdrawal_mg
  

plotdat<-sp::merge(VA_counties,Fn_of_Irri_withdrawals, by.x = "COUNTYFP", by.y = "County_Code")

# Check if merge was correct
# sum(Fn_of_Irri_withdrawals$Irr.Area.above.TH, na.rm = TRUE)
# sum(plotdat@data$Irr.Area.above.TH, na.rm=TRUE)

p1<-tm_shape(plotdat)+
  tm_polygons("C_irr", title = "Unreported Coefficient",
              breaks = c(0,0.05,0.1,0.2,0.5,0.75,1),
              # n=5,style="jenks",
              textNA = "Missing DEQ Irrigation Withdrawals / No Census data",
              id="NAMELSAD")+
  tm_layout(main.title = paste0(Year," Small farmers unreported coefficient : DEQ Irrigtation withdrawals"),
            legend.outside = FALSE,
            legend.title.size = 1.2,
            legend.text.size = 0.8,
            legend.position = c("left","top"),
            legend.bg.alpha = 1)
p1


Fn_of_Irri_withdrawals <- Fn_of_Irri_withdrawals[,c(1,8,2,4,9,10:14)]
  tmap_save(p1, paste0(WUDR_github,"/plots/Coefficient1/",Year, "DEQ_Avaliable_counties_Coefficient1.png"),  width = 8.5, height = 5, units = 'in')
 write.csv(Fn_of_Irri_withdrawals, paste0(WUDR_github,"/Output_Tables/",Year, "DEQ_Avaliable_counties_Coefficient1.csv"), row.names = FALSE)
# 
return(Fn_of_Irri_withdrawals)
}

DEQ_2002 <- small_counties_coefficient(2002)
DEQ_2007 <- small_counties_coefficient(2007)
DEQ_2012 <- small_counties_coefficient(2012)
DEQ_2017 <- small_counties_coefficient(2017)

#2 Counties with NO DEQ Irrigation withdrawals
###############################################################################
# C_tot Coefficient as function of total withdrawals
# Use single crop water demand (30 inches)

small_counties_coefficient3 <- function(Year){
  QS_data(Year) 
  fn_Area_TH(10, Year)
  
  # rm(bin.char, binned_irrigated_area, binned_operations,nass_binned,nass.ag.data, nass.ops.data, opeartions_list) # Not required for further calculations.
  
  
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
Fn_of_TOTAL_withdrawals <- left_join(Non.Reported_Coefficient1,Total_DEQ_withdarwals[, c("COUNTYFP","Facility_withdrawal_mg")], by = c("County_Code"= "COUNTYFP"))

Fn_of_TOTAL_withdrawals <- Fn_of_TOTAL_withdrawals %>%
  drop_na(Facility_withdrawal_mg)


load(paste0(WUDR_github,"/dat_load/June_August_Effective precipitation.RData"))

if (Year == 2002) {
  i = 1}else if (Year == 2007){
    i = 2} else if (Year == 2012){
      i = 3} else if (Year == 2017){
        i =4
      }


Irr_deficit <- ppt_list_yearly[[i]][,c(2,3)]
Irr_deficit$Irrigation <- round(762 - Irr_deficit$PPT,0) # considering 30 inches as crop water demand

Fn_of_TOTAL_withdrawals <- left_join(Fn_of_TOTAL_withdrawals, Irr_deficit, by = c("County"= "name"))

Fn_of_TOTAL_withdrawals$Vol_Unreported <- round(Fn_of_TOTAL_withdrawals$Irr.Area.Under.TH*(Fn_of_TOTAL_withdrawals$Irrigation/25.5)*27154/1000000,2)

Fn_of_TOTAL_withdrawals$C_tot = round (Fn_of_TOTAL_withdrawals$Vol_Unreported / Fn_of_TOTAL_withdrawals$Facility_withdrawal_mg ,2)

Fn_of_TOTAL_withdrawals$Method1_Unreported <- Fn_of_TOTAL_withdrawals$C_tot * Fn_of_TOTAL_withdrawals$Facility_withdrawal_mg

# Fn_of_TOTAL_withdrawals <- Fn_of_TOTAL_withdrawals %>% 
#   filter(C_tot >= 0)
  
# Get the DEQ irrigation withdrawals missing counties

MISSING_withdrawals <- left_join(Non.Reported_Coefficient1,IRR_DEQ_withdarwals[, c("COUNTYFP","Facility_withdrawal_mg")], by = c("County_Code"= "COUNTYFP"))

MISSING_withdrawals <- MISSING_withdrawals[is.na(MISSING_withdrawals$Facility_withdrawal_mg),]

MISSING_withdrawals <- MISSING_withdrawals[,c(1,8)]

# Fn_of_TOTAL_withdrawals <- left_join(MISSING_withdrawals, Fn_of_TOTAL_withdrawals, by = c("County_Code"))


Fn_of_TOTAL_withdrawals_Irrigtaion_Missing <- Fn_of_TOTAL_withdrawals %>% 
  filter(County_Code %in% MISSING_withdrawals$County_Code)

plotdat<-sp::merge(VA_counties,Fn_of_TOTAL_withdrawals, by.x = "COUNTYFP", by.y = "County_Code")

# Check if merge was correct
sum(Fn_of_TOTAL_withdrawals$Irr.Area.above.TH, na.rm = TRUE)
sum(plotdat@data$Irr.Area.above.TH, na.rm=TRUE)

p2<-tm_shape(plotdat)+
  tm_polygons("C_tot", title = "Unreported Coefficient",
              breaks = c(0,0.05,0.1,0.2,0.5,0.75,Inf),
             # n=5,style="jenks",
             textNA = "Missing Census data / No Irrigtaion deficit",
              id="NAMELSAD")+
  tm_layout(main.title = paste0(Year," Small farmers unreported coefficient: Total DEQ Withdrawals"),
            legend.outside = FALSE,
            legend.title.size = 1.2,
            legend.text.size = 0.8,
            legend.position = c("left","top"),
            legend.bg.alpha = 1)
p2

 tmap_save(p2, paste0(WUDR_github,"/plots/Coefficient1/",Year, "Single demand_DEQ_Missing_counties_Coefficient1.png"),  width = 8, height = 5, units = 'in')

Fn_of_TOTAL_withdrawals <- Fn_of_TOTAL_withdrawals[,c(1,8,2,4,9,10:14)]
 write.csv(Fn_of_TOTAL_withdrawals, paste0(WUDR_github,"/Output_Tables/",Year, "Single demand_DEQ_Missing_counties_Coefficient1.csv"))
return(Fn_of_TOTAL_withdrawals)
}

Tdeq_coef_2002 <- small_counties_coefficient3(2002)
Tdeq_coef_2007 <- small_counties_coefficient3(2007)
Tdeq_coef_2012 <- small_counties_coefficient3(2012)
Tdeq_coef_2017 <- small_counties_coefficient3(2017)


###################################################################
# Perform Maan-Kendal test on Area under TH
# H0 (null hypothesis): There is no trend present in the data.

# HA (alternative hypothesis): A trend is present in the data. (This could be a positive or negative trend)
# p-value is less than 0.05, we will reject the null

# Using Area under TH

# dat<- purrr::reduce(list(Tdeq_coef_2002[,c(1,2,4)],Tdeq_coef_2007[,c(2,4)],Tdeq_coef_2012[,c(2,4)],Tdeq_coef_2017[,c(2,4)]), dplyr::full_join, by = 'County_Code')
# 
# dat <- dat[,-c(2)]
# colnames(dat)[2:5] <- c("c2002", "c2007", "c2012", "c2017")
# 
# # get counties with data in all 4 years
# 
# dat <- dat[complete.cases(dat),]
# dat <- dat[order(dat$County),]
# 
# 
# MK_dat <- dat %>% 
#   pivot_longer(cols = c(2:5))
# colnames(MK_dat)[2:3] <- c("Year", "Area_Under_Th")
# 
# 
# MK_dat <- split(MK_dat$Area_Under_Th, MK_dat$County)
# MK <- lapply(MK_dat, MannKendall)
# MK
# 
# 
# 
# 
# for (i in 1:nrow(dat)) {
#   dat$Tau_MK[i] <- MK[[i]][1] 
#   dat$p_MK[i] <- MK[[i]][2] 
# }
# 
# dat$p_MK <- round(as.numeric(dat$p_MK) ,3)
# dat$Tau_MK <- round(as.numeric(dat$Tau_MK) ,3)

# write.csv(dat, paste0(WUDR_github,"/Output_Tables/MK_test_Area_underTH.csv"), row.names = FALSE)

#############################################################################
# Coefficient 1 Mann Kendall test
# Merge DEQ coefficient with Total withdrawal coefficient # HYBRID data frame for Coefficient


Fn_merge_coeff <- function(x,y){
CF <- full_join(x[,c(2,9)],y[,c(1,2,9)], by = "County_Code")
CF <- CF %>% 
  mutate(Coeff1 = ifelse(is.na(CF$C_irr), CF$C_tot, CF$C_irr))
return(CF)
}

CF_2002 <- Fn_merge_coeff(DEQ_2002,Tdeq_coef_2002)
CF_2007 <- Fn_merge_coeff(DEQ_2007,Tdeq_coef_2007)
CF_2012 <- Fn_merge_coeff(DEQ_2012,Tdeq_coef_2012)
CF_2017 <- Fn_merge_coeff(DEQ_2017,Tdeq_coef_2017)

dat<- purrr::reduce(list(CF_2002[,c(1,3,5)],CF_2007[,c(1,5)],CF_2012[,c(1,5)],CF_2017[,c(1,5)]), dplyr::full_join, by = 'County_Code')


dat<- left_join(dat, county.codes, by = "County_Code")
dat <- dat[,-c(1,2)]
dat <- dat[,c(5,1:4)]
colnames(dat)[2:5] <- c("c2002", "c2007", "c2012", "c2017")

# get counties with data in all 4 years

dat <- dat[complete.cases(dat),]
dat <- dat[order(dat$County_Name),]

### Maan-Kendall Test
MK_dat <- dat %>% 
  pivot_longer(cols = c(2:5))
colnames(MK_dat)[2:3] <- c("Year", "Coeff1")


MK_dat <- split(MK_dat$Coeff1, MK_dat$County_Name)
MK <- lapply(MK_dat, MannKendall)
MK


for (i in 1:nrow(dat)) {
  dat$Tau_MK[i] <- MK[[i]][1] 
  dat$p_MK[i] <- MK[[i]][2] 
}

dat$p_MK <- round(as.numeric(dat$p_MK) ,3)
dat$Tau_MK <- round(as.numeric(dat$Tau_MK) ,3)

# write.csv(dat, paste0(WUDR_github,"/Output_Tables/MK_test_Coeff.csv"), row.names = FALSE)


#######################################################
# Create summary table # Coeff1 = all coefficients , C_irr = Irrigtaion Coefficient, C_tot for Total withdrawals 
#  REPLACE THIS IN FOR LOOP

Coeff_Summary <- list()
Coeff_list <- list(CF_2002,CF_2007,CF_2012,CF_2017)


for (i in 1:4) {
     Coeff_Summary[[i]] <- Coeff_list[[i]] %>% 
  summarise(Min = min(Coeff1, na.rm = TRUE),
            Median = median(Coeff1, na.rm = TRUE),
            Mean = mean(Coeff1, na.rm = TRUE),
            Max = max(Coeff1, na.rm = TRUE))
}


names(Coeff_Summary) <- c(seq(2002,2017,5))
Coeff_Summary <- bind_rows(Coeff_Summary, .id = "Year")



# Check the name carefully
 # write.csv(Coeff_Summary, paste0(WUDR_github,"/Output_Tables/Summary of (DEQ MISSING) Total Coefficients.csv"), row.names = FALSE) # use C_tot in loop
 # write.csv(Coeff_Summary, paste0(WUDR_github,"/Output_Tables/Summary of (DEQ Avaliable) Irrigtaion Coefficients.csv"), row.names = FALSE) # use C_irr in loop
 # write.csv(Coeff_Summary, paste0(WUDR_github,"/Output_Tables/Summary ALL Coefficients (HYBRID) Coefficients.csv"), row.names = FALSE) # use C_irr in loop
#########################################################################

hybrid_coeff_plot <- function(Year){

if (Year == 2002) {
  i = CF_2002}else if (Year == 2007){
    i = CF_2007} else if (Year == 2012){
      i = CF_2012} else if (Year == 2017){
        i =CF_2017
      }
  
plotdat<-sp::merge(VA_counties,i[,c(1,5)], by.x = "COUNTYFP", by.y = "County_Code")

# Check if merge was correct
sum(i$Coeff1, na.rm = TRUE)
sum(plotdat@data$Coeff1, na.rm=TRUE)

p2<-tm_shape(plotdat)+
  tm_polygons("Coeff1", title = "Unreported Coefficient",
              breaks = c(0,0.05,0.1,0.2,0.5,0.75,Inf),
             # n=5,style="jenks",
             textNA = "Missing Census data / No DEQ data",
              id="NAMELSAD")+
  tm_text("NAME", size = 0.3)+
  tm_layout(main.title = paste0(Year," Small farmers unreported coefficient (Hybrid)"),
            legend.outside = FALSE,
            legend.title.size = 1.2,
            legend.text.size = 0.8,
            legend.position = c("left","top"),
            legend.bg.alpha = 1)


tmap_save(p2, paste0(WUDR_github,"/plots/Coefficient1/",Year, "Unreported Coefficients Hybrid.png"),  width = 8, height = 5, units = 'in')
}
hybrid_coeff_plot(2002)
hybrid_coeff_plot(2007)
hybrid_coeff_plot(2012)
hybrid_coeff_plot(2017)

# Merge unreported withdrawals in both datasets # HYBRID data frame for Unreported withdrawals

Fn_merge_wth <- function(x,y){
CF <- full_join(x[,c(2,10)],y[,c(1,2,10)], by = "County_Code")
CF <- CF %>% 
    mutate(Unreported = ifelse(is.na(CF$Method1_Unreported.x), CF$Method1_Unreported.y, CF$Method1_Unreported.x))
return(CF)
}

WH_2002 <- Fn_merge_wth(DEQ_2002,Tdeq_coef_2002)
WH_2007 <- Fn_merge_wth(DEQ_2007,Tdeq_coef_2007)
WH_2012 <- Fn_merge_wth(DEQ_2012,Tdeq_coef_2012)
WH_2017 <- Fn_merge_wth(DEQ_2017,Tdeq_coef_2017)

#######################################################
# Create summary table for unreported withdrawals # Unreported = all unreported withdrawals using both methods (hybrid) ,
# Irrigation withdrawals = Method1_Unreported.x, Total withdrawals = Method1_Unreported.y, 
#  REPLACE THIS IN FOR LOOP
WTH_Summary <- list()
WH_list <- list(WH_2002,WH_2007,WH_2012,WH_2017)


for (i in 1:4) {
     WTH_Summary[[i]] <- WH_list[[i]] %>% 
  summarise(Min = min(Method1_Unreported.y, na.rm = TRUE),
            Median = median(Method1_Unreported.y, na.rm = TRUE),
            Mean = mean(Method1_Unreported.y, na.rm = TRUE),
            Max = max(Method1_Unreported.y, na.rm = TRUE))
}
names(WTH_Summary) <- c(seq(2002,2017,5))
WTH_Summary <- bind_rows(WTH_Summary, .id = "Year")


 # write.csv(WTH_Summary, paste0(WUDR_github,"/Output_Tables/Summary ALL (HYBRID) Withdrawals.csv"), row.names = FALSE) #Unreported in the loop

 # write.csv(WTH_Summary, paste0(WUDR_github,"/Output_Tables/Summary of (DEQ Avaliable) Irrigtaion Withdrawals.csv"), row.names = FALSE) #Method1_Unreported.x in the loop
 # write.csv(WTH_Summary, paste0(WUDR_github,"/Output_Tables/Summary of (DEQ MISSING) Total Withdrawals.csv"), row.names = FALSE)  #Method1_Unreported.y in the loop


#####################################################################
# Use Fn_merge_unreported_irr when: You need to compare the unreported withdrawals for C_Tot and C_irr. common counties in both methods
# Here counties which have both irrigation n total deq withdrawals are selected. 

Fn_merge_unreported_irr<- function(x,y){
  CF <- left_join(x[, c(1,2,10)], y[,c(2,10)], by = "County_Code")
  colnames(CF)[3:4] <- c("Unrep_DEQ_Irrigation", "Unrep_Rainfall_deficit")
  return(CF)
}

Common_2002 <- Fn_merge_unreported_irr(DEQ_2002,Tdeq_coef_2002)
Common_2007 <- Fn_merge_unreported_irr(DEQ_2007,Tdeq_coef_2007)
Common_2012 <- Fn_merge_unreported_irr(DEQ_2012,Tdeq_coef_2012)
Common_2017 <- Fn_merge_unreported_irr(DEQ_2017,Tdeq_coef_2017)

Plot_scatter <- function(Year){

if (Year == 2002) {
  i = Common_2002}else if (Year == 2007){
    i = Common_2007} else if (Year == 2012){
      i = Common_2012} else if (Year == 2017){
        i =Common_2017
      }


p1 <- ggplot(i, aes(x=Unrep_DEQ_Irrigation, y=Unrep_Rainfall_deficit))+
  geom_point()+
  labs(title= Year,
       x="Unreported DEQ Irrigtaion Withdrawals", y = "Unreported Deficit Irrigtaion") +
  scale_y_continuous(limits = c(0, 200),  breaks = seq(0, 200, by = 50))+
  scale_x_continuous(limits = c(0, 150),  breaks = seq(0, 150, by = 50))

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
return(p1)
}
p1 <- Plot_scatter(2002)
p2<- Plot_scatter(2007)
p3<- Plot_scatter(2012)
p4<- Plot_scatter(2017)

library("gridExtra")
p <- grid.arrange(p1, p2, p3,p4, ncol = 2, nrow = 2)
p
# ggsave(paste0(WUDR_github,"/plots/Coefficient1/scatter.png"), plot = p, width = 9.5, height = 6, units = "in")

#####################################################################
# Use Fn_merge_Coeff when: You need to compare the coefficient  C_Tot and C_irr. common counties in both methods

# Fn_merge_Coeff<- function(x,y){
#   CF <- left_join(x[, c(1,2,9)], y[,c(2,9)], by = "County_Code")
#   colnames(CF)[3:4] <- c("C_Irr", "C_tot")
#   return(CF)
# }
# 
# Common_Coeff_2002 <- Fn_merge_Coeff(DEQ_2002,Tdeq_coef_2002)
# Common_Coeff_2007 <- Fn_merge_Coeff(DEQ_2007,Tdeq_coef_2007)
# Common_Coeff_2012 <- Fn_merge_Coeff(DEQ_2012,Tdeq_coef_2012)
# Common_Coeff_2017 <- Fn_merge_Coeff(DEQ_2017,Tdeq_coef_2017)
# 
# Plot_scatter <- function(Year){
# 
# if (Year == 2002) {
#   i = Common_Coeff_2002}else if (Year == 2007){
#     i = Common_Coeff_2007} else if (Year == 2012){
#       i = Common_Coeff_2012} else if (Year == 2017){
#         i =Common_Coeff_2017
#       }
# 
# 
# p1 <- ggplot(i, aes(x=C_Irr, y=C_tot))+
#   geom_point()+
#   labs(title= Year,
#        x="C_Irr", y = "C_Tot") +
#   scale_y_continuous(limits = c(0, 3),  breaks = seq(0, 3, by = 1))+
#   scale_x_continuous(limits = c(0, 3),  breaks = seq(0, 3, by = 3))
# 
# p1<- p1 + theme_bw()
# p1 <- p1+theme(axis.text.x=element_text(angle = 0, hjust = 0),
#                legend.position="None", 
#                legend.title=element_blank(),
#                legend.box = "horizontal", 
#                legend.background = element_rect(fill="white",
#                                                 size=0.5, linetype="solid", 
#                                                 colour ="white"),
#                legend.text=element_text(size=12),
#                axis.text=element_text(size=12, colour="black"),
#                axis.title=element_text(size=14, colour="black"),
#                axis.line = element_line(colour = "black", 
#                                         size = 0.5, linetype = "solid"),
#                axis.ticks = element_line(colour="black"),
#                panel.grid.major=element_line(colour = "light grey"), 
#                panel.grid.minor=element_blank())
# p1
# return(p1)
# }
# p1 <- Plot_scatter(2002)
# p2<- Plot_scatter(2007)
# p3<- Plot_scatter(2012)
# p4<- Plot_scatter(2017)
# 
# library("gridExtra")
# p2 <- grid.arrange(p1, p2, p3,p4, ncol = 2, nrow = 2)
# p2
