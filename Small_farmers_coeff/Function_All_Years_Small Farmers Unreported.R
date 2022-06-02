WUDR_github<-"F:/My Drive/WUDR/WUDR_Github/WUDR"
setwd(WUDR_github)

library(tidyverse) 
library(tmap)
library(rgdal)
library(stringr)
library(Kendall)
library(plotly)
library("gridExtra")
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

  Year = 2002
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
  

  Fn_of_Irri_withdrawals$C_irr = Fn_of_Irri_withdrawals$Vol_Unreported / Fn_of_Irri_withdrawals$Facility_withdrawal_mg 
  
  Fn_of_Irri_withdrawals$Method1_Unreported <- Fn_of_Irri_withdrawals$C_irr * Fn_of_Irri_withdrawals$Facility_withdrawal_mg
  

plotdat<-sp::merge(VA_counties,Fn_of_Irri_withdrawals, by.x = "COUNTYFP", by.y = "County_Code")

# Check if merge was correct
# sum(Fn_of_Irri_withdrawals$Irr.Area.above.TH, na.rm = TRUE)
# sum(plotdat@data$Irr.Area.above.TH, na.rm=TRUE)

p1<-tm_shape(plotdat)+
  tm_polygons("C_irr", title = "Unreported Coefficient",
              breaks = c(0,0.05,0.1,0.2,0.5,0.75,Inf),
              # n=5,style="jenks",
              textNA = "Missing DEQ Irrigation Withdrawals / No Census data",
              id="NAMELSAD")+
   # tm_text("NAME", size = 0.3)+
  tm_layout(main.title = paste0(Year," Small farm unreported\n(as a percentage of VDEQ Irrigation withdrawal)"),
            legend.outside = FALSE,
            legend.title.size = 1.2,
            legend.text.size = 0.8,
            legend.position = c("left","top"),
            legend.bg.alpha = 1)
p1


Fn_of_Irri_withdrawals <- Fn_of_Irri_withdrawals[,c(1,8,2,4,9,10:14)]
  # tmap_save(p1, paste0(WUDR_github,"/plots/Coefficient1/",Year, "DEQ_Avaliable_counties_Coefficient1.png"),  width = 8.5, height = 5, units = 'in')
 # write.csv(Fn_of_Irri_withdrawals, paste0(WUDR_github,"/Output_Tables/",Year, "DEQ_Avaliable_counties_Coefficient1.csv"), row.names = FALSE)
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
Unreported_Under_TH <- left_join(Non.Reported_Coefficient1,Total_DEQ_withdarwals[, c("COUNTYFP","Facility_withdrawal_mg")], by = c("County_Code"= "COUNTYFP"))

Unreported_Under_TH <- Unreported_Under_TH %>%
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

Unreported_Under_TH <- left_join(Unreported_Under_TH, Irr_deficit, by = c("County"= "name"))

Unreported_Under_TH$Vol_Unreported <- round(Unreported_Under_TH$Irr.Area.Under.TH*(Unreported_Under_TH$Irrigation/25.5)*27154/1000000,2)

Unreported_Under_TH$C_tot = Unreported_Under_TH$Vol_Unreported / Unreported_Under_TH$Facility_withdrawal_mg 

Unreported_Under_TH$Method1_Unreported <- Unreported_Under_TH$C_tot * Unreported_Under_TH$Facility_withdrawal_mg

# Unreported_Under_TH <- Unreported_Under_TH %>% 
#   filter(C_tot >= 0)
  
# Get the DEQ irrigation withdrawals missing counties

MISSING_withdrawals <- left_join(Non.Reported_Coefficient1,IRR_DEQ_withdarwals[, c("COUNTYFP","Facility_withdrawal_mg")], by = c("County_Code"= "COUNTYFP"))

MISSING_withdrawals <- MISSING_withdrawals[is.na(MISSING_withdrawals$Facility_withdrawal_mg),]

MISSING_withdrawals <- MISSING_withdrawals[,c(1,8)]

# Unreported_Under_TH <- left_join(MISSING_withdrawals, Unreported_Under_TH, by = c("County_Code"))


Unreported_Under_TH_Irrigation_Missing <- Unreported_Under_TH %>% 
  filter(County_Code %in% MISSING_withdrawals$County_Code)

plotdat<-sp::merge(VA_counties,Unreported_Under_TH, by.x = "COUNTYFP", by.y = "County_Code")

# Check if merge was correct
sum(Unreported_Under_TH$Irr.Area.above.TH, na.rm = TRUE)
sum(plotdat@data$Irr.Area.above.TH, na.rm=TRUE)

p2<-tm_shape(plotdat)+
  tm_polygons("C_tot", title = "Unreported Coefficient",
              breaks = c(0,0.05,0.1,0.2,0.5,0.75,Inf),
             # n=5,style="jenks",
             textNA = "Missing Census data / No Irrigation deficit",
              id="NAMELSAD")+
  tm_layout(main.title = paste0(Year," Small farm unreported\n(as a percentage of VDEQ total nonenergy withdrawal)"),
            legend.outside = FALSE,
            legend.title.size = 1.2,
            legend.text.size = 0.8,
            legend.position = c("left","top"),
            legend.bg.alpha = 1)
p2

 # tmap_save(p2, paste0(WUDR_github,"/plots/Coefficient1/",Year, "Single demand_DEQ_Missing_counties_Coefficient1.png"),  width = 8, height = 5, units = 'in')

Unreported_Under_TH <- Unreported_Under_TH[,c(1,8,2,4,9,10:14)]
 # write.csv(Unreported_Under_TH, paste0(WUDR_github,"/Output_Tables/",Year, "Single demand_DEQ_Missing_counties_Coefficient1.csv"))
return(Unreported_Under_TH)
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


#############################################################################
# Coefficient 1 Mann Kendall test




Mk_function <- function(dat1,dat2,dat3,dat4){


dat<- purrr::reduce(list(dat1[,c(1,2,9)],dat2[,c(2,9)],dat3[,c(2,9)],dat4[,c(2,9)]), dplyr::inner_join, by = 'County_Code')

dat <- dat[order(dat$County),]
dat <- dat[,-c(2)]

colnames(dat)[2:5] <- c("c2002", "c2007", "c2012", "c2017")

MK_dat <- dat %>% 
  pivot_longer(cols = c(2:5))
colnames(MK_dat)[2:3] <- c("Year", "Coeff1")


MK_dat <- split(MK_dat$Coeff1, MK_dat$County)
MK <- lapply(MK_dat, MannKendall)

for (i in 1:nrow(dat)) {
  dat$Tau_MK[i] <- MK[[i]][1] 
  dat$p_MK[i] <- MK[[i]][2] 
}

dat$p_MK <- round(as.numeric(dat$p_MK) ,3)
dat$Tau_MK <- round(as.numeric(dat$Tau_MK) ,3)

dat <- dat[order(dat$Tau_MK,decreasing = TRUE),]

return(dat)
}

# Counties with DEQ data in all 4 years
MK_Irr_counties <- Mk_function(DEQ_2002,DEQ_2007,DEQ_2012,DEQ_2017)


# All counties with data in all 4 years
MK_Tot_counties <- Mk_function(Tdeq_coef_2002,Tdeq_coef_2007,Tdeq_coef_2012,Tdeq_coef_2017)


Common_counties_TAU <- left_join(MK_Irr_counties[,c(1,6)], MK_Tot_counties[,c(1,6)], by = "County" )

colnames(Common_counties_TAU)[2:3] <- c("TAU_Irri" ,"TAU_Total")

# write.csv(MK_Irr_counties, paste0(WUDR_github,"/Output_Tables/MK_test_DEQ_Irr.csv"), row.names = FALSE)
# write.csv(MK_Tot_counties, paste0(WUDR_github,"/Output_Tables/MK_test_DEQ_Total.csv"), row.names = FALSE)
# write.csv(Common_counties_TAU, paste0(WUDR_github,"/Output_Tables/MK_test_Common_counties.csv"), row.names = FALSE)

#######################################################
# Create summary table for final coefficient
# Coeff1 = all coefficients , C_irr = Irrigation Coefficient, C_tot for Total withdrawals 
#  REPLACE THIS IN FOR LOOP
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

Coeff_Summary <- list()
Coeff_list <- list(CF_2002,CF_2007,CF_2012,CF_2017)


for (i in 1:4) {
     Coeff_Summary[[i]] <- Coeff_list[[i]] %>% 
  summarise(Min = min(C_irr, na.rm = TRUE),
            Median = median(C_irr, na.rm = TRUE),
            Mean = mean(C_irr, na.rm = TRUE),
            Max = max(C_irr, na.rm = TRUE))
}


names(Coeff_Summary) <- c(seq(2002,2017,5))
Coeff_Summary <- bind_rows(Coeff_Summary, .id = "Year")



# Check the name carefully
 # write.csv(Coeff_Summary, paste0(WUDR_github,"/Output_Tables/Summary of (DEQ MISSING) Total Coefficients.csv"), row.names = FALSE) # use C_tot in loop
 # write.csv(Coeff_Summary, paste0(WUDR_github,"/Output_Tables/Summary of (DEQ Avaliable) Irrigation Coefficients.csv"), row.names = FALSE) # use C_irr in loop
 # write.csv(Coeff_Summary, paste0(WUDR_github,"/Output_Tables/Summary ALL Coefficients (HYBRID) Coefficients.csv"), row.names = FALSE) # use C_irr in loop
#########################################################################

#################################################################
# Create summary table for coefficients for each county
# 

fn_coeff_county_summary <- function(dat1,dat2,dat3,dat4){
  dat<- purrr::reduce(list(dat1[,c(1,2,9)],dat2[,c(2,9)],dat3[,c(2,9)],dat4[,c(2,9)]), dplyr::inner_join, by = 'County_Code')
  
  dat <- dat[order(dat$County),]
  dat <- dat[,-c(2)]
  
  colnames(dat)[2:5] <- c("c2002", "c2007", "c2012", "c2017")
  
  MK_dat <- dat %>% 
    pivot_longer(cols = c(2:5))
  colnames(MK_dat)[2:3] <- c("Year", "Coeff1")
  
  
  Coeff_Summary_County<- MK_dat%>% 
    group_by(County) %>% 
    summarise(Min = min(Coeff1, na.rm = TRUE),
              Median = median(Coeff1, na.rm = TRUE),
              Mean = mean(Coeff1, na.rm = TRUE),
              Max = max(Coeff1, na.rm = TRUE)) %>% 
    mutate(Recent = MK_dat$Coeff1[MK_dat$Year== "c2017"])
  return(Coeff_Summary_County)
}

Coeff_summary_Irr_counties <- fn_coeff_county_summary(DEQ_2002,DEQ_2007,DEQ_2012,DEQ_2017)


# All counties with data in all 4 years
Coeff_summary_All_counties <- fn_coeff_county_summary(Tdeq_coef_2002,Tdeq_coef_2007,Tdeq_coef_2012,Tdeq_coef_2017)

write.csv(Coeff_summary_Irr_counties, paste0(WUDR_github,"/Output_Tables/DEQ_IRR_Summary.csv"), row.names = FALSE) # use C_tot in loop
write.csv(Coeff_summary_All_counties, paste0(WUDR_github,"/Output_Tables/DEQ_Total_summary.csv"), row.names = FALSE) # use C_irr in loop


##########################################################################

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
            Max = max(Method1_Unreported.y, na.rm = TRUE),
            Recent = )
}
names(WTH_Summary) <- c(seq(2002,2017,5))
WTH_Summary <- bind_rows(WTH_Summary, .id = "Year")


 # write.csv(WTH_Summary, paste0(WUDR_github,"/Output_Tables/Summary ALL (HYBRID) Withdrawals.csv"), row.names = FALSE) #Unreported in the loop

 # write.csv(WTH_Summary, paste0(WUDR_github,"/Output_Tables/Summary of (DEQ Ava liable) Irrigation Withdrawals.csv"), row.names = FALSE) #Method1_Unreported.x in the loop
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
  geom_smooth(method=lm)+
  labs(title= Year,
       x="Unreported DEQ Irrigation Withdrawals", y = "Unreported Deficit Irrigation") +
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
# Unreported based on DEQ irrigation withdrawals and DEQ Deficit Irrigation 
Plot_scatter <- function(Year){
if (Year == 2002) {
  i = DEQ_2002}else if (Year == 2007){
    i = DEQ_2007} else if (Year == 2012){
      i = DEQ_2012} else if (Year == 2017){
        i =DEQ_2017
      }

i$Pct_Under_TH <- (100*i$Irr.Area.Under.TH/i$Total.Irri.Area)
i$Unreported_DEQ_irrigtaion <- round(((i$Facility_withdrawal_mg*100)/
                                                   (100-i$Pct_Under_TH))-i$Facility_withdrawal_mg ,2)

p1 <- ggplot(i, aes(x=log(Unreported_DEQ_irrigtaion), y=log(Method1_Unreported)))+
  geom_point()+
  geom_smooth(method=lm)+
  labs(title= Year,
       x="Unreported based on \n DEQ Irrigation Withdrawals", y = "Unreported based \n on Deficit Irrigation") +
  scale_y_continuous(limits = c(1, 5),  breaks = seq(1, 5, by = 1))+
  scale_x_continuous(limits = c(0, 5),  breaks = seq(0, 5, by = 1))

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


p <- grid.arrange(p1, p2, p3,p4, ncol = 2, nrow = 2)
p

 # ggsave(paste0(WUDR_github,"/plots/Coefficient1/log_scatterupdated.png"), plot = p, width = 12, height = 8, units = "in")

#########################################################################################
# EXAMPLE TIMESERIES BOTH METHODS ########################################
####################################################################################



fn_coeff_dat <- function(dat1,dat2,dat3,dat4){
dat<- purrr::reduce(list(dat1[,c(1,2,9)],dat2[,c(2,9)],dat3[,c(2,9)],dat4[,c(2,9)]), dplyr::inner_join, by = 'County_Code')

dat <- dat[order(dat$County),]
dat <- dat[,-c(2)]

colnames(dat)[2:5] <- c("c2002", "c2007", "c2012", "c2017")
return(dat)

}

Irr_coeff <- fn_coeff_dat(DEQ_2002,DEQ_2007,DEQ_2012,DEQ_2017)
Irr_coeff$Avg <- rowMeans(Irr_coeff[,c(2:5)])
Irr_coeff <- left_join(Irr_coeff, county.codes, by = c("County"= "County_Name"))

load(paste0(WUDR_github, "/dat_load/All_Years_DEQ_data_total_nd_irr.Rdata"))

County_code <- 165 # Rock
County_code <- 33  # Caroline
County_code <- 131 # Northhampton 
 
Irr_Unreported_county <- Irri_deq_county %>% 
  filter(COUNTYFP == County_code) %>% 
  mutate(Unreported_Withdrawal = Facility_withdrawal_mg * Irr_coeff$Avg[Irr_coeff$County_Code==County_code])

#####################################

Total_coeff <- fn_coeff_dat(Tdeq_coef_2002,Tdeq_coef_2007,Tdeq_coef_2012,Tdeq_coef_2017)
Total_coeff$Avg <- rowMeans(Total_coeff[,c(2:5)])
Total_coeff <- left_join(Total_coeff, county.codes, by = c("County"= "County_Name"))

Tot_Unreported_county <- Total_deq_county %>% 
  filter(COUNTYFP == County_code) %>% 
  mutate(Unreported_Withdrawal = Facility_withdrawal_mg * Total_coeff$Avg[Total_coeff$County_Code==County_code])

County_NAM <- Total_coeff$County[Total_coeff$County_Code==County_code]
plot_data <- left_join(Tot_Unreported_county[,c(3,8)],Irr_Unreported_county[,c(3,8)], by = "YEAR")

colnames(plot_data)[2:3] <- c("C Total Unreported", "C Irri Unreported")

plot_data <- pivot_longer(plot_data, cols = c("C Total Unreported", "C Irri Unreported"))                         

p1 <- ggplot(plot_data, aes(x=YEAR, y=value, group = name))+
  geom_line(aes(color=name))+
  geom_point(aes(color=name))+
  labs(title= County_NAM,
       x="Year", y = "Unreported Withdrawals (MG)") +
  scale_y_continuous(limits = c(0, 40),  breaks = seq(0, 40, by = 10))+
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


ggsave(paste0(WUDR_github,"/plots/Coefficient1/NORTHAMPTON_unreported.png"), plot = p1, width = 9.5, height = 6, units = "in")






































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
