
WUDR_github<-"F:/My Drive/WUDR/WUDR_Github/WUDR"
setwd(WUDR_github)

library(tidyverse) 
library(tmap)
library(rgdal)
library(stringr)
library(Kendall)
library(purrr)
library(gridExtra)
options(scipen = 9999)

####################################################################
load(paste0(WUDR_github,"/dat_load/SF_Coeff.RData"))


## Counties with DEQ data
DEQ_Irr_Dat <- list(DEQ_2002,DEQ_2007,DEQ_2012,DEQ_2017) # USed to subtract Small Farmer withdrawals based on Coeff method
Y_census <- c(2002, 2007,2012,2017)

fn_large_farm <- function(dat){

dat <- dat[,-c(8,9)]
colnames(dat)[8] <- c("SM_F_Unreported")

dat$All_Irrigation_mg <-round(dat$Total.Irri.Area *(dat$Irrigation/25.5)*27154/1000000,2) #mg

dat$Large_Farm_unreported <- dat$All_Irrigation_mg - dat$Facility_withdrawal_mg-dat$SM_F_Unreported

dat$C_irr_lg <- 100*dat$Large_Farm_unreported / dat$Facility_withdrawal_mg

 # dat <- dat[,c(1,3,5,8:11)] #for write.csv else comment out for more details
return(dat)
}

Large_DEQ_IRR_Coeff <- lapply(DEQ_Irr_Dat, fn_large_farm)
names(Large_DEQ_IRR_Coeff) <- Y_census

# save(Large_DEQ_IRR_Coeff, file = paste0(WUDR_github,"/dat_load/Large_DEQ_IRR_Coeff.RData"))

# for (i in 1:length(Large_DEQ_IRR_Coeff)) {
# write.csv(Large_DEQ_IRR_Coeff[[i]], paste0(WUDR_github,"/Output_Tables/Large_DEQ_IRR_Coeff_", names(Large_DEQ_IRR_Coeff)[i] ,".csv"), row.names = FALSE)
# }

tmap_mode("plot")
VA_counties<-readOGR(paste0(WUDR_github, "/VA_counties_sp"), layer="VA_counties")

Year = 2012
fn_plot <- function(Year){
  if (Year == 2002) {
    i = 1}else if (Year == 2007){
      i = 2} else if (Year == 2012){
        i = 3} else if (Year == 2017){
          i =4
        }
  
  plot_Dat <- Large_DEQ_IRR_Coeff[[i]]
  plot_Dat$C_irr_lg <- ifelse(plot_Dat$C_irr_lg <0 ,0, plot_Dat$C_irr_lg)
  plot_Dat$Large_Farm_unreported <- ifelse(plot_Dat$Large_Farm_unreported <0 ,0, plot_Dat$Large_Farm_unreported)
  
  plot_Dat<-sp::merge(VA_counties,plot_Dat, by.x = "COUNTYFP", by.y = "County_Code")
  
  p1<-tm_shape(plot_Dat)+
    tm_polygons("C_irr_lg", title = "Unreported Coefficient (LF)",
                breaks = c(0,5,10,25,50,90,Inf),
                # n=5,style="jenks",
                textNA = "Missing DEQ Irrigation Withdrawal/No Census data/-ve coeff value",
                id="NAMELSAD")+
    # tm_text("NAME", size = 0.3)+
    tm_layout(main.title = paste0(Year," Large farm unreported \n(as a percentage of VDEQ Irrigation withdrawal)"),
              legend.outside = FALSE,
              legend.title.size = 1.2,
              legend.text.size = 0.8,
              legend.position = c("left","top"),
              legend.bg.alpha = 1)
  
  tmap_save(p1, paste0(WUDR_github,"/plots/Coefficient1/Large_Farm/",Year, "_LF_Coeff_DEQ_Avalaible_counties.png"),  width = 8.5, height = 5, units = 'in')
  
  p2<-tm_shape(plot_Dat)+
    tm_polygons("Large_Farm_unreported", title = "Unreported volume (MG)",
                breaks = c(0,10,50,100,200,500,Inf),
                # n=5,style="jenks",
                textNA = "Missing DEQ Irrigation Withdrawal/No Census data/-ve coeff value",
                id="NAMELSAD")+
    # tm_text("NAME", size = 0.3)+
    tm_layout(main.title = paste0(Year," Large farm unreported withdrawals"),
              legend.outside = FALSE,
              legend.title.size = 1.2,
              legend.text.size = 0.8,
              legend.position = c("left","top"),
              legend.bg.alpha = 1)
  
  tmap_save(p2, paste0(WUDR_github,"/plots/Coefficient1/Large_Farm/",Year, "_LF_VOL_DEQ_Avalaible_counties.png"),  width = 8.5, height = 5, units = 'in')
  
  
}

p1 <-fn_plot(2002)
p2<- fn_plot(2007)
p3<- fn_plot(2012)
p4<- fn_plot(2017)

####################################################################################################
#2 
# Counties without DEQ data

DEQ_Tot_area_Dat <- list(Tdeq_coef_2002,Tdeq_coef_2007,Tdeq_coef_2012,Tdeq_coef_2017) # USed to subtract Small Farmer withdrawals based on deficit method


Large_DEQ_Tot_area <- list()
for (i in 1:length(DEQ_Tot_area_Dat)) {
  
  Large_DEQ_Tot_area[[i]] <- DEQ_Tot_area_Dat[[i]][,-c(8,9)]
  colnames(Large_DEQ_Tot_area[[i]])[c(5,8)] <- c("Facility_All_Withdrawl_mg", "SM_F_Unreported")
  
  Large_DEQ_Tot_area[[i]] <- left_join(Large_DEQ_Tot_area[[i]], DEQ_Irr_Dat[[i]][,c(2,5)], by = "County_Code")
  Large_DEQ_Tot_area[[i]]$All_Irrigation_mg <-round(Large_DEQ_Tot_area[[i]]$Total.Irri.Area *(Large_DEQ_Tot_area[[i]]$Irrigation/25.5)*27154/1000000,2) #mg
  
  Large_DEQ_Tot_area[[i]]$Facility_withdrawal_mg[is.na(Large_DEQ_Tot_area[[i]]$Facility_withdrawal_mg)] <- 0
  
  Large_DEQ_Tot_area[[i]]$Large_Farm_unreported <- round((Large_DEQ_Tot_area[[i]]$All_Irrigation_mg - Large_DEQ_Tot_area[[i]]$Facility_withdrawal_mg-Large_DEQ_Tot_area[[i]]$SM_F_Unreported),5)
  
  Large_DEQ_Tot_area[[i]]$C_Tot_area_lg <- Large_DEQ_Tot_area[[i]]$Large_Farm_unreported / Large_DEQ_Tot_area[[i]]$Facility_All_Withdrawl_mg
  
    # Large_DEQ_Tot_area[[i]] <-  Large_DEQ_Tot_area[[i]][,c(1,3,5,8:10)]
  

}

names(Large_DEQ_Tot_area) <- Y_census  
# save(Large_DEQ_Tot_area, file = paste0(WUDR_github,"/dat_load/Large_Farm_Tot_area.RData"))
# for (i in 1:length(Large_DEQ_Tot_area)) {
# write.csv(Large_DEQ_Tot_area[[i]], paste0(WUDR_github,"/Output_Tables/Large_DEQ_Tot_area_", names(Large_DEQ_Tot_area)[i] ,".csv"), row.names = FALSE)
# }


Year = 2012
fn_plot_Tot_area <- function(Year){
  if (Year == 2002) {
    i = 1}else if (Year == 2007){
      i = 2} else if (Year == 2012){
        i = 3} else if (Year == 2017){
          i =4
        }
  
  plot_Dat <- Large_DEQ_Tot_area[[i]]
  plot_Dat$C_Tot_area_lg <- ifelse(plot_Dat$C_Tot_area_lg <0 ,0, plot_Dat$C_Tot_area_lg)
  plot_Dat$Large_Farm_unreported <- ifelse(plot_Dat$Large_Farm_unreported <0 ,0, plot_Dat$Large_Farm_unreported)
  
  plot_Dat<-sp::merge(VA_counties,plot_Dat, by.x = "COUNTYFP", by.y = "County_Code")
  
  p1<-tm_shape(plot_Dat)+
    tm_polygons("C_Tot_area_lg", title = "Unreported Coefficient",
                breaks = c(0,0.01,0.1,1,10,Inf),
                # n=5,style="jenks",
                textNA = "-ve coefficient value/No Census data",
                id="NAMELSAD")+
    # tm_text("NAME", size = 0.3)+
    tm_layout(main.title = paste0(Year," Large farm unreported \n (as a percentage of VDEQ Total withdrawal)"),
              legend.outside = FALSE,
              legend.title.size = 1.2,
              legend.text.size = 0.8,
              legend.position = c("left","top"),
              legend.bg.alpha = 1)
  
   tmap_save(p1, paste0(WUDR_github,"/plots/Coefficient1/Large_Farm/",Year, "_LF_Coeff_DEQ_Missing_counties.png"),  width = 8.5, height = 5, units = 'in')
  
  p2<-tm_shape(plot_Dat)+
    tm_polygons("Large_Farm_unreported", title = "Unreported Volume (MG)",
                breaks = c(0,10,50,100,200,500,Inf),
                # n=5,style="jenks",
                textNA = "-ve coefficient value/No Census data",
                id="NAMELSAD")+
    # tm_text("NAME", size = 0.3)+
    tm_layout(main.title = paste0(Year," Large farm unreported withdrawals"),
              legend.outside = FALSE,
              legend.title.size = 1.2,
              legend.text.size = 0.8,
              legend.position = c("left","top"),
              legend.bg.alpha = 1)
  
  tmap_save(p2, paste0(WUDR_github,"/plots/Coefficient1/Large_Farm/",Year, "_LF_VOL_DEQ_Missing_counties.png"),  width = 8.5, height = 5, units = 'in')
  
  
  }

p1 <-fn_plot_Tot_area(2002)
p2<- fn_plot_Tot_area(2007)
p3<- fn_plot_Tot_area(2012)
p4<- fn_plot_Tot_area(2017)


save(Large_DEQ_Tot_area,Large_DEQ_IRR_Coeff, file= paste0(WUDR_github,"/dat_load/LF_Coeff_both.RData"))


load(paste0(WUDR_github,"/dat_load/LF_Coeff_both.RData"))

Fn_merge_coeff <- function(x,y){
  CF <- full_join(x[,c(2,11)],y[,c(1,2,12)], by = "County_Code")
  return(CF)
}

CF_2002 <- Fn_merge_coeff(Large_DEQ_IRR_Coeff[[1]],Large_DEQ_Tot_area[[1]])
CF_2007 <- Fn_merge_coeff(Large_DEQ_IRR_Coeff[[2]],Large_DEQ_Tot_area[[2]])
CF_2012 <- Fn_merge_coeff(Large_DEQ_IRR_Coeff[[3]],Large_DEQ_Tot_area[[3]])
CF_2017 <- Fn_merge_coeff(Large_DEQ_IRR_Coeff[[4]],Large_DEQ_Tot_area[[4]])

Coeff_Summary <- list()
Coeff_list <- list(CF_2002,CF_2007,CF_2012,CF_2017)


for (i in 1:4) {
  Coeff_Summary[[i]] <- Coeff_list[[i]] %>% 
    summarise(Min = min(C_Tot_area_lg      , na.rm = TRUE),
              Median = median(C_Tot_area_lg      , na.rm = TRUE),
              Mean = mean(C_Tot_area_lg      , na.rm = TRUE),
              Max = max(C_Tot_area_lg      , na.rm = TRUE))
}


names(Coeff_Summary) <- c(seq(2002,2017,5))
Coeff_Summary <- bind_rows(Coeff_Summary, .id = "Year")



# Check the name carefully
write.csv(Coeff_Summary, paste0(WUDR_github,"/Output_Tables/LF_Summary of (DEQ MISSING) Total Coefficients.csv"), row.names = FALSE) # use C_tot in loop
write.csv(Coeff_Summary, paste0(WUDR_github,"/Output_Tables/LF_Summary of (DEQ Avaliable) Irrigation Coefficients.csv"), row.names = FALSE) # use C_irr in loop




































