
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
load(paste0(WUDR_github,"/dat_load/IrrCoeff.RData"))

DEQ_Irr_Dat <- list(DEQ_2002,DEQ_2007,DEQ_2012,DEQ_2017) # USed to subtract Small Farmer withdrawals based on Coeff method
Y_census <- c(2002, 2007,2012,2017)

fn_large_farm <- function(dat){

dat <- dat[,-c(8,9)]
colnames(dat)[8] <- c("SM_F_Unreported")

dat$All_Irrigation_mg <-round(dat$Total.Irri.Area *(dat$Irrigation/25.5)*27154/1000000,2) #mg

dat$Large_Farm_unreported <- dat$All_Irrigation_mg - dat$Facility_withdrawal_mg-dat$SM_F_Unreported

dat$C_irr_lg <- dat$Large_Farm_unreported / dat$Facility_withdrawal_mg

# dat <- dat[,c(1,3,5,8:11)] #for write.csv else comment out for more details
return(dat)
}

Large_DEQ_IRR_Coeff <- lapply(DEQ_Irr_Dat, fn_large_farm)
names(Large_DEQ_IRR_Coeff) <- Y_census

# save(Large_DEQ_IRR_Coeff, file = paste0(WUDR_github,"/dat_load/Large_DEQ_IRR_Coeff.RData"))
# for (i in 1:length(Large_DEQ_IRR_Coeff)) {
# write.csv(Large_DEQ_IRR_Coeff[[i]], paste0(WUDR_github,"/Output_Tables/Large_DEQ_IRR_Coeff_", names(Large_DEQ_IRR_Coeff)[i] ,".csv"), row.names = FALSE)
# }

DEQ_Under_TH_Dat <- list(Tdeq_coef_2002,Tdeq_coef_2007,Tdeq_coef_2012,Tdeq_coef_2017) # USed to subtract Small Farmer withdrawals based on deficit method

# Replace Under_THal Facility withdrawals with Irrigation withdrawals
Large_DEQ_Under_TH <- list()
for (i in 1:length(DEQ_Under_TH_Dat)) {
  
  Large_DEQ_Under_TH[[i]] <- DEQ_Under_TH_Dat[[i]][,-c(8,9)]
  colnames(Large_DEQ_Under_TH[[i]])[c(5,8)] <- c("Facility_All_Withdrawl_mg", "SM_F_Unreported")
  
  Large_DEQ_Under_TH[[i]] <- left_join(Large_DEQ_Under_TH[[i]], DEQ_Irr_Dat[[i]][,c(2,5)], by = "County_Code")
  Large_DEQ_Under_TH[[i]]$All_Irrigation_mg <-round(Large_DEQ_Under_TH[[i]]$Total.Irri.Area *(Large_DEQ_Under_TH[[i]]$Irrigation/25.5)*27154/1000000,2) #mg
  
  Large_DEQ_Under_TH[[i]]$Facility_withdrawal_mg[is.na(Large_DEQ_Under_TH[[i]]$Facility_withdrawal_mg)] <- 0
  
  Large_DEQ_Under_TH[[i]]$Large_Farm_unreported <- round((Large_DEQ_Under_TH[[i]]$All_Irrigation_mg - Large_DEQ_Under_TH[[i]]$Facility_withdrawal_mg-Large_DEQ_Under_TH[[i]]$SM_F_Unreported),5)
  
  Large_DEQ_Under_TH[[i]]$C_Under_TH_lg <- Large_DEQ_Under_TH[[i]]$Large_Farm_unreported / Large_DEQ_Under_TH[[i]]$Facility_All_Withdrawl_mg
  
  #  Large_DEQ_Under_TH[[i]] <-  Large_DEQ_Under_TH[[i]][,c(1,3,5,8:10)]
  

}

names(Large_DEQ_Under_TH) <- Y_census  
#  save(Large_DEQ_Under_TH, file = paste0(WUDR_github,"/dat_load/Large_DEQ_Under_TH_Coeff.RData"))
# for (i in 1:length(Large_DEQ_Under_TH)) {
#   write.csv(Large_DEQ_Under_TH[[i]], paste0(WUDR_github,"/Output_Tables/Large_DEQ_Under_TH_", names(Large_DEQ_Under_TH)[i] ,".csv"), row.names = FALSE)
# }

tmap_mode("plot")
VA_counties<-readOGR(paste0(WUDR_github, "/VA_counties_sp"), layer="VA_counties")

fn_plot <- function(Year){
if (Year == 2002) {
  i = 1}else if (Year == 2007){
    i = 2} else if (Year == 2012){
      i = 3} else if (Year == 2017){
        i =4
      }
  
plot_Dat <- filter(Large_DEQ_IRR_Coeff[[i]], C_irr_lg >0)

plot_Dat<-sp::merge(VA_counties,plot_Dat, by.x = "COUNTYFP", by.y = "County_Code")

p1<-tm_shape(plot_Dat)+
  tm_polygons("C_irr_lg", title = "Unreported Coefficient (C_irr_lg)",
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

tmap_save(p1, paste0(WUDR_github,"/plots/Coefficient1/",Year, "Large farm unreported DeqIrrigation.png"),  width = 8.5, height = 5, units = 'in')

return(p1)

}

# p1 <-fn_plot(2002)
# p2<- fn_plot(2007)
# p3<- fn_plot(2012)
# p4<- fn_plot(2017)


fn_plot_Under_TH <- function(Year){
  if (Year == 2002) {
    i = 1}else if (Year == 2007){
      i = 2} else if (Year == 2012){
        i = 3} else if (Year == 2017){
          i =4
        }
  
  plot_Dat <- filter(Large_DEQ_Under_TH[[i]], C_Under_TH_lg >0)
  
  
  plot_Dat<-sp::merge(VA_counties,plot_Dat, by.x = "COUNTYFP", by.y = "County_Code")
  
  p1<-tm_shape(plot_Dat)+
    tm_polygons("C_Under_TH_lg", title = "Unreported Coefficient (C_Under_TH_lg)",
                breaks = c(0,0.01,0.1,1,10,Inf),
                # n=5,style="jenks",
                textNA = "-ve coefficient value/No Census data",
                id="NAMELSAD")+
    # tm_text("NAME", size = 0.3)+
    tm_layout(main.title = paste0(Year," Large farm unreported \n (as a percentage of VDEQ Under_THAL withdrawal)"),
              legend.outside = FALSE,
              legend.title.size = 1.2,
              legend.text.size = 0.8,
              legend.position = c("left","top"),
              legend.bg.alpha = 1)
  
  tmap_save(p1, paste0(WUDR_github,"/plots/Coefficient1/",Year, "Large farm unreported Deq Under_THal.png"),  width = 8.5, height = 5, units = 'in')
  
  }

# p1 <-fn_plot_Under_TH(2002)
# p2<- fn_plot_Under_TH(2007)
# p3<- fn_plot_Under_TH(2012)
# p4<- fn_plot_Under_TH(2017)




Mk_function <- function(dat1,dat2,dat3,dat4,coloumn_number){
  
  
  dat<- purrr::reduce(list(dat1[,c(1,2,coloumn_number)],dat2[,c(2,coloumn_number)],dat3[,c(2,coloumn_number)],dat4[,c(2,coloumn_number)]), dplyr::inner_join, by = 'County_Code')
  
  dat <- dat[order(dat$County),]
  dat <- dat[,-c(2)]
  
  colnames(dat)[2:5] <- c("c2002", "c2007", "c2012", "c2017")
  
  MK_dat <- dat %>% 
    pivot_longer(cols = c(2:5))
  colnames(MK_dat)[2:3] <- c("Year", "Large_Coeff1")
  
  
  MK_dat <- split(MK_dat$Large_Coeff1, MK_dat$County)
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
MK_Large_Irr_counties <- Mk_function(Large_DEQ_IRR_Coeff[[1]],Large_DEQ_IRR_Coeff[[2]],Large_DEQ_IRR_Coeff[[3]],Large_DEQ_IRR_Coeff[[4]],11)


# All counties with data in all 4 years
MK_Under_TH_counties <- Mk_function(Large_DEQ_Under_TH[[1]],Large_DEQ_Under_TH[[2]],Large_DEQ_Under_TH[[3]],Large_DEQ_Under_TH[[4]],12)

write.csv(MK_Large_Irr_counties, paste0(WUDR_github,"/Output_Tables/MK_Irr-DEQ-Largefarms.csv"), row.names = FALSE)
write.csv(MK_Under_TH_counties, paste0(WUDR_github,"/Output_Tables/MK_Under_TH-DEQ-Largefarms.csv"), row.names = FALSE)








































