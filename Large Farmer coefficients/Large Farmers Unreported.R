
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

# dat <- DEQ_2002
fn_large_farm <- function(dat){

dat <- dat[,-c(8,9)]
colnames(dat)[8] <- c("SM_F_Unreported")

dat$All_Irrigation <-round(dat$Total.Irri.Area *(dat$Irrigation/25.5)*27154/1000000,2) #mg

dat$Large_Farm_unreported <- dat$All_Irrigation - dat$Facility_withdrawal_mg-dat$SM_F_Unreported

dat$C_irr_lg <- dat$Large_Farm_unreported / dat$Facility_withdrawal_mg

# dat <- dat[,c(1,3,5,8:11)] #for write.csv else comment out for more details
return(dat)
}


Large_DEQ_IRR_2002 <- fn_large_farm(DEQ_2002)
Large_DEQ_IRR_2007 <- fn_large_farm(DEQ_2007)
Large_DEQ_IRR_2012 <- fn_large_farm(DEQ_2012)
Large_DEQ_IRR_2017 <- fn_large_farm(DEQ_2017)

# write.csv(Large_DEQ_IRR_2002, paste0(WUDR_github,"/Output_Tables/Large_DEQ_IRR_2002.csv"), row.names = FALSE)
# write.csv(Large_DEQ_IRR_2007, paste0(WUDR_github,"/Output_Tables/Large_DEQ_IRR_2007.csv"), row.names = FALSE)
# write.csv(Large_DEQ_IRR_2012, paste0(WUDR_github,"/Output_Tables/Large_DEQ_IRR_2012.csv"), row.names = FALSE)
# write.csv(Large_DEQ_IRR_2017, paste0(WUDR_github,"/Output_Tables/Large_DEQ_IRR_2017.csv"), row.names = FALSE)

rm(DEQ_2002,DEQ_2007,DEQ_2012,DEQ_2017)
fn_large_farm_tot <- function(dat){
  
  dat <- dat[,-c(8,9)]
  colnames(dat)[8] <- c("SM_F_Unreported")
  
  dat$All_Irrigation <-round(dat$Total.Irri.Area *(dat$Irrigation/25.5)*27154/1000000,2) #mg
  
  dat$Large_Farm_unreported <- dat$All_Irrigation - dat$Facility_withdrawal_mg-dat$SM_F_Unreported
  
  dat$C_tot_lg <- dat$Large_Farm_unreported / dat$Facility_withdrawal_mg
  
  # dat <- dat[,c(1,3,5,8:11)]
  
  return(dat)
}

Large_DEQ_TOT_2002 <- fn_large_farm_tot(Tdeq_coef_2002)
Large_DEQ_TOT_2007 <- fn_large_farm_tot(Tdeq_coef_2007)
Large_DEQ_TOT_2012 <- fn_large_farm_tot(Tdeq_coef_2012)
Large_DEQ_TOT_2017 <- fn_large_farm_tot(Tdeq_coef_2017)

# write.csv(Large_DEQ_TOT_2002, paste0(WUDR_github,"/Output_Tables/Large_DEQ_TOT_2002.csv"), row.names = FALSE)
# write.csv(Large_DEQ_TOT_2007, paste0(WUDR_github,"/Output_Tables/Large_DEQ_TOT_2007.csv"), row.names = FALSE)
# write.csv(Large_DEQ_TOT_2012, paste0(WUDR_github,"/Output_Tables/Large_DEQ_TOT_2012.csv"), row.names = FALSE)
# write.csv(Large_DEQ_TOT_2017, paste0(WUDR_github,"/Output_Tables/Large_DEQ_TOT_2017.csv"), row.names = FALSE)
rm(Tdeq_coef_2002,Tdeq_coef_2007,Tdeq_coef_2012,Tdeq_coef_2017)

VA_counties<-readOGR(paste0(WUDR_github, "/VA_counties_sp"), layer="VA_counties")

fn_plot <- function(Year){
if (Year == 2002) {
  i = Large_DEQ_IRR_2002}else if (Year == 2007){
    i = Large_DEQ_IRR_2007} else if (Year == 2012){
      i = Large_DEQ_IRR_2012} else if (Year == 2017){
        i =Large_DEQ_IRR_2017
      }
i <- filter(i, C_irr_lg >0)

i<-sp::merge(VA_counties,i, by.x = "COUNTYFP", by.y = "County_Code")

p1<-tm_shape(i)+
  tm_polygons("C_irr_lg", title = "Unreported Coefficient (C_irr_lg)",
              breaks = c(0,5,10,25,50,90,Inf),
              # n=5,style="jenks",
              textNA = "Missing DEQ Irrigation Withdrawal/No Census data/-ve coeff value",
              id="NAMELSAD")+
  # tm_text("NAME", size = 0.3)+
  tm_layout(main.title = paste0(Year," Large farm unreported deficit Irrigation\n(as a percentage of VDEQ Irrigation withdrawal)"),
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


fn_plot_tot <- function(Year){
  if (Year == 2002) {
    i = Large_DEQ_TOT_2002}else if (Year == 2007){
      i = Large_DEQ_TOT_2007} else if (Year == 2012){
        i = Large_DEQ_TOT_2012} else if (Year == 2017){
          i =Large_DEQ_TOT_2017
        }
  i <- filter(i, C_tot_lg >0)
  
  i<-sp::merge(VA_counties,i, by.x = "COUNTYFP", by.y = "County_Code")
  
  p1<-tm_shape(i)+
    tm_polygons("C_tot_lg", title = "Unreported Coefficient (C_tot_lg)",
                breaks = c(0,0.5,5,10,25,50,90,Inf),
                # n=5,style="jenks",
                textNA = "-ve coefficient value/No Census data",
                id="NAMELSAD")+
    # tm_text("NAME", size = 0.3)+
    tm_layout(main.title = paste0(Year," Large farm unreported deficit Irrigation\n(as a percentage of VDEQ TOTAL withdrawal)"),
              legend.outside = FALSE,
              legend.title.size = 1.2,
              legend.text.size = 0.8,
              legend.position = c("left","top"),
              legend.bg.alpha = 1)
  
  tmap_save(p1, paste0(WUDR_github,"/plots/Coefficient1/",Year, "Large farm unreported Deq Total.png"),  width = 8.5, height = 5, units = 'in')
  
  return(p1)
  
  
}

# p1 <-fn_plot_tot(2002)
# p2<- fn_plot_tot(2007)
# p3<- fn_plot_tot(2012)
# p4<- fn_plot_tot(2017)




Mk_function <- function(dat1,dat2,dat3,dat4){
  
  
  dat<- purrr::reduce(list(dat1[,c(1,2,11)],dat2[,c(2,11)],dat3[,c(2,11)],dat4[,c(2,11)]), dplyr::inner_join, by = 'County_Code')
  
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
MK_Large_Irr_counties <- Mk_function(Large_DEQ_IRR_2002,Large_DEQ_IRR_2007,Large_DEQ_IRR_2012,Large_DEQ_IRR_2017)


# All counties with data in all 4 years
MK_Tot_counties <- Mk_function(Large_DEQ_TOT_2002,Large_DEQ_TOT_2007,Large_DEQ_TOT_2012,Large_DEQ_TOT_2017)

write.csv(MK_Large_Irr_counties, paste0(WUDR_github,"/Output_Tables/MK_Irr-DEQ-Largefarms.csv"), row.names = FALSE)
write.csv(MK_Tot_counties, paste0(WUDR_github,"/Output_Tables/MK_TOT-DEQ-Largefarms.csv"), row.names = FALSE)








































