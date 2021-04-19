WUDR_github<-"F:/My Drive/WUDR/WUDR_Github/WUDR_local"
setwd(WUDR_github)


library(tidyverse)
options(scipen=999)

deq_plots <- function(YEAR){
summary_dat<-read.csv(paste0(WUDR_github,"/csv_files/" , YEAR, "_deq_dat_availability_summary.csv"))
summary_dat <- summary_dat[, -c(1)]
summary_dat$GEOID <- as.numeric(as.character(summary_dat$GEOID))
summary_dat$Countycode <- as.numeric(summary_dat$Countycode)




load(paste0(WUDR_github,"/dat_load/VA_shapefile.RData"))
VA_counties@data$Countycode = str_remove(VA_counties@data$COUNTYFP, "^0+")
VA_counties@data$Countycode =as.numeric(VA_counties@data$Countycode)

plotdat<-sp::merge(VA_counties,summary_dat, by.x = "Countycode", by.y = "Countycode", all.x=TRUE)

tmap_mode("plot")
p1<-tm_shape(plotdat)+
  tm_polygons("Facility_withdrawal_mg", title = "Withdrawals mg",
              # breaks = c(0,1,5,10,20),
              n=6,style="jenks",
              id="NAMELSAD",
              # palette = "-Oranges",
              midpoint = NA,
              legend.hist = TRUE)+
  tm_layout(main.title = paste0(YEAR, "DEQ Facility withdrawals"),
            legend.outside = TRUE,
            legend.title.size = 1.2,
            legend.text.size = 0.8,
            legend.position = c("left","top"),
            legend.bg.alpha = 1)
p1

p2<-tm_shape(plotdat)+
  tm_polygons("Count", title = "Number of facilities",
              # breaks = c(0,1,5,10,20),
              n=3,style="jenks",
              id="NAMELSAD",
              # palette = "-Oranges",
              midpoint = NA,
              legend.hist = TRUE)+
  tm_layout(main.title = paste0(YEAR,"DEQ number of facilities"),
            legend.outside = TRUE,
            legend.title.size = 1.2,
            legend.text.size = 0.8,
            legend.position = c("left","top"),
            legend.bg.alpha = 1)
p2

tmap_save(p1, paste0(WUDR_github,"/plots/DEQ and Census Data Avaliability/",YEAR,"DEQ facility Withdrawals.png"),  width = 8, height = 5, units = 'in')
tmap_save(p1, paste0(WUDR_github,"/plots/DEQ and Census Data Avaliability/",YEAR,"DEQ facility Withdrawals.html"))
tmap_save(p2, paste0(WUDR_github,"/plots/DEQ and Census Data Avaliability/",YEAR,"DEQ number of facilities.png"),  width = 8, height = 5, units = 'in')
tmap_save(p2, paste0(WUDR_github,"/plots/DEQ and Census Data Avaliability/",YEAR,"DEQ number of facilities.html"))
}

deq_plots(2017)
deq_plots(2012)
deq_plots(2007)
deq_plots(2002)
