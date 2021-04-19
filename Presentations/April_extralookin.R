WUDR_github<-"F:/My Drive/WUDR/WUDR_Github/WUDR_local"
setwd(WUDR_github)

library(tidyverse)
library(tmap)

summary_2002<-read.csv(paste0(WUDR_github,"/csv_files/" , 2002, "_census_dat_availability_summary.csv"))
summary_2007<-read.csv(paste0(WUDR_github,"/csv_files/" , 2007, "_census_dat_availability_summary.csv"))
summary_2012<-read.csv(paste0(WUDR_github,"/csv_files/" , 2012, "_census_dat_availability_summary.csv"))
summary_2017<-read.csv(paste0(WUDR_github,"/csv_files/" , 2017, "_census_dat_availability_summary.csv"))

a <- merge.data.frame(summary_2002[,c(2,3,4,5)],summary_2017[,c(2,4,5)], by = "County_Code")
a$areadiff <- a$Irrigated_Acreage.y -a$Irrigated_Acreage.x
a$opsdiff <- a$Irrigated_Operations.y -a$Irrigated_Operations.x


load(paste0(WUDR_github,"/dat_load/VA_shapefile.RData"))
VA_counties@data$Countycode = str_remove(VA_counties@data$COUNTYFP, "^0+")
VA_counties@data$Countycode =as.numeric(VA_counties@data$Countycode)

plotdat<-sp::merge(VA_counties,a, by.x = "Countycode", by.y = "County_Code", all.x=TRUE)

tmap_mode("plot")
p1<-tm_shape(plotdat)+
  tm_polygons("areadiff", title = "Difference in Irrigated area",
              breaks = c(-4638,-3274,-921,-373,0,254,1178),
              # n=5,style="jenks",
              id="NAMELSAD",
              palette = "BrBG",
              # contrast=0.9,
               midpoint = NA,
              legend.hist = TRUE)+
  tm_layout(main.title = " 2002- 2017 Difference in Irrigated area",
            legend.outside = TRUE,
            legend.title.size = 1.2,
            legend.text.size = 0.8,
            legend.position = c("left","top"),
            legend.bg.alpha = 1)
 p1

 tmap_save(p1, paste0(WUDR_github,"/plots/DEQ and Census Data Avaliability/","Difference in Irrigated area.png"),  width = 8, height = 4, units = 'in')
 tmap_save(p1, paste0(WUDR_github,"/plots/DEQ and Census Data Avaliability/","Difference in Irrigated area.html"))
 
 p2<-tm_shape(plotdat)+
   tm_polygons("opsdiff", title = "Difference in number of operations",
               # breaks = c(0,1,5,10,20),
               n=5,style="jenks",
               id="NAMELSAD",
               palette = "-Oranges",
               midpoint = NA,
               legend.hist = TRUE)+
   tm_layout(main.title = " 2002- 2017 Difference in number of operations",
             legend.outside = TRUE,
             legend.title.size = 1.2,
             legend.text.size = 0.8,
             legend.position = c("left","top"),
             legend.bg.alpha = 1)
 p2
 
 tmap_save(p2, paste0(WUDR_github,"/plots/DEQ and Census Data Avaliability/","Difference in Irrigated opearations.png"),  width = 10, height = 5, units = 'in')
 tmap_save(p2, paste0(WUDR_github,"/plots/DEQ and Census Data Avaliability/","Difference in Irrigated operations.html"),  width = 10, height = 6.5, units = 'in')
 
 write.csv(a, "difference in area and operations 2002-2017.csv")
 
 #########################
 summary_2002<-read.csv(paste0(WUDR_github,"/csv_files/" , 2002, "_deq_dat_availability_summary.csv"))
 summary_2007<-read.csv(paste0(WUDR_github,"/csv_files/" , 2007, "_deq_dat_availability_summary.csv"))
 summary_2012<-read.csv(paste0(WUDR_github,"/csv_files/" , 2012, "_deq_dat_availability_summary.csv"))
 summary_2017<-read.csv(paste0(WUDR_github,"/csv_files/" , 2017, "_deq_dat_availability_summary.csv"))
 
 a <- merge.data.frame(summary_2002[,c(3,5,7,8)],summary_2017[,c(3,7,8)], by = "Countycode")
 a$withdiff <- a$Facility_withdrawal_mg.y -a$Facility_withdrawal_mg.x
 a$facdiff <- a$Count.y -a$Count.x
 
 plotdat<-sp::merge(VA_counties,a, by.x = "Countycode", by.y = "Countycode", all.x=TRUE)
 
 tmap_mode("plot")
 p1<-tm_shape(plotdat)+
   tm_polygons("withdiff", title = "Difference in withdrawals",
               # breaks = c(-1000,-500,-300,-200,100,0,50,100),
               n=4,style="jenks",
               id="NAMELSAD",
               palette = "-Oranges",
               midpoint = NA,
               legend.hist = TRUE)+
   tm_layout(main.title = " 2002- 2017 Difference in DEQ withdrawals",
             legend.outside = TRUE,
             legend.title.size = 1.2,
             legend.text.size = 0.8,
             legend.position = c("left","top"),
             legend.bg.alpha = 1)
 p1
 
 tmap_save(p1, paste0(WUDR_github,"/plots/DEQ and Census Data Avaliability/","Difference in deq with.png"),  width = 10, height = 5, units = 'in')
 tmap_save(p1, paste0(WUDR_github,"/plots/DEQ and Census Data Avaliability/","Difference in deq with.html"),  width = 10, height = 6.5, units = 'in')
 
 p2<-tm_shape(plotdat)+
   tm_polygons("facdiff", title = "Difference in number of facilities",
               # breaks = c(0,1,5,10,20),
               n=5,style="jenks",
               id="NAMELSAD",
               palette = "-Oranges",
               midpoint = NA,
               legend.hist = TRUE)+
   tm_layout(main.title = " 2002- 2017 Difference in number of facilities",
             legend.outside = TRUE,
             legend.title.size = 1.2,
             legend.text.size = 0.8,
             legend.position = c("left","top"),
             legend.bg.alpha = 1)
 p2
 
 tmap_save(p2, paste0(WUDR_github,"/plots/DEQ and Census Data Avaliability/","Difference in Irrigated fac.png"),  width = 10, height = 5, units = 'in')
 tmap_save(p2, paste0(WUDR_github,"/plots/DEQ and Census Data Avaliability/","Difference in Irrigated fac.html"),  width = 10, height = 6.5, units = 'in')
 
 write.csv(a, "difference in deq withdrawal and fac 2002-2017.csv")
 