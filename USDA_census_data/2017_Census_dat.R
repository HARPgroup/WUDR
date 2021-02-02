WUDR_github<-"F:/My Drive/WUDR/WUDR_Github/WUDR_local"
setwd(WUDR_github)

#load libraries

library(sqldf)
library(tidyverse)
library(dplyr)
library(rgdal)
library(compare)
library(tmap)
# source("F:/My Drive/WUDR/Codes/agglandperfarm.R")
# source("F:/My Drive/WUDR/Codes/captalize.R")
#Load data form Quick stats server at (https://quickstats.nass.usda.gov/results/04A33D68-9ADB-36BD-8D71-1B20E03484F8) 
#Data retrieved date: 11/18/2020

Censusdata<-read.csv(paste0(WUDR_github,"/csv_files/2017_Census_Summary.csv"))

#Irigated area per farm  
Censusdata$Irrigated_area_per_farm<-round(Censusdata$Irrigated_Acreage/Censusdata$Irrigated_Operations,0)
head(Censusdata)

year=2017

##Plotting
VA_counties<-readOGR("F:/My Drive/VA_shapefile_updated", layer="VA_counties_new")
VA_counties<-readOGR(paste0(WUDR_github,"/VA_counties_sp"), layer="VA_counties_new")

# remove zero from lead the COUNTYFP in spatial data
VA_counties@data$Countycode = str_remove(VA_counties@data$COUNTYFP, "^0+")


plotdat<-sp::merge(VA_counties,Censusdata, by.x = "Countycode", by.y = "COUNTY_CODE", all.x=TRUE)


##Check
sum(Censusdata$Irrigated_Operations)
sum(plotdat@data$Irrigated_Operations, na.rm=TRUE)

sum(Censusdata$Irrigated_Acreage, na.rm = TRUE)
sum(plotdat@data$Irrigated_Acreage, na.rm=TRUE)

sum(Censusdata$Size_Bins)
sum(plotdat@data$Size_Bins, na.rm=TRUE)

sum(Censusdata$Ops_Bins)
sum(plotdat@data$Ops_Bins, na.rm=TRUE)

sum(Censusdata$Irrigated_area_per_farm,na.rm=TRUE)
sum(plotdat@data$Irrigated_area_per_farm, na.rm=TRUE)
#######

##Plot
tmap_mode("plot")
p1<-tm_shape(plotdat)+
  tm_polygons("Irrigated_Operations", title = "Irrigated operations",
              # breaks = c(0,1,5,10,20),
              n=6,style="jenks",
              id="NAMELSAD",
              legend.hist = TRUE)+
  tm_layout(main.title = paste0(year," Number of Irrigated operations"),
            legend.outside = TRUE,
            legend.title.size = 1.2,
            legend.text.size = 0.8,
            legend.position = c("left","top"),
            legend.bg.alpha = 1)+
  tm_credits("Source: USDA:NASS", size=0.7, position = c("right","top"))
p1
tmap_save(p1, paste0(year,"_irrigated_ops.png"),  width = 10, height = 5, units = 'in')
tmap_save(p1, paste0(year,"_irrigated_ops.html"),  width = 10, height = 6.5, units = 'in')

tmap_mode("plot")
p2<-tm_shape(plotdat)+
  tm_polygons("Irrigated_Acreage", title = "Acreage (acres)",
              # breaks = c(0,1,5,10,20),
              n=6,style="jenks",
              id="NAMELSAD",
              legend.hist = TRUE)+
  tm_layout(main.title = paste0(year," Irrigated Acreage (acres)"),
            legend.outside = TRUE,
            legend.title.size = 1.2,
            legend.text.size = 0.8,
            legend.position = c("left","top"),
            legend.bg.alpha = 1)+
  tm_credits("Source: USDA:NASS", size=0.7, position = c("right","top"))
p2
tmap_save(p2, paste0(year,"_irrigated_acreage.png"),  width = 10, height = 5, units = 'in')
tmap_save(p2, paste0(year,"_irrigated_acreage.html"),  width = 10, height = 6.5, units = 'in')

p3<-tm_shape(plotdat)+
  tm_polygons("Size_Bins", title = "Number of size bins",
              breaks = c(0,1,3,6,10),
              #n=5,style="jenks",
              id="NAMELSAD",
              legend.hist = TRUE)+
  tm_layout(main.title = paste0(year," Number of size bins reporting irrigated acreage data)"),
            legend.outside = TRUE,
            legend.title.size = 1.2,
            legend.text.size = 0.8,
            legend.position = c("left","top"),
            legend.bg.alpha = 1)+
  tm_credits("Source: USDA:NASS", size=0.7, position = c("right","top"))
p3
tmap_save(p3, paste0(year,"_size bins.png"),  width = 10, height = 5, units = 'in')
tmap_save(p3, paste0(year,"_size bins.html"),  width = 10, height = 6.5, units = 'in')

p4<-tm_shape(plotdat)+
  tm_polygons("Ops_Bins", title = "Number of opeartion bins",
              #breaks = c(0,1,3,6,10),
              n=4,style="jenks",
              id="NAMELSAD",
              legend.hist = TRUE)+
  tm_layout(main.title = paste0(year," Number of operation bins reporting irrigated acreage data)"),
            legend.outside = TRUE,
            legend.title.size = 1.2,
            legend.text.size = 0.8,
            legend.position = c("left","top"),
            legend.bg.alpha = 1)+
  tm_credits("Source: USDA:NASS", size=0.7, position = c("right","top"))
p4
tmap_save(p4, paste0(year,"_operation bins.png"),  width = 10, height = 5, units = 'in')
tmap_save(p4, paste0(year,"_operation bins.html"),  width = 10, height = 6.5, units = 'in')

p5<-tm_shape(plotdat)+
  tm_polygons("Irrigated_area_per_farm", title = "Irrigated Area per Farm",
              # breaks = c(0,1,5,10,20),
              n=6,style="jenks",
              id="NAMELSAD",
              legend.hist = TRUE)+
  tm_layout(main.title = paste0(year," Irrigated Area per Farm"),
            legend.outside = TRUE,
            legend.title.size = 1.2,
            legend.text.size = 0.8,
            legend.position = c("left","top"),
            legend.bg.alpha = 1)+
  tm_credits("Source: USDA:NASS", size=0.7, position = c("right","top"))
p5
tmap_save(p5, paste0(year,"_Irrigated Area per Far.png"),  width = 10, height = 5, units = 'in')
tmap_save(p5, paste0(year,"_Irrigated Area per Far.html"),  width = 10, height = 6.5, units = 'in')


#########################################################################
#Same plots as above but only for counties withd ata in both datsets
########################################################################

WUDR_github<-"F:/My Drive/WUDR/WUDR_Github/WUDR_local"
setwd(WUDR_github)

pacman::p_load(tidyverse, rgdal, tmap)
year=2017
common_dat<-read.csv(paste0(WUDR_github,"/csv_files/Common_counties_dat.csv"))
Censusdata<-read.csv(paste0(WUDR_github,"/csv_files/2017_Census_Summary.csv"))

#Irigated area per farm  
Censusdata$Irrigated_area_per_farm<-round(Censusdata$Irrigated_Acreage/Censusdata$Irrigated_Operations,0)
head(Censusdata)

common_bins<-subset(Censusdata, (COUNTY_CODE %in% common_dat$COUNTYFP))

year=2017

VA_counties<-readOGR("F:/My Drive/VA_shapefile_updated", layer="VA_counties_new")
VA_counties<-readOGR(paste0(WUDR_github,"/VA_counties_sp"), layer="VA_counties_new")

# remove zero from lead the COUNTYFP in spatial data
VA_counties@data$Countycode = str_remove(VA_counties@data$COUNTYFP, "^0+")


plotdat<-sp::merge(VA_counties,common_bins, by.x = "Countycode", by.y = "COUNTY_CODE", all.x=TRUE)


tmap_mode("plot")
p1<-tm_shape(plotdat)+
  tm_polygons("Irrigated_Operations", title = "Irrigated operations",
              # breaks = c(0,1,5,10,20),
              n=6,style="jenks",
              id="NAMELSAD",
              legend.hist = TRUE)+
  tm_layout(main.title = paste0(year," Number of Irrigated operations in Counties with data in both datsets"),
            legend.outside = TRUE,
            legend.title.size = 1.2,
            legend.text.size = 0.8,
            legend.position = c("left","top"),
            legend.bg.alpha = 1)+
  tm_credits("Source: USDA:NASS", size=0.7, position = c("right","top"))
p1
tmap_save(p1, paste0(WUDR_github,"/plots/",year,"_irrigated_ops_common_counties.png"),  width = 10, height = 5, units = 'in')


tmap_mode("view")
p2<-tm_shape(plotdat)+
  tm_polygons("Irrigated_Acreage", title = "Acreage (acres)",
              # breaks = c(0,1,5,10,20),
              n=6,style="jenks",
              id="NAMELSAD",
              legend.hist = TRUE)+
  tm_layout(main.title = paste0(year," Irrigated Acreage (acres) in counties with data in both datsets"),
            legend.outside = TRUE,
            legend.title.size = 1.2,
            legend.text.size = 0.8,
            legend.position = c("left","top"),
            legend.bg.alpha = 1)+
  tm_credits("Source: USDA:NASS", size=0.7, position = c("right","top"))
p2
tmap_save(p2, paste0(WUDR_github,"/plots/",year,"_irrigated_acreage_common_counties.png"),  width = 10, height = 5, units = 'in')

p3<-tm_shape(plotdat)+
  tm_polygons("Size_Bins", title = "Number of size bins",
              breaks = c(0,1,3,6,10),
              #n=5,style="jenks",
              id="NAMELSAD",
              legend.hist = TRUE)+
  tm_layout(main.title = paste0(year," Number of size bins reporting irrigated acreage data \n for counties in both datsets"),
            legend.outside = TRUE,
            legend.title.size = 1.2,
            legend.text.size = 0.8,
            legend.position = c("left","top"),
            legend.bg.alpha = 1)+
  tm_credits("Source: USDA:NASS", size=0.7, position = c("right","top"))
p3
tmap_save(p3, paste0(WUDR_github,"/plots/",year,"_size bins_common_counties.png"),  width = 10, height = 5, units = 'in')

p4<-tm_shape(plotdat)+
  tm_polygons("Ops_Bins", title = "Number of opeartion bins",
              #breaks = c(0,1,3,6,10),
              n=4,style="jenks",
              id="NAMELSAD",
              legend.hist = TRUE)+
  tm_layout(main.title = paste0(year," Number of operation bins reporting irrigated acreage data\n for counties in both datsets"),
            legend.outside = TRUE,
            legend.title.size = 1.2,
            legend.text.size = 0.8,
            legend.position = c("left","top"),
            legend.bg.alpha = 1)+
  tm_credits("Source: USDA:NASS", size=0.7, position = c("right","top"))
p4
tmap_save(p4, paste0(paste0(WUDR_github,"/plots/",year,"_operation bins_common_counties.png")),  width = 10, height = 5, units = 'in')
tmap_save(p4, paste0(year,"_operation bins.html"),  width = 10, height = 6.5, units = 'in')

