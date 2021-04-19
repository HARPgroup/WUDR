WUDR_github<-"F:/My Drive/WUDR/WUDR_Github/WUDR_local"
setwd(WUDR_github)

library(tidyverse)
library(tmap)
options(scipen=999)

#Counties 80% withdrawals in census datset but not included in DEQ dataset
plot <- function(YEAR){
summary<-read.csv(paste0(WUDR_github,"/csv_files/", YEAR, "80 perc withdarwal USDA_not_in_DEQ.csv"))
summary <- summary[, c(2,3,4,10)]
UnderTh_dat <- read.csv(paste0(WUDR_github, "/csv_files/",YEAR,"Underthreshold_Summary.csv"))
UnderTh_dat <- UnderTh_dat[,-c(1)]


load(paste0(WUDR_github,"/dat_load/VA_shapefile.RData"))
county.codes <- read_csv(paste0(WUDR_github, "/csv_files/county_codes_census.csv")) 

UnderTh_dat <- merge.data.frame(county.codes,UnderTh_dat, by.x = "County_Name", by.y = "County" )
df.summary <- left_join(summary,UnderTh_dat, by = c("Countycode" = "County_Code"))


df.summary$Irr.Area.above.TH <- ifelse(df.summary$Total.Irri.Area == 0, df.summary$Irr.Area.above.TH == "NA", df.summary$Irr.Area.above.TH )
df.summary$Irr.Area.Under.TH <- ifelse(df.summary$Total.Irri.Area == 0, df.summary$Irr.Area.Under.TH == "NA", df.summary$Irr.Area.Under.TH )


VA_counties@data$Countycode = str_remove(VA_counties@data$COUNTYFP, "^0+")
plotdat<-sp::merge(VA_counties,df.summary, by.x = "Countycode", by.y = "Countycode")

#Check if merge was correct
sum(df.summary$Irr.Area.above.TH, na.rm = TRUE)
sum(plotdat@data$Irr.Area.above.TH, na.rm=TRUE)

tmap_mode("view")

p1<-tm_shape(plotdat)+
  tm_polygons("Irr.Area.Under.TH", title = "Irrigated Area under threshold ",
              # breaks = c(0,1,5,10,20),
              n=3,style="jenks",
              id="NAME",
              legend.hist = TRUE)+
  tm_layout(main.title = paste0(YEAR," Irrigated Area under threshold in counties \nwith more withdrawal in USDA dataset (acres)"),
            legend.outside = TRUE,
            legend.title.size = 1.2,
            legend.text.size = 0.8,
            legend.position = c("left","top"),
            legend.bg.alpha = 1)
p1

tmap_save(p1, paste0(WUDR_github,"/plots/Census_Area_under-TH/",YEAR,"_Irr.Area.Under.TH_census high counties.png"),  width = 10, height = 5, units = 'in')
tmap_save(p1, paste0(WUDR_github,"/plots/Census_Area_under-TH/",YEAR,"_Irr.Area.Under.TH_census high counties.html"),  width = 10, height = 5, units = 'in')


write.csv(df.summary, paste0(WUDR_github,"/csv_files/",YEAR,"_Irr.Area.Under.TH_census high counties.csv"))
}

plot(2002)
plot(2007)
plot(2012)
plot(2017)


#Counties 80% withdrawals in DEQ datset but not included in census dataset
plot2 <- function(YEAR){
  summary<-read.csv(paste0(WUDR_github,"/csv_files/", YEAR, "80 perc withdarwal DEQ_not_in_USDA.csv"))
  summary <- summary[, c(2,3,4,10)]
  UnderTh_dat <- read.csv(paste0(WUDR_github, "/csv_files/",YEAR,"Underthreshold_Summary.csv"))
  UnderTh_dat <- UnderTh_dat[,-c(1)]
  
  
  load(paste0(WUDR_github,"/dat_load/VA_shapefile.RData"))
  county.codes <- read_csv(paste0(WUDR_github, "/csv_files/county_codes_census.csv")) 
  
  UnderTh_dat <- merge.data.frame(county.codes,UnderTh_dat, by.x = "County_Name", by.y = "County" )
  df.summary <- left_join(summary,UnderTh_dat, by = c("Countycode" = "County_Code"))
  
  
  df.summary$Irr.Area.above.TH <- ifelse(df.summary$Total.Irri.Area == 0, df.summary$Irr.Area.above.TH == "NA", df.summary$Irr.Area.above.TH )
  df.summary$Irr.Area.Under.TH <- ifelse(df.summary$Total.Irri.Area == 0, df.summary$Irr.Area.Under.TH == "NA", df.summary$Irr.Area.Under.TH )
  
  
  VA_counties@data$Countycode = str_remove(VA_counties@data$COUNTYFP, "^0+")
  plotdat<-sp::merge(VA_counties,df.summary, by.x = "Countycode", by.y = "Countycode")
  
  #Check if merge was correct
  sum(df.summary$Irr.Area.above.TH, na.rm = TRUE)
  sum(plotdat@data$Irr.Area.above.TH, na.rm=TRUE)
  plotdat@data$Irr.Area.above.TH <- as.numeric(plotdat@data$Irr.Area.above.TH)
  tmap_mode("plot")
  
  # 
  # na_sum <- sum(is.na(plotdat@data$Irr.Area.above.TH))
  # if (na_sum > 2) {
  #   sbreak= 1
  #  } else {
  #     sbreak = 3
  #   }
  # 
  p1<-tm_shape(plotdat)+
    tm_polygons("Irr.Area.Under.TH", title = "Irrigated Area under threshold ",
                # breaks = c(0,1,5,10,20),
                n=3,style="jenks",
                id="NAME",
                legend.hist = TRUE)+
    tm_layout(main.title = paste0(YEAR," Irrigated Area under threshold in counties \nwith more withdrawal in DEQ dataset (acres)"),
              legend.outside = TRUE,
              legend.title.size = 1.2,
              legend.text.size = 0.8,
              legend.position = c("left","top"),
              legend.bg.alpha = 1)
  p1
  
  tmap_save(p1, paste0(WUDR_github,"/plots/Census_Area_under-TH/",YEAR,"_Irr.Area.Under.TH_DEQ high counties.png"),  width = 10, height = 5, units = 'in')
  tmap_save(p1, paste0(WUDR_github,"/plots/Census_Area_under-TH/",YEAR,"_Irr.Area.Under.TH_DEQ high counties.html"),  width = 10, height = 5, units = 'in')
  
  
  write.csv(df.summary, paste0(WUDR_github,"/csv_files/",YEAR,"_Irr.Area.Under.TH_DEQ high counties.csv"))
}

plot2(2002)
plot2(2007)
plot2(2012) # this doesnt work as there is just one county here "Westmoreland county"
plot2(2017) # this doesnt work as there is just one county here "Nelson county"

plot3 <- function(YEAR){
  summary<-read.csv(paste0(WUDR_github,"/csv_files/" , YEAR, "_deq_census_dat_avaliability_summary.csv"))

  summary <- summary[, -c(1)]
  summary <- filter(summary, Data.Status == "Available in USDA dataset")
  
  UnderTh_dat <- read.csv(paste0(WUDR_github, "/csv_files/",YEAR,"Underthreshold_Summary.csv"))
  UnderTh_dat <- UnderTh_dat[,-c(1)]
  
  
  load(paste0(WUDR_github,"/dat_load/VA_shapefile.RData"))
  county.codes <- read_csv(paste0(WUDR_github, "/csv_files/county_codes_census.csv")) 
  
  UnderTh_dat <- merge.data.frame(county.codes,UnderTh_dat, by.x = "County_Name", by.y = "County" )
  df.summary <- left_join(summary,UnderTh_dat, by = c("Countycode" = "County_Code"))
  
  
  df.summary$Irr.Area.above.TH <- ifelse(df.summary$Total.Irri.Area == 0, df.summary$Irr.Area.above.TH == "NA", df.summary$Irr.Area.above.TH )
  df.summary$Irr.Area.Under.TH <- ifelse(df.summary$Total.Irri.Area == 0, df.summary$Irr.Area.Under.TH == "NA", df.summary$Irr.Area.Under.TH )
  
  
  VA_counties@data$Countycode = str_remove(VA_counties@data$COUNTYFP, "^0+")
  plotdat<-sp::merge(VA_counties,df.summary, by.x = "Countycode", by.y = "Countycode")
  
  #Check if merge was correct
  sum(df.summary$Irr.Area.above.TH, na.rm = TRUE)
  sum(plotdat@data$Irr.Area.above.TH, na.rm=TRUE)
  
  tmap_mode("plot")
  
  p1<-tm_shape(plotdat)+
    tm_polygons("Irr.Area.Under.TH", title = "Irrigated Area under threshold ",
                # breaks = c(0,1,5,10,20),
                n=3,style="jenks",
                id="NAME",
                legend.hist = TRUE)+
    tm_layout(main.title = paste0(YEAR," Irrigated Area under threshold in counties \nwith only census data (acres)"),
              legend.outside = TRUE,
              legend.title.size = 1.2,
              legend.text.size = 0.8,
              legend.position = c("left","top"),
              legend.bg.alpha = 1)
  p1
  
  tmap_save(p1, paste0(WUDR_github,"/plots/Census_Area_under-TH/",YEAR,"_Irr.Area.Under.TH_only census counties.png"),  width = 10, height = 5, units = 'in')
  tmap_save(p1, paste0(WUDR_github,"/plots/Census_Area_under-TH/",YEAR,"_Irr.Area.Under.TH_only census counties.html"),  width = 10, height = 5, units = 'in')
  
  
  write.csv(df.summary, paste0(WUDR_github,"/csv_files/",YEAR,"_Irr.Area.Under.TH_only census counties.csv"))
}

plot3(2002)
plot3(2007)
plot3(2012)
plot3(2017)

plot4 <- function(YEAR){
  summary<-read.csv(paste0(WUDR_github,"/csv_files/" , YEAR, "_deq_census_dat_avaliability_summary.csv"))
  
  summary <- summary[, -c(1)]
  summary <- filter(summary, Data.Status == "Missing in both datsets")
  
  UnderTh_dat <- read.csv(paste0(WUDR_github, "/csv_files/",YEAR,"Underthreshold_Summary.csv"))
  UnderTh_dat <- UnderTh_dat[,-c(1)]
  
  
  load(paste0(WUDR_github,"/dat_load/VA_shapefile.RData"))
  county.codes <- read_csv(paste0(WUDR_github, "/csv_files/county_codes_census.csv")) 
  
  UnderTh_dat <- merge.data.frame(county.codes,UnderTh_dat, by.x = "County_Name", by.y = "County" )
  df.summary <- left_join(summary,UnderTh_dat, by = c("Countycode" = "County_Code"))
  
  
  df.summary$Irr.Area.above.TH <- ifelse(df.summary$Total.Irri.Area == 0, df.summary$Irr.Area.above.TH == "NA", df.summary$Irr.Area.above.TH )
  df.summary$Irr.Area.Under.TH <- ifelse(df.summary$Total.Irri.Area == 0, df.summary$Irr.Area.Under.TH == "NA", df.summary$Irr.Area.Under.TH )
  
  
  VA_counties@data$Countycode = str_remove(VA_counties@data$COUNTYFP, "^0+")
  plotdat<-sp::merge(VA_counties,df.summary, by.x = "Countycode", by.y = "Countycode")
  
  #Check if merge was correct
  sum(df.summary$Irr.Area.above.TH, na.rm = TRUE)
  sum(plotdat@data$Irr.Area.above.TH, na.rm=TRUE)
  
  tmap_mode("plot")
  
  p1<-tm_shape(plotdat)+
    tm_polygons("Irr.Area.Under.TH", title = "Irrigated Area under threshold ",
                # breaks = c(0,1,5,10,20),
                n=3,style="jenks",
                id="NAME",
                legend.hist = TRUE)+
    tm_layout(main.title = paste0(YEAR," Irrigated Area under threshold in counties \nwith data missing in both datsets (acres)"),
              legend.outside = TRUE,
              legend.title.size = 1.2,
              legend.text.size = 0.8,
              legend.position = c("left","top"),
              legend.bg.alpha = 1)
  p1
  
  tmap_save(p1, paste0(WUDR_github,"/plots/Census_Area_under-TH/",YEAR,"_Irr.Area.Under.TH_missingboth datasets.png"),  width = 10, height = 5, units = 'in')
  tmap_save(p1, paste0(WUDR_github,"/plots/Census_Area_under-TH/",YEAR,"_Irr.Area.Under.TH_missingboth datasets.html"),  width = 10, height = 5, units = 'in')
  
  
  write.csv(df.summary, paste0(WUDR_github,"/csv_files/",YEAR,"_Irr.Area.Under.TH_missingboth datasets.csv"))
}

plot4(2002)
plot4(2007)
plot4(2012) #Zero Area under Irr
plot4(2017) # Zero area under Irr


plot5 <- function(YEAR){
  summary<-read.csv(paste0(WUDR_github,"/csv_files/" , YEAR, "_deq_census_dat_avaliability_summary.csv"))
  
  summary <- summary[, -c(1)]
  summary <- filter(summary, Data.Status == "Available in both datasets")
  
  UnderTh_dat <- read.csv(paste0(WUDR_github, "/csv_files/",YEAR,"Underthreshold_Summary.csv"))
  UnderTh_dat <- UnderTh_dat[,-c(1)]
  
  
  load(paste0(WUDR_github,"/dat_load/VA_shapefile.RData"))
  county.codes <- read_csv(paste0(WUDR_github, "/csv_files/county_codes_census.csv")) 
  
  UnderTh_dat <- merge.data.frame(county.codes,UnderTh_dat, by.x = "County_Name", by.y = "County" )
  df.summary <- left_join(summary,UnderTh_dat, by = c("Countycode" = "County_Code"))
  
  
  df.summary$Irr.Area.above.TH <- ifelse(df.summary$Total.Irri.Area == 0, df.summary$Irr.Area.above.TH == "NA", df.summary$Irr.Area.above.TH )
  df.summary$Irr.Area.Under.TH <- ifelse(df.summary$Total.Irri.Area == 0, df.summary$Irr.Area.Under.TH == "NA", df.summary$Irr.Area.Under.TH )
  
  
  VA_counties@data$Countycode = str_remove(VA_counties@data$COUNTYFP, "^0+")
  plotdat<-sp::merge(VA_counties,df.summary, by.x = "Countycode", by.y = "Countycode")
  
  #Check if merge was correct
  sum(df.summary$Irr.Area.above.TH, na.rm = TRUE)
  sum(plotdat@data$Irr.Area.above.TH, na.rm=TRUE)
  
  tmap_mode("plot")
  
  p1<-tm_shape(plotdat)+
    tm_polygons("Irr.Area.Under.TH", title = "Irrigated Area under threshold ",
                # breaks = c(0,1,5,10,20),
                n=3,style="jenks",
                id="NAME",
                legend.hist = TRUE)+
    tm_layout(main.title = paste0(YEAR," Irrigated Area under threshold in counties \nwith data available in both datsets (acres)"),
              legend.outside = TRUE,
              legend.title.size = 1.2,
              legend.text.size = 0.8,
              legend.position = c("left","top"),
              legend.bg.alpha = 1)
  p1
  
  tmap_save(p1, paste0(WUDR_github,"/plots/Census_Area_under-TH/",YEAR,"_Irr.Area.Under.TH_availableboth datasets.png"),  width = 10, height = 5, units = 'in')
  tmap_save(p1, paste0(WUDR_github,"/plots/Census_Area_under-TH/",YEAR,"_Irr.Area.Under.TH_availableboth datasets.html"),  width = 10, height = 5, units = 'in')
  
  
  write.csv(df.summary, paste0(WUDR_github,"/csv_files/",YEAR,"_Irr.Area.Under.TH_availableboth datasets.csv"))
}

plot5(2002)
plot5(2007)
plot5(2012)
plot5(2017)

plot6 <- function(YEAR){
  summary<-read.csv(paste0(WUDR_github,"/csv_files/" , YEAR, "_deq_census_dat_avaliability_summary.csv"))
  
  summary <- summary[, -c(1)]
  summary <- filter(summary, Data.Status == "Available in DEQ dataset")
  
  UnderTh_dat <- read.csv(paste0(WUDR_github, "/csv_files/",YEAR,"Underthreshold_Summary.csv"))
  UnderTh_dat <- UnderTh_dat[,-c(1)]
  
  
  load(paste0(WUDR_github,"/dat_load/VA_shapefile.RData"))
  county.codes <- read_csv(paste0(WUDR_github, "/csv_files/county_codes_census.csv")) 
  
  UnderTh_dat <- merge.data.frame(county.codes,UnderTh_dat, by.x = "County_Name", by.y = "County" )
  df.summary <- left_join(summary,UnderTh_dat, by = c("Countycode" = "County_Code"))
  
  
  df.summary$Irr.Area.above.TH <- ifelse(df.summary$Total.Irri.Area == 0, df.summary$Irr.Area.above.TH == "NA", df.summary$Irr.Area.above.TH )
  df.summary$Irr.Area.Under.TH <- ifelse(df.summary$Total.Irri.Area == 0, df.summary$Irr.Area.Under.TH == "NA", df.summary$Irr.Area.Under.TH )
  
  
  VA_counties@data$Countycode = str_remove(VA_counties@data$COUNTYFP, "^0+")
  plotdat<-sp::merge(VA_counties,df.summary, by.x = "Countycode", by.y = "Countycode")
  
  #Check if merge was correct
  sum(df.summary$Irr.Area.above.TH, na.rm = TRUE)
  sum(plotdat@data$Irr.Area.above.TH, na.rm=TRUE)
  
  tmap_mode("plot")
  
  p1<-tm_shape(plotdat)+
    tm_polygons("Irr.Area.Under.TH", title = "Irrigated Area under threshold ",
                # breaks = c(0,1,5,10,20),
                n=3,style="jenks",
                id="NAME",
                legend.hist = TRUE)+
    tm_layout(main.title = paste0(YEAR," Irrigated Area under threshold in counties \nwith data available in DEQ datset (acres)"),
              legend.outside = TRUE,
              legend.title.size = 1.2,
              legend.text.size = 0.8,
              legend.position = c("left","top"),
              legend.bg.alpha = 1)
  p1
  
  tmap_save(p1, paste0(WUDR_github,"/plots/Census_Area_under-TH/",YEAR,"_Irr.Area.Under.TH_availableDEQ datasets.png"),  width = 10, height = 5, units = 'in')
  tmap_save(p1, paste0(WUDR_github,"/plots/Census_Area_under-TH/",YEAR,"_Irr.Area.Under.TH_availableDEQ datasets.html"),  width = 10, height = 5, units = 'in')
  
  
  write.csv(df.summary, paste0(WUDR_github,"/csv_files/",YEAR,"_Irr.Area.Under.TH_availableDEQ datasets.csv"))
}

plot6(2002)
plot6(2007)
plot6(2012)
plot6(2017)




