
WUDR_github<-"F:/My Drive/WUDR/WUDR_Github/WUDR_local"
setwd(WUDR_github)

library(tidyverse)
library(tmap)
library(rgdal)
library(stringr)

load(paste0(WUDR_github,"/dat_load/VA_shapefile.RData"))
county.codes <- read_csv(paste0(WUDR_github, "/csv_files/county_codes_census.csv")) 

df.summary <- read_csv(paste0(WUDR_github, "/csv_files/Percentage Irri.data.Under TH.csv")) 


df.summary <- df.summary[,-1]

df.summary <- merge.data.frame(df.summary, county.codes, by.x = "County", by.y = "County_Name")

df.summary$Irr.Area.above.TH <- ifelse(df.summary$Total.Irri.Area == 0, df.summary$Irr.Area.above.TH == NA, df.summary$Irr.Area.above.TH )
df.summary$Irr.Area.Under.TH <- ifelse(df.summary$Total.Irri.Area == 0, df.summary$Irr.Area.Under.TH == NA, df.summary$Irr.Area.Under.TH )


VA_counties@data$Countycode = str_remove(VA_counties@data$COUNTYFP, "^0+")
plotdat<-sp::merge(VA_counties,df.summary, by.x = "Countycode", by.y = "County_Code")

#Check if merge was correct
sum(df.summary$Irr.Area.above.TH, na.rm = TRUE)
sum(plotdat@data$Irr.Area.above.TH, na.rm=TRUE)

p1<-tm_shape(plotdat)+
  tm_polygons("Irr.Area.Under.TH", title = "Irrigated Area ",
              # breaks = c(0,1,5,10,20),
              n=5,style="jenks",
              id="NAMELSAD",
              legend.hist = TRUE)+
  tm_layout(main.title = paste0(2017," Irrigated Area under threshold  (acres)"),
            legend.outside = TRUE,
            legend.title.size = 1.2,
            legend.text.size = 0.8,
            legend.position = c("left","top"),
            legend.bg.alpha = 1)
p1

tmap_save(p1, paste0(WUDR_github,"/plots/Irr.Area.Under.TH.png"),  width = 10, height = 5, units = 'in')
map_save(p1, paste0(WUDR_github,"/plots/Irr.Area.Under.TH.html"),  width = 10, height = 5, units = 'in')

p2<-tm_shape(plotdat)+
  tm_polygons("Pct.under.TH.of.total.Irr.area", title = "Percentage Under threshold ",
              # breaks = c(0,1,5,10,20),
              n=5,style="jenks",
              id="NAMELSAD",
              legend.hist = TRUE)+
  tm_layout(main.title = paste0(2017," Percentage of total irrigated area under threshold "),
            legend.outside = TRUE,
            legend.title.size = 1.2,
            legend.text.size = 0.8,
            legend.position = c("left","top"),
            legend.bg.alpha = 1)
p2

tmap_save(p2, paste0(WUDR_github,"/plots/Pct.under.TH.of.total.Irr.area.png"),  width = 10, height = 5, units = 'in')
tmap_save(p2, paste0(WUDR_github,"/plots/Pct.under.TH.of.total.Irr.area.html"),  width = 10, height = 5, units = 'in')

p3<-tm_shape(plotdat)+
  tm_polygons("Pct.under.TH.of.Irri.area.abv.TH", title = "Percentage Under threshold ",
              breaks = c(0,42,125,210,380,515, Inf),
              labels = c('0-42','42-125','125-210','210-380', '380-515', 
                         'All Irrigated area under threshhold'),
              # n=5,
              id="NAMELSAD",
              legend.hist = TRUE)+
  tm_layout(main.title = paste0(2017," Percentage Irrigated area under threshold of irrigated area above threshold "),
            legend.outside = TRUE,
            legend.title.size = 1.2,
            legend.text.size = 0.8,
            legend.position = c("left","top"),
            legend.bg.alpha = 1)
# 
#   tm_add_legend(
#     labels = c('0-42','42-125','125-210','210-380', '380-515', 
#                'All Irrigated area under threshhold'))

p3

tmap_save(p3, paste0(WUDR_github,"/plots/Pct.under.TH.of.Irri.area.abv.TH.png"),  width = 10, height = 5, units = 'in')
tmap_save(p3, paste0(WUDR_github,"/plots/Pct.under.TH.of.total.Irr.area.html"),  width = 10, height = 5, units = 'in')

