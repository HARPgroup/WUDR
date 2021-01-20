WUDR_github<-"F:/My Drive/WUDR/WUDR_Github/WUDR_local"
setwd(WUDR_github)

pacman::p_load(dplyr, rgdal, tmap)
year=2017
deq_dat<-read.csv(paste0(WUDR_github,"/csv_files/deqdat_summary2017.csv"))
data_deq<-deq_dat[complete.cases(deq_dat),]

census_dat<-read.csv(paste0(WUDR_github,"/csv_files/2017_Census_Summary.csv"))
census_dat<-merge.data.frame(census_dat,deq_dat[,c("GEOID","COUNTYFP")], by.x = "COUNTY_CODE", by.y ="COUNTYFP", all.x = TRUE )

data_census<-census_dat[complete.cases(census_dat),]

data_deq$data_status<- data_deq$GEOID %in% data_census$GEOID
data_deq$data_status<-ifelse(data_deq$data_status == TRUE, "Available in both datasets", "Available in DEQ dataset")

data_census$data_status<-data_census$GEOID%in% data_deq$GEOID
data_census$data_status<-ifelse(data_census$data_status == TRUE, "Available in both datasets", "Available in USDA dataset")

dat1<-merge.data.frame(data_census[,c("GEOID", "data_status")],
                       data_deq[,c("GEOID", "data_status")], by.x="GEOID", by.y="GEOID", all.x = TRUE, all.y=TRUE)

dat1$data_status.x <- ifelse(is.na(dat1$data_status.x), dat1$data_status.y, dat1$data_status.x)


deq_dat$status<-deq_dat$GEOID %in% dat1$GEOID
deq_dat_missing<-filter(deq_dat, status== FALSE)
deq_dat_missing$status[deq_dat_missing$status==FALSE] <- c("Missing in both datsets")

dat_final<-as.data.frame(mapply(c,dat1[,c("GEOID", "data_status.x")],deq_dat_missing[,c("GEOID","status")]))
colnames(dat_final)[2]<- "Data Status"

VA_counties<-readOGR(paste0(WUDR_github,"/VA_counties_sp"), layer="VA_counties_new")

plotdat<-sp::merge(VA_counties,dat_final, 
                   by.x = "GEOID", by.y = "GEOID", all.x=TRUE)

summary<-plotdat@data[,c(1,3,10,11)]
write.csv(summary,paste0(WUDR_github,"/csv_files/deqdat_avaliability_summary", year,".csv"))

dev.off()
MYpal<-c("#1c9099", "#756bb1","#fdbb84","#bcbddc")
tmap_mode("plot")
p1<-tm_shape(plotdat)+
  tm_polygons("Data Status", title = "Status",
              # breaks = c(0,1,5,10,20),
              #n=5,style="jenks",
              palette = MYpal,
              id="NAMELSAD",
              legend.hist = TRUE)+
  tm_layout(main.title = paste0(year," Data availability from USDA census and DEQ reporting"),
            legend.outside = TRUE,
            legend.title.size = 1,
            legend.text.size = 0.8,
            legend.position = c("left","top"),
            legend.bg.alpha = 1)
p1

tmap_save(p1, paste0(WUDR_github,"/plots/DEQ_irrigated_facilites-1.png"),  width = 10, height = 5, units = 'in')
tmap_save(p1, paste0(WUDR_github,"/plots/DEQ_irrigated_facilites-1.html"),  width = 10, height = 6.5, units = 'in')
