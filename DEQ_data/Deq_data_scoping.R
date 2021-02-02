WUDR_github<-"F:/My Drive/WUDR/WUDR_Github/WUDR_local"
setwd(WUDR_github)

pacman::p_load(dplyr, rgdal, tmap)
year=2017
deq_dat<-read.csv(paste0(WUDR_github,"/csv_files/2017withdrawal_DEQ.csv"))
levels(deq_dat$Use.Type)

#Get the withdrawal dat for use type of irrigation only
deq_dat<-dplyr::filter(deq_dat, Use.Type =="irrigation")

# winter_dat<-dplyr::filter(deq_dat, Month <=4)
# sum(winter_dat$Water.Use.MGM)
# unique(winter_dat$Facility)

summary_multiple_intakes<-deq_dat%>%
  dplyr::group_by(Facility,Month,FIPS.Code)%>%
  dplyr::summarise(F_HydroID=first(Facility_hydroid),
                   Facility_withdrawal_mgm=sum(Water.Use.MGM))

summary_facilities<-summary_multiple_intakes%>%
  dplyr::group_by(Facility,FIPS.Code)%>%
  dplyr::summarise(F_HydroID=first(F_HydroID),
                   Facility_withdrawal_mgm=sum(Facility_withdrawal_mgm))

summary_counties<-summary_facilities%>%
  dplyr::group_by(FIPS.Code)%>%
  dplyr::summarise(F_HydroID=first(F_HydroID),
                   Count=n(),
                   Facility_withdrawal_mg=sum(Facility_withdrawal_mgm))


summary_counties$withdrawal.per.facility<-summary_counties$Facility_withdrawal_mg/summary_counties$Count


VA_counties<-readOGR("F:/My Drive/VA_shapefile_updated", layer="VA_counties_new")

plotdat<-sp::merge(VA_counties,summary_counties, 
                   by.x = "GEOID", by.y = "FIPS.Code", all.x=TRUE)

summary<-plotdat@data[,c(1,3,6,10,12,13,14)]
# write.csv(summary, paste0("deqdat_summary", year,".csv"))


tmap_mode("plot")
p1<-tm_shape(plotdat)+
  tm_polygons("Count", title = "Irrigated operations",
              # breaks = c(0,1,5,10,20),
              n=5,style="jenks",
              id="NAMELSAD",
              legend.hist = TRUE)+
  tm_layout(main.title = paste0(year," Number of Irrigated operations (DEQ)"),
            legend.outside = TRUE,
            legend.title.size = 1.2,
            legend.text.size = 0.8,
            legend.position = c("left","top"),
            legend.bg.alpha = 1)+
  tm_credits("Source: DEQ", size=0.7, position = c("right","top"))
p1

# tmap_save(p1, paste0(year,"DEQ_irrigated_facilites.png"),  width = 10, height = 6.5, units = 'in')

p2<-tm_shape(plotdat)+
  tm_polygons("Facility_withdrawal_mg", title = "Facility withdrawals",
              # breaks = c(0,1,5,10,20),
              n=5,style="jenks",
              id="NAMELSAD",
              legend.hist = TRUE)+
  tm_layout(main.title = paste0(year," Facility withdrawals (DEQ)"),
            legend.outside = TRUE,
            legend.title.size = 1.2,
            legend.text.size = 0.8,
            legend.position = c("left","top"),
            legend.bg.alpha = 1)+
  tm_credits("Source: DEQ", size=0.7, position = c("right","top"))
p2

# tmap_save(p2, paste0(year,"DEQ_withdrawals.png"),  width = 10, height = 6.5, units = 'in')
