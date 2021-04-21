WUDR_github<-"F:/My Drive/WUDR/WUDR_Github/WUDR_local"
setwd(WUDR_github)

pacman::p_load(dplyr, rgdal, tmap)
year=2002
deq_dat<-read.csv(paste0(WUDR_github,"/csv_files/2002withdrawal_DEQ.csv"))
VA_counties<-readOGR("F:/My Drive/VA_shapefile_updated", layer="VA_counties_new")

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

##
#Get the withdrawal dat for use type of agriculture only
deq_agr<-dplyr::filter(deq_dat, Use.Type =="agriculture")

agg_summary_multiple_intakes<-deq_agr%>%
  dplyr::group_by(Facility,Month,FIPS.Code)%>%
  dplyr::summarise(F_HydroID=first(Facility_hydroid),
                   Facility_withdrawal_mgm=sum(Water.Use.MGM))

agg_summary_facilities<-agg_summary_multiple_intakes%>%
  dplyr::group_by(Facility,FIPS.Code)%>%
  dplyr::summarise(F_HydroID=first(F_HydroID),
                   Facility_withdrawal_mg=sum(Facility_withdrawal_mgm))

# agg_summary_counties<-agg_summary_facilities%>%
#   dplyr::group_by(FIPS.Code)%>%
#   dplyr::summarise(F_HydroID=first(F_HydroID),
#                    Count=n(),
#                    Facility_withdrawal_mg=sum(Facility_withdrawal_mg))

agg_dat<-sp::merge(VA_counties,agg_summary_counties, 
                       by.x = "GEOID", by.y = "FIPS.Code", all.x=TRUE)

countynames<-VA_counties@data[,c(1,10)] 
agg_summary_facilities<-merge.data.frame(countynames,agg_summary_facilities, 
                                         by.x = "GEOID", by.y = "FIPS.Code", all.y=TRUE)

# agg_summary<-agg_dat@data[,c(1,3,6,10,12,13)]

USDA_not_in_DEQ<-read.csv(paste0(WUDR_github,"/csv_files/80 perc withdarwal USDA_not_in_DEQ.csv"))

USDA_not_in_DEQ_agg<-agg_summary_facilities %>%
  filter(GEOID %in% USDA_not_in_DEQ$GEOID)

USDA_not_in_DEQ_agg<-merge.data.frame(USDA_not_in_DEQ_agg[,c(1,3,4,5)],
                                           USDA_not_in_DEQ, by.x = "GEOID", by.y = "GEOID")
USDA_not_in_DEQ_agg<-dplyr::select(USDA_not_in_DEQ_agg,-c("X"))                              

# write.csv(USDA_not_in_DEQ_agg, paste0(WUDR_github,"/csv_files/Agriculture Faciltiy details USDA_not_in_DEQ.csv"))

################################################
# Get Counties with higher rank in Cenus and lower in USDA#
####################################
#
rank_dat<-read.csv(paste0(WUDR_github,"/csv_files/Rand and Difference in Ranks.csv"))

colnames(rank_dat)

high_diff_counties<- rank_dat %>%
  slice_max(order_by = Acreage_withdarwal_difference, n = 5)
  
high_rank_diff_counties_agg<-agg_summary_facilities %>%
  filter(GEOID %in% high_diff_counties$GEOID)

High_rank_counties<-high_diff_counties[,c(2,3)]
High_USDA_acregae_counties<-USDA_not_in_DEQ[,c(3,4)]
USDA_high_counties<-rbind.data.frame(High_rank_counties,High_USDA_acregae_counties)


##Agg data for counties with High acreage in USDA
dat<-deq_agr %>%
  filter(FIPS.Code %in% USDA_high_counties$GEOID)

###Filter for Summer months 

dat_summer<-dat %>%
  filter(between(Month, 5, 9))


Summer_agg_summary_multiple_intakes<-dat_summer%>%
  dplyr::group_by(Facility,Month,FIPS.Code)%>%
  dplyr::summarise(F_HydroID=first(Facility_hydroid),
                   Facility_withdrawal_mgm=sum(Water.Use.MGM))

Summer_agg_summary_facilities<-Summer_agg_summary_multiple_intakes%>%
  dplyr::group_by(Facility,FIPS.Code)%>%
  dplyr::summarise(F_HydroID=first(F_HydroID),
                   Facility_withdrawal_mg=sum(Facility_withdrawal_mgm))

summer_agg_withdrawals<-merge.data.frame(Summer_agg_summary_facilities,agg_summary_facilities[,c(2,4,5)], by="F_HydroID", all.x = TRUE)

summer_agg_withdrawals$percetage_summer_withdarwals<- 100*summer_agg_withdrawals$Facility_withdrawal_mg.x/summer_agg_withdrawals$Facility_withdrawal_mg.y
  
write.csv(summer_agg_withdrawals,paste0(WUDR_github,"/csv_files/summer_agg_withdrawals.csv"))
