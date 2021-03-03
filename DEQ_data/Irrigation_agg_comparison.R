WUDR_github<-"F:/My Drive/WUDR/WUDR_Github/WUDR_local"
setwd(WUDR_github)

pacman::p_load(tidyverse, rgdal, tmap)
year=2017
deq_dat<-read.csv(paste0(WUDR_github,"/csv_files/2017withdrawal_DEQ.csv"))
VA_counties<-readOGR("F:/My Drive/VA_shapefile_updated", layer="VA_counties_new")
County_names<-VA_counties@data[,c(1,10)]

levels(deq_dat$Use.Type)

irr_data<- dplyr::filter(deq_dat, Use.Type =="irrigation")
agg_data<- dplyr::filter(deq_dat, Use.Type =="agriculture")


Irrigation_multiple_intakes<-irr_data%>%
  dplyr::group_by(Facility,Month,FIPS.Code)%>%
  dplyr::summarise(F_HydroID=first(Facility_hydroid),
                   Facility_withdrawal_mgm=sum(Water.Use.MGM))

Irrigation_facilities<-Irrigation_multiple_intakes%>%
  dplyr::group_by(Facility,FIPS.Code)%>%
  dplyr::summarise(F_HydroID=first(F_HydroID),
                   Facility_withdrawal_mgm=sum(Facility_withdrawal_mgm))


Agriculture_multiple_intakes<-agg_data%>%
  dplyr::group_by(Facility,Month,FIPS.Code)%>%
  dplyr::summarise(F_HydroID=first(Facility_hydroid),
                   Facility_withdrawal_mgm=sum(Water.Use.MGM))

Agriculture_facilities<-Agriculture_multiple_intakes%>%
  dplyr::group_by(Facility,FIPS.Code)%>%
  dplyr::summarise(F_HydroID=first(F_HydroID),
                   Yearly_F_withdrawal_mg=sum(Facility_withdrawal_mgm))

dat_summer<-agg_data %>%
  filter(between(Month, 5, 9))


Summer_agg_summary_multiple_intakes<-dat_summer%>%
  dplyr::group_by(Facility,Month,FIPS.Code)%>%
  dplyr::summarise(F_HydroID=first(Facility_hydroid),
                   Facility_withdrawal_mgm=sum(Water.Use.MGM))

Summer_agg_summary_facilities<-Summer_agg_summary_multiple_intakes%>%
  dplyr::group_by(Facility,FIPS.Code)%>%
  dplyr::summarise(F_HydroID=first(F_HydroID),
                   Summer_F_withdrawal_mg=sum(Facility_withdrawal_mgm))

summer_agg_withdrawals<-merge.data.frame(Summer_agg_summary_facilities,Agriculture_facilities[,c(3,4)], by="F_HydroID", all.x = TRUE)

summer_agg_withdrawals$percetage_summer_withdarwals<- 100*summer_agg_withdrawals$Summer_F_withdrawal_mg/summer_agg_withdrawals$Yearly_F_withdrawal_mg

summer_agg_withdrawals<-merge.data.frame(summer_agg_withdrawals,County_names, by.x = "FIPS.Code",by.y = "GEOID", all.x = TRUE)

summer_agg_withdrawals<-summer_agg_withdrawals[,c(1,7,3,2,4,5,6)]

summer_agg_withdrawals<-arrange(summer_agg_withdrawals, desc(percetage_summer_withdarwals))

summer_agg_withdrawals<-cbind.data.frame(summer_agg_withdrawals[,c(1:4)],(round(summer_agg_withdrawals[,c(5:7)], 2)))


write.csv(summer_agg_withdrawals,paste0(WUDR_github,"/csv_files/All_ag_facities_summer_withdrawals.csv"))


dat_feb<-agg_data %>%
  filter(Month ==2)

Feb_agg_summary_multiple_intakes<-dat_feb%>%
  dplyr::group_by(Facility,Month,FIPS.Code)%>%
  dplyr::summarise(F_HydroID=first(Facility_hydroid),
                   Facility_withdrawal_mgm=sum(Water.Use.MGM))

Feb_agg_summary_facilities<-Feb_agg_summary_multiple_intakes%>%
  dplyr::group_by(Facility,FIPS.Code)%>%
  dplyr::summarise(F_HydroID=first(F_HydroID),
                   Summer_F_withdrawal_mg=sum(Facility_withdrawal_mgm))

Summer_agg_summary_facilities$permonth <- Summer_agg_summary_facilities$Summer_F_withdrawal_mg/5

Summer_agg_summary_facilities$dif_feb<- Summer_agg_summary_facilities$permonth-Feb_agg_summary_facilities$Summer_F_withdrawal_mg

##################################
Winter_dat<- dat_feb<-agg_data %>%
  filter(Month %in% c(2,5,6,7,8,9))


Monthly_dat<- dat_feb<-agg_data %>%
  filter(Month %in% c(2,5,6,7,8,9))
  
  
Monthly_dat <- Monthly_dat[, c(5,6,14,8,10)]  
  
Monthly_agg_summary_multiple_intakes<-Monthly_dat%>%
  dplyr::group_by(Facility,Month,FIPS.Code)%>%
  dplyr::summarise(F_HydroID=first(Facility_hydroid),
                   Facility_withdrawal_mgm=sum(Water.Use.MGM))

Monthly_agg_summary_multiple_intakes$Month<- month.abb[Monthly_agg_summary_multiple_intakes$Month]
wide_dat_monthly <- Monthly_agg_summary_multiple_intakes %>%
  pivot_wider(values_from = Facility_withdrawal_mgm, names_from = Month)

wide_dat_monthly<-wide_dat_monthly[complete.cases(wide_dat_monthly), ]

colnames(wide_dat_monthly)

Diff_feb<-wide_dat_monthly

for( i in 4:length(wide_dat_monthly)){
  
  Diff_feb[i]<- (wide_dat_monthly[,i]- wide_dat_monthly[,4])*100/wide_dat_monthly[,4]
}

Diff_feb$mean <-  rowMeans(Diff_feb[,c(5:9)], na.rm=TRUE)

Summer_percentage_Difference<- filter(Diff_feb, mean > 0)  


Summer_percentage_Difference[is.na(Summer_percentage_Difference)] <- 0


Summer_percentage_Difference<- cbind.data.frame(Summer_percentage_Difference[,c(1:3)], 
                                                round(Summer_percentage_Difference[, c(5:10)], 2))
col <- paste0("Per_Diff_", colnames(Summer_percentage_Difference[,c(4:9)]))
colnames(Summer_percentage_Difference)[4:9]<- col

Summer_percentage_Difference<- merge.data.frame( wide_dat_monthly, Summer_percentage_Difference[, c(3:9)], by.x= "F_HydroID" , by.y = "F_HydroID")
                                                 
                                                 
Summer_percentage_Difference <- arrange(Summer_percentage_Difference, desc(Per_Diff_mean))                                            
                                                 
                                

write.csv(Summer_percentage_Difference,paste0(WUDR_github,"/csv_files/Summer_Withdrawals_percentage_Difference.csv"))
