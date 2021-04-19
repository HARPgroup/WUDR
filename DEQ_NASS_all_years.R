WUDR_github<-"F:/My Drive/WUDR/WUDR_Github/WUDR_local"
setwd(WUDR_github)

pacman::p_load(dplyr, rgdal, tmap, sqldf,ggpubr,expss, stringr, gridExtra)
options(scipen=999)


## Load shapefile

load(paste0(WUDR_github,"/dat_load/VA_shapefile.RData"))
VA_counties@data$Countycode = str_remove(VA_counties@data$COUNTYFP, "^0+")
##Load Summary to get the data in both datasets

YEAR = 2017
Census_DEQ_data_availability <- function(YEAR){
deq_dat<-read.csv(paste0(WUDR_github,"/csv_files/", YEAR, "withdrawal_DEQ.csv"))
census_dat<- read.csv(paste0(WUDR_github,"/csv_files/", YEAR, "County_data_NASS.csv"))
census_dat <- census_dat[,-c(1)]

levels(deq_dat$Use.Type)
unique(deq_dat$Year)

#Get the withdrawal dat for use type of irrigation only

deq_dat<-dplyr::filter(deq_dat, Use.Type =="irrigation")

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

summary_deq<-plotdat@data[,c(1,11,6,10,12,13,14)]

tmap_mode("plot")
p1<-tm_shape(plotdat)+
  tm_polygons("Count", title = "Irrigated operations",
              # breaks = c(0,1,5,10,20),
              n=5,style="jenks",
              id="NAMELSAD",
              legend.hist = TRUE)+
  tm_layout(main.title = paste0(YEAR," Number of Irrigated operations (DEQ)"),
            legend.outside = TRUE,
            legend.title.size = 1.2,
            legend.text.size = 0.8,
            legend.position = c("left","top"),
            legend.bg.alpha = 1)+
  tm_credits("Source: DEQ", size=0.7, position = c("right","top"))
p1

 #tmap_save(p1, paste0(WUDR_github,"/plots/DEQ and Census Data Avaliability/",YEAR,"DEQ_irrigated_facilites.png"),  width = 10, height = 6.5, units = 'in')

p2<-tm_shape(plotdat)+
  tm_polygons("Facility_withdrawal_mg", title = "Facility withdrawals",
              # breaks = c(0,1,5,10,20),
              n=5,style="jenks",
              id="NAMELSAD",
              legend.hist = TRUE)+
  tm_layout(main.title = paste0(YEAR," Facility withdrawals (DEQ)"),
            legend.outside = TRUE,
            legend.title.size = 1.2,
            legend.text.size = 0.8,
            legend.position = c("left","top"),
            legend.bg.alpha = 1)+
  tm_credits("Source: DEQ", size=0.7, position = c("right","top"))
p2

#tmap_save(p2, paste0(WUDR_github,"/plots/DEQ and Census Data Avaliability/", YEAR,"DEQ_withdrawals.png"),  width = 10, height = 6.5, units = 'in')


#################################################################################
############## DATA AVALIABLE IN BOTH DATSETS ###################################
#################################################################################

census_dat<-merge.data.frame(census_dat,summary_deq[,c("GEOID","Countycode")], by.x = "County_Code", by.y ="Countycode", all.x = TRUE )
data_census<-census_dat[!(census_dat$Irrigated_Acreage=="-9999"),]

data_deq<-summary_deq[complete.cases(summary_deq),]

data_deq$data_status<- data_deq$GEOID %in% data_census$GEOID
data_deq$data_status<-ifelse(data_deq$data_status == TRUE, "Available in both datasets", "Available in DEQ dataset")

data_census$data_status<-data_census$GEOID%in% data_deq$GEOID
data_census$data_status<-ifelse(data_census$data_status == TRUE, "Available in both datasets", "Available in USDA dataset")

#############################
#Histogrmas for counties  ##
############################
p3<-data_census%>%
  filter(data_status=="Available in both datasets")%>%
  ggplot(aes(x=Irrigated_Acreage))+
  geom_histogram()+
  labs(subtitle = paste0(YEAR,": Irrigated Acreage for data in both DEQ and Census datsets"), 
       x= "Irrigated Acreage", y="Count")+
  theme_light()


p3
#ggsave(paste0(WUDR_github,"/plots/DEQ and Census Data Avaliability/", YEAR, "Hist Irrigated acreage both datasets.png"),p3, width = 8, height = 5, units="in")


p4<-data_census%>%
  filter(data_status=="Available in USDA dataset")%>%
  ggplot(aes(x=Irrigated_Acreage))+
  geom_histogram()+
  labs(subtitle = paste0(YEAR,":Irrigated Acreage for data in only Census datset"), 
       x= "Irrigated Acreage", y="Count")+
  theme_light()


p4
#ggsave(paste0(WUDR_github,"/plots/DEQ and Census Data Avaliability/", YEAR, "Hist Irrigated acreage only Census dateset.png"),p4, width = 8, height = 5, units="in")
p5<-grid.arrange(p3, p4, ncol=1)


dat1<-merge.data.frame(data_census,
                       data_deq[,c("GEOID","Countycode","NAMELSAD", "data_status")], by.x="GEOID", by.y="GEOID", all.x = TRUE, all.y=TRUE)

dat1$data_status.x <- ifelse(is.na(dat1$data_status.x), dat1$data_status.y, dat1$data_status.x)
dat1$County_Code <- ifelse(is.na(dat1$County_Code), dat1$Countycode, dat1$County_Code)
dat1 <- dat1[,c(1,2,3,8)]
dat_final <- VA_counties@data[,c(1,11,10)]

dat_final$status<-dat_final$GEOID %in% dat1$GEOID
summary_missing<-filter(dat_final, status== FALSE)
summary_missing$status[summary_missing$status==FALSE] <- c("Missing in both datsets")

dat_final <- full_join(dat1[,c("GEOID", "data_status.x")],summary_missing[,c("GEOID","status")], by = "GEOID")
dat_final$data_status.x <- ifelse(is.na(dat_final$data_status.x), dat_final$status, dat_final$data_status.x)

colnames(dat_final)[2]<- "Data Status"
dat_final <- dat_final[,-c(3)]


plotdat<-sp::merge(VA_counties,dat_final, 
                   by.x = "GEOID", by.y = "GEOID", all.x=TRUE)

summary2<-plotdat@data[,c(1,10,11,12)]

dev.off()
MYpal<-c("#1c9099", "#756bb1","#fdbb84","#bcbddc")
tmap_mode("plot")
p6<-tm_shape(plotdat)+
  tm_polygons("Data Status", title = "Status",
              # breaks = c(0,1,5,10,20),
              #n=5,style="jenks",
              palette = MYpal,
              id="NAMELSAD",
              legend.hist = TRUE)+
  tm_layout(main.title = paste0(YEAR," Data availability from USDA census and DEQ reporting"),
            legend.outside = TRUE,
            legend.title.size = 1,
            legend.text.size = 0.8,
            legend.position = c("left","top"),
            legend.bg.alpha = 1)
p6

#tmap_save(p6, paste0(WUDR_github,"/plots/DEQ and Census Data Avaliability/", YEAR, "DEQ_irrigated_facilites-1.png"),  width = 10, height = 5, units = 'in')
#tmap_save(p6, paste0(WUDR_github,"/plots/DEQ and Census Data Avaliability/", YEAR, "DEQ_irrigated_facilites-1.html"),  width = 10, height = 6.5, units = 'in')

write.csv(summary_deq,paste0(WUDR_github,"/csv_files/" , YEAR, "_deq_dat_availability_summary.csv"))
write.csv(data_census,paste0(WUDR_github,"/csv_files/" , YEAR, "_census_dat_availability_summary.csv"))
write.csv(summary2,paste0(WUDR_github,"/csv_files/" , YEAR, "_deq_census_dat_avaliability_summary.csv"))

census_summary <<- data_census
deq_summary <<- summary_deq
summary_all_data <<- summary2
}

 
 Census_DEQ_data_availability(2017)
 Census_DEQ_data_availability (2012)
 Census_DEQ_data_availability (2007)
 Census_DEQ_data_availability (2002)
 
 
 
 ##GO to code DEQ_NASS_common_dat.R after this
 
 
 # summary_list <- list()
# summary_list[[1]] <- data_census%>%
#   filter(data_status == "Available in both datasets")%>%
#   summarise(Acreage_sum=sum(Irrigated_Acreage),
#             Operation_sum=sum(Irrigated_Operations),
#             Count = n())
# 
# summary_list[[2]] <- data_census%>%
#   filter(data_status == "Available in USDA dataset")%>%
#   summarise(Acreage_sum=sum(Irrigated_Acreage),
#             Operation_sum=sum(Irrigated_Operations),
#             Count = n())
# 
# summary_list[[3]] <- data_deq%>%
#   filter(data_status == "Available in DEQ dataset")%>%
#   summarise(withdrawal_sum=sum(Facility_withdrawal_mg),
#             Faciltiy_sum=sum(Count),
#             Count = n())
# 
# summary_list[[4]] <- data_deq%>%
#   filter(data_status == "Available in both datasets")%>%
#   summarise(withdrawal_sum=sum(Facility_withdrawal_mg),
#             Faciltiy_sum=sum(Count),
#             Count = n())
# 
# names(summary_list) <- c("Census Available in both datasets", "Available in USDA dataset", "DEQ Available in DEQ dataset", "DEQ vailable in both datasets")
# assign(paste0(YEAR, "summary_list"), summary_list) 
