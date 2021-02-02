# 1.
# I want to see the details of the facilites which are in DEQ dataset and not in USDA
# Specifically these facilities are in  six such counties. King George, Essex, Henrico, Greensville, Richmond, Petersburg

# Load the initial data which containes all the intake names and see the summary 

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

##############Load deq data to get GEOID for VDEQ only counties
deq_only<-read.csv(paste0(WUDR_github,"/csv_files/deqdat_avaliability_summary2017.csv"))
deq_only<-filter(deq_only,Data.Status=="Available in DEQ dataset")

deq_only_facilities<-merge.data.frame(summary_multiple_intakes,deq_only, by.x = "FIPS.Code", by.y ="GEOID" )

#################################
#Here DEQ only facilites provides deatils on facilties in DEQ but not in USDA

#Summary
summary_only_DEQ<-deq_only_facilities%>%
  group_by(Facility, NAMELSAD)%>%
  summarise(sum= sum(Facility_withdrawal_mgm))


write.csv(summary_only_DEQ,(paste0(WUDR_github,"/csv_files/only DEQ facilites.csv")))

#######################################
#Farms in these counties
###Essex County#######
#####################

#CLOVERFIELD FARM : Google search confirms the farm in essex  
#F L DICKINSON FARM: Google search didnt gave any detail but lat lon suggest that withdarawal is from Rappahannock River and is agricultual land. 
#Marlbank Farms: lat long shows a farm and a center pivot nearby
#Sunnyside Farm: exists (ground water well)

#######################

#Greensville County

###########################
#PIERCE FARM: Exists
#YARDEN FARM: Exists
#

#Henrico County
#Strange's Greenhouse 4201 Creighton Road	: Nursey Exists
#VARINA ON THE JAMES: Exists

#King George County
# Greenhost Incorporated	: Exists
# WOODLAWN \ WALSINGHAM FARMS: Exists

# Petersburg city	
# COE FARM	 : Farm pond

#Richmond city	
# 12	Strange's Garden Center W. Broad St.	 : Nursery


# 2 Histogram data 
census_dat<-read.csv(paste0(WUDR_github,"/csv_files/2017_Census_Summary.csv"))
census_dat<-merge.data.frame(census_dat,deq_dat[,c("GEOID","COUNTYFP")], by.x = "COUNTY_CODE", by.y ="COUNTYFP", all.x = TRUE )

data_census<-census_dat[complete.cases(census_dat),]

data_census$data_status<-data_census$GEOID%in% data_deq$GEOID
data_census$data_status<-ifelse(data_census$data_status == TRUE, "Available in both datasets", "Available in USDA dataset")

data_census%>%
  filter(data_status == "Available in both datasets")%>%
  summarise(sum=sum(Irrigated_Acreage))
  
only_USDA<-data_census%>%
  filter(data_status == "Available in USDA dataset" )

range(only_USDA$Irrigated_Acreage)
sum(only_USDA$Irrigated_Acreage)



