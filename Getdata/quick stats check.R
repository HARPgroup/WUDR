
WUDR_github<-"F:/My Drive/WUDR/WUDR_Github/WUDR_local"
setwd(WUDR_github)

##2017 data 
library(tidyverse)

# VA only data, saved from before
va.data<-read.csv(paste0(WUDR_github,"/csv_files/2017_Census_Summary.csv"))
OG.farm.size.ops<-read.csv(paste0(WUDR_github,"/csv_files/binned_operations.csv"))
OG.farm.size.size<-read.csv(paste0(WUDR_github,"/csv_files/binned_irrigatedarea.csv"))


ops_correct.sum <- sum(va.data$Irrigated_Operations)
area_correct.sum <- sum(as.numeric(as.character(va.data$Irrigated_Acreage)), na.rm = TRUE)
#2053

## Downloaded data using query

###Compare the number of operations data downloaded using quick stats with the one using R

## irrigated Ops
quick_stats_ops<-read.csv(paste0(WUDR_github,"/csv_files/quick_stats_ops.csv"))

Qstats_ops2017 <- filter(quick_stats_ops, Year == 2017 )
sum(Qstats_ops2017$Value)

Qstats_ops2017 <- Qstats_ops2017 %>% 
  dplyr::select(Year = "Year", County = "County" ,County_code = "County.ANSI", Data.Category = "Data.Item",
                Domain = "Domain", Irrigated_ops = "Value")

#2053


#irrigated area
quick_stats_area<-read.csv(paste0(WUDR_github,"/csv_files/quickstats_irrigaed area.csv"))

# Table with just irrigated acreage
irr.land<-subset(quick_stats_area, Data.Item == "AG LAND, IRRIGATED - ACRES")
irr.land<-subset(irr.land, Domain == "TOTAL")

irr.land <- irr.land %>% 
  dplyr::select(Year = "Year", County = "County" ,County_code = "County.ANSI", Data.Category = "Data.Item", Domain = "Domain", Irrigated_ops = "Value")

#irrigated ops
quick_stats_ops<-read.csv(paste0(WUDR_github,"/csv_files/quickstats-ops_bins.csv"))

# Table with just irrigated acreage
irr.ops<-subset(quick_stats_ops, Data.Item == "AG LAND, IRRIGATED - NUMBER OF OPERATIONS")
irr.ops<-subset(irr.land, Domain == "TOTAL")

irr.ops <- irr.ops %>% 
  dplyr::select(Year = "Year", County = "County" ,County_code = "County.ANSI", Data.Category = "Data.Item", Domain = "Domain", Irrigated_ops = "Value")


##Check for 2017
irr.land_17 <- filter(irr.land, Year == 2017 )

irr.land_17$VALUE <- as.character(irr.land_17$Irrigated_area)
irr.land_17$VALUE<-as.numeric(gsub(pattern=",", replacement="", irr.land_17$Irrigated_area))
sum(irr.land_17$VALUE, na.rm = T)
#60125


#################################################
##Irrigated area bins

## Count number of counties with irrigation data by farm size
size.id <- grep(pattern="AREA OPERATED", quick_stats_area$Domain.Category) 
farm.size <- quick_stats_area[size.id ,]


farm.size <- farm.size %>% 
  dplyr::select(Year = "Year", County = "County", County_code = "County.ANSI", Data.Category = "Data.Item",
                Domain = "Domain", size_bins = "Domain.Category" ,Irrigated_area = "Value")

## 
ops.id <- grep(pattern="AREA OPERATED", quick_stats_area$Domain.Category) 
farm.size <- quick_stats_area[ops.id ,]


farm.size <- farm.size %>% 
  dplyr::select(Year = "Year", County = "County", County_code = "County.ANSI", Data.Category = "Data.Item",
                Domain = "Domain", size_bins = "Domain.Category" ,Irrigated_area = "Value")



farm.size <- filter(farm.size, Year == 2017 )
n.counties <- length(unique(farm.size$County_code))
n.sizebins <- length(unique(farm.size$size_bins))

farm.size.ops <- matrix(data=NA, ncol=n.sizebins, nrow=n.counties)
colnames(farm.size.ops) <- unique(farm.size$size_bins)
rownames(farm.size.ops) <- unique(farm.size$County)

farm.size.size <- farm.size.ops

for (c in 1:n.counties){
  county.data <- subset(farm.size, County_code == unique(farm.size$County_code)[c])
  county.ops <- subset(Qstats_ops2017, Data.Category == "AG LAND - NUMBER OF OPERATIONS")
  county.sizes <- subset(irr.land_17, Data.Category == "AG LAND, IRRIGATED - ACRES")
  for (s in 1:n.sizebins){
    size.ops<-subset(county.ops, Data.Category == unique(farm.size$size_bins)[s])
    size.sizes<-subset(county.sizes, Data.Category == unique(farm.size$Data.Category)[s])
    if (dim(size.ops)[1] == 1) {farm.size.ops[c,s] <- paste(size.ops$Irrigated_ops)}
    if (dim(size.sizes)[1] == 1) {farm.size.size[c,s] <- paste(size.sizes$Irrigated_area)}
  }
}
rm(county.ops, county.sizes, county.data)