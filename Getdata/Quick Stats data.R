WUDR_github<-"F:/My Drive/WUDR/WUDR_Github/WUDR_local"
setwd(WUDR_github)


va.data<-read.csv(paste0(WUDR_github,"/csv_files/2017_Census_Summary.csv"))
O.farm.size.ops<-read.csv(paste0(WUDR_github,"/csv_files/binned_operations.csv"))
O.farm.size.size<-read.csv(paste0(WUDR_github,"/csv_files/binned_irrigatedarea.csv"))

##2017 data 
library(tidyverse)

quick_stats_area<-read.csv(paste0(WUDR_github,"/csv_files/quickstats_irrigaed area.csv"))


quick_stats_ops<-read.csv(paste0(WUDR_github,"/csv_files/quickstats-ops_bins.csv"))

colnames(quick_stats_area[,c(2,11,10,19,20)])
quick_stats_binned <- merge(quick_stats_area[,c(2,11,10,19,20)], quick_stats_ops[,c(2,10,19,20)], by.x=c("County", "Domain.Category", "Year"), by.y=c("County", "Domain.Category", "Year"))


quick_stats_binned$Irrigated_Area <- as.character(quick_stats_binned$Value.x)

quick_stats_binned$Irrigated_Area<-as.numeric(gsub(pattern=",", replacement="", quick_stats_binned$Irrigated_Area))

quick_stats_binned[is.na(quick_stats_binned)]<- c("-9999")

###################################################################
###  County Summary ###############################################
###################################################################

QS_data <- function(YEAR){

County_summary <- subset(quick_stats_binned, Domain.Category == "NOT SPECIFIED")
County_summary <- County_summary %>% 
  dplyr::select(Year = "Year", County = "County" ,County_code = "County.ANSI", 
                Irrigated_ops = "Value.y", Irrigated.Area = "Irrigated_Area")

dat_2017_QS <-   filter(County_summary, Year == YEAR ) 

dat_2017_OG<-read.csv(paste0(WUDR_github,"/csv_files/2017_Census_Summary.csv"))          

sum(dat_2017_OG$Irrigated_Acreage, na.rm = T)
sumd <- (filter (dat_2017_QS, Irrigated.Area!= -9999))
sum (as.numeric(sumd$Irrigated.Area), na.rm = T)
#60125


##################################################################
############## Binned data                                       #
#################################################################


QS2017 <- filter(quick_stats_binned, Year == YEAR )

## Count number of counties with irrigation data by farm size
size.id <- grep(pattern="AREA OPERATED", QS2017$Domain.Category) 
farm.size <- QS2017[size.id ,]

n.counties <- length(unique(farm.size$County.ANSI))
n.sizebins <- length(unique(farm.size$Domain.Category))

farm.size.ops <- matrix(data=NA, ncol=n.sizebins, nrow=n.counties)
colnames(farm.size.ops) <- unique(farm.size$Domain.Category)
rownames(farm.size.ops) <- unique(farm.size$County)

farm.size.size <- farm.size.ops

for (c in 1:n.counties){
  county.data <- subset(farm.size, County.ANSI == unique(farm.size$County.ANSI)[c])
  county.ops <- county.data[, -c(5,7)]
  county.sizes <- county.data[, -c(5,6)]
  for (s in 1:n.sizebins){
    size.ops<-subset(county.ops, Domain.Category == unique(farm.size$Domain.Category)[s])
    size.sizes<-subset(county.sizes, Domain.Category == unique(farm.size$Domain.Category)[s])
    if (dim(size.ops)[1] == 1) {farm.size.ops[c,s] <- paste(size.ops$Value.y)}
    if (dim(size.sizes)[1] == 1) {farm.size.size[c,s] <- paste(size.sizes$Irrigated_Area)}
  }
}
rm(county.ops, county.sizes, county.data)


# Summary Table 
data.summary <- data.frame(COUNTY_NAME = unique(farm.size$County),
                           COUNTY_CODE = rep(NA,n.counties),
                           Irrigated_Operations = rep(NA, n.counties),
                           Irrigated_Acreage = rep(NA, n.counties),
                           Size_Bins = rep(NA, n.counties),
                           Ops_Bins = rep(NA, n.counties)
)
data.summary$COUNTY_NAME <- as.character(paste(data.summary$COUNTY_NAME))


for (c in 1:n.counties){
  county.name<-data.summary$COUNTY_NAME[c]
  data.summary$COUNTY_CODE[c] <- as.character(paste(dat_2017_QS$County_code[
    as.character(paste(dat_2017_QS$County)) == county.name]))
  data.summary$Irrigated_Operations[c] <- dat_2017_QS$Irrigated_ops[
    as.character(paste(dat_2017_QS$County)) == county.name]
  data.summary$Irrigated_Acreage[c] <- dat_2017_QS$Irrigated.Area[
    as.character(paste(dat_2017_QS$County)) == county.name]
  
  # Binned data
  county.ops<-farm.size.ops[rownames(farm.size.ops) == county.name,]
  data.summary$Ops_Bins[c] <-  sum(!is.na(county.ops))
  county.sizes<-farm.size.size[rownames(farm.size.size) == county.name,]
  data.summary$Size_Bins[c]<- sum(!is.na(county.sizes) & county.sizes != -9999)
}

binned_operations <<- as.data.frame(farm.size.ops[, c(2,3,7,9,10,11,12,5,6,8,1,4)])
binned_irrigated_area  <<- as.data.frame(farm.size.size[, c(2,3,7,9,10,11,12,5,6,8,1,4)])
data.summary2 <<- data.summary
}

QS_data(2017)

QS_data(2012)

QS_data(2007)

QS_data(2002)