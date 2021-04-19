##
##  Script outline:
##	Load USDA Census Data
##  Includes county-level irrigated acreage by farm size?

# Set working directory and load data
data.folder<-"F:/My Drive/WUDR_data/FullData"
  # Note that you'll have to change the path directory 
  # to the location where you've saved the data
setwd(data.folder) 

# Full dataset for all of US
#data <- read.table(file="2017_cdqt_data.txt", sep="\t", header=TRUE)

# VA only data, saved from before
va.data<-read.csv(file="VA_Census_2012_Full.csv")
dim(data) # 7.3 mil records

## Trim to Virginia
#va.data <- subset(data, STATE_NAME =="VIRGINIA")
dim(va.data) # 196,274 records

## Trim to table 10 - irrigation data
table.10 <- subset(va.data, CENSUS_TABLE == 10)
table.10.albemarle <- subset(table.10, COUNTY_NAME == "ALBEMARLE")
## Includes county-level info!

keeper.cols <- c("CENSUS_TABLE", "SECTOR_DESC", "SHORT_DESC", "COMMODITY_DESC",
                 "AGG_LEVEL_DESC", "STATE_NAME", "COUNTY_CODE", "COUNTY_NAME",
                 "DOMAINCAT_DESC",  "VALUE" )

## Count number of counties with total irrigation operations and acreage
##############################################################################
# Table with just number of operations with irrigation
irr.ops<-subset(table.10, SHORT_DESC == "AG LAND - NUMBER OF OPERATIONS")
irr.ops<-subset(irr.ops, AGG_LEVEL_DESC == "COUNTY")
irr.ops$VALUE <- as.numeric(paste(irr.ops$VALUE))
irr.ops <- irr.ops[, colnames(irr.ops) %in% keeper.cols]

summary(irr.ops$VALUE) # all numeric, range from 2 to 160
length (unique(irr.ops$COUNTY_CODE)) # Data for 94 counties

# Table with just irrigated acreage
irr.land<-subset(table.10, SHORT_DESC == "AG LAND, IRRIGATED - ACRES")
irr.land<-subset(irr.land, DOMAINCAT_DESC == "")
irr.land<-subset(irr.land, AGG_LEVEL_DESC == "COUNTY")
irr.land <- irr.land[, colnames(irr.land) %in% keeper.cols]

irr.land$VALUE <- as.character(irr.land$VALUE)
irr.land$VALUE<-as.numeric(gsub(pattern=",", replacement="", irr.land$VALUE))
sum(!is.na(irr.land$VALUE)) # 84 counties without NA data
dim(irr.land) #94 counties
length(unique(irr.land$COUNTY_CODE)) #94 unique counties
summary(irr.land$VALUE)

## Count number of counties with irrigation data by farm size
size.id <- grep(pattern="AREA OPERATED", table.10$DOMAINCAT_DESC) 
farm.size <- table.10[size.id ,]
farm.size <- subset(farm.size, AGG_LEVEL_DESC == "COUNTY")

n.counties <- length(unique(farm.size$COUNTY_CODE))
n.sizebins <- length(unique(farm.size$DOMAINCAT_DESC))

farm.size.ops <- matrix(data=NA, ncol=n.sizebins, nrow=n.counties)
colnames(farm.size.ops) <- unique(farm.size$DOMAINCAT_DESC)
rownames(farm.size.ops) <- unique(farm.size$COUNTY_NAME)

farm.size.size <- farm.size.ops

for (c in 1:n.counties){
  county.data <- subset(farm.size, COUNTY_CODE == unique(farm.size$COUNTY_CODE)[c])
  county.ops <- subset(county.data, SHORT_DESC == "AG LAND, IRRIGATED - NUMBER OF OPERATIONS")
  county.sizes <- subset(county.data, SHORT_DESC == "AG LAND, IRRIGATED - ACRES")
  for (s in 1:n.sizebins){
    size.ops<-subset(county.ops, DOMAINCAT_DESC == unique(farm.size$DOMAINCAT_DESC)[s])
    size.sizes<-subset(county.sizes, DOMAINCAT_DESC == unique(farm.size$DOMAINCAT_DESC)[s])
    if (dim(size.ops)[1] == 1) {farm.size.ops[c,s] <- paste(size.ops$VALUE)}
    if (dim(size.sizes)[1] == 1) {farm.size.size[c,s] <- paste(size.sizes$VALUE)}
  }
}
rm(county.ops, county.sizes, county.data)

# Summary Table 
data.summary <- data.frame(COUNTY_NAME = unique(farm.size$COUNTY_NAME),
                           COUNTY_CODE = rep(NA,n.counties),
                           Irrigated_Operations = rep(NA, n.counties),
                           Irrigated_Acreage = rep(NA, n.counties),
                           Size_Bins = rep(NA, n.counties),
                           Ops_Bins = rep(NA, n.counties)
                           )
data.summary$COUNTY_NAME <- as.character(paste(data.summary$COUNTY_NAME))

for (c in 1:n.counties){
  county.name<-data.summary$COUNTY_NAME[c]
  data.summary$COUNTY_CODE[c] <- as.character(paste(irr.ops$COUNTY_CODE[
    as.character(paste(irr.ops$COUNTY_NAME)) == county.name]))
  data.summary$Irrigated_Operations[c] <- irr.ops$VALUE[
    as.character(paste(irr.ops$COUNTY_NAME)) == county.name]
  data.summary$Irrigated_Acreage[c] <- irr.land$VALUE[
    as.character(paste(irr.land$COUNTY_NAME)) == county.name]
  
  # Binned data
  county.ops<-farm.size.ops[rownames(farm.size.ops) == county.name,]
  data.summary$Ops_Bins[c] <-  sum(!is.na(county.ops))
  county.sizes<-farm.size.size[rownames(farm.size.size) == county.name,]
  data.summary$Size_Bins[c]<- sum(!is.na(county.sizes) & county.sizes != "(D)")
}


# Write tables to file
write.csv(farm.size.ops,file="2012binned_operations.csv")
write.csv(farm.size.size,file="2012binned_irrigatedarea.csv")
write.csv(data.summary, file="2012_Census_Summary.csv")

