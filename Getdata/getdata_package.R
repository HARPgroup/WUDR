# install.packages("devtools")
# devtools::install_github("rdinter/usdarnass")
WUDR_github<-"F:/My Drive/WUDR/WUDR_Github/WUDR_local"
setwd(WUDR_github)

library("usdarnass")
library("tidyverse")
nass_set_key("02065135-7387-30C4-929C-D2963D721E02")
# First time, reload your enviornment so you can use the key without restarting R.
readRenviron("~/.Renviron")
# You can check it with:
Sys.getenv("NASS_KEY")

Year = c(1997,2002,2007,2012,2017)
# Year = 2017
nass.ag.data <- nass_data(year = Year,
                       commodity_desc ="AG LAND",
                       short_desc = "AG LAND, IRRIGATED - ACRES",
                       domain_desc =c("AREA OPERATED","TOTAL"),
                       agg_level_desc = "COUNTY",
                       state_ansi = "51")

nass.ag.data <- nass.ag.data %>% 
  select("Year" = year, "County_Name" = county_name, "County_Code" = county_ansi, "Region" = asd_desc, "Region_code" = asd_code,
         "Domain" = domain_desc,"Size.bins"= domaincat_desc, "Irr.Area.Acres" = Value)

nass.ops.data <- nass_data(year = Year,
                          commodity_desc ="AG LAND",
                          short_desc = "AG LAND, IRRIGATED - NUMBER OF OPERATIONS",
                          domain_desc =c("AREA OPERATED","TOTAL"),
                          agg_level_desc = "COUNTY",
                          state_ansi = "51")


nass.ops.data <- nass.ops.data %>% 
  select("Year" = year,"County_Name" = county_name, "County_Code" = county_ansi, "Region" = asd_desc, "Region_code" = asd_code,
         "Domain" = domain_desc, "Size.bins"= domaincat_desc, "Irr.Ops" = Value)

                                                                                                        

nass.ag.data$merge <- do.call(paste, c(nass.ag.data[,c(3,7,1)], sep = ""))  
nass.ops.data$merge <- do.call(paste, c(nass.ops.data[,c(3,7,1)], sep = "")) 

absent <- subset(nass.ag.data, !(merge %in% nass.ops.data$merge))


###For 1997 the total no of operations in each county is reported.

nass_binned <- merge(nass.ag.data[,c(1:8)],nass.ops.data[,c(1,3,7,8)] , by.x=c("County_Code", "Size.bins", "Year"), by.y=c("County_Code", "Size.bins", "Year"))

nass_binned$Irr.Area.Acres <- as.character(nass_binned$Irr.Area.Acres)

nass_binned$Irr.Area.Acres<-as.numeric(gsub(pattern=",", replacement="", nass_binned$Irr.Area.Acres))

nass_binned[is.na(nass_binned)]<- c("-9999")

