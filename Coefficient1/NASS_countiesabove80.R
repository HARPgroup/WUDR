WUDR_github<-"F:/My Drive/WUDR/WUDR_Github/WUDR_local"
setwd(WUDR_github)


library(tidyverse)
options(scipen=999)

summary_2002<-read.csv(paste0(WUDR_github,"/csv_files/2002_census_dat_availability_summary.csv"))
summary_2007<-read.csv(paste0(WUDR_github,"/csv_files/2007_census_dat_availability_summary.csv"))
summary_2012<-read.csv(paste0(WUDR_github,"/csv_files/2012_census_dat_availability_summary.csv"))
summary_2017<-read.csv(paste0(WUDR_github,"/csv_files/2017_census_dat_availability_summary.csv"))


#######County high in both datasets
summarylist <- list(summary_2002, summary_2007, summary_2012, summary_2017)
summary <- lapply(summarylist, function(x) filter(x, data_status == "Available in both datasets"))
summary <- lapply(summary, function(x) x[,-c(1)])
summary <- lapply(summary, function(x) mutate(x, percet_acreage = round(100*x$Irrigated_Acreage/sum(x$Irrigated_Acreage),0)))
summary <- lapply(summary, function(x) x %>% arrange(desc(percet_acreage)))

names(summary) <- c("s2002", "s2007", "s2012", "s2017")

#Percent atleast 80

p80_census_acreage<-lapply(summary, function(x) subset(x, c(TRUE, cumsum(percet_acreage) <= 80)[-nrow(x)]))


lapply(p80_census_acreage, function(x) sum(x$percet_acreage))
All_years_top80 <- p80_census_acreage %>% reduce(inner_join, by = "County_Code")
All_years_top80 <- All_years_top80[,-c(10, 15,16,18,23,24,26,31,32)]             

names <-c("County_code","County_Name", "Irrigated_Operations.2002" , "Irrigated_Acreage.2002","Size_Bins.2002","Ops_Bins.2002", "GEOID.2002" , "data_status.2002" ,
          "percet_acreage.2002" , "Irrigated_Operations.2007",   "Irrigated_Acreage.2007"   ,   "Size_Bins.2007"   ,"Ops_Bins.2007"   ,"percet_acreage.2007" ,
          "Irrigated_Acreage.2012"  ,  "Size_Bins.2012"     ,       "Ops_Bins.2012"  ,          
           "percet_acreage.2012"   ,    "Irrigated_Operations.2017", "Irrigated_Acreage.2017",   
           "Size_Bins.2017"     ,       "Ops_Bins.2017" ,            "percet_acreage.2017"   )

colnames(All_years_top80) <- names


All_years_top80_fulljoin <- p80_census_acreage %>% reduce(full_join, by = "County_Code")
All_years_top80_fulljoin <- All_years_top80_fulljoin[,-c(10, 15,16,18,23,24,26,31,32)]  
