WUDR_github<-"F:/My Drive/WUDR/WUDR_Github/WUDR_local"
setwd(WUDR_github)

# load libraries
library(sqldf)
library(tidyverse)
library(dplyr)
library(rgdal)
library(compare)
library(tmap)


WUDR_github<-"F:/My Drive/WUDR/WUDR_Github/WUDR_local"
setwd(WUDR_github)

Irrigated.data<-read.csv(paste0(WUDR_github,"/csv_files/binned_irrigatedarea.csv"), stringsAsFactors=FALSE)

Irrigated.data <- Irrigated.data[, c(1:13)]
Irrigated.data[is.na(Irrigated.data)] <- 0
Irrigated.data[Irrigated.data=="(D)"]<- 0


Fun <- function(x){
  gsub(',', '', x)
}

dat<- as.data.frame (apply(Irrigated.data[,-1], 2, Fun))

Fun_num <- function (x) {
  as.numeric(as.character(x))
}

dat<- as.data.frame (apply(dat, 2, Fun_num))


Irrigated.data <- cbind(Irrigated.data[,1], dat)

Colnames <- c("County", "below10", "between_10_50","between_50_70","between_70_100" 
              ,"between_100_139","between_140_179","between_180_219","between_220_259","between_260_499"
              ,"between_500_999","between_1000_1999", "Above_2000")

colnames(Irrigated.data) <- Colnames

rownames(Irrigated.data) <- Irrigated.data[,1]



summary.irr <- data.frame(County=as.character(Irrigated.data$County), 
                          Irri.sum=rep(NA,dim(Irrigated.data)[1]))

Irrigated.data <- Irrigated.data[,-1]
summary.irr$Irri.sum <- rowSums(Irrigated.data, na.rm = TRUE)

census.data<-read.csv(paste0(WUDR_github,"/csv_files/2017_Census_Summary.csv"), stringsAsFactors=FALSE)

summary.irr$County.sum <- census.data$Irrigated_Acreage

##Check this
summary.irr$County.sum<- ifelse(is.na(summary.irr$County.sum), summary.irr$Irri.sum, summary.irr$County.sum)

summary.irr$D_area <- summary.irr$County.sum - summary.irr$Irri.sum



#########################Select size bins with D values  in sorted list

####Load The irrigated area again

Irrigated.data<-read.csv(paste0(WUDR_github,"/csv_files/binned_irrigatedarea.csv"), stringsAsFactors=FALSE)

Irrigated.data <- Irrigated.data[, c(1:13)]
Irrigated.data[Irrigated.data=="(D)"]<- -9999


Fun <- function(x){
  gsub(',', '', x)
}

dat<- as.data.frame (apply(Irrigated.data[,-1], 2, Fun))


Fun_num <- function (x) {
  as.numeric(as.character(x))
}

dat<- as.data.frame (apply(dat, 2, Fun_num))

Irrigated.data <- cbind(County = Irrigated.data[,1], dat)

Colnames <- c("County", "below10", "between_10_50","between_50_70","between_70_100"
              ,"between_100_139","between_140_179","between_180_219","between_220_259","between_260_499"
              ,"between_500_999","between_1000_1999", "Above_2000")

colnames(Irrigated.data) <- Colnames

rownames(Irrigated.data) <- Irrigated.data[,1]

Irrigated.data <- Irrigated.data[,-1]

area_list <- setNames(split(Irrigated.data, seq(nrow(Irrigated.data))), rownames(Irrigated.data))
for (i in 1: length(area_list)){
  area_list[[i]] <- as.data.frame(t(area_list[[i]]))
}

names_list <- names(area_list)
sorted <- list()
for (i in 1:94) {
  x <- as.data.frame(area_list[[i]]) 
  
  x$size.bins <- rownames(x)
  x <-  filter(x ,x[,1]  == -9999)
  
  sorted[[i]] <- x
}   ##Here Sorted list contins size bins with only D values


size.names <- c( "below10", "between_10_50","between_50_70","between_70_100" 
                 ,"between_100_139","between_140_179","between_180_219","between_220_259","between_260_499"
                 ,"between_500_999","between_1000_1999", "Above_2000") 

#Load the bin size details and perctage area irrigated in each bin size
bin.char <- read.csv(paste0(WUDR_github,"/csv_files/bin.char.csv"), header=TRUE)
bin.char$size <- size.names

pct.irr <- list ()

for (i in 1:94) {
  x <- as.data.frame(sorted[[i]]) 
  x <-  merge(bin.char[,c(2,8)], x , by.x = "size", by.y = "size.bins")
  x <- as.data.frame(x[,c(1:2)])
  pct.irr[[i]] <- x
}  # This list contains the percentage area irrigated in each size bin where D values are absent


area.irr <- list()
for (i in 1:94) {
  
  
  x <- as.data.frame(pct.irr[[i]])
  x$area <- x$avg_perc_irr * summary.irr[i,2]
  
  area.irr[[i]] <- x
}  # Irrigated area for D values


recaled_dat <- area.irr

for (i in 1:94) {
  
  recaled_dat[[i]][4] <- Map(function(x, y)  x * y/sum(x), area.irr[[i]][3], as.numeric(summary.irr[i,4]))
  
}

sum(recaled_dat[[1]][4])

# THis rescaled dat list replaces the D values after reclaing it to difference

colnames <- c("Size bin", "Average Pct Irri", " Area Irrigated", " Rescaled Area")

recaled_dat <- lapply(recaled_dat, setNames, colnames)

names(recaled_dat) <- names(area_list)


#####################################################################################
########### Bring back rescaled D values to orginial list ##########################


dat_D_filled <- list()
for (i in 1:94) {
  x <- as.data.frame(recaled_dat[[i]]) 
  y <- as.data.frame(area_list[[i]])
  y$size <- rownames(y)
  z <-  full_join(x, y , by = c("Size bin"= "size"))
  z[,5] <- ifelse((z[,5] == -9999), z[,4], z[,5])
  z <- as.data.frame(z[,c(1,5)])
  z[,2] <- round(z[,2], 0)
  dat_D_filled[[i]] <- z
}

colnames <- c("Size bin",  "Area_irr_rescaled Area")


names(dat_D_filled) <- names_list

save(dat_D_filled, file=paste0(WUDR_github,"/dat_load/dat_D_filled.RData"))
sum(dat_D_filled[[1]][2], na.rm = TRUE)


######################################################################################
##### Calculate Area below TH



