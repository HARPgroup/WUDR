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

# write.csv(summary.irr ,paste0(WUDR_github,"/csv_files/2017census.irrigated_diff.csv"))

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


###########################################################################
#Load operations data ##################################################
######################################################################

Operations.data <- read.csv(paste0(WUDR_github,"/csv_files/binned_operations.csv"))
Operations.data[is.na(Operations.data)] <- -99


Colnames <- c("County", "below10", "between_10_50","between_50_70","between_70_100" 
              ,"between_100_139","between_140_179","between_180_219","between_220_259","between_260_499"
              ,"between_500_999","between_1000_1999", "Above_2000")

colnames(Operations.data) <- Colnames

rownames(Operations.data) <- Operations.data[,1]

Operations.data <- Operations.data[,-1]


opeartions_list <- setNames(split(Operations.data, seq(nrow(Operations.data))), rownames(Operations.data))

for (i in 1: length(opeartions_list)){
  opeartions_list[[i]] <- as.data.frame(t(opeartions_list[[i]]))
}

for (i in 1:94) {
  x <- as.data.frame(opeartions_list[[i]]) 
  
  x$ops.bins <- rownames(x)
  x <- x[,c(2,1)]
  opeartions_list[[i]] <- x
  rownames(opeartions_list[[i]]) <- NULL
} 

##########################################################################
#########################################################################
pct.irr <- list ()

for (i in 1:94) {
  x <- as.data.frame(sorted[[i]]) 
  x <-  merge(bin.char[,c(2,5,8)], x , by.x = "size", by.y = "size.bins")
  x <- as.data.frame(x[,c(1:2,3)])
  
  y <- as.data.frame(opeartions_list[[i]]) 
  
  z <- merge(x,y , by.x = "size", by.y = "ops.bins")
  pct.irr[[i]] <- z
}  # This list contains the percentage area irrigated in each size bin where D values are absent

colnames <- c("Size", "Avg_Size", "Avg_Pct_Irri", "Ops")

pct.irr <- lapply(pct.irr, setNames, colnames)

area.irr <- list()
for (i in 1:94) {
  
  x <- as.data.frame(pct.irr[[i]])
  x$area <- x$Avg_Pct_Irri * x$Avg_Size * x$Ops
  
  area.irr[[i]] <- x
}  # Irrigated area for D values


recaled_dat <- area.irr

for (i in 1:94) {
  
  recaled_dat[[i]][6] <- Map(function(x, y)  x * y/sum(x), area.irr[[i]][5], as.numeric(summary.irr[i,4]))
  
}

sum(recaled_dat[[1]][6])

# THis rescaled dat list replaces the D values after reclaing it to difference

colnames <- c("Size bin", "Avg_Size", "Average Pct Irri", "Ops", "Area Irrigated", " Rescaled Area")

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
  z[,7] <- ifelse((z[,7] == -9999), z[,6], z[,7])
  z <- as.data.frame(z[,c(1,7)])
  z[,2] <- round(z[,2], 0)
  dat_D_filled[[i]] <- z
}

sum(dat_D_filled[[1]][2], na.rm=TRUE)

colnames <- c("Size bin",  "Area_irr_rescaled Area")


names(dat_D_filled) <- names_list

save(dat_D_filled, file=paste0(WUDR_github,"/dat_load/dat_D_filled.RData"))





