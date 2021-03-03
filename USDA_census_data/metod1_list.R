WUDR_github<-"F:/My Drive/WUDR/WUDR_Github/WUDR_local"
setwd(WUDR_github)

# load libraries
library(sqldf)
library(tidyverse)
library(dplyr)
library(rgdal)
library(compare)
library(tmap)
library(janitor)

###
Irrigated.data<-read.csv(paste0(WUDR_github,"/csv_files/binned_irrigatedarea.csv"), stringsAsFactors=FALSE)

Irrigated.data <- Irrigated.data[, c(1:13)]
Irrigated.data[is.na(Irrigated.data)] <- 0
Irrigated.data[Irrigated.data=="(D)"]<- 0

Counties <- Irrigated.data$County


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

Irrigated.data <- Irrigated.data[,-1]


area_list <- setNames(split(Irrigated.data, seq(nrow(Irrigated.data))), rownames(Irrigated.data))

for (i in 1: length(area_list)){
area_list[[i]] <- as.data.frame(t(area_list[[i]]))
}




##############################################################
####   Operations Data #######################################
##############################################################

Operations.data <- read.csv(paste0(WUDR_github,"/csv_files/binned_operations.csv"))
Operations.data[is.na(Operations.data)] <- 0


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


total <-  mapply(FUN = `cbind`,  area_list,opeartions_list, SIMPLIFY = FALSE)

colnames <- c("Area", "Operations")

total <- lapply(total, setNames, colnames)

total <- lapply(total, function(x){
  x$Area.per.op <- round (x$Area / x$Operations, 2)
  return(x)
})

Avg.Farm.size <- as.data.frame(as.numeric(c("5","29.5", "59.5","84.5","119.5","159.5", "199.5","239.5",
                                 "379.5", "749.5","1499.5", "3500")))


total <- mapply(FUN = `cbind`,  total,Avg.Farm.size, SIMPLIFY = FALSE)

colnames <- c("Area", "Operations", "Area.per.op" ,"Avg.Farm.size")

total <- lapply(total, setNames, colnames)

total <- lapply(total, function(x){
  x$Avg.Pect.Irr <- round (x$Area.per.op / x$Avg.Farm.size, 2)
  return(x)
})

Non.irr.TH <- 10

Under.Th<-list()
Over.Th <- list()

for (i in 1: length(total)){
Under.Th[[i]] <- total[[i]] %>%
  subset(Area.per.op < Non.irr.TH) %>% 
         summarise(sum(Area))
}

names(total)

names(Under.Th) <- names(total)

for (i in 1: length(total)){
  Over.Th[[i]] <- total[[i]] %>%
    subset(Area.per.op >= Non.irr.TH) %>% 
    summarise(sum(Area))
}

names(Over.Th) <- names(total)

TH_dat <- mapply(FUN = `cbind`,  Under.Th,Over.Th, SIMPLIFY = FALSE)

colnames <- c("Irr.Area.Under.TH" , "Irr.Area.Over.TH")

TH_dat <- lapply(TH_dat, setNames, colnames)

TH_dat <- lapply(TH_dat, function(x){
  x$Pct.Non.repoted <- round (100*x$Irr.Area.Under.TH / x$Irr.Area.Over.TH, 2)
  return(x)
})

Unreported <-  lapply(TH_dat, function(x) x%>% select(Pct.Non.repoted, Irr.Area.Under.TH))


df <- data.frame(matrix(unlist(Unreported), nrow=length(Unreported), byrow=TRUE))
df$county <- names(Unreported)

colnames(df) <- c("Pct.Non.repoted", "Irr.Area.Under.TH", "County") 
df <- df[,c(3,2,1)]

df[is.na(df)] <- 0
df[is.infinite(df)] <- 100
df2 <- df
df2[mapply(is.infinite, df2)] <- 100
