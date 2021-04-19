WUDR_github<-"F:/My Drive/WUDR/WUDR_Github/WUDR_local"
setwd(WUDR_github)
# load libraries
library(sqldf)
library(tidyverse)
library(dplyr)
library(rgdal)
library(compare)

load(paste0(WUDR_github,"/dat_load/dat_D_filled.RData"))
load(paste0(WUDR_github,"/dat_load/opeartions_list.RData"))

sum(dat_D_filled[[1]][2], na.rm=TRUE)


new_df2 <- lapply(opeartions_list, function(x){
  x$'Size bin' <- rownames(x)
  rownames(x) <- NULL
  x <- x[,c(1,2)]
  return(x)
})

total <- Map(merge, dat_D_filled,new_df2, by.x = 'Size bin', by.y = 'ops.bins')

total <- lapply(total, function(x){
  x<- x[c(2,3,10,12,4,6,7,8,9,11,5,1),]
  return(x)
})


colnames <- c("Size.bin", "Area", "Operations")

total <- lapply(total, setNames, colnames)

total <- lapply(total, function(x){
  x$Area.per.op <- round (x$Area / x$Operations, 2)
  return(x)
})

Avg.Farm.size <- as.data.frame(as.numeric(c("5","29.5", "59.5","84.5","119.5","159.5", "199.5","239.5",
                                            "379.5", "749.5","1499.5", "3500")))


total <- mapply(FUN = `cbind`,  total,Avg.Farm.size, SIMPLIFY = FALSE)

colnames <- c("Size.bin","Area", "Operations", "Area.per.op" ,"Avg.Farm.size")

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
  x$Pct.Non.repoted.of.total.irrarea <- round (100*x$Irr.Area.Under.TH / sum(x$Irr.Area.Over.TH,x$Irr.Area.Under.TH), 2)
  x$Pct.Non.repoted.of.area.abv.TH <- round (100*x$Irr.Area.Under.TH / x$Irr.Area.Over.TH, 2)
  return(x)
})


Unreported <-  lapply(TH_dat, function(x) x%>% select(Pct.Non.repoted.of.total.irrarea,Pct.Non.repoted.of.area.abv.TH, Irr.Area.Under.TH,Irr.Area.Over.TH))


df <- data.frame(matrix(unlist(Unreported), nrow=length(Unreported), byrow=TRUE))
df$county <- names(Unreported)

colnames(df) <- c("Pct.under.TH.of.total.Irr.area", "Pct.under.TH.of.Irri.area.abv.TH","Irr.Area.Under.TH", "Irr.Area.above.TH", "County") 
df$Total.Irri.Area <- apply(df[,c(3,4)],1,sum)
df <- df[,c(5,6,4,3,2,1)]

# write.csv(df, paste0(WUDR_github,"/csv_files/Percentage Irri.data.Under TH.csv"))

