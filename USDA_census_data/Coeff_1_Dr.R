##
##  Script outline:
##	Process USDA County-level irrigated area data
##  Different options for dealing with D values 

rm(list = ls())

# Set working directory and load data
# data.folder<-"F:/My Drive/WUDR/WUDR_Github/WUDR_local"
# figure.loc <- "G:/My Drive/VT/Research/Virginia/USGS_WUDR_AgWithdrawals/Coeff_1/Histograms"
# 
# setwd(data.folder) 

WUDR_github<-"F:/My Drive/WUDR/WUDR_Github/WUDR_local"
setwd(WUDR_github)
data.ops <- read.csv(paste0(WUDR_github,"/csv_files/binned_operations.csv"), header=TRUE)
data.size <- read.csv(paste0(WUDR_github,"/csv_files/binned_irrigatedarea.csv"), header=TRUE)

# Create bin.char to summarize information for each size bin
n.sizes <-ncol(data.ops) - 1
bin.char<-data.frame(size=colnames(data.ops)[2:(n.sizes+1)], 
                     minsize = rep(NA,n.sizes), maxsize = rep(NA,n.sizes),
                     avgsize = rep(NA,n.sizes))
bin.char$minsize <- c(1, 10, 50, 70, 100, 140, 180, 220, 260, 500, 1000, 2000)
bin.char$maxsize <- c(9.9, 49.9, 69.9, 99.9, 139, 179, 219, 259, 499, 999, 1999, 5000)
bin.char$avgsize <- apply(bin.char[,2:3],1,"mean")
bin.char$size<-as.character(bin.char$size)
bin.char$cntys_w_data <- NA
bin.char$cntys_w_Ds <- NA
bin.char$avg_perc_irr <- NA

# Create an object that lists size groupings in a nice format for plotting
size.bins <-gsub(pattern="AREA.OPERATED...",replacement="",x=bin.char$size)
size.bins <-gsub(pattern=".TO.", replacement=" to ", x=size.bins)
size.bins <-gsub(pattern=".ACRES.", replacement=" acres", x=size.bins)
size.bins <-gsub(pattern=".OR.MORE.", replacement=" or more ", x=size.bins)

# Create a table for each bin size to store all county info for farms of that size
# Store in a list of length 12 
bin.table <- data.frame(County=as.character(data.ops$X), 
                        irr.ops=rep(NA,dim(data.ops)[1]), 
                        irr.acres=rep(NA,dim(data.ops)[1]),
                        irr.AcresperOp=rep(NA,dim(data.ops)[1]),
                        irr.perc=rep(NA,dim(data.ops)[1]))
bin.list <- rep(list(bin.table),length(size.bins))

# Loop through each size category 
# For a given bin size: 
#   Load in data for farms of that size in each county
#   From county-level irr.ops and irr.acres 
#   calculate county-level irr.perc (irrigated acreage per op divided by avg farm size)
#   create histogram of irr.perc in each county
#   calculate average irr.perc and add to bin.char table

for (i in 1:length(size.bins)){
  bin.table <- bin.list[[i]]
  size <- bin.char$size[i]
  bin.table$irr.ops <- data.ops[,colnames(data.ops) == size]
  bin.table$irr.acres <- data.size[,colnames(data.size) == size]
  
  # remove any commas in irr.acres
  bin.table$irr.acres <- gsub(pattern=",",replacement="",x=bin.table$irr.acres)
  
  # How many counties have operations and acreage info
  has.ops <- !is.na(bin.table$irr.ops)
  has.acres <- !is.na(bin.table$irr.ops) & bin.table$irr.acres != "(D)"
  bin.table$irr.AcresperOp[has.acres] <- as.numeric(paste(bin.table$irr.acres[has.acres]))/
    as.numeric(paste(bin.table$irr.ops[has.acres]))
  size.avg <-bin.char$avgsize[i]
  bin.table$irr.perc <- bin.table$irr.AcresperOp/size.avg
  
  # Save info to bin.char
  bin.char$cntys_w_data[i] <- sum(has.acres)
  bin.char$avg_perc_irr[i] <- mean(bin.table$irr.perc,na.rm=TRUE)
  bin.char$cntys_w_Ds[i] <- sum(has.ops) - sum(has.acres)
  
  # create and save figure
  setwd(WUDR_github)
  ppi<-300
  filename<-paste0(size.bins[i],".png")
  png(file=filename,width=4*ppi,height=4*ppi,res=ppi)
  hist(bin.table$irr.perc, main=size.bins[i], xlab="Percent of Farm Irrigated")
  abline(v=mean(bin.table$irr.perc,na.rm=TRUE), col="red",lwd=2)
  dev.off()
  
  # Save size category table to list
  bin.list[[i]] <- bin.table
}

## Suggested approach for dealing with D values
#   for each county
#     calculate "missing acreage" 
#       (difference between county irr acreage and sum of size bin irr acreage)
#     for each size w/ (D)
#     estimate acreage based on average percent irrigated for all counties with that size bin
#     sum D estimated acreage values 
#     rescale D estimated values so sum in each (D) county equals "missing" acreage
#
# Also: report estimate acreage in each county that is below reporting threshold
#
#

write.csv(bin.char, paste0(WUDR_github,"/csv_files/bin.char.csv"))


