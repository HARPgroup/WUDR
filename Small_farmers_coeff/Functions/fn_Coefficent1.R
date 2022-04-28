# Load NASS data

load(paste0(WUDR_github, "/dat_load/Quickstats_All_years_data.Rdata"))

##QS_data function takes Year as an input and gives three files required. 
#binned_operations- The no of operations in each bin size for a census year
#binned_irrigated_area - Irrigated area in each bin size for a census year
#County_Summary - Summary Census data in each county 


QS_data <- function(YEAR){
  County_summary <- subset(nass_binned, Domain == "TOTAL") #Total gives county Totals for irrigated area and Irrigated operations
  # County_summary <- County_summary %>% 
  #   dplyr::select(Year = "Year", County = "County" ,County_Code = "County_Code", 
  #                 Irr.Ops = "Value.y", Irr.Area.Acres = "Irrigated_Area")
  
  dat_YEAR_QS <-   filter(County_summary, Year == YEAR ) # County Totals for a particular year
  
  QS_year <- filter(nass_binned, Year == YEAR )  # This gives irrigated acreage and operations for each size bin in a county for a particular year
  
  ## Count number of counties with irrigation data by farm size
  size.id <- grep(pattern="AREA OPERATED", QS_year$Size.bins) 
  farm.size <- QS_year[size.id ,]              # Remove the total values
  
  n.counties <- length(unique(farm.size$County_Code))
  n.sizebins <- length(unique(farm.size$Size.bins))
  
  farm.size.ops <- matrix(data=NA, ncol=n.sizebins, nrow=n.counties)
  colnames(farm.size.ops) <- unique(farm.size$Size.bins)
  rownames(farm.size.ops) <- unique(farm.size$County_Name)
  
  farm.size.size <- farm.size.ops
  
  for (c in 1:n.counties){
    county.data <- subset(farm.size, County_Code == unique(farm.size$County_Code)[c])
    county.ops <- county.data[, -c(7,8)]
    county.sizes <- county.data[, -c(7,9)]
    for (s in 1:n.sizebins){
      size.ops<-subset(county.ops, Size.bins == unique(farm.size$Size.bins)[s])
      size.sizes<-subset(county.sizes, Size.bins == unique(farm.size$Size.bins)[s])
      if (dim(size.ops)[1] == 1) {farm.size.ops[c,s] <- paste(size.ops$Irr.Ops)}
      if (dim(size.sizes)[1] == 1) {farm.size.size[c,s] <- paste(size.sizes$Irr.Area.Acres)}
    }
  }
  rm(county.ops, county.sizes, county.data)
  
  
  # Summary Table 
  data.summary <- data.frame(County_Name = unique(farm.size$County_Name),
                             County_Code = rep(NA,n.counties),
                             Irrigated_Operations = rep(NA, n.counties),
                             Irrigated_Acreage = rep(NA, n.counties),
                             Size_Bins = rep(NA, n.counties),
                             Ops_Bins = rep(NA, n.counties)
  )
  data.summary$County_Name <- as.character(paste(data.summary$County_Name))
  
  
  for (c in 1:n.counties){
    County_Name<-data.summary$County_Name[c]
    data.summary$County_Code[c] <- as.character(paste(dat_YEAR_QS$County_Code[
      as.character(paste(dat_YEAR_QS$County_Name)) == County_Name]))
    data.summary$Irrigated_Operations[c] <- dat_YEAR_QS$Irr.Ops[
      as.character(paste(dat_YEAR_QS$County_Name)) == County_Name]
    data.summary$Irrigated_Acreage[c] <- dat_YEAR_QS$Irr.Area.Acres[
      as.character(paste(dat_YEAR_QS$County_Name)) == County_Name]
    
    # Binned data
    county.ops<-farm.size.ops[rownames(farm.size.ops) == County_Name,]
    data.summary$Ops_Bins[c] <-  sum(!is.na(county.ops))
    county.sizes<-farm.size.size[rownames(farm.size.size) == County_Name,]
    data.summary$Size_Bins[c]<- sum(!is.na(county.sizes) & county.sizes != -9999)
  }
  
  binned_operations <<- as.data.frame(farm.size.ops[, c(2,3,7,9,10,11,12,5,6,8,1,4)]) # No of operations for each size bin in a county
  binned_irrigated_area  <<- as.data.frame(farm.size.size[, c(2,3,7,9,10,11,12,5,6,8,1,4)]) # Irrigated Acreage for each size bin in a county
  County_Summary <<- data.summary   # Totals Summary
   
  ##write.csv(County_Summary, paste0(WUDR_github,"/csv_files/", YEAR, "County_data_NASS.csv")) 
  
  
  
  ####################################################Bin dat summary
  
  ################################################################################
  ####Bin dat summary ###########################################################
  
  data.ops <- binned_operations
  data.ops <-cbind(rownames(data.ops),data.ops)
  data.size <- binned_irrigated_area
  data.size <-cbind(rownames(data.size),data.size)
  
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
  bin.table <- data.frame(County=as.character(data.ops[,1]), 
                          irr.ops=rep(NA,dim(data.ops)[1]), 
                          irr.acres=rep(NA,dim(data.ops)[1]),
                          irr.AcresperOp=rep(NA,dim(data.ops)[1]),
                          irr.perc=rep(NA,dim(data.ops)[1]))
  bin.list <- rep(list(bin.table),length(size.bins))
  
  for (i in 1:length(size.bins)){
    bin.table <- bin.list[[i]]
    size <- bin.char$size[i]
    bin.table$irr.ops <- data.ops[,colnames(data.ops) == size]
    bin.table$irr.acres <- data.size[,colnames(data.size) == size]
    
    # remove any commas in irr.acres
    bin.table$irr.acres <- gsub(pattern=",",replacement="",x=bin.table$irr.acres)
    
    # How many counties have operations and acreage info
    has.ops <- !is.na(bin.table$irr.ops)
    has.acres <- !is.na(bin.table$irr.ops) & bin.table$irr.acres != "-9999"
    bin.table$irr.AcresperOp[has.acres] <- as.numeric(paste(bin.table$irr.acres[has.acres]))/
      as.numeric(paste(bin.table$irr.ops[has.acres]))
    size.avg <-bin.char$avgsize[i]
    bin.table$irr.perc <- bin.table$irr.AcresperOp/size.avg
    
    # Save info to bin.char
    bin.char$cntys_w_data[i] <- sum(has.acres)
    bin.char$avg_perc_irr[i] <- mean(bin.table$irr.perc,na.rm=TRUE)
    bin.char$cntys_w_Ds[i] <- sum(has.ops) - sum(has.acres)
    
    # create and save figure
    
    # ppi<-300
    # filename<-paste0(WUDR_github,"/plots/Census_data_bin.char/",YEAR,"_",size.bins[i],".png")
    # png(file=filename,width=4*ppi,height=4*ppi,res=ppi)
    # hist(bin.table$irr.perc, main=size.bins[i], xlab="Percent of Farm Irrigated")
    # abline(v=mean(bin.table$irr.perc,na.rm=TRUE), col="red",lwd=2)
    # dev.off()
    
    # Save size category table to list
    bin.list[[i]] <- bin.table
    
    bin.char <<- bin.char
  }
  
  
  
  
  ##############################################################################
  ###Rescale the data
  
  
  Irrigated.data<- as.data.frame(binned_irrigated_area)
  
  
  
  Fun_num <- function (x) {
    as.numeric(as.character(x))
  }
  
  dat<- as.data.frame (apply(Irrigated.data, 2, Fun_num))
  
  county_names <- rownames(binned_irrigated_area)
  
  Irrigated.data <- cbind(county_names, dat)
  
  Irrigated.data[is.na(Irrigated.data)] <- 0
  
  
  Colnames <- c("County", "below10", "between_10_50","between_50_70","between_70_100" 
                ,"between_100_139","between_140_179","between_180_219","between_220_259","between_260_499"
                ,"between_500_999","between_1000_1999", "Above_2000")
  
  
  colnames(Irrigated.data) <- Colnames
  
  
  
  summary.irr <- data.frame(County=as.character(Irrigated.data$County), 
                            Irri.sum=rep(NA,dim(Irrigated.data)[1]))
  
  sum_dat <- Irrigated.data[,-1]
  sum_dat[sum_dat=="-9999"]<-0
  
  summary.irr$Irri.sum <- rowSums(sum_dat, na.rm = TRUE)
  
  
  ###Load County Summary data
  census.data <- County_Summary
  
  summary.irr$County.sum <- as.numeric(census.data$Irrigated_Acreage)
  
  summary.irr$County.sum <- ifelse(summary.irr$County.sum=="-9999", summary.irr$Irri.sum, summary.irr$County.sum)
  
  summary.irr$D_area <- summary.irr$County.sum - summary.irr$Irri.sum
  
  ####
  ####Load The irrigated area again
  
  Irrigated.data<- as.data.frame(binned_irrigated_area)
  
  
  
  Fun_num <- function (x) {
    as.numeric(as.character(x))
  }
  
  dat<- as.data.frame (apply(Irrigated.data, 2, Fun_num))
  
  county_names <- rownames(binned_irrigated_area)
  
  Irrigated.data <- cbind(county_names, dat)
  
  Irrigated.data[is.na(Irrigated.data)] <- 0
  
  
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
  for (i in 1: length(area_list)) {
    x <- as.data.frame(area_list[[i]]) 
    
    x$size.bins <- rownames(x)
    x <-  filter(x ,x[,1]  == -9999)
    
    sorted[[i]] <- x
  }   ##Here Sorted list contains size bins with only D values
  
  
  size.names <- c( "below10", "between_10_50","between_50_70","between_70_100" 
                   ,"between_100_139","between_140_179","between_180_219","between_220_259","between_260_499"
                   ,"between_500_999","between_1000_1999", "Above_2000")
  
  bin.char$size <- size.names
  
  ###########################################################################
  #Load operations data ##################################################
  ######################################################################
  
  Operations.data <- binned_operations
  dat<- as.data.frame (apply(Operations.data, 2, Fun_num))
  
  county_names <- rownames(binned_operations)
  
  Operations.data <- cbind(county_names, dat)
  
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
  
  for (i in 1: length(opeartions_list)) {
    x <- as.data.frame(opeartions_list[[i]]) 
    
    x$ops.bins <- rownames(x)
    x <- x[,c(2,1)]
    opeartions_list[[i]] <- x
    rownames(opeartions_list[[i]]) <- NULL
  } 
  opeartions_list <<- opeartions_list
  pct.irr <- list ()
  
  for (i in 1:length(sorted)) {
    x <- as.data.frame(sorted[[i]]) 
    x <-  merge(bin.char[,c(1,4,7)], x , by.x = "size", by.y = "size.bins")
    x <- as.data.frame(x[,-c(4)])
    
    y <- as.data.frame(opeartions_list[[i]]) 
    
    z <- merge(x,y, by.x = "size", by.y = "ops.bins")
    pct.irr[[i]] <- z
  }  # This list contains the percentage area irrigated in each size bin where D values are absent
  
  colnames <- c("Size", "Avg_Size", "Avg_Pct_Irri", "Ops")
  
  pct.irr <- lapply(pct.irr, setNames, colnames)
  
  area.irr <- list()
  for (i in 1:length(pct.irr)) {
    
    x <- as.data.frame(pct.irr[[i]])
    x$area <- x$Avg_Pct_Irri * x$Avg_Size * x$Ops
    
    area.irr[[i]] <- x
  }  # Irrigated area for D values
  
  
  recaled_dat <- area.irr
  
  for (i in 1:length(area.irr)) {
    
    recaled_dat[[i]][6] <- Map(function(x, y)  x * y/sum(x), area.irr[[i]][5], as.numeric(summary.irr[i,4]))
    
  }
  
  sum(recaled_dat[[1]][6])
  
  # THis rescaled data list replaces the D values after rescaling it to difference
  
  colnames <- c("Size bin", "Avg_Size", "Average Pct Irri", "Ops", "Area Irrigated", " Rescaled Area")
  
  recaled_dat <- lapply(recaled_dat, setNames, colnames)
  
  names(recaled_dat) <- names(area_list)
  
  dat_D_filled <- list()
  for (i in 1:length(recaled_dat)) {
    x <- as.data.frame(recaled_dat[[i]]) 
    y <- as.data.frame(area_list[[i]])
    y$size <- rownames(y)
    z <-  full_join(x, y , by = c("Size bin"= "size"))
    z[,7] <- ifelse((z[,7] == -9999), z[,6], z[,7])
    z <- as.data.frame(z[,c(1,7)])
    z[,2] <- round(z[,2], 0)
    dat_D_filled[[i]] <- z
  }
  
  
  
  colnames <- c("Size bin",  "Area_irr_rescaled Area")
  
  
  names(dat_D_filled) <- names_list
  
  dat_D_filled <<-dat_D_filled
  
}


###################################################################################
#### Irrigated area under threshold
################################################################################

# This function calculates the area under threshold for a particular year.

fn_Area_TH <- function(threshold, YEAR){
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
    
    total <<- total
  })
  
  Non.irr.TH <- threshold 
  
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
    x$Pct.Non.repoted.of.total.irrarea   <- round (100*x$Irr.Area.Under.TH / sum(x$Irr.Area.Over.TH,x$Irr.Area.Under.TH), 2)
    x$Pct.Non.repoted.of.area.abv.TH <- round (100*x$Irr.Area.Under.TH / x$Irr.Area.Over.TH, 2)
    return(x)
  })
  
  
  Unreported <-  lapply(TH_dat, function(x) x%>% select(Pct.Non.repoted.of.total.irrarea,Pct.Non.repoted.of.area.abv.TH, Irr.Area.Under.TH,Irr.Area.Over.TH))
  
  
  df <- data.frame(matrix(unlist(Unreported), nrow=length(Unreported), byrow=TRUE))
  df$county <- names(Unreported)
  
  colnames(df) <- c("Pct.under.TH.of.total.Irr.area", "Pct.under.TH.of.Irri.area.abv.TH","Irr.Area.Under.TH", "Irr.Area.above.TH", "County") 
  df$Total.Irri.Area <- apply(df[,c(3,4)],1,sum)
  df <- df[,c(5,6,4,3,2,1)]
  df$AcreageUnderTh <- df$Total.Irri.Area*df$Pct.under.TH.of.total.Irr.area/100
  
  df <- df %>% 
    filter(Total.Irri.Area >0)
  
  Non.Reported <<-df
  #write.csv(Non.Reported, paste0(WUDR_github, "/csv_files/",YEAR,"Underthreshold_Summary.csv"))
  
}
