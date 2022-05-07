WUDR_github<-"F:/My Drive/WUDR/WUDR_Github/WUDR"
setwd(WUDR_github)
options(scipen=999)
pacman::p_load("tidyverse", "stringr","rgdal")
pacman::p_load("data.table")
pacman::p_load("tmap", "sp")
load(paste0(WUDR_github, "/dat_load/Dat_filled_5_03.RData")) 


########################################################################################################
######## Get unique crops in different years                                          ##################
#######################################################################################################
Unique_Crops <- list()
Unique_Crops <- lapply(Dat_filled, function(x)  as.data.frame(unique(x$commodity_desc)))
Unique_Crops <- lapply(Unique_Crops, setNames, c("Unique_Crops"))

# write.csv(Unique_Crops[[i]], "Unique_Crops.csv")

CWR <- read.csv(paste0(WUDR_github,"/csv_files/Coefficient3/CWR.csv"))


########################################################################################################
# GET PPT at county scale for all counties #############################################################
#######################################################################################################



# So far we have used different ppt values. Yearly_PPT_Growing_Season.csv is the rainfall in enitre growing season. 
# max_ppt_Yearly_PPT_Growing_Season.csv is the rainfall with daily maximum. Any rainfall above 38.5 mm is considered runoff.
# Third and the currently used one is the rolling_max_ppt_Yearly_PPT_Growing_Season.csv

# Yearly_PPT_Growing_Season <- read.csv(paste0(WUDR_github,"/csv_files/Coefficient3/Yearly_PPT_Growing_Season.csv"))

#PPt values changed
# Yearly_PPT_Growing_Season <- read.csv(paste0(WUDR_github,"/csv_files/Coefficient3/max_ppt_Yearly_PPT_Growing_Season.csv"))

Yearly_PPT_Growing_Season <- read.csv(paste0(WUDR_github,"/csv_files/Coefficient3/June_Augustrolling_max_ppt_Yearly_PPT_Growing_Season.csv"))
ldata <-  pivot_longer(Yearly_PPT_Growing_Season, cols = (3:ncol(Yearly_PPT_Growing_Season)))
ldata <- ldata[,-c(1)]
ldata <- ldata[complete.cases(ldata), ]
ldata$name <- toupper(ldata$name)
ldata$name <-gsub(".", " ", ldata$name , fixed=TRUE)
ldata$name <-gsub("1", "", ldata$name , fixed=TRUE)
ldata$name[ldata$name == "SUFFOLK"] <- "SUFFOLK CITY"
ldata$name[ldata$name == "VIRGINIA BEACH"] <- "VIRGINIA BEACH CITY"
ldata$name[ldata$name == "CHESAPEAKE"] <- "CHESAPEAKE CITY"
ldata$name[ldata$name == "FAIRFAX "] <- "FAIRFAX"
colnames(ldata)[3] <- "PPT"
ldata$PPT <- round(ldata$PPT, 2)
ppt_list_yearly <- split( ldata , f = ldata$Year)

# save(ppt_list_yearly, file = paste0(WUDR_github,"/dat_load/June_August_Effective precipitation.RData"))
Crop_irri_dat <- list()

for (i in 1:4) {
  Crop_irri_dat[[i]] <- left_join(Dat_filled[[i]][,c(1,2,4,6,8,9,15)], CWR, by = c("commodity_desc"= "Crop") )
  Crop_irri_dat[[i]] <- left_join(Crop_irri_dat[[i]], ppt_list_yearly[[i]][,c(2:3)], by = c("county_name"= "name") )
}

Crop_irri_dat <- lapply(Crop_irri_dat, function(x) x %>%
                          mutate(Deficit = WR- PPT))

Crop_irri_dat <- lapply(Crop_irri_dat, function(x) x %>%
                          mutate(Area_mm = Final_area * 4046856422.4))

Crop_irri_dat <- lapply(Crop_irri_dat, function(x) x %>%
                          mutate(Deficit_Irrigated_mg = round(Area_mm*Deficit*0.000000264172/1000000,2))) # COnvert to million  gallons
names(Crop_irri_dat) <- names(Dat_filled)
#########################################################################################################
# Creating Summary Plots




#Positive values only
Positive_summary <- lapply(Crop_irri_dat, function(x) x %>%
                             filter(Deficit_Irrigated_mg >=0) %>% 
                             group_by(county_code) %>% 
                             summarise(county_name = first(county_name),
                                       year = first(year),
                                       Final_area = sum(Final_area, na.rm = TRUE),
                                       WR = sum(WR),
                                       PPT = first(PPT),
                                       Deficit = sum(Deficit),
                                       Deficit_Irrigated_mg = sum(Deficit_Irrigated_mg, na.rm = TRUE)))


names(Positive_summary) <- names(Dat_filled)

# save(Positive_summary, file = paste0(WUDR_github,"/dat_load/Irrigation_deficit.RData"))
