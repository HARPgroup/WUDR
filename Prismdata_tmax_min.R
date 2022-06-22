WUDR_github<-setwd("F:/My Drive/WUDR/WUDR_Github/WUDR/DEQ_data")
# load(paste0(WUDR_github, "/DEQ-foundational_datset_All_years.RData")
setwd(WUDR_github)
pacman::p_load(tidyverse, rgdal, tmap, lubridate)
options(scipen=999)
list.files()


Precip_prism <- read.csv(paste0(WUDR_github,"/CountyPPT.csv"))
Tmax_prism <- read.csv(paste0(WUDR_github,"/CountyTMAX.csv"))
Tmin_prism <- read.csv(paste0(WUDR_github,"/CountyTMIN.csv"))
Tmean_prism <- read.csv(paste0(WUDR_github,"/CountyTMEAN.csv"))


Prism_Dates <- read.csv(paste0(WUDR_github,"/Dates.csv"))
Prism_States <- read.csv(paste0(WUDR_github,"/StateNames.csv"))
Prism_Counties <- read.csv(paste0(WUDR_github,"/CountyNames.csv"))



Prism_dat_convertion <- function(Dat){

names(Dat) <- substring(names(Dat), 2)




colnames(Prism_Counties) <- c("Id", "County")                             


colnames(Prism_States) <- c("Id", "State")                                    

loc_dat <- merge.data.frame(Prism_Counties, Prism_States, by= "Id")

VA_loc <- loc_dat %>% 
  filter(State == "VA")

PPT_VA <- Dat[,(colnames(Dat) %in% VA_loc$Id)]
colnames(PPT_VA) <- VA_loc$County

PPT_VA$Date <- Prism_Dates$x


PPT_VA$Date<- as.Date(PPT_VA$Date)

PPT_VA$Month <- lubridate::month(PPT_VA$Date)

PPT_VA$Year <- lubridate::year(PPT_VA$Date)

PPT_VA <-  PPT_VA[,c(134:136, 1:133)]


PPT_VA <- PPT_VA %>% 
  subset(select = - Date) %>% 
  filter(Year >= 2002)

return(PPT_VA)

}

Tmax <- Prism_dat_convertion(Tmax_prism)
Tmin <- Prism_dat_convertion(Tmin_prism)
Tmean <- Prism_dat_convertion(Tmean_prism)
ppt <- Prism_dat_convertion(Precip_prism)

D <- diversity(ppt$`Newport News`)
pop = ppt$`Newport News`
N = sum(pop)
p = n/N
H = -sum(p*log(p))

PPT_Growing_Season <- PPT_VA %>% 
  subset(select = - Date) %>% 
  filter(Year >= 2002) %>% 
  filter(between(Month, 6,8))

PPT_Growing_Season <- PPT_Growing_Season[, colSums(is.na(PPT_Growing_Season)) != nrow(PPT_Growing_Season)]



# Build loop for this 
ppt_df <- matrix(NA, nrow(PPT_Growing_Season), ncol(PPT_Growing_Season))
colnames(ppt_df) <- colnames(PPT_Growing_Season)


for (i in 3:ncol(ppt_df)) {
  c <-  as.data.frame(PPT_Growing_Season[,i]) %>%
    mutate(tot = lag(PPT_Growing_Season[,i], 1, default = 0) + PPT_Growing_Season[,i],
           runoff = case_when(tot >= 38.5 ~  tot-38.5,
                              tot <  38.5 ~ 0))
  c$runoff2 <- ifelse(c$`PPT_Growing_Season[, i]` == 0, 0, c$runoff)
  
  c$ppt <- c$`PPT_Growing_Season[, i]`-c$runoff2
  ppt <- ifelse(c$ppt < 0, 0, c$ppt)
  
  ppt_df[,i] <- ppt
  rm(c)
}

ppt_df[,1] <- PPT_Growing_Season[,1]
ppt_df[,2] <- PPT_Growing_Season[,2]