WUDR_github<-"F:/My Drive/WUDR/WUDR_Github/WUDR"
setwd(WUDR_github)
library(tidyverse)
library(hydroGOF)
library(scales)
library(rgdal)
library(tmap)
library(tmaptools)

options(scipen=999)
# LOAD DATA FOR LMER 
load(paste0(WUDR_github, "/Regression/LMER_input.RData"))

VA_counties<-readOGR(paste0(WUDR_github, "/VA_counties_sp"), layer="VA_counties")

county.codes <- read.csv(paste0(WUDR_github, "/csv_files/county_codes_census.csv")) 


COunty_mean <- Var_res %>% 
  dplyr::group_by(COUNTYFP) %>% 
  dplyr::summarise(Mean_DEQ_withdarwals = mean(DEQ_Irrigation_withdrawals, na.rm=TRUE))

Var_res <- left_join(Var_res,COunty_mean, by = "COUNTYFP")
Var_res$Norm_DEQ_withdarwals <- Var_res$DEQ_Irrigation_withdrawals/Var_res$Mean_DEQ_withdarwals

# Var_res = Var_res[!duplicated(Var_res[,c(1,2)]),]


dat <- left_join(Var_res[,c(1,2,6,10,11)], Var_pred, by = c("COUNTYFP", "YEAR"))
# dat <- dat %>% mutate_at(vars(-c(1,2)), funs(round(., 2)))

dat$COUNTYFP<- as.factor(dat$COUNTYFP)

dat2 <- dplyr::filter(dat, Norm_DEQ_withdarwals >0)

dat2[,c(6:14)] <- as.data.frame(scale(dat2[,c(6:14)])) #standardization mean of 0 SD 1
# dat2$YEAR <- dat2$YEAR-2002+1
dat2 <- dat2[complete.cases(dat2), ]


###################################################################################################################
## The following model was selected

Best_model<- glm(Norm_DEQ_withdarwals ~ PPT + Min_Temp + Extreme_days + 
                    GC, data = dat2)
summary(Best_model)
T2 <- summary(Best_model)

write.csv(T2$coefficients, "DEQ_thesis_table.csv")


dat2$predict = predict(Best_model)*dat2$Mean_DEQ_withdarwals
GOF <- gof(dat2$predict,dat2$DEQ_Irrigation_withdrawals)
GOF
# ggof(dat2$predict,dat2$DEQ_Irrigation_withdrawals)

###################################################################################################################
p1 <- ggplot(dat2, aes(x=DEQ_Irrigation_withdrawals,y=predict))+
  geom_point()+
  geom_smooth(method=lm)+
  labs(title= "",
       x="DEQ Reported Withdrawals (MG)", y = " Regression based DEQ Withdrawals (MG)") +
  scale_x_continuous(trans = log10_trans(), 
                     breaks = c(-1,1, 10, 100, 1000,5000), 
                     limits=c(-2,20000))+
  scale_y_continuous(trans = log10_trans(), 
                     breaks = c(-1,1, 10, 100, 1000,5000), 
                     limits=c(-2,20000))


p1<- p1 + theme_bw()
p1 <- p1+theme(axis.text.x=element_text(angle = 0, hjust = 0),
               legend.position="None", 
               legend.title=element_blank(),
               legend.box = "horizontal", 
               legend.background = element_rect(fill="white",
                                                size=0.5, linetype="solid", 
                                                colour ="white"),
               legend.text=element_text(size=12),
               axis.text=element_text(size=12, colour="black"),
               axis.title=element_text(size=14, colour="black"),
               axis.line = element_line(colour = "black", 
                                        size = 0.5, linetype = "solid"),
               axis.ticks = element_line(colour="black"),
               panel.grid.major=element_line(colour = "light grey"), 
               panel.grid.minor=element_blank())
p1

ggsave(paste0(WUDR_github,"/Regression/plots/","ThesisDEQ vs calcualted.png"), plot = p1, width = 9.5, height = 6, units = "in")

#########################################################################################################################
# Load VA Shapefile to be used later
VA_counties<-readOGR(paste0(WUDR_github, "/VA_counties_sp"), layer="VA_counties")

county.codes <- read_csv(paste0(WUDR_github, "/csv_files/county_codes_census.csv"))

load(paste0(WUDR_github,"/dat_load/PRECIP_JUNE_AUGUST_ALL_YEARS.RData")) # PRISM PPT data

load(paste0(WUDR_github,"/dat_load/PRISM_county_codes.RData")) # County Codes for PRISM


load(paste0(WUDR_github, "/dat_load/All_Years_DEQ_data_Total_nd_irr.Rdata")) 


##########################################################################################################################
## Dry Year
# DEQ reported Witdharwals

DEQ_Withdrawals <- Irri_deq_county %>% group_by(YEAR) %>% 
  filter(YEAR <2018) %>% 
  summarise(DEQ_Withdarwals = sum(Facility_withdrawal_mg))

DEQ_withdrawals_County <- Irri_deq_county %>% 
  filter(YEAR <2018) %>% 
  group_by(YEAR,COUNTYFP) %>% 
  summarise(DEQ_Withdarwals = sum(Facility_withdrawal_mg))


DEQ_withdrawals_Dry_Year <- DEQ_withdrawals_County %>% 
  filter(DEQ_Withdarwals >0) %>% 
  group_by(COUNTYFP) %>% 
  slice(which.max(DEQ_Withdarwals))

#############################################################################################################################
# Based on Effective precip approch
PPT <- lapply(ppt_list_yearly, function(x)
  mutate(x, Irrigation = round(508 - PPT,0)))
PPT <- bind_rows(PPT, .id = "Year")

PPT <- left_join(PPT,PRISM_county_codes, by = "name" )
PPT <- PPT[,c(1,5,2,3,4)]
colnames(PPT)[c(1:2,4)] <- c("YEAR", "COUNTYFP","Precip")
PPT$YEAR <- as.numeric(PPT$YEAR)

PPT$COUNTYFP <- as.factor(PPT$COUNTYFP)

PPT_DEQ_years <- left_join(dat2[,c(1:3)],PPT ,by = c("YEAR", "COUNTYFP"))
PPT_DEQ_years <- PPT_DEQ_years[,c(1,2,4,3,5,6)]

Quntiles_PPT <- PPT_DEQ_years %>% 
  group_by(COUNTYFP) %>%
  mutate(name = first(name),
         Dry_precip = min(Precip),
            Precip_25= quantile (Precip, probs = 0.25),
         Precip_mean= mean(Precip),
            Precip_75= quantile (Precip, probs = 0.75),
         Wet_precip = max(Precip))

Dry_Year <- PPT_DEQ_years %>% group_by(COUNTYFP) %>% slice(which.min(Precip))
Wet_Year <- PPT_DEQ_years %>% group_by(COUNTYFP) %>% slice(which.max(Precip))

Avg_Year <- Quntiles_PPT %>% group_by(COUNTYFP) %>%
  arrange(abs(Precip - Precip_mean)) %>%
  slice(1)

Moderate_Dry_Year <- Quntiles_PPT %>% group_by(COUNTYFP) %>%
  arrange(abs(Precip - Precip_25)) %>%
  slice(1)

Moderate_Wet_Year <- Quntiles_PPT %>% group_by(COUNTYFP) %>%
  arrange(abs(Precip - Precip_75)) %>%
  slice(1)


########################################################################################################################### 
# Predicted Variables for DRy Years
# load(paste0(WUDR_github, "/Regression/LMER_input.RData"))

# Dry_year_Predict_var <- left_join(Dry_Year[,c(1,2)],Var_pred, by=c("COUNTYFP", "YEAR"))

# dat$COUNTYFP <- as.numeric(as.character(dat$COUNTYFP))
Dry_Year$COUNTYFP <- as.factor(Dry_Year$COUNTYFP)
# dat$YEAR <- dat$YEAR+2002-1

Dry_year_Predict_var <- left_join(Dry_Year[,c(1,2)],dat2, by=c("COUNTYFP", "YEAR"))
 Dry_year_Predict_var = Dry_year_Predict_var[!duplicated(Dry_year_Predict_var$COUNTYFP),]
# Dry_year_Predict_var <- inner_join(Dry_Year,dat2, by=c("COUNTYFP", "YEAR"))
# 
# Dry_year_Predict_var <- filter(Var_pred,Dry_year_Predict_var$COUNTYFP %in% Var_pred$COUNTYFP)
# Dry_year_Predict_var <- dplyr::filter(Dry_year_Predict_var, Norm_DEQ_withdarwals >0)

# Drydata <- filter(Dry_year_Predict_var, Norm_DEQ_withdarwals >0)
# 
 
Drydata <- filter(Dry_year_Predict_var, Mean_DEQ_withdarwals >0)
Drydata_input <- as.data.frame(Drydata[,c(6,8,12,14)])

Drydata$Predicted<- predict(Best_model, Drydata_input)*Drydata$Mean_DEQ_withdarwals



GOF_dry <- gof(Drydata$Predicted,Drydata$DEQ_Irrigation_withdrawals)
GOF_dry
########################################################################################################################### 
# Predicted Variables for DRy Years
Wet_Year$COUNTYFP <- as.factor(Wet_Year$COUNTYFP)
# dat$YEAR <- dat$YEAR+2002-1

Wet_Year_Predict_var <- left_join(Wet_Year[,c(1,2)],dat2, by=c("COUNTYFP", "YEAR"))
Wet_Year_Predict_var = Wet_Year_Predict_var[!duplicated(Wet_Year_Predict_var$COUNTYFP),]
 

Wetdata <- filter(Wet_Year_Predict_var, Mean_DEQ_withdarwals >0) # We need mean for prediction

Wetdata_input <- as.data.frame(Wetdata[,c(6,8,12,14)])
Wetdata_input$Extreme_days <-ifelse(is.nan(Wetdata_input$Extreme_days) ,0 , Wetdata_input$Extreme_days)
Wetdata$Predicted<- predict(Best_model, Wetdata_input)*Wetdata$Mean_DEQ_withdarwals



GOF_Wet <- gof(Wetdata$Predicted,Wetdata$DEQ_Irrigation_withdrawals)
GOF_Wet

###############################
dat_test <- left_join(Drydata[,c(1,15)],Wetdata[,c(1,15)], by = "COUNTYFP")
colnames(dat_test)[c(2,3)] <- c("Dry Year", "Wet Year")

#############################################################################################################################
Wetdata$Pct_Mean <- 100* Wetdata$Predicted/Wetdata$Mean_DEQ_withdarwals
plot_Dat <- Wetdata[,c(1,17)]
plot_Dat$COUNTYFP <- as.numeric(as.character(plot_Dat$COUNTYFP))
plot_Dat<-sp::merge(VA_counties,plot_Dat, by = "COUNTYFP")
# plot_Dat2 <- plot_Dat[ ! st_is_empty( plot_Dat ) , ]
tmap_mode("view")
tmap_mode("plot")
p1<-tm_shape(plot_Dat)+
  tm_polygons("Pct_Mean", title = "Percentage(%)",
                breaks = c(0,50,75,100,125,150,200,Inf),
              # n=5,style="jenks",
              textNA = "Missing DEQ Irrigation Withdrawals",
              id="NAMELSAD")+
  # tm_text("NAME", size = 0.3)+
  tm_layout(main.title = "Percentage of mean withdrawals  \nin wet years",
            legend.outside = FALSE,
            legend.title.size = 1.2,
            legend.text.size = 0.8,
            legend.position = c("left","top"),
            legend.bg.alpha = 1)
p1

#tmap_save(p1, paste0(WUDR_github,"/Regression/plots/Wet Year predicted withdrwals.png"),  width = 8.5, height = 5, units = 'in')


#############################################################################################################################

Drydata$Pct_Mean <- 100* Drydata$Predicted/Drydata$Mean_DEQ_withdarwals
plot_Dat <- Drydata[,c(1,17)]
plot_Dat$COUNTYFP <- as.numeric(as.character(plot_Dat$COUNTYFP))
plot_Dat<-sp::merge(VA_counties,plot_Dat, by = "COUNTYFP")

tmap_mode("view")
tmap_mode("plot")
p2<-tm_shape(plot_Dat)+
  tm_polygons("Pct_Mean", title = "Percentage(%)",
               breaks = c(0,50,75,100,125,150,200,Inf),
              # n=5,style="jenks",
              textNA = "Missing DEQ Irrigation Withdrawals",
              id="NAMELSAD")+
  # tm_text("NAME", size = 0.3)+
  tm_layout(main.title = "Percentage of mean withdrawals  \nin dry years",
            legend.outside = FALSE,
            legend.title.size = 1.2,
            legend.text.size = 0.8,
            legend.position = c("left","top"),
            legend.bg.alpha = 1)
p2


#tmap_save(p2, paste0(WUDR_github,"/Regression/plots/Dry Year predicted withdrwals.png"),  width = 8.5, height = 5, units = 'in')

############################################################################################################################


Avg_Year$COUNTYFP <- as.factor(Avg_Year$COUNTYFP)
# dat$YEAR <- dat$YEAR+2002-1

Avg_Year_Predict_var <- left_join(Avg_Year[,c(1,2)],dat2, by=c("COUNTYFP", "YEAR"))
Avg_Year_Predict_var = Avg_Year_Predict_var[!duplicated(Avg_Year_Predict_var$COUNTYFP),]


Avgdata <- filter(Avg_Year_Predict_var, Mean_DEQ_withdarwals >0) # We need mean for prediction

Avgdata_input <- as.data.frame(Avgdata[,c(6,8,12,14)])
# Avgdata_input$Extreme_days <-ifelse(is.nan(Avgdata_input$Extreme_days) ,0 , Avgdata_input$Extreme_days)
Avgdata$Predicted<- predict(Best_model, Avgdata_input)*Avgdata$Mean_DEQ_withdarwals



GOF_Avg <- gof(Avgdata$Predicted,Avgdata$DEQ_Irrigation_withdrawals)
GOF_Avg

#############################################################################################################################
Avgdata$Pct_Mean <- 100* Avgdata$Predicted/Avgdata$Mean_DEQ_withdarwals
plot_Dat <- Avgdata[,c(1,17)]

plot_Dat$COUNTYFP <- as.numeric(as.character(plot_Dat$COUNTYFP))
plot_Dat<-sp::merge(VA_counties,plot_Dat, by = "COUNTYFP")

tmap_mode("view")
tmap_mode("plot")
p3<-tm_shape(plot_Dat)+
  tm_polygons("Pct_Mean", title = "Percentage(%)",
               breaks = c(0,50,75,100,125,150,200,Inf),
              # n=5,style="jenks",
              textNA = "Missing DEQ Irrigation Withdrawals",
              id="NAMELSAD")+
  # tm_text("NAME", size = 0.3)+
  tm_layout(main.title = "Percentage of mean withdrawals  \nin average years",
            legend.outside = FALSE,
            legend.title.size = 1.2,
            legend.text.size = 0.8,
            legend.position = c("left","top"),
            legend.bg.alpha = 1)
p3

#tmap_save(p3, paste0(WUDR_github,"/Regression/plots/Avg Year predicted withdrwals.png"),  width = 8.5, height = 5, units = 'in')



############################################################################################################################
# Medium dry

Moderate_Dry_Year$COUNTYFP <- as.factor(Moderate_Dry_Year$COUNTYFP)
# dat$YEAR <- dat$YEAR+2002-1

Moderate_Dry_Year_Predict_var <- left_join(Moderate_Dry_Year[,c(1,2)],dat2, by=c("COUNTYFP", "YEAR"))
Moderate_Dry_Year_Predict_var = Moderate_Dry_Year_Predict_var[!duplicated(Moderate_Dry_Year_Predict_var$COUNTYFP),]


Moderate_Drydata <- filter(Moderate_Dry_Year_Predict_var, Mean_DEQ_withdarwals >0) # We need mean for prediction

Moderate_Drydata_input <- as.data.frame(Moderate_Drydata[,c(6,8,12,14)])
# Moderate_Drydata_input$Extreme_days <-ifelse(is.nan(Moderate_Drydata_input$Extreme_days) ,0 , Moderate_Drydata_input$Extreme_days)
Moderate_Drydata$Predicted<- predict(Best_model, Moderate_Drydata_input)*Moderate_Drydata$Mean_DEQ_withdarwals



GOF_Moderate_Dry <- gof(Moderate_Drydata$Predicted,Moderate_Drydata$DEQ_Irrigation_withdrawals)
GOF_Moderate_Dry

#############################################################################################################################

Moderate_Drydata$Pct_Mean <- 100* Moderate_Drydata$Predicted/Moderate_Drydata$Mean_DEQ_withdarwals
plot_Dat <- Moderate_Drydata[,c(1,17)]

plot_Dat$COUNTYFP <- as.numeric(as.character(plot_Dat$COUNTYFP))
plot_Dat<-sp::merge(VA_counties,plot_Dat, by = "COUNTYFP")

tmap_mode("view")
tmap_mode("plot")
p4<-tm_shape(plot_Dat)+
  tm_polygons("Pct_Mean", title = "Percentage(%)",
              breaks = c(0,50,75,100,125,150,200,Inf),
              # n=5,style="jenks",
              textNA = "Missing DEQ Irrigation Withdrawals",
              id="NAMELSAD")+
  # tm_text("NAME", size = 0.3)+
  tm_layout(main.title = "Percentage of mean withdrawals  \nin moderate dry years",
            legend.outside = FALSE,
            legend.title.size = 1.2,
            legend.text.size = 0.8,
            legend.position = c("left","top"),
            legend.bg.alpha = 1)
p4


#tmap_save(p4, paste0(WUDR_github,"/Regression/plots/Moderate_Dry Year predicted withdrwals.png"),  width = 8.5, height = 5, units = 'in')



###############################################################################################################################


Moderate_Wet_Year$COUNTYFP <- as.factor(Moderate_Wet_Year$COUNTYFP)
# dat$YEAR <- dat$YEAR+2002-1

Moderate_Wet_Year_Predict_var <- left_join(Moderate_Wet_Year[,c(1,2)],dat2, by=c("COUNTYFP", "YEAR"))
Moderate_Wet_Year_Predict_var = Moderate_Wet_Year_Predict_var[!duplicated(Moderate_Wet_Year_Predict_var$COUNTYFP),]


Moderate_Wetdata <- filter(Moderate_Wet_Year_Predict_var, Mean_DEQ_withdarwals >0) # We need mean for prediction

Moderate_Wetdata_input <- as.data.frame(Moderate_Wetdata[,c(6,8,12,14)])
# Moderate_Wetdata_input$Extreme_days <-ifelse(is.nan(Moderate_Wetdata_input$Extreme_days) ,0 , Moderate_Wetdata_input$Extreme_days)
Moderate_Wetdata$Predicted<- predict(Best_model, Moderate_Wetdata_input)*Moderate_Wetdata$Mean_DEQ_withdarwals



GOF_Moderate_Wet <- gof(Moderate_Wetdata$Predicted,Moderate_Wetdata$DEQ_Irrigation_withdrawals)
GOF_Moderate_Wet

#############################################################################################################################

Moderate_Wetdata$Pct_Mean <- 100* Moderate_Wetdata$Predicted/Moderate_Wetdata$Mean_DEQ_withdarwals
plot_Dat <- Moderate_Wetdata[,c(1,17)]

plot_Dat$COUNTYFP <- as.numeric(as.character(plot_Dat$COUNTYFP))
plot_Dat<-sp::merge(VA_counties,plot_Dat, by = "COUNTYFP")

tmap_mode("view")
tmap_mode("plot")
p5<-tm_shape(plot_Dat)+
  tm_polygons("Pct_Mean", title = "Percentage(%)",
              breaks = c(0,50,75,100,125,150,200,Inf),
              # n=5,style="jenks",
              textNA = "Missing DEQ Irrigation Withdrawals",
              id="NAMELSAD")+
  # tm_text("NAME", size = 0.3)+
  tm_layout(main.title = "Percentage of mean withdrawals \nin moderate wet years",
            legend.outside = FALSE,
            legend.title.size = 1.2,
            legend.text.size = 0.8,
            legend.position = c("left","top"),
            legend.bg.alpha = 1)
p5

#tmap_save(p5, paste0(WUDR_github,"/Regression/plots/Moderate_Wet Year predicted withdrwals.png"),  width = 8.5, height = 5, units = 'in')


p6 <- tmap_arrange(p1,p2, p3, p4,p5,  widths = c(.75, .75))

tmap_save(p6, paste0(WUDR_github,"/Regression/plots/All_scenarios.png"),  width = 12, height = 12, units = 'in')



################################################################################################################################

StateSumamries <- list(Drydata,Moderate_Drydata,Avgdata,Moderate_Wetdata,Wetdata)
names(StateSumamries) <- c("Drydata","Moderate_Drydata","Avgdata","Moderate_Wetdata","Wetdata")

State_Summary_Scenarios<- list()

for (i in 1:5) {
  State_Summary_Scenarios[[i]] <-  StateSumamries[[i]][,c(3,4,16)] %>% 
    dplyr::summarise(DEQ_Withdrawals = sum(DEQ_Irrigation_withdrawals, na.rm = TRUE),
                     Mean_Withdarwals = sum(Mean_DEQ_withdarwals,na.rm = TRUE),
                     Modelled_withdarwals = sum(Predicted,na.rm = TRUE))
}


names(State_Summary_Scenarios) <- c("Drydata","Moderate_Drydata","Avgdata","Moderate_Wetdata","Wetdata")

State_Summary_Scenarios <- bind_rows(State_Summary_Scenarios, .id = "Scenario")


################################################################################################################
# County Level 
COunty_mean$COUNTYFP <- as.factor(COunty_mean$COUNTYFP)

COunty_mean <- COunty_mean %>% 
  filter(Mean_DEQ_withdarwals >0)

All_Scenarios<- purrr::reduce(list(COunty_mean, Drydata[,c(1,16)],Moderate_Drydata[,c(1,16)],Avgdata[,c(1,16)], Moderate_Wetdata[,c(1,16)],Wetdata[,c(1,16)]), dplyr::full_join, by = 'COUNTYFP') # Irrigation coefficient for counties with data

colnames(All_Scenarios)[2:7] <- c("Mean_DEQ" ,"Dry_withdrawals","Moderate_Dry_withdrawals", "Average_withdarwals","Moderate_Wet_withdrawals","Wet_withdrawals")


write.csv(State_Summary_Scenarios, paste0(WUDR_github,"/Regression/Dry_wet_summary.csv"), row.names = FALSE)












