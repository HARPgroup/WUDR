WUDR_github<-"F:/My Drive/WUDR/WUDR_Github/WUDR"
setwd(WUDR_github)

library(tidyverse) 
library(scales)
library(lme4)
library(lmerTest)
library(hydroGOF)
library(pbkrtest)
library(rgdal)
options(scipen = 9999)

# LOAD DATA FOR LMER 
load(paste0(WUDR_github, "/Regression/LMER_input.RData"))
source("F:/My Drive/WUDR/WUDR_Github/WUDR/Regression/VIF Function.R", echo=TRUE)  # LOAD VIF FUNCTION

dat <- left_join(Var_res[,c(1,2,6)], Var_pred, by = c("COUNTYFP", "YEAR"))
# dat <- dat %>% mutate_at(vars(-c(1,2)), funs(round(., 2)))

dat$COUNTYFP<- as.factor(dat$COUNTYFP)

dat2 <- dplyr::filter(dat, DEQ_Irrigation_withdrawals >0)

dat2[,c(4:12)] <- as.data.frame(scale(dat2[,c(4:12)])) #standardization mean of 0 SD 1
dat2$YEAR <- dat2$YEAR-2002+1
dat2 <- dat2[complete.cases(dat2), ]

##MODEL 1 All variables
DEQ_model <- lmer(log(DEQ_Irrigation_withdrawals) ~ PPT +Min_Temp+Zero_PPT_days+ Extreme_days+SDI+GC+(YEAR|COUNTYFP)+(1|COUNTYFP),
                                  data=dat2, control = lmerControl(optimizer ="Nelder_Mead"),REML = TRUE)
DEQ_model

# vif.mer(DEQ_model)
 drop1(DEQ_model,test="Chisq")
# anova(DEQ_model)


step_res  <- step(DEQ_model, direction = c("backwards"))
final <- get_model(step_res)
anova(final)
vif.mer(final)

# final <- lmer(log(DEQ_Irrigation_withdrawals) ~ PPT +Min_Temp+Zero_PPT_days+ Extreme_days+(YEAR|COUNTYFP)+(1|COUNTYFP),
#                                   data=dat2, control = lmerControl(optimizer ="Nelder_Mead"),REML = TRUE)
# final
# 
# anova(final)

fixef(final)

#####################################################################################
# Predict

dat2$Predict<-  exp(predict(final))
gof(dat2$Predict,dat2$DEQ_Irrigation_withdrawals)

## Make a plot for Predicted vs Observed

plot_dat <- pivot_longer(dat2[,c(3,13)],cols = c(1,2), names_to = "Type",values_to ="Withdrawals")
plot_dat$Dat <- NA

plot_dat$Dat<- ifelse(plot_dat$Type == "Predict", "Modelled DEQ withdrawals", "Reported DEQ withdrawals")


p1 <- ggplot(dat2, aes(x=DEQ_Irrigation_withdrawals,y=Predict))+
  geom_point()+
  geom_smooth(method=lm)+
  labs(title= "Reported irrigation and Modelled withdrawals",
       x="Reported DEQ Irrigation Withdrawals (MG)", y = " Regression Withdrawals (MG)") +
  scale_x_continuous(trans = log10_trans(), 
                     breaks = c(-1,1, 10, 100, 1000,4500), 
                     limits=c(-2,7000))+
  scale_y_continuous(trans = log10_trans(), 
                     breaks = c(-1,1, 10, 100, 1000, 4500), 
                     limits=c(-2,7000))


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
# 
 # ggsave(paste0(WUDR_github,"/Regression/plots/","ThesisDEQ irrigation withdrawals vs predicted.png"), plot = p1, width = 9.5, height = 6, units = "in")
# library(plotly)
# ggplotly(p1)


########################################################################################################
#Get data for each scenario

load(paste0(WUDR_github,"/dat_load/PRECIP_JUNE_AUGUST_ALL_YEARS.RData")) # PRISM PPT data
load(paste0(WUDR_github,"/dat_load/PRISM_county_codes.RData")) 
load(paste0(WUDR_github, "/Regression/LMER_input.RData"))
source("F:/My Drive/WUDR/WUDR_Github/WUDR/Regression/VIF Function.R", echo=TRUE)  # LOAD VIF FUNCTION

dat <- left_join(Var_res[,c(1,2,6)], Var_pred, by = c("COUNTYFP", "YEAR"))
# dat <- dat %>% mutate_at(vars(-c(1,2)), funs(round(., 2)))

dat$COUNTYFP<- as.factor(dat$COUNTYFP)

dat2 <- dplyr::filter(dat, DEQ_Irrigation_withdrawals >0)
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
  dplyr::group_by(COUNTYFP) %>%
  dplyr::mutate(name = first(name),
         Dry_precip = min(Precip),
            Precip_25= quantile (Precip, probs = 0.25),
         Precip_mean= mean(Precip),
            Precip_75= quantile (Precip, probs = 0.75),
         Wet_precip = max(Precip))

Dry_Year <- PPT_DEQ_years %>% group_by(COUNTYFP) %>% slice(which.min(Precip))
# Wet_Year <- PPT_DEQ_years %>% group_by(COUNTYFP) %>% slice(which.max(Precip))

Avg_Year <- Quntiles_PPT %>% group_by(COUNTYFP) %>%
  arrange(abs(Precip - Precip_mean)) %>%
  slice(1)

Moderate_Dry_Year <- Quntiles_PPT %>% group_by(COUNTYFP) %>%
  arrange(abs(Precip - Precip_25)) %>%
  slice(1)

#######################################################################
# Regression for dry year
Dry_Year$COUNTYFP <- as.factor(Dry_Year$COUNTYFP)

Dry_year_Predict_var <- left_join(Dry_Year[,c(1,2)],dat2, by=c("COUNTYFP", "YEAR"))
 Dry_year_Predict_var = Dry_year_Predict_var[!duplicated(Dry_year_Predict_var$COUNTYFP),]
Dry_year_Predict_var <- Dry_year_Predict_var[complete.cases(Dry_year_Predict_var), ]

 
Drydata <- filter(Dry_year_Predict_var, DEQ_Irrigation_withdrawals    >0)
Drydata_input <- as.data.frame(Drydata[,c(1,2,4,6,9,10)])

Drydata_input$YEAR <- Drydata_input$YEAR-2002+1
Drydata_input[,c(3:6)] <- as.data.frame(scale(Drydata_input[,c(3:6)])) #standardization mean of 0 SD 1


Drydata$Predicted<- exp(predict(final, Drydata_input, allow.new.levels = TRUE))



GOF_dry <- gof(Drydata$Predicted,Drydata$DEQ_Irrigation_withdrawals)
GOF_dry

#######################################################################
# Regression for Moderate dry year
Moderate_Dry_Year$COUNTYFP <- as.factor(Moderate_Dry_Year$COUNTYFP)

Moderate_Dry_year_Predict_var <- left_join(Moderate_Dry_Year[,c(1,2)],dat2, by=c("COUNTYFP", "YEAR"))
 Moderate_Dry_year_Predict_var = Moderate_Dry_year_Predict_var[!duplicated(Moderate_Dry_year_Predict_var$COUNTYFP),]

Moderate_Dry_year_Predict_var <- Moderate_Dry_year_Predict_var[complete.cases(Moderate_Dry_year_Predict_var), ]
 
Moderate_Drydata <- filter(Moderate_Dry_year_Predict_var, DEQ_Irrigation_withdrawals    >0)
Moderate_Drydata_input <- as.data.frame(Moderate_Drydata[,c(1,2,4,6,9,10)])

Moderate_Drydata_input$YEAR <- Moderate_Drydata_input$YEAR-2002+1
Moderate_Drydata_input[,c(3:6)] <- as.data.frame(scale(Moderate_Drydata_input[,c(3:6)])) #standardization mean of 0 SD 1


Moderate_Drydata$Predicted<- exp(predict(final, Moderate_Drydata_input, allow.new.levels = TRUE))



GOF_Moderate_Dry <- gof(Moderate_Drydata$Predicted,Moderate_Drydata$DEQ_Irrigation_withdrawals)
GOF_Moderate_Dry

#######################################################################
# Regression for Moderate dry year
Avg_Year$COUNTYFP <- as.factor(Avg_Year$COUNTYFP)

Avg_year_Predict_var <- left_join(Avg_Year[,c(1,2)],dat2, by=c("COUNTYFP", "YEAR"))
 Avg_year_Predict_var = Avg_year_Predict_var[!duplicated(Avg_year_Predict_var$COUNTYFP),]
Avg_year_Predict_var <- Avg_year_Predict_var[complete.cases(Avg_year_Predict_var), ]

 
Avgdata <- filter(Avg_year_Predict_var, DEQ_Irrigation_withdrawals    >0)
Avgdata_input <- as.data.frame(Avgdata[,c(1,2,4,6,9,10)])

Avgdata_input$YEAR <- Avgdata_input$YEAR-2002+1
Avgdata_input[,c(3:6)] <- as.data.frame(scale(Avgdata_input[,c(3:6)])) #standardization mean of 0 SD 1


Avgdata$Predicted<- exp(predict(final, Avgdata_input, allow.new.levels = TRUE))



GOF_Avg <- gof(Avgdata$Predicted,Avgdata$DEQ_Irrigation_withdrawals)
GOF_Avg




##############################################################################################

StateSumamries <- list(Drydata,Moderate_Drydata,Avgdata)
names(StateSumamries) <- c("Drydata","Moderate_Drydata","Avgdata")

State_Summary_Scenarios<- list()

for (i in 1:3) {
  State_Summary_Scenarios[[i]] <-  StateSumamries[[i]][,c(3,13)] %>% 
    dplyr::summarise(DEQ_Withdrawals = sum(DEQ_Irrigation_withdrawals, na.rm = TRUE),
                     Regression_withdarwals = sum(Predicted,na.rm = TRUE))
}


names(State_Summary_Scenarios) <- c("Drydata","Moderate_Drydata","Avgdata")

State_Summary_Scenarios <- bind_rows(State_Summary_Scenarios, .id = "Scenario")

State_Summary_Scenarios





##################################################################
# Unreported withdarwals

load(paste0(WUDR_github, "/Regression/LMER_input.RData"))

Var_res$LF_Unreported_Positive <- ifelse(Var_res$LF_Unreported_Eff_precip_method_all_counties < 0, 0, Var_res$LF_Unreported_Eff_precip_method_all_counties)

Var_res$Tot_unreported <- Var_res$SF_Unreported_Eff_precip_method_all_counties+Var_res$LF_Unreported_Positive 



dat <- left_join(Var_res[,c(1,2,11)], Var_pred, by = c("COUNTYFP", "YEAR"))
# dat <- dat %>% mutate_at(vars(-c(1,2)), funs(round(., 2)))

dat$COUNTYFP<- as.factor(dat$COUNTYFP)

dat2 <- dplyr::filter(dat, Tot_unreported >0)

dat2[,c(4:12)] <- as.data.frame(scale(dat2[,c(4:12)])) #standardization mean of 0 SD 1
dat2$YEAR <- dat2$YEAR-2002+1
dat2 <- dat2[complete.cases(dat2), ]

   ##MODEL 1 All variables
DEQ_model <- lmer(log(Tot_unreported) ~ PPT +Min_Temp+Zero_PPT_days+ Extreme_days+SDI+GC+(YEAR|COUNTYFP)+(1|COUNTYFP),
                                  data=dat2, control = lmerControl(optimizer ="Nelder_Mead"),REML = TRUE)
DEQ_model

# vif.mer(DEQ_model)
# drop1(DEQ_model,test="Kenward-Roger")
# anova(DEQ_model)


step_res  <- step(DEQ_model, direction = c("backwards"))
step_res
final <- get_model(step_res)
anova(final)

#####################################################################################
# Predict

dat2$Predict<-  exp(predict(final))
gof(dat2$Predict,dat2$Tot_unreported)


## Make a plot for Predicted vs Observed


p1 <- ggplot(dat2, aes(x=Tot_unreported,y=Predict))+
  geom_point()+
  geom_smooth(method=lm)+
  labs(title= "",
       x="Calculated Unreported withdrawals (MG)", y = " Regression based Unreported withdrawals (MG)") +
  scale_x_continuous(trans = log10_trans(), 
                     breaks = c(-1,1, 10, 100, 1000,4000), 
                     limits=c(-2,7000))+
  scale_y_continuous(trans = log10_trans(), 
                     breaks = c(-1,1, 10, 100, 1000, 4000), 
                     limits=c(-2,7000))


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
# 
# ggsave(paste0(WUDR_github,"/Regression/plots/","Thesisunreported vs calcualted.png"), plot = p1, width = 9.5, height = 6, units = "in")
# library(plotly)
# ggplotly(p1)

########################################################################################################
#Get data for each scenario

#######################################################################
# Regression for dry year

dat <- left_join(Var_res[,c(1,2,11)], Var_pred, by = c("COUNTYFP", "YEAR"))
# dat <- dat %>% mutate_at(vars(-c(1,2)), funs(round(., 2)))

dat$COUNTYFP<- as.factor(dat$COUNTYFP)

dat2 <- dplyr::filter(dat, Tot_unreported >0)

dat2[,c(4:12)] <- as.data.frame(scale(dat2[,c(4:12)])) #standardization mean of 0 SD 1

dat2 <- dat2[complete.cases(dat2), ]
Dry_Year$COUNTYFP <- as.factor(Dry_Year$COUNTYFP)

Dry_year_Predict_var <- left_join(Dry_Year[,c(1,2)],dat2, by=c("COUNTYFP", "YEAR"))
 Dry_year_Predict_var = Dry_year_Predict_var[!duplicated(Dry_year_Predict_var$COUNTYFP),]
Dry_year_Predict_var <- Dry_year_Predict_var[complete.cases(Dry_year_Predict_var), ]

Dry_year_Predict_var$YEAR <- Dry_year_Predict_var$YEAR-2002+1
 
Drydata <- filter(Dry_year_Predict_var, Tot_unreported    >0)
Drydata_input <- as.data.frame(Drydata[,c(1,2,4,9)])


# Drydata_input[,c(3:6)] <- as.data.frame(scale(Drydata_input[,c(3:6)])) #standardization mean of 0 SD 1


Drydata$Predicted<- exp(predict(final, Drydata_input, allow.new.levels = TRUE))



GOF_dry <- round(gof(Drydata$Predicted,Drydata$Tot_unreported),2)
GOF_dry

#######################################################################
# Regression for Moderate dry year
Moderate_Dry_Year$COUNTYFP <- as.factor(Moderate_Dry_Year$COUNTYFP)

Moderate_Dry_year_Predict_var <- left_join(Moderate_Dry_Year[,c(1,2)],dat2, by=c("COUNTYFP", "YEAR"))
 Moderate_Dry_year_Predict_var = Moderate_Dry_year_Predict_var[!duplicated(Moderate_Dry_year_Predict_var$COUNTYFP),]

Moderate_Dry_year_Predict_var <- Moderate_Dry_year_Predict_var[complete.cases(Moderate_Dry_year_Predict_var), ]
 
Moderate_Drydata <- filter(Moderate_Dry_year_Predict_var, Tot_unreported    >0)
Moderate_Drydata_input <- as.data.frame(Moderate_Drydata[,c(1,2,4,9)])

Moderate_Drydata_input$YEAR <- Moderate_Drydata_input$YEAR-2002+1



Moderate_Drydata$Predicted<- exp(predict(final, Moderate_Drydata_input, allow.new.levels = TRUE))



GOF_Moderate_Dry <- gof(Moderate_Drydata$Predicted,Moderate_Drydata$Tot_unreported)
GOF_Moderate_Dry

#######################################################################
# Regression for Moderate dry year
Avg_Year$COUNTYFP <- as.factor(Avg_Year$COUNTYFP)

Avg_year_Predict_var <- left_join(Avg_Year[,c(1,2)],dat2, by=c("COUNTYFP", "YEAR"))
 Avg_year_Predict_var = Avg_year_Predict_var[!duplicated(Avg_year_Predict_var$COUNTYFP),]
Avg_year_Predict_var <- Avg_year_Predict_var[complete.cases(Avg_year_Predict_var), ]

 
Avgdata <- filter(Avg_year_Predict_var, Tot_unreported    >0)
Avgdata_input <- as.data.frame(Avgdata[,c(1,2,4,9)])

Avgdata_input$YEAR <- Avgdata_input$YEAR-2002+1



Avgdata$Predicted<- exp(predict(final, Avgdata_input, allow.new.levels = TRUE))



GOF_Avg <- gof(Avgdata$Predicted,Avgdata$Tot_unreported)
GOF_Avg




##############################################################################################

StateSumamries <- list(Drydata,Moderate_Drydata,Avgdata)
names(StateSumamries) <- c("Drydata","Moderate_Drydata","Avgdata")

State_Summary_Scenarios<- list()

for (i in 1:3) {
  State_Summary_Scenarios[[i]] <-  StateSumamries[[i]][,c(3,13)] %>% 
    dplyr::summarise(Tot_unreported = sum(Tot_unreported, na.rm = TRUE),
                     Regression_withdarwals = sum(Predicted,na.rm = TRUE))
}


names(State_Summary_Scenarios) <- c("Drydata","Moderate_Drydata","Avgdata")

State_Summary_Scenarios <- bind_rows(State_Summary_Scenarios, .id = "Scenario")

State_Summary_Scenarios



