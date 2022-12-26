WUDR_github<-"F:/My Drive/WUDR/WUDR_Github/WUDR"
setwd(WUDR_github)
library(tidyverse)
library(hydroGOF)
library(scales)
library(rgdal)
library(tmap)
library(tmaptools)




load(paste0(WUDR_github, "/Regression/LMER_input.RData"))

Var_res$LF_Unreported_Positive <- ifelse(Var_res$LF_Unreported_Eff_precip_method_all_counties < 0, 0, Var_res$LF_Unreported_Eff_precip_method_all_counties)

Var_res$Tot_unreported <- Var_res$SF_Unreported_Eff_precip_method_all_counties+Var_res$LF_Unreported_Positive 



Var_res <- Var_res %>% 
  filter(Tot_unreported >0)


COunty_mean <- Var_res %>% 
  dplyr::group_by(COUNTYFP) %>% 
  dplyr::summarise(Mean_Unreported_withdarwals = mean(Tot_unreported, na.rm=TRUE))

Var_res <- left_join(Var_res,COunty_mean, by = "COUNTYFP")
Var_res <- Var_res[,c(1,2,11,12)]


Var_res$Norm_unrep_withdarwals <- Var_res$Tot_unreported/Var_res$Mean_Unreported_withdarwals

dat <- left_join(Var_res, Var_pred, by = c("COUNTYFP", "YEAR"))
# dat <- dat %>% mutate_at(vars(-c(1,2)), funs(round(., 2)))

dat$COUNTYFP<- as.factor(dat$COUNTYFP)



dat2 <- dplyr::filter(dat, Tot_unreported >0)

dat2[,c(6:14)] <- as.data.frame(scale(dat2[,c(6:14)])) #standardization mean of 0 SD 1

dat2 <- dat2[complete.cases(dat2), ]

#######################################################################################
#


Norm_MOdel_1 <- glm(Norm_unrep_withdarwals ~ PPT +Min_Temp+Zero_PPT_days+ Extreme_days+SDI+GC,
                                  data=dat2)
library(MASS)
m1 <- stepAIC(Norm_MOdel_1, direction = "both")
summary(m1)

Best_model <- glm(Norm_unrep_withdarwals ~ PPT  + Extreme_days, data = dat2)
Best_model
T1 <- summary(Best_model)

write.csv(T1$coefficients, "Unreported_thesis_table.csv")
dat2$predict = predict(Best_model)*dat2$Mean_Unreported_withdarwals
GOF <- gof(dat2$predict,dat2$Tot_unreported)
GOF


p1 <- ggplot(dat2, aes(x=Tot_unreported,y=predict))+
  geom_point()+
  geom_smooth(method=lm)+
  labs(title= "",
       x="Calculated Unreported withdrawals (MG)", y = " Regression based Unreported withdrawals (MG)") +
  scale_x_continuous(trans = log10_trans(), 
                     breaks = c(-1,1, 10, 100, 1000,5000), 
                     limits=c(-2,7000))+
  scale_y_continuous(trans = log10_trans(), 
                     breaks = c(-1,1, 10, 100, 1000, 5000), 
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

ggsave(paste0(WUDR_github,"/Regression/plots/","Thesisunreported vs calcualted.png"), plot = p1, width = 9.5, height = 6, units = "in")

########################################################################################
# Based on Effective precip approch

load(paste0(WUDR_github,"/dat_load/PRECIP_JUNE_AUGUST_ALL_YEARS.RData")) # PRISM PPT data
load(paste0(WUDR_github,"/dat_load/PRISM_county_codes.RData")) 
# load(paste0(WUDR_github, "/Regression/LMER_input.RData"))
# source("F:/My Drive/WUDR/WUDR_Github/WUDR/Regression/VIF Function.R", echo=TRUE)  # LOAD VIF FUNCTION

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

 
Drydata <- filter(Dry_year_Predict_var, Norm_unrep_withdarwals    >0)
Drydata_input <- as.data.frame(Drydata[,c(1,2,6,12)])


Drydata$Predicted<- predict(Best_model, Drydata_input)*Drydata$Mean_Unreported_withdarwals


GOF_dry <- gof(Drydata$Predicted,Drydata$Tot_unreported)
GOF_dry

#########

Moderate_Dry_Year$COUNTYFP <- as.factor(Moderate_Dry_Year$COUNTYFP)

Moderate_Dry_year_Predict_var <- left_join(Moderate_Dry_Year[,c(1,2)],dat2, by=c("COUNTYFP", "YEAR"))
 Moderate_Dry_year_Predict_var = Moderate_Dry_year_Predict_var[!duplicated(Moderate_Dry_year_Predict_var$COUNTYFP),]
Moderate_Dry_year_Predict_var <- Moderate_Dry_year_Predict_var[complete.cases(Moderate_Dry_year_Predict_var), ]

 
Moderate_Drydata <- filter(Moderate_Dry_year_Predict_var, Norm_unrep_withdarwals    >0)
Moderate_Drydata_input <- as.data.frame(Moderate_Drydata[,c(1,2,6,12)])


Moderate_Drydata$Predicted<- predict(Best_model, Moderate_Drydata_input)*Moderate_Drydata$Mean_Unreported_withdarwals


GOF_Moderate_Dry <- gof(Moderate_Drydata$Predicted,Moderate_Drydata$Tot_unreported)
GOF_Moderate_Dry



#############


Avg_Year$COUNTYFP <- as.factor(Avg_Year$COUNTYFP)

Avg_year_Predict_var <- left_join(Avg_Year[,c(1,2)],dat2, by=c("COUNTYFP", "YEAR"))
 Avg_year_Predict_var = Avg_year_Predict_var[!duplicated(Avg_year_Predict_var$COUNTYFP),]
Avg_year_Predict_var <- Avg_year_Predict_var[complete.cases(Avg_year_Predict_var), ]

 
Avgdata <- filter(Avg_year_Predict_var, Norm_unrep_withdarwals    >0)
Avgdata_input <- as.data.frame(Avgdata[,c(1,2,6,12)])


Avgdata$Predicted<- predict(Best_model, Avgdata_input)*Avgdata$Mean_Unreported_withdarwals


GOF_Avg <- gof(Avgdata$Predicted,Avgdata$Tot_unreported)
GOF_Avg


############

StateSumamries <- list(Drydata,Moderate_Drydata,Avgdata)
names(StateSumamries) <- c("Drydata","Moderate_Drydata","Avgdata")

State_Summary_Scenarios<- list()

for (i in 1:3) {
  State_Summary_Scenarios[[i]] <-  StateSumamries[[i]][,c(3,16)] %>% 
    dplyr::summarise(Unreported_withdrawals = sum(Tot_unreported, na.rm = TRUE),
                     Regression_withdarwals = sum(Predicted,na.rm = TRUE))
}


names(State_Summary_Scenarios) <- c("Drydata","Moderate_Drydata","Avgdata")

State_Summary_Scenarios <- bind_rows(State_Summary_Scenarios, .id = "Scenario")

State_Summary_Scenarios
 


fn_increase <- function(Final, Initial){
  increase = 100*(Final-Initial)/Initial
  return(increase)
}
fn_increase(17080,12866)

fn_increase(17080,9692)

fn_increase(15818,12658)

fn_increase(15818,10386)

