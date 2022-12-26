WUDR_github<-"F:/My Drive/WUDR/WUDR_Github/WUDR"
setwd(WUDR_github)

library(tidyverse) 
library(scales)
library(lme4)
library(lmerTest)
library(hydroGOF)
library(pbkrtest)
options(scipen = 9999)

# LOAD DATA FOR LMER 
load(paste0(WUDR_github, "/Regression/LMER_input.RData"))
source("F:/My Drive/WUDR/WUDR_Github/WUDR/Regression/VIF Function.R", echo=TRUE)  # LOAD VIF FUNCTION

######################################################################

# LMER Model for Total Withdrawals

dat <- left_join(Var_res[,c(1,2,9)], Var_pred, by = c("COUNTYFP", "YEAR"))
# dat <- dat %>% mutate_at(vars(-c(1,2)), funs(round(., 2)))

dat$COUNTYFP<- as.factor(dat$COUNTYFP)

dat2 <- dplyr::filter(dat, Total_irrigation_withdarwals_calculated >0)

dat2[,c(4:12)] <- as.data.frame(scale(dat2[,c(4:12)])) #standardization mean of 0 SD 1
dat2$YEAR <- dat2$YEAR-2002+1
dat2 <- dat2[complete.cases(dat2), ]


Total_Withdrawals_Model <- lmer(log(Total_irrigation_withdarwals_calculated) ~ PPT +Min_Temp+Max_Temp+Zero_PPT_days+ Extreme_days+SDI+GC+(YEAR|COUNTYFP)+(1|COUNTYFP),
             data=dat2, control = lmerControl(optimizer ="Nelder_Mead"),REML = TRUE)
Total_Withdrawals_Model

dat2$Predict<-  exp(predict(Total_Withdrawals_Model))

vif.mer(Total_Withdrawals_Model)
gof(dat2$Predict, dat2$Total_irrigation_withdarwals_calculated) 

## Make a plot for Predicted vs Observed

plot_dat <- pivot_longer(dat2[,c(3,13)],cols = c(1,2), names_to = "Type",values_to ="Withdrawals")
plot_dat$Dat <- NA

plot_dat$Dat<- ifelse(plot_dat$Type == "Predict", "Modelled", "Observed")


p1 <- ggplot(dat2, aes(x=Total_irrigation_withdarwals_calculated,y=Predict))+
  geom_point()+
  geom_smooth(method=lm)+
  labs(title= "Total irrigation vs Predicted withdrawals",
       x="Total Irrigation Withdrawals (log10)", y = " Modelled Withdrawals (log10)") +
  scale_x_continuous(trans = log10_trans(), 
                     breaks = c(-1,1, 10, 100, 1000,2500,4500), 
                     limits=c(-2,7000))+
  scale_y_continuous(trans = log10_trans(), 
                     breaks = c(-1,1, 10, 100, 1000,2500,4500), 
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
# ggsave(paste0(WUDR_github,"/Regression/plots/","Total irrigation withdrawals vs predicted.png"), plot = p1, width = 9.5, height = 6, units = "in")
# library(plotly)
# ggplotly(p1)

######################################################################
#LMER for DEQ withdrawals

Deq_dat <- left_join(Var_res[,c(1,2,6)], Var_pred, by = c("COUNTYFP", "YEAR"))
# Deq_dat <- Deq_dat %>% mutate_at(vars(-c(1,2)), funs(round(., 2)))

Deq_dat$COUNTYFP<- as.factor(Deq_dat$COUNTYFP)

Deq_dat2 <- dplyr::filter(Deq_dat, DEQ_Irrigation_withdrawals >0)
Deq_dat2[,c(4:10)] <- as.data.frame(scale(Deq_dat2[,c(4:10)])) #standardization mean of 0 SD 1
Deq_dat2$YEAR <- Deq_dat2$YEAR-2002+1


Deq_dat2 <- Deq_dat2[complete.cases(Deq_dat2), ]


Deq_withdrawals_model1 <- lmer(log(DEQ_Irrigation_withdrawals) ~ PPT +Min_Temp+Max_Temp+Zero_PPT_days+ Extreme_days+SDI+GC+(YEAR|COUNTYFP)+(1|COUNTYFP),
             data=Deq_dat2)
Deq_withdrawals_model1


Deq_withdrawals_model1 <- lmer(log(DEQ_Irrigation_withdrawals) ~ PPT +Min_Temp+Max_Temp+Zero_PPT_days+ Extreme_days+SDI+GC+(YEAR|COUNTYFP)+(1|COUNTYFP),
             data=Deq_dat2)
##### Predict DEQ withdrawals
Deq_dat2$Predict<-  exp(predict(Deq_withdrawals_model1))
Intercepts <- coef(Deq_withdrawals_model1)


####### 
vif.mer(Deq_withdrawals_model1) # VIF for DEQ withdrawals

gof(Deq_dat2$Predict, Deq_dat2$DEQ_Irrigation_withdrawals) 


p1 <- ggplot(Deq_dat2, aes(x=DEQ_Irrigation_withdrawals,y=Predict))+
  geom_point()+
  geom_smooth(method=lm)+
  labs(title= "DEQ reported Irrigation vs  Predicted withdrawals",
       x="DEQ Reported Withdrawals (log10)", y = " Modelled Withdrawals (log10)") 
# +
  # scale_x_continuous(
  #   trans = log10_trans(), 
  #                    breaks = c(-1,1, 10, 100, 1000,2500,5000), 
  #                    limits=c(NA,7000))+
  # scale_y_continuous(trans = log10_trans(), 
  #                    breaks = c(-1,1, 10, 100, 1000,2500,5000), 
  #                    limits=c(NA,7000))


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

# ggsave(paste0(WUDR_github,"/Regression/plots/","Deq Reported withdrawals vs predicted.png"), plot = p1, width = 9.5, height = 6, units = "in")


############################################################################
############################################################################ Needs to be run
model <- Total_Withdrawals_Model
coefs<-data.frame(coef(summary(model)))
df.KR <- get_Lb_ddf(model, fixef(model))
coefs$p.KR <- 2 * (1 - pt(abs(coefs$t.value), df.KR))


model <- Deq_withdrawals_model1
coefs<-data.frame(coef(summary(model)))
df.KR <- get_Lb_ddf(model, fixef(model))
coefs$p.KR <- 2 * (1 - pt(abs(coefs$t.value), df.KR))

##############################################################################
# PLOT FOR ALL RESPONSE VARIBLES

Deq_dat2$resid <- residuals(Deq_withdrawals_model1)
library(reshape2)
plotDF <- melt(Deq_dat2[, c("PPT" ,"Min_Temp" ,"Mean_Temp" ,"Max_Temp" , "Zero_PPT_days" , "Extreme_days", "SDI" , "GC", "resid" )], id="resid")

library(ggplot2)
p1 <- ggplot(plotDF, aes(x=value, y=resid)) + 
  geom_point() + facet_wrap(~ variable,scales="free")
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
 
# ggsave(paste0(WUDR_github,"/Regression/plots/","Residuals vs predicted.png"), plot = p1, width = 9.5, height = 6, units = "in")


plot(Deq_dat2$PPT,resid(Deq_withdrawals_model1))


#############################################################################################
###### DEQ ALL MODELS
#############################################################################################
Deq_dat <- left_join(Var_res[,c(1,2,6)], Var_pred, by = c("COUNTYFP", "YEAR"))
# Deq_dat <- Deq_dat %>% mutate_at(vars(-c(1,2)), funs(round(., 2)))

Deq_dat$COUNTYFP<- as.factor(Deq_dat$COUNTYFP)

Deq_dat2 <- dplyr::filter(Deq_dat, DEQ_Irrigation_withdrawals >0)
Deq_dat2[,c(4:10)] <- as.data.frame(scale(Deq_dat2[,c(4:10)])) #standardization mean of 0 SD 1
Deq_dat2$YEAR <- Deq_dat2$YEAR-2002+1


Deq_dat2 <- Deq_dat2[complete.cases(Deq_dat2), ]

#####################################
# 1. MODEL ALL remove Tmean

Deq_withdrawals_model1<- lmer(log(DEQ_Irrigation_withdrawals) ~ PPT +Min_Temp+Zero_PPT_days+ Extreme_days+SDI+GC+(YEAR|COUNTYFP)+(1|COUNTYFP),
             data=Deq_dat2)
summary(Deq_withdrawals_model1)
vif.mer(Deq_withdrawals_model1) # VIF for DEQ withdrawals
anova(Deq_withdrawals_model1)
# 
#           PPT      Min_Temp      Max_Temp Zero_PPT_days  Extreme_days           SDI            GC 
#      2.524088      1.344078      3.544359      2.484911      2.771842      1.213279      2.917082
#      


# 2. Four parameter model
Deq_withdrawals_model2 <- lmer(log(DEQ_Irrigation_withdrawals) ~ PPT +Min_Temp+Zero_PPT_days+ Extreme_days+(YEAR|COUNTYFP)+(1|COUNTYFP),
             data=Deq_dat2)
# summary (Deq_withdrawals_model2,ddf="Kenward-Roger")
VIF <- vif.mer(Deq_withdrawals_model2) 
# write.csv(VIF, "BEST_LMER_Model_VIF.csv")
A_table <- anova(Deq_withdrawals_model2)

# write.csv(A_table, "BEST_LMER_Model_ANOVA.csv")
     #      PPT      Max_Temp Zero_PPT_days  Extreme_days 
     # 2.237918      3.476087      1.610892      2.716557 

# 3. Two parameter model
Deq_withdrawals_model3 <- lmer(log(DEQ_Irrigation_withdrawals) ~ PPT +Min_Temp+(YEAR|COUNTYFP)+(1|COUNTYFP),
             data=Deq_dat2)
Deq_withdrawals_model3
vif.mer(Deq_withdrawals_model3) 
anova(Deq_withdrawals_model3)

#      PPT Max_Temp 
# 1.623079 1.623079

# 4. One parameter model
Deq_withdrawals_model4 <- lmer(log(DEQ_Irrigation_withdrawals) ~ PPT +(YEAR|COUNTYFP)+(1|COUNTYFP),
             data=Deq_dat2)
Deq_withdrawals_model4
vif.mer(Deq_withdrawals_model4) 
anova(Deq_withdrawals_model4)
# PPT 
#   1

## Irrigation model
Deq_withdrawals_model5 <- lmer(log(DEQ_Irrigation_withdrawals) ~ Irrigation +(YEAR|COUNTYFP)+(1|COUNTYFP),
             data=Deq_dat2)
Deq_withdrawals_model5
vif.mer(Deq_withdrawals_model5) 

###############################################################################
##############################################################################
##CALCULATE AIC
#############################################################################
Models_AIC <- AIC(Deq_withdrawals_model1, Deq_withdrawals_model2,Deq_withdrawals_model3,Deq_withdrawals_model4,Deq_withdrawals_model5)


write.csv(Models_AIC, paste0(WUDR_github,"/Regression/Plots/DEQ_AIC_models.csv"))
Models_AIC <- anova(Deq_withdrawals_model1, Deq_withdrawals_model2,Deq_withdrawals_model3,Deq_withdrawals_model4,Deq_withdrawals_model5)

drop1(Deq_withdrawals_model1,test="Chisq")
drop1(Deq_withdrawals_model1,test="Chisq",ddf = "Kenward-Roger")
drop1(Deq_withdrawals_model1,test="Chisq",ddf = "lme4")

#Deq_withdrawals_model2 is teh best model

##### Predict DEQ withdrawals
Deq_dat2$Predict<-  exp(predict(Deq_withdrawals_model2))
gof(Deq_dat2$Predict,Deq_dat2$DEQ_Irrigation_withdrawals)

p1 <- ggplot(Deq_dat2, aes(x=DEQ_Irrigation_withdrawals,y=Predict))+
  geom_point()+
  geom_smooth(method=lm)+
  labs(title= "DEQ reported Irrigation vs  Predicted withdrawals",
       x="DEQ Reported Withdrawals (log10)", y = " Modelled Withdrawals (log10)") +
scale_x_continuous(
  trans = log10_trans(),
                   breaks = c(-1,1, 10, 100, 1000,2500,5000),
                   limits=c(NA,7000))+
scale_y_continuous(trans = log10_trans(),
                   breaks = c(-1,1, 10, 100, 1000,2500,5000),
                   limits=c(NA,7000))


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


ggsave(paste0(WUDR_github,"/Regression/plots/","DLMER_Modelled vs reported.png"), plot = p1, width = 9.5, height = 6, units = "in")


########################################################################
# LMER Model for Total Withdrawals
# library(lmerTest)
################################################################################
dat <- left_join(Var_res[,c(1,2,9)], Var_pred, by = c("COUNTYFP", "YEAR"))
# dat <- dat %>% mutate_at(vars(-c(1,2)), funs(round(., 2)))

dat$COUNTYFP<- as.factor(dat$COUNTYFP)

dat2 <- dplyr::filter(dat, Total_irrigation_withdarwals_calculated >0)

dat2[,c(4:12)] <- as.data.frame(scale(dat2[,c(4:12)])) #standardization mean of 0 SD 1
dat2$YEAR <- dat2$YEAR-2002+1
dat2 <- dat2[complete.cases(dat2), ]

##MODEL 1 All variables
Total_Withdrawals_Model_1 <- lmer(log(Total_irrigation_withdarwals_calculated) ~ PPT +Min_Temp+Zero_PPT_days+ Extreme_days+SDI+GC+(YEAR|COUNTYFP)+(1|COUNTYFP),
             data=dat2, control = lmerControl(optimizer ="Nelder_Mead"),REML = TRUE)
Total_Withdrawals_Model_1

vif.mer(Total_Withdrawals_Model_1)

##MODEL 2 Four parameters
Total_Withdrawals_Model_2 <- lmer(log(Total_irrigation_withdarwals_calculated) ~ PPT +Min_Temp+Zero_PPT_days+ Extreme_days+(YEAR|COUNTYFP)+(1|COUNTYFP),
             data=dat2, control = lmerControl(optimizer ="Nelder_Mead"),REML = TRUE)
Total_Withdrawals_Model_2

vif.mer(Total_Withdrawals_Model_2)
anova(Total_Withdrawals_Model_1)

##MODEL 3 Two parameters
Total_Withdrawals_Model_3 <- lmer(log(Total_irrigation_withdarwals_calculated) ~ PPT +Min_Temp+(YEAR|COUNTYFP)+(1|COUNTYFP),
             data=dat2, control = lmerControl(optimizer ="Nelder_Mead"),REML = TRUE)
Total_Withdrawals_Model_3

vif.mer(Total_Withdrawals_Model_3)
anova(Total_Withdrawals_Model_3)

##MODEL 4 One parameters
Total_Withdrawals_Model_4 <- lmer(log(Total_irrigation_withdarwals_calculated) ~ PPT +(YEAR|COUNTYFP)+(1|COUNTYFP),
             data=dat2, control = lmerControl(optimizer ="Nelder_Mead"),REML = TRUE)
Total_Withdrawals_Model_4

vif.mer(Total_Withdrawals_Model_4)

Intercepts_PPT <- coef(Total_Withdrawals_Model_4)

##MODEL Irrigation
Total_Withdrawals_Model_5 <- lmer(log(Total_irrigation_withdarwals_calculated) ~ Irrigation +(YEAR|COUNTYFP)+(1|COUNTYFP),
             data=dat2, control = lmerControl(optimizer ="Nelder_Mead"),REML = TRUE)
Total_Withdrawals_Model_5

Intercepts_Irr <- coef(Total_Withdrawals_Model_5)
vif.mer(Total_Withdrawals_Model_5)

# anova(Total_Withdrawals_Model_1, Total_Withdrawals_Model_2)
# KRmodcomp(Total_Withdrawals_Model_1, Total_Withdrawals_Model_2)
Models_AIC<- AIC(Total_Withdrawals_Model_1,Total_Withdrawals_Model_2,Total_Withdrawals_Model_3,Total_Withdrawals_Model_4,Total_Withdrawals_Model_5)

write.csv(Models_AIC, paste0(WUDR_github,"/Regression/Plots/Total_AIC_models.csv"))


Models_AIC <- anova(Total_Withdrawals_Model_1, Total_Withdrawals_Model_2,Total_Withdrawals_Model_3,Total_Withdrawals_Model_4,Total_Withdrawals_Model_5)

drop1(Total_Withdrawals_Model_1,test="Chisq")
drop1(Total_Withdrawals_Model_1,test="Chisq",ddf = "Kenward-Roger")
drop1(Total_Withdrawals_Model_1,test="Chisq",ddf = "lme4")

################################################################################################
# 
# LOAD DATA FOR LMER 
load(paste0(WUDR_github, "/Regression/LMER_input.RData"))
COunty_mean <- Var_res %>% 
  group_by(COUNTYFP) %>% 
  summarise(Mean_DEQ_withdarwals = mean(DEQ_Irrigation_withdrawals, na.rm=TRUE))

Var_res <- left_join(Var_res,COunty_mean, by = "COUNTYFP")
Var_res$Norm_DEQ_withdarwals <- Var_res$DEQ_Irrigation_withdrawals/Var_res$Mean_DEQ_withdarwals

dat <- left_join(Var_res[,c(1,2,6,10,11)], Var_pred, by = c("COUNTYFP", "YEAR"))
# dat <- dat %>% mutate_at(vars(-c(1,2)), funs(round(., 2)))

 dat$COUNTYFP<- as.factor(dat$COUNTYFP)

dat2 <- dplyr::filter(dat, Norm_DEQ_withdarwals >0)

dat2[,c(6:14)] <- as.data.frame(scale(dat2[,c(6:14)])) #standardization mean of 0 SD 1
dat2$YEAR <- dat2$YEAR-2002+1
dat2 <- dat2[complete.cases(dat2), ]


Norm_MOdel_1 <- glm(Norm_DEQ_withdarwals ~ PPT +Min_Temp+Zero_PPT_days+ Extreme_days+SDI+GC,
                                  data=dat2)
summary(Norm_MOdel_1)


# Norm_MOdel_2 <- glm(Norm_DEQ_withdarwals ~ PPT +Min_Temp+Zero_PPT_days+ Extreme_days,
#                                   data=dat2)
# summary(Norm_MOdel_2)
# 
# Norm_MOdel_3 <- glm(Norm_DEQ_withdarwals ~ PPT +Min_Temp,
#                                   data=dat2)
# summary(Norm_MOdel_3)
# 
# 
# Norm_MOdel_4 <- glm(Norm_DEQ_withdarwals ~ PPT,
#                                   data=dat2)
# summary(Norm_MOdel_4)
# 
# Norm_MOdel_5 <- glm(Norm_DEQ_withdarwals ~ Irrigation,
#                                   data=dat2)
# summary(Norm_MOdel_5)
# Models_AIC<- AIC(Norm_MOdel_1,Norm_MOdel_2,Norm_MOdel_3,Norm_MOdel_4,Norm_MOdel_5)
# 
# write.csv(Models_AIC, paste0(WUDR_github,"/Regression/Plots/Normalised_AIC_models.csv"))
step(Norm_MOdel_1, test="LRT")

Best_model1<- glm(Norm_DEQ_withdarwals ~ PPT + Min_Temp + Extreme_days + 
      GC, data = dat2)

Best_model <- glm(log(Norm_DEQ_withdarwals) ~ PPT + Min_Temp + Extreme_days + 
    GC, data = dat2)
summary(Best_model)
summary(Best_model1)
dat2$predict = predict(Best_model)*dat2$Mean_DEQ_withdarwals
GOF <- gof(dat2$predict,dat2$DEQ_Irrigation_withdrawals)
# write.csv(GOF, paste0(WUDR_github,"/Regression/GOF.csv"))


 
ggsave(paste0(WUDR_github,"/Regression/plots/","Normlaised_predicted_withdarwals vs DEQ reported.png"), plot = p1, width = 9.5, height = 6, units = "in")

##############################################################
# Predict normalised factor
dat2$predict2 = predict(Best_model)
GOF <- gof(dat2$predict2,dat2$Norm_DEQ_withdarwals)

p1 <- ggplot(dat2, aes(x=Norm_DEQ_withdarwals,y=predict2))+
  geom_point()+
  geom_smooth(method=lm)+
  labs(title= "DEQ Withdarwals vs  Predicted withdrawals",
       x="DEQ Normalised Withdrawals ", y = " Modelled Normalised Withdrawals") +
  scale_x_continuous(
    trans = log10_trans(), 
    breaks = c(-2,-1,0,5,10,15), 
    limits=c(-5,15))+
  scale_y_continuous(
     trans = log10_trans(), 
                     breaks = c(-2,-1,0,5,10,15), 
                     limits=c(-5,15))
p1

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

ggsave(paste0(WUDR_github,"/Regression/plots/","Normlaised_predicted_withdarwals_coeff vs normalised.png"), plot = p1, width = 9.5, height = 6, units = "in")


# lmer(Reaction ~ Days + (Days|Subject), sleepstudy)
# drop1(Total_Withdrawals_Model_1,test="Chisq",ddf = "Kenward-Roger")
# 
# drop1(Total_Withdrawals_Model_1)
# if(requireNamespace("pbkrtest", quietly = TRUE))
#   summary(Total_Withdrawals_Model_1, ddf="Kenward-Roger")
# 
if(requireNamespace("pbkrtest", quietly = TRUE))
  summary(Deq_withdrawals_model2, ddf="Kenward-Roger")
# 
 library(lmerTest)
# lmerTest::step(Total_Withdrawals_Model_1,ddf="Kenward-Roger")
 lmerTest::step(Deq_withdrawals_model1,ddf="Kenward-Roger")
# hist(Var_res$Norm_TOT_withdarwals)