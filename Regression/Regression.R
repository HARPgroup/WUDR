
WUDR_github<-"F:/My Drive/WUDR/WUDR_Github/WUDR"
setwd(WUDR_github)

library(tidyverse) 
library(rgdal)
library(vegan)
library(DescTools)
options(scipen = 9999)
# Load VA Shapefile to be used later
VA_counties<-readOGR(paste0(WUDR_github, "/VA_counties_sp"), layer="VA_counties")

county.codes <- read.csv(paste0(WUDR_github, "/csv_files/county_codes_census.csv")) 
####################################################################


load(paste0(WUDR_github,"/dat_load/SF_times_series_Median.RData")) # Small Farm time series

load(paste0(WUDR_github, "/dat_load/All_Years_DEQ_data_Total_nd_irr.Rdata"))  #DEQ reported data


load(paste0(WUDR_github,"/dat_load/PRECIP_JUNE_AUGUST_ALL_YEARS.RData")) # PRISM PPT data

load(paste0(WUDR_github,"/dat_load/PRISM_county_codes.RData")) # County Codes for PRISM

load(paste0(WUDR_github,"/dat_load/LF_All_times_series.RData")) # Large Farm time series

PPT <- lapply(ppt_list_yearly, function(x)
  mutate(x, Irrigation = round(508 - PPT,0)))
PPT <- bind_rows(PPT, .id = "Year")

PPT <- left_join(PPT,PRISM_county_codes, by = "name" )
PPT <- PPT[,c(1,5,2,3,4)]
colnames(PPT)[1:2] <- c("YEAR", "COUNTYFP")
PPT$YEAR <- as.numeric(PPT$YEAR)

colnames(SF_Unreported_Median_UnderTh)[2] <- c("COUNTYFP")
colnames(SF_Unreported_Median_UnderTh)[1] <- c("YEAR")

colnames(TS_LF_Unreported_Median_Area)[2] <- c("COUNTYFP")
colnames(TS_LF_Unreported_Median_Area)[1] <- c("YEAR")




#####################################################################
load(paste0(WUDR_github,"/DEQ_data/Prism_temp_data.Rdata"))

Tmax_yearly <- Tmax %>% 
  filter(between(Month,6,8)) %>% 
  dplyr::group_by(Year) %>% 
  summarise_all(max)

Tmax_yearly <- Tmax_yearly[,-2]

Tmax_yearly <- pivot_longer(Tmax_yearly,c(2:134), names_to = "County", values_to = "Max_Temp")


Tmax_yearly$County<-gsub(".", " ", Tmax_yearly$County, fixed=TRUE)
Tmax_yearly$County<-gsub("1", "city", Tmax_yearly$County, fixed=TRUE)
# Tmax_yearly$County<-gsub("  ", "", Tmax_yearly$County, fixed=TRUE)

Tmax_yearly <- left_join(Tmax_yearly, VA_counties@data[,c(3,6)], by = c("County"= "NAME"))
Tmax_yearly <- left_join(Tmax_yearly, VA_counties@data[,c(3,10)], by = c("County"= "NAMELSAD"))
Tmax_yearly$COUNTYFP = ifelse(is.na(Tmax_yearly$COUNTYFP.x),Tmax_yearly$COUNTYFP.y,Tmax_yearly$COUNTYFP.x)
Tmax_yearly[,c(4,5)] = NULL
colnames(Tmax_yearly)[1] <- "YEAR"
#####################################################################
Tmin_yearly <- Tmin %>% 
  filter(between(Month,6,8)) %>% 
  dplyr::group_by(Year) %>% 
  summarise_all(min)

Tmin_yearly <- Tmin_yearly[,-2]

Tmin_yearly <- pivot_longer(Tmin_yearly,c(2:134), names_to = "County", values_to = "Min_Temp")


Tmin_yearly$County<-gsub(".", " ", Tmin_yearly$County, fixed=TRUE)
Tmin_yearly$County<-gsub("1", "city", Tmin_yearly$County, fixed=TRUE)
# Tmin_yearly$County<-gsub("  ", "", Tmin_yearly$County, fixed=TRUE)

Tmin_yearly <- left_join(Tmin_yearly, VA_counties@data[,c(3,6)], by = c("County"= "NAME"))
Tmin_yearly <- left_join(Tmin_yearly, VA_counties@data[,c(3,10)], by = c("County"= "NAMELSAD"))
Tmin_yearly$COUNTYFP = ifelse(is.na(Tmin_yearly$COUNTYFP.x),Tmin_yearly$COUNTYFP.y,Tmin_yearly$COUNTYFP.x)
Tmin_yearly[,c(4,5)] = NULL
colnames(Tmin_yearly)[1] <- "YEAR"

#######################################################################
Tmean_yearly <- Tmean %>% 
  filter(between(Month,6,8)) %>% 
  dplyr::group_by(Year) %>% 
  summarise_all(mean)

Tmean_yearly <- Tmean_yearly[,-2]

Tmean_yearly <- pivot_longer(Tmean_yearly,c(2:134), names_to = "County", values_to = "Mean_Temp")


Tmean_yearly$County<-gsub(".", " ", Tmean_yearly$County, fixed=TRUE)
Tmean_yearly$County<-gsub("1", "city", Tmean_yearly$County, fixed=TRUE)
# Tmean_yearly$County<-gsub("  ", "", Tmean_yearly$County, fixed=TRUE)

Tmean_yearly <- left_join(Tmean_yearly, VA_counties@data[,c(3,6)], by = c("County"= "NAME"))
Tmean_yearly <- left_join(Tmean_yearly, VA_counties@data[,c(3,10)], by = c("County"= "NAMELSAD"))
Tmean_yearly$COUNTYFP = ifelse(is.na(Tmean_yearly$COUNTYFP.x),Tmean_yearly$COUNTYFP.y,Tmean_yearly$COUNTYFP.x)
Tmean_yearly[,c(4,5)] = NULL
colnames(Tmean_yearly)[1] <- "YEAR"


######## No of extreme days i.e. temperature greater than 30


T_Extreme <- Tmean %>% 
  filter(between(Month,6,8)) 

T_Extreme <- pivot_longer(T_Extreme,c(3:135), names_to = "County", values_to = "Mean_Temp")

T_Extreme <- T_Extreme %>% 
  dplyr::group_by(Year,County) %>% 
  summarise(Extreme_days = sum(Mean_Temp >=30, na.rm=TRUE))

T_Extreme$County<-gsub(".", " ", T_Extreme$County, fixed=TRUE)
T_Extreme$County<-gsub("1", "city", T_Extreme$County, fixed=TRUE)
# T_Extreme$County<-gsub("  ", "", T_Extreme$County, fixed=TRUE)

T_Extreme <- left_join(T_Extreme, VA_counties@data[,c(3,6)], by = c("County"= "NAME"))
T_Extreme <- left_join(T_Extreme, VA_counties@data[,c(3,10)], by = c("County"= "NAMELSAD"))
T_Extreme$COUNTYFP = ifelse(is.na(T_Extreme$COUNTYFP.x),T_Extreme$COUNTYFP.y,T_Extreme$COUNTYFP.x)
T_Extreme[,c(4,5)] = NULL
colnames(T_Extreme)[1] <- "YEAR"


######### Non rainfall days

load(paste0(WUDR_github,"/dat_load/PRECIP_2002-2019_daily.RData"))


Zero_PPT <- PPT_VA[-3] %>% 
  filter(between(Month,6,8)) 

Zero_PPT <- pivot_longer(Zero_PPT,c(3:135), names_to = "County", values_to = "PPT")

Zero_PPT <- Zero_PPT %>% 
  dplyr::group_by(Year,County) %>% 
  summarise(Zero_PPT_days = sum(PPT == 0, na.rm=TRUE))

Zero_PPT$County<-gsub(".", " ", Zero_PPT$County, fixed=TRUE)
Zero_PPT$County<-gsub("1", "city", Zero_PPT$County, fixed=TRUE)
# Zero_PPT$County<-gsub("  ", "", Zero_PPT$County, fixed=TRUE)

Zero_PPT <- left_join(Zero_PPT, VA_counties@data[,c(3,6)], by = c("County"= "NAME"))
Zero_PPT <- left_join(Zero_PPT, VA_counties@data[,c(3,10)], by = c("County"= "NAMELSAD"))
Zero_PPT$COUNTYFP = ifelse(is.na(Zero_PPT$COUNTYFP.x),Zero_PPT$COUNTYFP.y,Zero_PPT$COUNTYFP.x)
Zero_PPT[,c(4,5)] = NULL
colnames(Zero_PPT)[1] <- "YEAR"


#####################################################################
# Shannon diversity index  3 Monthly distribution (If 1 means rainfall was similar in all the months and is less the rainfall occured more in one of the months)

#  https://link.springer.com/article/10.1007/s002650050263


load(paste0(WUDR_github,"/dat_load/PRECIP_2002-2019_daily.RData"))


SDI_dat <- PPT_VA[-3] %>% 
  filter(between(Month,6,8)) 

SDI_dat <- pivot_longer(SDI_dat,c(3:135), names_to = "County", values_to = "PPT")


Yearly_sum <- SDI_dat %>% 
  dplyr::group_by(Year,County) %>% 
   summarise(Yearly_sum = sum(PPT))


Monthly_sum <- SDI_dat %>% 
  dplyr::group_by(County,Year,Month) %>% 
  summarise(Monthly_sum = sum(PPT))

Monthly_sum <- left_join(Monthly_sum, Yearly_sum, by = c("Year", "County"))
Monthly_sum$Prop <- Monthly_sum$Monthly_sum/Monthly_sum$Yearly_sum
Monthly_sum$Prop_2 <- Monthly_sum$Prop*log(Monthly_sum$Prop)
SDI <- Monthly_sum %>% 
  dplyr::group_by(County, Year) %>% 
  summarise(SDI = -sum(Prop_2)/log(3))


SDI$County<-gsub(".", " ", SDI$County, fixed=TRUE)
SDI$County<-gsub("1", "city", SDI$County, fixed=TRUE)
# SDI$County<-gsub("  ", "", SDI$County, fixed=TRUE)

SDI <- left_join(SDI, VA_counties@data[,c(3,6)], by = c("County"= "NAME"))
SDI <- left_join(SDI, VA_counties@data[,c(3,10)], by = c("County"= "NAMELSAD"))
SDI$COUNTYFP = ifelse(is.na(SDI$COUNTYFP.x),SDI$COUNTYFP.y,SDI$COUNTYFP.x)
SDI[,c(4,5)] = NULL
colnames(SDI)[2] <- "YEAR"

##################################################
# Gini coefficient ranges from 0 to 1 where higher values represent greater income inequality and where
# 0 represents perfect income equality 
# 1 represents perfect income inequality 


load(paste0(WUDR_github,"/dat_load/PRECIP_2002-2019_daily.RData"))


Gini_dat <- PPT_VA[-3] %>% 
  filter(between(Month,6,8)) 

Gini_dat <- pivot_longer(Gini_dat,c(3:135), names_to = "County", values_to = "PPT")

Gini_cal <- Gini_dat %>% 
  dplyr::group_by(Year, County) %>% 
  summarise( GC = Gini(PPT, unbiased=TRUE))

Gini_cal$County<-gsub(".", " ", Gini_cal$County, fixed=TRUE)
Gini_cal$County<-gsub("1", "city", Gini_cal$County, fixed=TRUE)
# Gini_cal$County<-gsub("  ", "", Gini_cal$County, fixed=TRUE)

Gini_cal <- left_join(Gini_cal, VA_counties@data[,c(3,6)], by = c("County"= "NAME"))
Gini_cal <- left_join(Gini_cal, VA_counties@data[,c(3,10)], by = c("County"= "NAMELSAD"))
Gini_cal$COUNTYFP = ifelse(is.na(Gini_cal$COUNTYFP.x),Gini_cal$COUNTYFP.y,Gini_cal$COUNTYFP.x)
Gini_cal[,c(4,5)] = NULL
colnames(Gini_cal)[1] <- "YEAR"

############################################
# Consecutive rainfall days

# load(paste0(WUDR_github,"/dat_load/PRECIP_2002-2019_daily.RData"))
# 
# 
# Con_PPT <- PPT_VA[-3] %>% 
#   filter(between(Month,6,8)) 
# 
# con_PPT <- pivot_longer(Zero_PPT,c(3:135), names_to = "County", values_to = "PPT")
# 





#######################################################################


Var_res <- purrr::reduce(list(Total_deq_county[,c(1,3,7)],SF_Unreported_Median_UnderTh[,c(1,2,7)],SF_Unreported_Median_Irr_Coeff[,c(1,2,6)],
                              Irri_deq_county[,c(1,3,7)],TS_LF_Coeff_Unreported_median[,c(1,2,6)],TS_LF_Unreported_Median_Area[,c(1,2,11)] ,PPT[,c(1,2,4,5)]),
                         dplyr::full_join, by = c("COUNTYFP","YEAR"))

Var_res$Total_irrigation_withdarwals_calculated <- as.numeric(cbind(SUM = apply(Var_res[,c(4,6,8)], 1, function(x) sum(x[x > 0], na.rm = TRUE))))
     
colnames(Var_res)[c(3:8,11)] <- c("Total_DEQ_withdrawals","SF_Unreported_Eff_precip_method_all_counties", "SF_Unreported_Coeff_method_Deq_irri_counties",
                            "DEQ_Irrigation_withdrawals","LF_Unreported_Coeff_method_Deq_irri_counties","LF_Unreported_Eff_precip_method_all_counties","Total_irrigation_withdarwals_calculated")
Var_res <- Var_res %>% 
  filter(YEAR <2018)

# Var_res <- Var_res %>% 
#   filter(Irrigation >0)

# Var_res$Total_DEQ_withdrawals <- round(Var_res$Total_DEQ_withdrawals,2)

Var_pred <- Var_res[,c(1,2,9,10)]

Var_pred <- purrr::reduce(list(Var_pred,Tmin_yearly[-2],Tmean_yearly[-2],Tmax_yearly[-2],Zero_PPT[-2],T_Extreme[-2], SDI[-1], Gini_cal[-2]),
                         dplyr::full_join, by = c("COUNTYFP","YEAR"))
Var_pred <- Var_pred %>% 
  filter(YEAR <2018)
Var_res <- Var_res[,-c(9,10)]

# save(Var_pred,Var_res, file = paste0(WUDR_github, "/Regression/LMER_input.RData"))



# write.csv(Var_pred, paste0(WUDR_github, "/Regression/Predictor_Variables.csv"))
# write.csv(Var_res, paste0(WUDR_github, "/Regression/Response_Variables.csv"))
################################################################################
# Check Distribution of response variables



# plot_dat <- pivot_longer(Var_res,cols = c(3:9), names_to = "D_Type", values_to = "Vals")
# 
# p1 <- ggplot(plot_dat, aes(x = log(Vals))) +    # Draw each column as histogram
#   geom_histogram() + 
#   facet_wrap(~ D_Type, scales = "free")
# p1<- p1 + theme_bw()
# p1 <- p1+theme(axis.text.x=element_text(angle = 0, hjust = 0),
#                legend.position="top",
#                legend.title=element_blank(),
#                legend.box = "horizontal",
#                legend.background = element_rect(fill="white",
#                                                 size=0.5, linetype="solid",
#                                                 colour ="white"),
#                legend.text=element_text(size=12),
#                axis.text=element_text(size=12, colour="black"),
#                axis.title=element_text(size=14, colour="black"),
#                axis.line = element_line(colour = "black",
#                                         size = 0.5, linetype = "solid"),
#                axis.ticks = element_line(colour="black"),
#                panel.grid.major=element_line(colour = "light grey"),
#                panel.grid.minor=element_blank())
# p1
# 
# 
# p1 <- ggplot(plot_dat, aes(x = log(Vals))) +    # Draw each column as histogram
#   geom_histogram(aes(y = ..density..)) + 
#   geom_density(col = "#1b98e0", size = 1) + 
#   facet_wrap(~ D_Type, scales = "free")
# p1<- p1 + theme_bw()
# p1 <- p1+theme(axis.text.x=element_text(angle = 0, hjust = 0),
#                legend.position="top",
#                legend.title=element_blank(),
#                legend.box = "horizontal",
#                legend.background = element_rect(fill="white",
#                                                 size=0.5, linetype="solid",
#                                                 colour ="white"),
#                legend.text=element_text(size=12),
#                axis.text=element_text(size=12, colour="black"),
#                axis.title=element_text(size=14, colour="black"),
#                axis.line = element_line(colour = "black",
#                                         size = 0.5, linetype = "solid"),
#                axis.ticks = element_line(colour="black"),
#                panel.grid.major=element_line(colour = "light grey"),
#                panel.grid.minor=element_blank())
# p1

# ggsave(paste0(WUDR_github,"/Regression/plots/","Log_transformed.png"), plot = p1, width = 9.5, height = 6, units = "in")


#########################################################################
### Log transformation is preferred
#########################################################################


######################################################################
# Get data in required format


library(lme4)
library(lmerTest)
library(ggeffects)
library(broom)
library(gtsummary)
library(statmod)

dat <- left_join(Var_res[,c(1,2,9)], Var_pred, by = c("COUNTYFP", "YEAR"))
dat <- dat %>% mutate_at(vars(-c(1,2)), funs(round(., 2)))

dat$COUNTYFP<- as.factor(dat$COUNTYFP)
# dat$YEAR<- as.factor(dat$YEAR)

dat2 <- dplyr::filter(dat, Total_irrigation_withdarwals_calculated >0)

dat2[,c(4:12)] <- as.data.frame(scale(dat2[,c(4:12)])) #standardization mean of 0 SD 1
dat2$YEAR <- dat2$YEAR-2002+1
dat2 <- dat2[complete.cases(dat2), ]
lmm2 <- lmer(log(Total_irrigation_withdarwals_calculated) ~ PPT +Min_Temp+Max_Temp+Zero_PPT_days+ Extreme_days+SDI+GC+(YEAR|COUNTYFP)+(1|COUNTYFP),
                         data=dat2, control = lmerControl(optimizer ="Nelder_Mead"),REML = TRUE)
lmm2

anova(lmm2)
# confint(lmm2)

dat2$Predict <- exp(predict(lmm2))

############################################################
# DEQ withdrawals

dat <- left_join(Var_res[,c(1,2,6)], Var_pred, by = c("COUNTYFP", "YEAR"))
dat <- dat %>% mutate_at(vars(-c(1,2)), funs(round(., 2)))

dat$COUNTYFP<- as.factor(dat$COUNTYFP)
# dat$YEAR<- as.factor(dat$YEAR)


dat2 <- dplyr::filter(dat, DEQ_Irrigation_withdrawals >0)
dat2[,c(4:12)] <- as.data.frame(scale(dat2[,c(4:12)])) #standardization mean of 0 SD 1
dat2$YEAR <- dat2$YEAR-2002+1


dat2 <- dat2[complete.cases(dat2), ]

# write.csv(dat2, paste0(WUDR_github, "/Regression/DEQ_Model_data.csv"))

lmm2 <- lmer(log(DEQ_Irrigation_withdrawals) ~ PPT +Min_Temp+Max_Temp+Zero_PPT_days+ Extreme_days+SDI+GC+(YEAR|COUNTYFP)+(1|COUNTYFP),
             data=dat2)
lmm2


# anovalmer(lmm2)


anova(lmm2)
summary(lmm2)$coef
ranova(lmm2)

ls_means(lmm2)


#######################################################################################
####



model <- lmm2
coefs<-data.frame(coef(summary(model)))
df.KR <- get_ddf_Lb(model, fixef(model))
coefs$p.KR <- 2 * (1 - pt(abs(coefs$t.value), df.KR))

dat2$Predict<-  exp(predict(lmm2))
Intercepts <- coef(lmm2)
rsq <- function (x, y) cor(x, y) ^ 2

# save(model,dat,dat2,file=paste0(WUDR_github,"/Regression/plots/","DEQ_LMER_model.Rda"))


rsq(dat2$DEQ_Irrigation_withdrawals,dat2$Predict)

plot_dat <- pivot_longer(dat2[,c(3,13)],cols = c(1,2), names_to = "Type",values_to ="Withdrawals")
plot_dat$Dat <- NA

plot_dat$Dat<- ifelse(plot_dat$Type == "Predict", "Modelled", "Observed")



p1 <- ggplot(dat2, aes(x=log10(DEQ_Irrigation_withdrawals),y=log10(Predict)))+
  geom_point()+
  geom_smooth(method=lm)+
  labs(title= "DEQ Reported vs Predicted withdrawals",
       x="DEQ Irrigtaion Withdrawals (log10)", y = " Modelled Withdrawals (log10)") 
# +
#   scale_y_continuous(limits = c(-6, 12),  breaks = seq(-6, 12, by = 2))+
#   scale_x_continuous(limits = c(-6, 12),  breaks = seq(-6, 12, by = 2))

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

ggsave(paste0(WUDR_github,"/Regression/plots/","DEQ Reported vs predicted.png"), plot = p1, width = 9.5, height = 6, units = "in")



S_Summary <- plot_dat %>% 
  group_by(Type) %>% 
  summarize(Q25 = quantile(Withdrawals, probs = 0.25),
            Median = quantile(Withdrawals, probs = 0.5),
            Q75 = quantile(Withdrawals, probs = 0.75),
            Mean = mean(Withdrawals))
library(hydroGOF)
pbias(dat2$Predict, dat2$DEQ_Irrigation_withdrawals)  # Lower than DEQ reported
NSE(dat2$Predict, dat2$DEQ_Irrigation_withdrawals)   
gof(dat2$Predict, dat2$DEQ_Irrigation_withdrawals) 

p1 <- ggplot(Irrigated_amount, aes(x=IRR_DEQ_withdrawals, y=All_Irrigation))+
  geom_point()+
  geom_smooth(method=lm)+
  labs(title= "Scatter plot 2002-2017",
       x="DEQ Irrigtaion Withdrawals (log)", y = "Irrigation amount based on Deficit Irrigation Method\n (using max total Irrigated area) (log)") 
# +
#   scale_y_continuous(limits = c(-6, 12),  breaks = seq(-6, 12, by = 2))+
#   scale_x_continuous(limits = c(-6, 12),  breaks = seq(-6, 12, by = 2))

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

lmm2 <- lmer(log(DEQ_Irrigation_withdrawals) ~ PPT +Min_Temp+Max_Temp+Zero_PPT_days+ Extreme_days+SDI+GC+(YEAR|COUNTYFP)+(1|COUNTYFP),
             data=dat2, control = lmerControl(optimizer ="Nelder_Mead"))
lmm2

vif.mer <- function (fit) {	
  ## adapted from rms::vif
  
  v <- vcov(fit)		# variance-covariance matrix
  nam <- names(fixef(fit))	# names of predictor variables
  
  ## exclude intercepts
  ns <- sum(1 * (nam == "Intercept" | nam == "(Intercept)"))
  if (ns > 0) {
    v <- v[-(1:ns), -(1:ns), drop = FALSE]
    nam <- nam[-(1:ns)]
  }
  
  d <- diag(v)^0.5
  v <- diag(solve(v/(d %o% d)))
  names(v) <- nam
  v
}

vif.mer(lmm2)
Intercepts <- coef(lmm2)

plot_dat <- pivot_longer(dat2[,c(3,13)],cols = c(1,2), names_to = "Type",values_to ="Withdrawals")
plot_dat$Dat <- NA

plot_dat$Dat<- ifelse(plot_dat$Type == "Predict", "Modelled", "Observed")


library(scales)


p1 <- ggplot(dat2, aes(x=Total_irrigation_withdarwals_calculated,y=Predict))+
  geom_point()+
  geom_smooth(method=lm)+
  labs(title= "Total irrigation vs Predicted withdrawals",
       x="Total Irrigation Withdrawals (log10)", y = " Modelled Withdrawals (log10)") +
  scale_x_continuous(trans = log10_trans(), 
                     breaks = c(1, 10, 100, 1000,5000), 
                     limits=c(min,max))+
  scale_y_continuous(trans = log10_trans(), 
                     breaks = c(1, 10, 100, 1000,5000), 
                     limits=c(min,max))


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

ggsave(paste0(WUDR_github,"/Regression/plots/","Total irrigation withdrawals vs predicted.png"), plot = p1, width = 9.5, height = 6, units = "in")












### Corelation for Predictor variables
co_dat <- Var_pred[complete.cases(Var_pred),] 
P_dat<- co_dat[,c(3:11)]
cor(P_dat)

library(psych)
 


p <- corPlot(P_dat, cex = 0.75)
dev.off() 

corrplot(M, method = 'square', order = 'AOE', addCoef.col = 'black', tl.pos = 'd',
         cl.pos = 'n', col = COL2('BrBG'))



library("Hmisc")
res2 <- rcorr(as.matrix(P_dat))


library(PerformanceAnalytics)
chart.Correlation(P_dat, histogram=TRUE, pch=19)


















#######################################################################
# DEQ reported
library(lme4)
library(lmerTest)


dat <- left_join(Var_res[,c(1,2,6)], Var_pred, by = c("COUNTYFP", "YEAR"))
dat <- dat %>% mutate_at(vars(-c(1,2)), funs(round(., 2)))

dat$COUNTYFP<- as.factor(dat$COUNTYFP)
dat$YEAR<- as.factor(dat$YEAR)

# # dat2 <- dat %>% 
# #   mutate(SF_Unreported_Coeff_method_Deq_irri_counties = ifelse(SF_Unreported_Coeff_method_Deq_irri_counties <0, NA,SF_Unreported_Coeff_method_Deq_irri_counties))
# 
# 
# lmm <- lmer(DEQ_Irrigation_withdrawals ~ PPT + Irrigation+Min_Temp+Mean_Temp+Max_Temp+Zero_PPT_days+Extreme_days+SDI+GC+(1 | YEAR) +(1 | COUNTYFP), data = dat,REML = FALSE)  
# # Random intercepts for YEAR and county (different baselines, different average effect per YEAR and county).
# lmm
# summary(lmm)
# coef(lmm)
# anova(lmm)

dat2 <- dat %>% 
   mutate(DEQ_Irrigation_withdrawals = ifelse(DEQ_Irrigation_withdrawals <=0, NA,DEQ_Irrigation_withdrawals))
 
lmm <- lmer(DEQ_Irrigation_withdrawals ~ PPT + Irrigation+Min_Temp+Mean_Temp+Max_Temp+Zero_PPT_days+ Extreme_days+SDI+GC+(1+YEAR) +(1+COUNTYFP),
      data=dat2, REML = FALSE)
summary(lmm) 

lmm2 <- lmer(DEQ_Irrigation_withdrawals ~ PPT + Irrigation+Min_Temp+Mean_Temp+Max_Temp+Zero_PPT_days+ Extreme_days+SDI+GC+(YEAR|COUNTYFP),
      data=dat2, REML = FALSE)
summary(lmm2) 

anova(lmm2)
confint(lmm2)
predict(lmm2)
plot(lmm2)





#############################################
# Total Withdrawals
dat <- left_join(Var_res[,c(1,2,11)], Var_pred, by = c("COUNTYFP", "YEAR"))
dat <- dat %>% mutate_at(vars(-c(1,2)), funs(round(., 2)))

dat$COUNTYFP<- as.factor(dat$COUNTYFP)
dat$YEAR<- as.factor(dat$YEAR)
lmm2 <- lmer(Total_irrigation_withdarwals_calculated ~ PPT + Irrigation+Min_Temp+Mean_Temp+Max_Temp+Zero_PPT_days+ Extreme_days+SDI+GC+(1+YEAR|COUNTYFP),
      data=dat, REML = FALSE)
summary(lmm2) 
Intercepts <- coef(lmm2)
anova(lmm2)
confint(lmm2)
predict(lmm2)
plot(lmm2)



#########################################
# Small Farm Unreported
dat <- left_join(Var_res[,c(1:2,5)], Var_pred, by = c("COUNTYFP", "YEAR"))

dat <- dat %>% 
  mutate(SF_Unreported_Coeff_method_Deq_irri_counties = ifelse(SF_Unreported_Coeff_method_Deq_irri_counties <0, NA,SF_Unreported_Coeff_method_Deq_irri_counties))


lmm <- lmer(SF_Unreported_Coeff_method_Deq_irri_counties ~ PPT + Irrigation+Min_Temp+Mean_Temp+Max_Temp+Zero_PPT_days+Extreme_days+(1 | YEAR)+(1 | COUNTYFP), data = dat,REML = FALSE)  
# Random intercepts and slopes for YEAR and county (different baselines, different average effect per YEAR and county).
lmm
# anova(lmm)
summary(lmm) 

lmm <- lmer(SF_Unreported_Coeff_method_Deq_irri_counties ~ PPT + Irrigation+Min_Temp+Mean_Temp+Max_Temp+Zero_PPT_days+ Extreme_days+(1+YEAR|COUNTYFP), data = dat,REML = FALSE) 
# The effect of YEAR will vary between COUNTYFP. Random intercepts for YEAR, random slopes for COUNTYFP influenced by YEAR.

lmm
summary(lmm) 

lmm2 <- lmer(SF_Unreported_Coeff_method_Deq_irri_counties ~ PPT + Irrigation+Min_Temp+Mean_Temp+Max_Temp+Zero_PPT_days+ Extreme_days+SDI+GC+(1+YEAR|COUNTYFP),
             data=dat, REML = FALSE)
summary(lmm2) 
anova(lmm2)
plot(lmm2)
#########################################
# Large Farm Unreported
dat <- left_join(Var_res[,c(1:2,7)], Var_pred, by = c("COUNTYFP", "YEAR"))

dat <- dat %>% 
  mutate(LF_Unreported_Coeff_method_Deq_irri_counties = ifelse(LF_Unreported_Coeff_method_Deq_irri_counties <0, NA,LF_Unreported_Coeff_method_Deq_irri_counties))


lmm <- lmer(LF_Unreported_Coeff_method_Deq_irri_counties ~ PPT + Irrigation+Min_Temp+Mean_Temp+Max_Temp+Zero_PPT_days+Extreme_days+(1 | YEAR)+(1 | COUNTYFP), data = dat,REML = FALSE)  
# Random intercepts and slopes for YEAR and county (different baselines, different average effect per YEAR and county).
lmm
# anova(lmm)
summary(lmm) 

lmm <- lmer(LF_Unreported_Coeff_method_Deq_irri_counties ~ PPT + Irrigation+Min_Temp+Mean_Temp+Max_Temp+Zero_PPT_days+ Extreme_days+(1+YEAR|COUNTYFP), data = dat,REML = FALSE) 
# The effect of YEAR will vary between COUNTYFP. Random intercepts for YEAR, random slopes for COUNTYFP influenced by YEAR.

lmm
summary(lmm) 

