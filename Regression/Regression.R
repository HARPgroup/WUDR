
WUDR_github<-"F:/My Drive/WUDR/WUDR_Github/WUDR"
setwd(WUDR_github)

library(tidyverse) 
library(rgdal)
library(vegan)
library(DescTools)
options(scipen = 9999)
# Load VA Shapefile to be used later
VA_counties<-readOGR(paste0(WUDR_github, "/VA_counties_sp"), layer="VA_counties")

county.codes <- read_csv(paste0(WUDR_github, "/csv_files/county_codes_census.csv")) 
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
  group_by(Year) %>% 
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
  group_by(Year) %>% 
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
  group_by(Year) %>% 
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
  group_by(Year,County) %>% 
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
  group_by(Year,County) %>% 
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
  group_by(County,Year) %>% 
   summarise(Yearly_sum = sum(PPT))


Monthly_sum <- SDI_dat %>% 
  group_by(County,Year,Month) %>% 
  summarise(Monthly_sum = sum(PPT))

Monthly_sum <- left_join(Monthly_sum, Yearly_sum, by = c("Year", "County"))
Monthly_sum$Prop <- Monthly_sum$Monthly_sum/Monthly_sum$Yearly_sum
Monthly_sum$Prop_2 <- Monthly_sum$Prop*log(Monthly_sum$Prop)
SDI <- Monthly_sum %>% 
  group_by(County, Year) %>% 
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
  group_by(Year, County) %>% 
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



colnames(Var_res)[3:8] <- c("Total_DEQ_withdrawals","SF_Unreported_Eff_precip_method_all_counties", "SF_Unreported_Coeff_method_Deq_irri_counties",
                            "DEQ_Irrigation_withdrawals","LF_Unreported_Coeff_method_Deq_irri_counties","LF_Unreported_Eff_precip_method_all_counties")

# Var_res <- Var_res %>% 
#   filter(Irrigation >0)

# Var_res$Total_DEQ_withdrawals <- round(Var_res$Total_DEQ_withdrawals,2)

Var_pred <- Var_res[,c(1,2,9,10)]

Var_pred <- purrr::reduce(list(Var_pred,Tmin_yearly[-2],Tmean_yearly[-2],Tmax_yearly[-2],Zero_PPT[-2],T_Extreme[-2], SDI[-1], Gini_cal[-2]),
                         dplyr::full_join, by = c("COUNTYFP","YEAR"))
Var_pred <- Var_pred %>% 
  filter(YEAR <2018)

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
 


lmm2 <- lmer(DEQ_Irrigation_withdrawals ~ PPT + Irrigation+Min_Temp+Mean_Temp+Max_Temp+Zero_PPT_days+ Extreme_days+SDI+GC+(1+YEAR|COUNTYFP),
      data=dat2, REML = FALSE)
summary(lmm2) 
Intercepts <- coef(lmm2)
anova(lmm2)
confint(lmm2)

# Random intercepts and slopes for YEAR and county (different baselines, different average effect per YEAR and county).


# lmm1 <-  lmer(DEQ_Irrigation_withdrawals ~ PPT + Irrigation+Min_Temp+Mean_Temp+Max_Temp+Zero_PPT_days+ 
#                 Extreme_days+SDI+GC+(1+YEAR|COUNTYFP)+(1|YEAR)+(1|COUNTYFP), data = dat,REML = FALSE) 
# # The effect of YEAR will vary between COUNTYFP. Random intercepts for YEAR, random slopes for COUNTYFP influenced by YEAR.
# lmm1
# 
# 
# 
#  lmm2
# summary(lmm2) 
# anova(lmm2)
# 
# lmm3 <- lmer(DEQ_Irrigation_withdrawals ~ PPT + Irrigation+Min_Temp+Mean_Temp+Max_Temp+Zero_PPT_days+ Extreme_days+SDI+GC+(1+YEAR|COUNTYFP), data = dat)  # The best one
# 
# anova(lmm3)
# 
# 
# lmm4 <- lmer(DEQ_Irrigation_withdrawals ~ PPT + Irrigation+Min_Temp+Mean_Temp+Max_Temp+Zero_PPT_days+ Extreme_days+SDI+GC+(1+YEAR)+(1 |YEAR:COUNTYFP), data = dat)  
# 
# anova(lmm4)
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
coef(lmm2)
anova(lmm2)
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

