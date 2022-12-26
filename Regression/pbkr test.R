WUDR_github<-"F:/My Drive/WUDR/WUDR_Github/WUDR"
setwd(WUDR_github)

dat2<- read.csv(paste0(WUDR_github, "/Regression/DEQ_Model_data.csv"))
dat2 <- dat2[,-c(1)]

library(lme4)


lmm2 <- lmer(log(DEQ_Irrigation_withdrawals) ~ PPT +Min_Temp+Max_Temp+Zero_PPT_days+ Extreme_days+SDI+GC+(YEAR|COUNTYFP)+(1|COUNTYFP),
             data=dat2)
lmm2


######
library(pbkrtest)
library(MuMIn)


####
model <- lmm2
coefs<-data.frame(coef(summary(model)))
df.KR <- get_ddf_Lb(model, fixef(model))
coefs$p.KR <- 2 * (1 - pt(abs(coefs$t.value), df.KR))

dat2$Predict<-  exp(predict(lmm2))
rsq <- function (x, y) cor(x, y) ^ 2