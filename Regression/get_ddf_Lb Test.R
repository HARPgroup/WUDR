library(lme4)
library(pbkrtest)
library(MuMIn)

load("F:/My Drive/WUDR/WUDR_Github/WUDR/Regression/plots/DEQ_LMER_model.Rda")

coefs<-data.frame(coef(summary(model)))
df.KR <- get_ddf_Lb(model, fixef(model))
coefs$p.KR <- 2 * (1 - pt(abs(coefs$t.value), df.KR))