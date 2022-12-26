##
##
##	VWUDS - fit regressions of water withdrawals to climate/econ parameters
##		1. Load in flat VWUDS and explanatory variables
##		2. Formulate regression models and evaluate fit:
##			- Log (U) ~ a + b1y + BX
##			- a: county-specific intercept term
##			- b1: county-specific time trend
##			- BX: sector-specific vector of explanatory variables and regression coefficients
##		3. Revised form for municipal (population as explanatory variable)
##		4. 4 sectors x 7 temporal aggregation periods = 28 models
## 		5. Save results (model fit and regression parameters)
##	Run on R version 3.3.1


rm(list=ls(all=TRUE))

library(reshape)
library(lme4)
library(pbkrtest)
library(MuMIn)

# Set directory locations
data.loc<-"G:/My Drive/VT/Research/Virginia/VWUDS_WaterUse/Seasonal Analysis/JWRPM_Files/ModelData"
results.loc<-"G:/My Drive/VT/Research/Virginia/VWUDS_WaterUse/Seasonal Analysis/JWRPM_Files/ModelResults"

#######################################################################
## 1. Load in Flat Data
#######################################################################

## Load in VWUDS WY and seasonal county summaries
#--------------------------------------------------------

setwd(data.loc)

ag.data<-read.csv("AgFlat.csv",sep=",")
en.data<-read.csv("EnergyFlat.csv",sep=",")
ind.data<-read.csv("IndFlat.csv",sep=",")
mun.data<-read.csv("MunFullFlat.csv",sep=",")

data.list<-list(ag.data, en.data, ind.data, mun.data)

#############################################################################################
## 2. Fit linear mixed-effects models 
#############################################################################################
##
##	

## Specify model form for each sector
ag.form<- "log(usage) ~ Tmean + TotalP + gini + BEA_PI_DT + AgPayCBP + AgLC + GDP_Ag + USDA_PriceRatio + (Year |County)"
en.form<- "log(usage) ~ Tmean + TotalP + gini + BEA_PI_DT + GDP_PC_Change + EnergyPrice + ElectricitySales + EnergyProduction + (Year |County)"
ind.form<- "log(usage) ~ Tmean + TotalP + gini + BEA_PI_DT + GDP_PC_Change + ManPayCBP + GDP_Man + (Year |County)"
mun.form<- "log(withdrawal) ~ Tmean + TotalP + gini + BEA_PI_DT + GDP_PC_Change + pop + DevDensity + (Year | County)"


form.list<-list(ag.form, en.form, ind.form, mun.form)
n.var<-c(8, 8, 7, 7)	# number of explanatory variables in each form

timeIDs<-c("wy","w","c","wn","sp","sm","fa")
sectorIDs<-c("Agriculture","Energy","Industry","Municipal")

## Matrices to store conditional and marginal R2 values and ICC values (four sectors, seven time periods)
## 	ICC: Within-group and between-group contributions to variance
## 	marginal R2 is variance explained by fixed effects
## 	conditional R2 is variance explained by entire model (fixed and random effects)

R2.save<-matrix(data=NA,nrow=7,ncol=4)
rownames(R2.save)<-timeIDs
colnames(R2.save)<-sectorIDs
ICC.save<-R2.save
 
## Lists to store models (length = 4, one per sector)
## first index is sector, second is season
Model.master.list<-list()
	
## list to store fixed effects estimates (length = 4, one per sector)
FE.coef.list<-list()			# list of matrices (one per sector) with FE estimates 

## =====================================
## Loop through sectors and fit models
## =====================================
for (s in 1:4){
	data<-data.list[[s]]
	form<-form.list[[s]]
	Model.sector.list<-list()	# list to store models (length=7)

	## create matrix to store fixed effects estimates
	FE.matrix<-matrix(data=NA,nrow=7,ncol=n.var[s])
	rownames(FE.matrix)<-timeIDs

	## standardize explanatory variables
	data.clean<-data
	data.clean$Year<-data$Year - min(data$Year) + 1		# years past minimum year
	if (s < 4) {	# standardize variables for non-municipal tables
		for (v in 10:dim(data)[2]){		
			data.clean[,v]<-(data[,v]- mean(data[,v],na.rm=TRUE))/sd(data[,v],na.rm=TRUE)
		}
	}else{		# standardize variables for municipal table
		for (v in c(10:33,41:42)){		
			data.clean[,v]<-(data[,v]- mean(data[,v],na.rm=TRUE))/sd(data[,v],na.rm=TRUE)
		}
	}	
	for (t in 1:7){	# loop through time periods
		# transform basic model form to time-specific form
		# time specific usage, temp, totalp and gini
		form1<-gsub(pattern="usage",replacement=paste("usage_",timeIDs[t],sep=""),form)
		form2<-gsub(pattern="withdrawal",replacement=paste("withdrawal_",timeIDs[t],sep=""),form1)
		form3<-gsub(pattern="Tmean",replacement=paste("Tmean_",timeIDs[t],sep=""),form2)
		form4<-gsub(pattern="TotalP",replacement=paste("TotalP_",timeIDs[t],sep=""),form3)
		form.final<-gsub(pattern="gini",replacement=paste("gini_",timeIDs[t],sep=""),form4)
		
		## Fit model
		data.nonzero<-data.clean[data.clean[,2+t] > 0 ,]
		model<-lmer(form.final, data=data.nonzero)

		# Save R2 values 
		R2.mar<-r.squaredGLMM(model)[1,1]
		R2.con<-r.squaredGLMM(model)[1,2]
		R2.char<-paste(as.character(round(R2.con,3)), " (", as.character(round(R2.mar,3)), ")", sep="")
		R2.save[t,s]<-R2.char

		## Save ICC
		varsd<-as.data.frame(VarCorr(model))
		sigma_grp<-sum(varsd$sdcor[1:3])
		sigma_res<-varsd$sdcor[4]
		icc<-sigma_grp^2/(sigma_grp^2 + sigma_res^2)
		ICC.save[t,s]<-icc

		## Save fixed effects coefficients
		coefs<-data.frame(coef(summary(model)))
		fe.coefs<-coefs[2:dim(coefs)[1],1]
		FE.matrix[t,]<-fe.coefs
		colnames(FE.matrix)<-rownames(coefs)[2:dim(coefs)[1] ]

		## Save model
		Model.sector.list[[t]]<- model
	}	# CLOSE TIME LOOP (t)

	## Save model list and coefficient matrix
	Model.master.list[[s]]<-Model.sector.list
	FE.coef.list[[s]]<-FE.matrix
}		# CLOSE SECTOR LOOP (s)

## ==============================================================
## Loop through models 
##	calculate variable inflation factors
##	calculate variable significance (pbkrtest package)
## ==============================================================

## Function to calculate VIF in mixed model (from Zurr et al., 2010)	
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

## Calculate VIF for each sector/time period
max.vif<-matrix(data=NA,nrow=7,ncol=4)
rownames(max.vif)<-timeIDs
colnames(max.vif)<-sectorIDs

for (s in 1:length(sectorIDs)){
	for (t in 1:length(timeIDs)){
		model<-Model.master.list[[s]][[t]]
		max.vif[t,s]<-max(vif.mer(model))
	}
}
max.vif	# all values are below 3 - good!

## Calculate variable significance
FE.coef.details<-list()		# first index is sector, second is time

for (s in 1:length(sectorIDs)){
	coefs.sector.list<-list()	# length = 7
	for (t in 1:length(timeIDs)){
		model<-Model.master.list[[s]][[t]]
		coefs<-data.frame(coef(summary(model)))
		df.KR <- get_ddf_Lb(model, fixef(model))
		coefs$p.KR <- 2 * (1 - pt(abs(coefs$t.value), df.KR))
		coefs.sector.list[[t]]<-coefs
	}
	FE.coef.details[[s]]<-coefs.sector.list
}

save.image("G:\\My Drive\\VT\\Research\\Virginia\\VWUDS_WaterUse\\Seasonal Analysis\\JWRPM_Files\\Scripts\\StatsModels_R1.RData")

#########################################################################################
## 3. Save results
#########################################################################################

## Save AIC and RMSE results to tables
setwd(results.loc)

write.table(R2.save,file="R2_results.csv",row.names=TRUE,col.names=NA,sep=",")
write.table(ICC.save,file="ICC_results.csv",row.names=TRUE,col.names=NA,sep=",")

## Combine FE coefficients into single table per sector and save

test<-rbind(FE.coef.details[[4]][[1]], FE.coef.details[[4]][[2]])

for (s in 1:4){
	FE.table<-rbind(FE.coef.details[[s]][[1]], FE.coef.details[[s]][[2]], FE.coef.details[[s]][[3]],
	FE.coef.details[[s]][[4]], FE.coef.details[[s]][[5]], FE.coef.details[[s]][[6]], FE.coef.details[[s]][[7]])
	file.name<-paste(sectorIDs[s],"_FEcoef.csv",sep="")
	write.table(FE.table,file=file.name,row.names=TRUE,col.names=NA,sep=",")
	}

## Data summary table

data.summary<-matrix(data=NA,nrow=4,ncol=4)
rownames(data.summary)<-sectorIDs
colnames(data.summary)<-c("Counties","Facilities","Avg Length","Observations")
for (s in 1:4){
	data<-data.list[[s]]
	data.trim<-data
	if (s == 1){
		data.trim<-data[!is.na(data$AgPayCBP),]
	}
	if (s == 3){
		data.trim<-data[!is.na(data$ManPayCBP),]
	}
	# clean out zeros, standardize explanatory variables
	wy.nonzero<-subset(data.trim,usage_wy > 0)


	n.county<-length(unique(wy.nonzero$County))
	data.summary[s,1]<-n.county
	county.years<-c()
	for (c in 1:n.county){
		county.subset<-subset(wy.nonzero, County == unique(wy.nonzero$County)[c])
		county.length<-length(unique(county.subset$Year))
		county.years<-c(county.years,county.length)
		}
	data.summary[s,3]<-round(mean(county.years),0)
	data.summary[s,4]<-dim(wy.nonzero)[1]
	}

# Count facilities
setwd("G:/My Drive/VT/Research/Virginia/VWUDS_WaterUse/Seasonal Analysis/DataDumpTables")

ag.raw<-read.csv("DataDump_bySector_ag.csv",header=TRUE)
energy.raw<-read.csv("DataDump_bySector_Energy.csv",header=TRUE)
ind.raw<-read.csv("DataDump_bySector_ind.csv",header=TRUE)
mun.raw<-read.csv("DataDump_bySector_mun.csv",header=TRUE)

## remove hydropower facilities from energy table
energy.raw<-subset(energy.raw,Use.Type != "hydropower")	
use.levels<-droplevels(energy.raw$Use.Type)
energy.raw$Use.Type<-use.levels
loc.levels<-droplevels(energy.raw$Locality)
energy.raw$Locality<-loc.levels

raw.list<-list(ag.raw,energy.raw,ind.raw,mun.raw)
for (s in 1:4){
	table<-raw.list[[s]]
	n.fac<-length(unique(table$Facility))	
	data.summary[s,2]<-n.fac
	}

setwd(results.loc)
write.table(data.summary,file="VWUDS_DataSummary.csv",row.names=TRUE,col.names=NA,sep=",")

#############################################################################################
## 6. Error plots (LMER full model)
#############################################################################################

setwd(paste(results.loc,"/ErrorPlots",sep=""))
ppi<-200

## Plots of observations versus predictions
for (s in 1:length(sectorIDs)){
	data<-data.list[[s]]
	data.trim<-data
	if (s == 1){
		data.trim<-data[!is.na(data$AgPayCBP),]
	}
	if (s == 3){
		data.trim<-data[!is.na(data$ManPayCBP),]
	}
	for (t in 1:length(timeIDs)){
		if (s == 4){
			obs.column<-t+33
		} else {
			obs.column<-t+2
		}
		data.nonzero<-data.trim[data.trim[,obs.column] > 0 ,]
		model<-Model.master.list[[s]][[t]]
		pred.model<-exp(predict(model))
		obs<-data.nonzero[,obs.column]

		filename<-paste(sectorIDs[s],"_",timeIDs[t],".png",sep="")
		png(file=filename,width=5*ppi,height=5*ppi,res=ppi)	
		plot.title<-paste(sectorIDs[s]," ",timeIDs[t],sep="")
		plot.bounds<-c(min(log(obs),log(pred.model)),max(log(obs),log(pred.model)))
		plot(log(obs), log(pred.model), xlim=plot.bounds, ylim=plot.bounds, main=plot.title)
		abline(a=0,b=1)
		dev.off()
	}
}

# Residual Plots (start with wy, warm and cool)
setwd(paste(results.loc,"/ResidualPlots",sep=""))

for (s in 1:4){
	# PLOTTING PARAMETERS
	ppi<-300
	filename<-paste("Residuals_",sectorIDs[s],".png",sep="")
	png(file=filename,width=6*ppi,height=9*ppi,res=ppi)		
	par(mfrow=c(3,2),xpd=TRUE)								

	# WATER YEAR PLOTS
	data<-data.list[[s]]
	data.trim<-data
	if (s == 1){
		data.trim<-data[!is.na(data$AgPayCBP),]
	}
	if (s == 3){
		data.trim<-data[!is.na(data$ManPayCBP),]
	}
	# clean out zeros, standardize explanatory variables
	wy.nonzero<-subset(data.trim,usage_wy > 0)		
	model<-Model.master.list[[s]][[1]]
	resid.df<-data.frame(Year=wy.nonzero$Year, County=wy.nonzero$County, resid= residuals(model) )
	# generate palette
	n.county<-length(unique(resid.df$County))
	n.year<-length(unique(resid.df$Year))
	# residual plot by county
	county.palette<-rainbow(n=n.county)
	plot(NA,ylim=c(min(resid.df$resid),max(resid.df$resid)), xlim=c(0,n.county),
		xlab="County", ylab="Residual" ,main = "Water Year Model")
		for (c in 1:n.county){
			county.resid<-subset(resid.df,County == unique(resid.df$County)[c])
			points(rep(c,dim(county.resid)[1]),county.resid$resid, bg=county.palette[c], pch=21)
			}
	# residual plot by year
	year.palette<-rainbow(n=n.year)
	plot(NA,ylim=c(min(resid.df$resid),max(resid.df$resid)), xlim=c(min(resid.df$Year),max(resid.df$Year)),
		xlab="Year", ylab="Residual" )
		for (y in 1:n.year){
			year.y<-unique(resid.df$Year)[y]
			year.resid<-subset(resid.df,Year == year.y)
			points(rep(year.y,dim(year.resid)[1]),year.resid$resid, bg=year.palette[y], pch=21)
			}

	# WARM SEASON PLOTS
	# clean out zeros, standardize explanatory variables
	w.nonzero<-subset(data.trim,usage_w > 0)		
	model<-Model.master.list[[s]][[2]]
	resid.df<-data.frame(Year=w.nonzero$Year, County=w.nonzero$County, resid= residuals(model) )
	# generate palette
	n.county<-length(unique(resid.df$County))
	n.year<-length(unique(resid.df$Year))
	# residual plot by county
	county.palette<-rainbow(n=n.county)
	plot(NA,ylim=c(min(resid.df$resid),max(resid.df$resid)), xlim=c(0,n.county),
		xlab="County", ylab="Residual" ,main = "Warm Season Model")
		for (c in 1:n.county){
			county.resid<-subset(resid.df,County == unique(resid.df$County)[c])
			points(rep(c,dim(county.resid)[1]),county.resid$resid, bg=county.palette[c], pch=21)
			}
	# residual plot by year
	year.palette<-rainbow(n=n.year)
	plot(NA,ylim=c(min(resid.df$resid),max(resid.df$resid)), xlim=c(min(resid.df$Year),max(resid.df$Year)),
		xlab="Year", ylab="Residual" ,main = "Warm Season Model")
		for (y in 1:n.year){
			year.y<-unique(resid.df$Year)[y]
			year.resid<-subset(resid.df,Year == year.y)
			points(rep(year.y,dim(year.resid)[1]),year.resid$resid, bg=year.palette[y], pch=21)
			}

	# COOL SEASON PLOTS
	c.nonzero<-subset(data.trim,usage_c > 0)		
	model<-Model.master.list[[s]][[3]]
	# clean out zeros, standardize explanatory variables
	resid.df<-data.frame(Year=c.nonzero$Year, County=c.nonzero$County, resid= residuals(model) )
	# generate palette
	n.county<-length(unique(resid.df$County))
	n.year<-length(unique(resid.df$Year))
	# residual plot by county
	county.palette<-rainbow(n=n.county)
	plot(NA,ylim=c(min(resid.df$resid),max(resid.df$resid)), xlim=c(0,n.county),
		xlab="County", ylab="Residual" ,main = "Cool Season Model")
		for (c in 1:n.county){
			county.resid<-subset(resid.df,County == unique(resid.df$County)[c])
			points(rep(c,dim(county.resid)[1]),county.resid$resid, bg=county.palette[c], pch=21)
			}
	# residual plot by year
	year.palette<-rainbow(n=n.year)
	plot(NA,ylim=c(min(resid.df$resid),max(resid.df$resid)), xlim=c(min(resid.df$Year),max(resid.df$Year)),
		xlab="Year", ylab="Residual" ,main = "Cool Season Model" )
		for (y in 1:n.year){
			year.y<-unique(resid.df$Year)[y]
			year.resid<-subset(resid.df,Year == year.y)
			points(rep(year.y,dim(year.resid)[1]),year.resid$resid, bg=year.palette[y], pch=21)
			}
	dev.off()
	}	# close sector loop (s)
	

save.image("G:\\My Drive\\VT\\Research\\Virginia\\VWUDS_WaterUse\\Seasonal Analysis\\JWRPM_Files\\Scripts\\StatsModels_R1.RData")
#load("G:\\My Drive\\VT\\Research\\Virginia\\VWUDS_WaterUse\\Seasonal Analysis\\JWRPM_Files\\Scripts\\StatsModels.RData")






