WUDR_github<-"F:/My Drive/WUDR/WUDR_Github/WUDR_local"
setwd(WUDR_github)

pacman::p_load(dplyr, rgdal, tmap, sqldf,ggpubr)
options(scipen=999)

##Load Summary to get the data in both datasets
summary_dat<-read.csv(paste0(WUDR_github,"/csv_files/deqdat_avaliability_summary2017.csv"))

#Create a datframe for data in both dataframes
both_datasets_data<-dplyr::filter(summary_dat, Data.Status=="Available in both datasets")


#Load DEQ dataset
deq_dat<-read.csv(paste0(WUDR_github,"/csv_files/deqdat_summary2017.csv"))

#Remove NA (NA for all the are same)
data_deq<-deq_dat[complete.cases(deq_dat),]


#Load Census Data
census_dat<-read.csv(paste0(WUDR_github,"/csv_files/2017_Census_Summary.csv"))

#Here NA for acreage is more

#Corelation datset for deq
core_deq<-merge.data.frame(both_datasets_data,deq_dat, by.x = "COUNTYFP", by.y ="COUNTYFP" )

core_deq<-core_deq %>%
  dplyr::select(COUNTYFP,GEOID=GEOID.x,NAMELSAD=NAMELSAD,Count,Facility_withdrawal_mg)


core_census<-merge.data.frame(both_datasets_data,census_dat, by.x = "COUNTYFP", by.y ="COUNTY_CODE" )

core_census<-core_census %>%
  dplyr::select(COUNTYFP=COUNTYFP,GEOID=GEOID,NAMELSAD,Irrigated_Acreage,Irrigated_Operations)

dat<-merge.data.frame(core_deq,core_census)
colnames(dat)[4]<-c("DEQ No of Facilities")
colnames(dat)[5]<-c("DEQ reported withdrawals")
colnames(dat)[6]<-c("Census Irrigated Acreage")
colnames(dat)[7]<-c("Census No of Irrigated Operations")

# write.csv(dat, paste0(WUDR_github,"/csv_files/irrigated_summary_both_datasets.csv"))
res <- cor.test(dat$`DEQ No of Facilities`, dat$`Census No of Irrigated Operations`, 
                method = "pearson")
res #Moderate


# Pearson's product-moment correlation
# 
# data:  core_deq$Count and core_census$Irrigated_Operations
# t = 2.7972, df = 35, p-value = 0.008321
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.1200533 0.6600472
# sample estimates:
#       cor 
# 0.4274449 

res <- cor.test(dat$`DEQ reported withdrawals`, dat$`Census Irrigated Acreage`, 
                method = "pearson")
res #Strong


# Pearson's product-moment correlation
# 
# data:  core_deq$Facility_withdrawal_mg and core_census$Irrigated_Acreage
# t = 5.8041, df = 35, p-value = 0.000001399
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.4867531 0.8348884
# sample estimates:
#       cor 
# 0.7003196 

# load("F:/My Drive/WUDR/DEQ_Data_Retrevial/corelation.RData")

dat<-merge.data.frame(core_deq,core_census)

p<-ggscatter(dat, x="Facility_withdrawal_mg", y="Irrigated_Acreage", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Facility Withdrawal million gallon (DEQ)",
          ylab = "Irrigated Acreage (USDA)")
p

ggsave(paste0(WUDR_github,"/plots/Acreage_withdarawal.png"),p, width = 5, height = 5, units="in")

p2<-ggscatter(dat, x="Count", y="Irrigated_Operations", 
             add = "reg.line", conf.int = TRUE, 
             cor.coef = TRUE, cor.method = "pearson",
             xlab = "Agriculture facilities (DEQ)",
             ylab = "Irrigated Operations (USDA)")
p2

ggsave(paste0(WUDR_github,"/plots/Facilties_operations.png"),p2, width = 5, height = 5, units="in")


##Rank
core_deq$F_withdarwal_rank<-rank(desc(core_deq$Facility_withdrawal_mg),ties.method= "first")
core_census$Irri_acreage_rank<-rank(desc(core_census$Irrigated_Acreage))
core_deq$count_f_rank<-rank(desc(core_deq$Count),ties.method= "first")
core_census$operations_rank<-rank(desc(core_census$Irrigated_Operations),ties.method= "first")


rank_dat<-merge.data.frame(core_deq[,c(2,3,7,8)],core_census[,c(2,6,7)], by="GEOID")



#minimum edit distance
#remove absolute
#hist
rank_dat$Diff_acreage_withdrawal<-rank_dat$F_withdarwal_rank-rank_dat$Irri_acreage_rank
rank_dat$Diff_oper_count<-abs(rank_dat$count_f_rank-rank_dat$operations_rank)


##Percentage
total_withdrawal_deq<-sum(core_deq$Facility_withdrawal_mg)
core_deq$percet_withdrawal<-round(100*core_deq$Facility_withdrawal_mg/total_withdrawal_deq,0)

total_withdrawal_census<-sum(core_census$Irrigated_Acreage)
core_census$percet_acreage<-round(100*core_census$Irrigated_Acreage/total_withdrawal_census,0)
