WUDR_github<-"F:/My Drive/WUDR/WUDR_Github/WUDR_local"
setwd(WUDR_github)

pacman::p_load(dplyr, rgdal, tmap, sqldf,ggpubr,expss )
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
write.csv(dat, paste0(WUDR_github,"/csv_files/Common_counties_dat.csv"))

p<-ggscatter(dat, x="Facility_withdrawal_mg", y="Irrigated_Acreage", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Facility Withdrawal million gallon (DEQ)",
          ylab = "Irrigated Acreage (USDA)")+ scale_y_log10() +scale_x_log10()
p

# ggsave(paste0(WUDR_github,"/plots/Acreage_withdarawal_log.png"),p, width = 5, height = 5, units="in")

p2<-ggscatter(dat, x="Count", y="Irrigated_Operations", 
             add = "reg.line", conf.int = TRUE, 
             cor.coef = TRUE, cor.method = "pearson",
             xlab = "Agriculture facilities (DEQ)",
             ylab = "Irrigated Operations (USDA)")+ scale_y_log10() +scale_x_log10()
p2

 # ggsave(paste0(WUDR_github,"/plots/Facilties_operations_log.png"),p2, width = 5, height = 5, units="in")


##Rank
core_deq$Rank_DEQ_F_Withdrawls<-rank(desc(core_deq$Facility_withdrawal_mg),ties.method= "first")
core_census$Rank_census_Irr_Acreage<-rank(desc(core_census$Irrigated_Acreage))
core_deq$Rank_Deq_fac<-rank(desc(core_deq$Count),ties.method= "first")
core_census$Rank_census_Operations<-rank(desc(core_census$Irrigated_Operations),ties.method= "first")


rank_dat<-merge.data.frame(core_deq[,c(2,3,6,7)],core_census[,c(2,6,7)], by="GEOID")



#minimum edit distance

rank_dat$Acreage_withdarwal_difference<-rank_dat$Rank_DEQ_F_Withdrawls-rank_dat$Rank_census_Irr_Acreage
rank_dat$Facilities_Operations_difference<-rank_dat$Rank_Deq_fac-rank_dat$Rank_census_Operations
# write.csv(rank_dat, paste0(WUDR_github,"/csv_files/Rand and Difference in Ranks.csv"))


p3<-ggplot(data = rank_dat, aes(x=Acreage_withdarwal_difference))+
  geom_histogram()+
  labs(subtitle = "Difference in county ranks for Irrigated acreage in Cenus data and DEQ reported withdrawals", 
       x= "Difference in rank", y="Count", caption = "(-ve sign of rank indicates higher acreage area rank for a county in census data)")+
theme_light()
 

p3
# ggsave(paste0(WUDR_github,"/plots/Rank_diff_acreage_withdarwals.png"),p3, width = 8, height = 5, units="in")

p4<-ggplot(data = rank_dat, aes(x=Facilities_Operations_difference))+
  geom_histogram()+
  labs(subtitle = "Difference in county ranks for Operations in Cenus data and Faciltiites in DEQ data", 
       x= "Difference in rank", y="Count", caption = "(-ve sign of rank indicates higher operations rank for a county in census data)")+
  theme_light()


p4
# ggsave(paste0(WUDR_github,"/plots/Rank_diff_opeartions_fac.png"),p4, width = 8, height = 5, units="in")

p5<-ggplot(data = rank_dat, aes(x=Acreage_withdarwal_difference))+
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")+
  labs(subtitle = "Density plot of difference in ranks for Irrigated acreage in Cenus data and DEQ reported withdrawals", 
       x= "Difference in rank", y="Density", caption = "(-ve sign of rank indicates higher acreage area rank for a county in census data)")+
  theme_light()
p5 
# ggsave(paste0(WUDR_github,"/plots/density_acreage_with.png"),p5, width = 8, height = 5, units="in")

p6<-ggplot(data = rank_dat, aes(x=Facilities_Operations_difference))+
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")+
  labs(subtitle = "Density plot of difference in ranks for for Operations in Cenus data and Faciltiites in DEQ data", 
       x= "Difference in ranks", y="Density", caption = "(-ve sign of rank indicates higher operations rank for a county in census data)")+
  theme_light()
p6 

# ggsave(paste0(WUDR_github,"/plots/density_opeartions_fac.png"),p6, width = 8, height = 5, units="in")

##Percentage
total_withdrawal_deq<-sum(core_deq$Facility_withdrawal_mg)
core_deq$percet_withdrawal<-round(100*core_deq$Facility_withdrawal_mg/total_withdrawal_deq,0)
core_deq<-core_deq %>% arrange(desc(percet_withdrawal))


total_withdrawal_census<-sum(core_census$Irrigated_Acreage)
core_census$percet_acreage<-round(100*core_census$Irrigated_Acreage/total_withdrawal_census,0)
core_census<-core_census %>% arrange(desc(percet_acreage))

#Percent atleast 80
p80_deq_with<-subset(core_deq, c(TRUE, cumsum(percet_withdrawal) <= 80)[-nrow(core_deq)])
sum(p80_deq_with$percet_withdrawal)

p80_census_acreage<-subset(core_census, c(TRUE, cumsum(percet_acreage) <= 80)[-nrow(core_census)])
sum(p80_census_acreage$percet_acreage)

##
p80_deq_with$status<-p80_deq_with$GEOID %in% p80_census_acreage$GEOID
# OR df2[df2$genecolumn %in% df1$gene_list_column_name,]
p80_deq_with$status[p80_deq_with$status==FALSE] <- c("High in DEQ data not in USDA")
p80_deq_with$status[p80_deq_with$status==TRUE] <- c("High in both datasets")
# write.csv(p80_deq_with, paste0(WUDR_github,"/csv_files/DEQ 80 percent withdarwals.csv"))
##
p80_census_acreage$status<-p80_census_acreage$GEOID %in% p80_deq_with$GEOID
p80_census_acreage$status[p80_census_acreage$status==FALSE] <- c("High in USDA data not in DEQ")
p80_census_acreage$status[p80_census_acreage$status==TRUE] <- c("High in both datasets")
# write.csv(p80_census_acreage, paste0(WUDR_github,"/csv_files/USDA 80 percent acreage.csv"))

bothdatasets<-dplyr::filter(p80_census_acreage, status=="High in both datasets")
bothdatasets<-merge.data.frame(bothdatasets,p80_deq_with[,c("GEOID","Count","Facility_withdrawal_mg",
                                                            "Rank_DEQ_F_Withdrawls",  "Rank_Deq_fac", "percet_withdrawal")], by= "GEOID")

# write.csv(bothdatasets, paste0(WUDR_github,"/csv_files/Common 80 percent acreage and withdrawal.csv"))

DEQ_not_in_USDA<-subset(p80_deq_with, !(GEOID %in% bothdatasets$GEOID))
# write.csv(DEQ_not_in_USDA, paste0(WUDR_github,"/csv_files/80 perc withdarwal DEQ_not_in_USDA.csv"))
USDA_not_in_DEQ<-subset(p80_census_acreage, !(GEOID %in% bothdatasets$GEOID))
# write.csv(USDA_not_in_DEQ, paste0(WUDR_github,"/csv_files/80 perc withdarwal USDA_not_in_DEQ.csv"))


