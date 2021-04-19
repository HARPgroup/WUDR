WUDR_github<-"F:/My Drive/WUDR/WUDR_Github/WUDR_local"
setwd(WUDR_github)
library('gridExtra')

library(tidyverse)
options(scipen=999)

summary_2002<-read.csv(paste0(WUDR_github,"/csv_files/2002_census_dat_availability_summary.csv"))
summary_2002 <- summary_2002%>%
  filter(data_status=="Available in USDA dataset") %>% 
  select(c("County_Name","Irrigated_Acreage"))

summary_2007<-read.csv(paste0(WUDR_github,"/csv_files/2007_census_dat_availability_summary.csv"))
summary_2007 <- summary_2007%>%
  filter(data_status=="Available in USDA dataset") %>% 
  select(c("County_Name","Irrigated_Acreage"))
summary_2012<-read.csv(paste0(WUDR_github,"/csv_files/2012_census_dat_availability_summary.csv"))
summary_2012 <- summary_2012%>%
  filter(data_status=="Available in USDA dataset") %>% 
  select(c("County_Name","Irrigated_Acreage"))
summary_2017<-read.csv(paste0(WUDR_github,"/csv_files/2017_census_dat_availability_summary.csv"))
summary_2017 <- summary_2017%>%
  filter(data_status=="Available in USDA dataset") %>% 
  select(c("County_Name","Irrigated_Acreage"))

summary <- merge.data.frame(summary_2002 , summary_2007, by= "County_Name", all.x = TRUE, all.y = TRUE)
summary <- merge.data.frame(summary , summary_2012, by= "County_Name", all.x = TRUE, all.y = TRUE)
summary <- merge.data.frame(summary , summary_2017, by= "County_Name", all.x = TRUE, all.y = TRUE)

colnames(summary) <- c("County", "Year 2002", "Year 2007", "Year 2012", "Year 2017")

dat_long <- pivot_longer(summary, cols = c("Year 2002", "Year 2007", "Year 2012", "Year 2017"))

#############################
#Histogrmas for counties  ##
############################
p1<-dat_long %>% 
  ggplot(aes(x=value))+
  geom_histogram()+
  labs(title = "Irrigated Acreage for data in only Census datset", 
       x= "Irrigated Acreage", y="Count")+
   scale_x_continuous(breaks=seq(0, 4500, 500))+
  theme_bw()+ 
 facet_wrap(~name)+
  theme(legend.position="none", 
        legend.text=element_text(size=12),
        axis.text=element_text(size=12, colour="black"),
        axis.title=element_text(size=14, colour="black"),
        axis.line = element_line(colour = "black", 
                                 size = 0.5, linetype = "solid"),
        axis.ticks = element_line(colour="black"),
        panel.grid.major=element_line(colour = "light grey"), 
        panel.grid.minor=element_blank(),
        strip.text = element_text(size = 12, color = "black" ),
        strip.background = element_rect(
          color="black", fill ="#FFE4C4", size=1.5, linetype="solid"))
p1

ggsave(paste0(WUDR_github,"/plots/commondat/Hist.png"),p1, width = 10, height = 6, units="in")
