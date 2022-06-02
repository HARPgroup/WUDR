
WUDR_github<-"F:/My Drive/WUDR/WUDR_Github/WUDR"
setwd(WUDR_github)
county.codes <- read.csv(paste0(WUDR_github,"/csv_files/county_codes_census.csv"))

TS_mean_LF <- read.csv(paste0(WUDR_github,"/Output_Tables/", "TS_large_farmers_coeff.csv"))
TS_median_LF <- read.csv(paste0(WUDR_github,"/Output_Tables/", "Median_TS_large_farmers_coeff.csv"))

TS_mean_sum <- sum(TS_mean_LF$Unreported_Coeff_Based)
TS_median_sum <- sum(TS_median_LF$Unreported_Coeff_Based)

# Median is half of mean



plot_dat_merge <- left_join(TS_median_LF,TS_mean_LF[,-c(4,3)], by= c("COUNTYFP", "YEAR"))
colnames(plot_dat_merge)[c(4,6,8)] <- c("DEQ Reported Irrigtaion Withdrawals","Unreported Median Coeff","Unreported Mean Coeff")

plot_dat <- pivot_longer(plot_dat_merge[,-c(5,7)], cols = c("DEQ Reported Irrigtaion Withdrawals","Unreported Median Coeff","Unreported Mean Coeff"), names_to = "Type" ,values_to = "Unreported Wth")

plot_dat <- split( plot_dat , f = plot_dat$County_Name)


for (i in 1:length(plot_dat)) {
  p1 <- ggplot(plot_dat[[i]], aes(x=YEAR, y=`Unreported Wth`, group = Type ))+
    geom_line(aes(color=Type))+
    geom_point(aes(color=Type))+
    labs(title= plot_dat[[i]][1,3] ,
         x="Year", y = "Unreported Withdrawals (MG)")+
    scale_x_continuous(limits = c(2002, 2017),  breaks = seq(2002, 2017, by = 2))
  
  p1<- p1 + theme_bw()
  p1 <- p1+theme(axis.text.x=element_text(angle = 0, hjust = 0),
                 legend.position="top",
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
  
  nam = plot_dat[[i]][1,3]
  ggsave(paste0(WUDR_github,"/plots/Coefficient1/timeseries/Large Farmers/Median_Comparison_large_Farm/", nam,".png"), plot = p1, width = 9.5, height = 6, units = "in")
  
}
