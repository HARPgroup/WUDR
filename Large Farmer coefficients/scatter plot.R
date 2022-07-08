# Scatter Plot

WUDR_github<-"F:/My Drive/WUDR/WUDR_Github/WUDR"
setwd(WUDR_github)
library(tidyverse) 
Irrigated_amount<- read.csv( paste0(WUDR_github,"/Output_Tables/", "Timeseries_Tot_large_farms.csv"))
Irrigated_amount$l_Irr_DEQ <- log(Irrigated_amount$IRR_DEQ_withdrawals)
# plot_dat <- pivot_longer(Irrigated_amount[,c(7,10)], cols = c("IRR_DEQ_withdrawals","All_Irrigation"))

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
ggplotly(p1)  

ggsave(paste0(WUDR_github,"/plots/Coefficient1/Scatter_AgCenusus VS DeqIRR.png"), plot = p1, width = 9.5, height = 6, units = "in")

p1 <- ggplot(Irrigated_amount, aes(x=log(TOT_DEQ_Withdrawals), y=log(All_Irrigation)))+
  geom_point()+
  geom_smooth(method=lm)+
  labs(title= "Scatter plot 2002-2017",
       x="DEQ TOTAL Withdrawals (log)", y = "Irrigation amount based on Deficit Irrigation Method\n (using max total Irrigated area) (log)") +
  scale_y_continuous(limits = c(-6, 12),  breaks = seq(-6, 12, by = 2))+
  scale_x_continuous(limits = c(-6, 12),  breaks = seq(-6, 12, by = 2))

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

ggsave(paste0(WUDR_github,"/plots/Coefficient1/Scatter_AgCenusus VS DeqTOT.png"), plot = p1, width = 9.5, height = 6, units = "in")
