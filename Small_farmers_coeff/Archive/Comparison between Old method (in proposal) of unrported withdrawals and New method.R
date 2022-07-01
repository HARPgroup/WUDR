WUDR_github<-"F:/My Drive/WUDR/WUDR_Github/WUDR"
setwd(WUDR_github)
load(paste0(WUDR_github,"/dat_load/SF_Coeff.RData"))
library(scales)
library(tidyverse)
options(scipen = 99999)

old_cal <- function(dat){
dat$Pct.under.TH.of.total.Irr.area <- (100*dat$Irr.Area.Under.TH)/dat$Total.Irri.Area
dat$Initial_Method1_Unreported <- ((dat$Facility_withdrawal_mg*100)/
                                                   (100-dat$Pct.under.TH.of.total.Irr.area))-dat$Facility_withdrawal_mg
dat <- dat[!is.infinite(dat$Initial_Method1_Unreported),]

return(dat)
}

DEQ_2002 <- old_cal(DEQ_2002)
DEQ_2007 <- old_cal(DEQ_2007)
DEQ_2012 <- old_cal(DEQ_2012)
DEQ_2017 <- old_cal(DEQ_2017)

DEQ_2002$Year <- 2002
DEQ_2007$Year <- 2007
DEQ_2012$Year <- 2012
DEQ_2017$Year <- 2017
plot_dat <- rbind.data.frame(DEQ_2002[,c(10,12,13)],DEQ_2007[,c(10,12,13)],DEQ_2012[,c(10,12,13)],DEQ_2017[,c(10,12,13)])




plot_dat$Initial_Method1_Unreported <- round(plot_dat$Initial_Method1_Unreported,2)
plot_dat$Year <- as.character(plot_dat$Year)

plot_dat <- plot_dat %>% 
  filter(Initial_Method1_Unreported >0)


p1 <- ggplot(plot_dat, aes(x=Initial_Method1_Unreported, y=Method1_Unreported, color=Year))+
  geom_point()+
 geom_abline(slope=1, intercept= 0 )+
   # geom_smooth(method=lm)+
  labs(title = "SF Unreported withdrawals (MG)",
       x="Using Initial Pct Area proportions (MG)", y = "Using Deficit Irrigtaion (MG)")+
scale_x_log10(breaks = c(0,0.01,0.1,1,5, 10,25, 50,100,200),
# #                    # labels =  ~ifelse(.x <= 1, label_number(accuracy = .0001)(.x), label_number(accuracy = 1)(.x)),
                     limits=c(NA,200))+
  
  scale_y_log10(breaks = c(0,0.01,0.1,1,5, 10,25, 50,100,200),
# #                      # labels =  ~ifelse(.x <= 1, label_number(accuracy = .0001)(.x), label_number(accuracy = 1)(.x)),
                       limits=c(NA,200))
  
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



ggsave(paste0(WUDR_github,"/plots/Coefficient1/Scatter_old_new_Method.png"), plot = p1, width = 12, height = 8, units = "in")

plot_dat$Ratio <- plot_dat$Method1_Unreported/plot_dat$Initial_Method1_Unreported
median(plot_dat$Ratio)
mean(plot_dat$Ratio)
