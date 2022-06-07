
WUDR_github<-"F:/My Drive/WUDR/WUDR_Github/WUDR"
setwd(WUDR_github)
load(paste0(WUDR_github,"/dat_load/PRECIP_2002-2019_daily.RData")) # Daily data 2002-2019

Start_date <- 4
End_date <- 10
PPT_Growing_Season <- PPT_VA %>% 
  subset(select = - Date) %>% 
  filter(between(Year, 2002,2018)) %>% 
  filter(between(Month, Start_date,End_date))

########################################################
# Total Rainfall

PPT_Growing_Season <- PPT_Growing_Season[, colSums(is.na(PPT_Growing_Season)) != nrow(PPT_Growing_Season)]

Monthly_PPT_Growing_Season <- PPT_Growing_Season %>% 
  group_by(Year, Month) %>% 
  summarise(across(everything(), sum))


Yearly_PPT_Growing_Season <- Monthly_PPT_Growing_Season %>% 
  subset(select = - Month) %>% 
  group_by(Year) %>% 
  summarise(across(everything(), sum))

Precip_dat <- t(Yearly_PPT_Growing_Season)
colnames(Precip_dat) <- Precip_dat[1,]

Precip_dat <- as.data.frame(Precip_dat[-c(1),])

counties <- c("Smyth","Roanoke", "Prince.Edward", "Sussex" ,"Alleghany", "Cumberland", "Northampton","Augusta","Caroline","Rappahannock","Loudoun","Shenandoah", "Essex")

Precip_dat$Names <- rownames(Precip_dat)

Precip_dat <- Precip_dat %>% 
  filter(Names %in% counties)
Precip_dat$Names <- NULL

Precip_Mean <- colMeans(Precip_dat)

Year  <- colnames(Precip_dat)
Precip_Mean <- rbind.data.frame(Year,Precip_Mean)
Precip_Mean <- t(Precip_Mean)
rownames(Precip_Mean) <- NULL
colnames(Precip_Mean) <- c("Year", "PPT")
Precip_Mean <- as.data.frame(Precip_Mean)

Precip_Mean$PPT <- as.numeric(Precip_Mean$PPT)
Precip_Mean$Year <- as.numeric(Precip_Mean$Year)

Precip_Mean$PPT <- as.numeric(Precip_Mean$PPT)/25.4

Precip_Mean$PPT <- round(Precip_Mean$PPT,1)

p<-ggplot(Precip_Mean, aes(x= Year, y=PPT)) +
  geom_line()+
  # geom_point()+
  labs(
    x = "Year",
    y = "Precip (in)",
    title = paste0("Precipitation in VA (" , month.abb[Start_date], "-", month.abb[End_date], ")"))+
  scale_x_continuous(breaks= seq(2002,2019,2))+
  scale_y_continuous(limits = c(0,45),breaks= seq(0,45,5))+
  geom_text(aes(label = PPT),position = position_dodge(width = 1),
            vjust = -0.5, size = 4)
#
p

p<-p+   theme_bw()+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position="none", 
        plot.title = element_text(color = "black", size = 22, face = "bold"),
        legend.text=element_text(size=22),
        axis.text=element_text(size=14, colour="black"),
        axis.title=element_text(size=16, colour="black"),
        axis.line = element_line(colour = "black", 
                                 size = 0.5, linetype = "solid"),
        axis.ticks = element_line(colour="black"),
        panel.grid.major=element_line(colour = "light grey"), 
        panel.grid.minor=element_blank(),
        strip.text = element_text(size = 14, color = "black" ),
        strip.background = element_rect(
          color="black", fill ="#FFE4C4", size=1, linetype="solid"))
p


###EFFECTIVE PRECIP

# Build loop for this 
Effective_ppt <- matrix(NA, nrow(PPT_Growing_Season), ncol(PPT_Growing_Season))
colnames(Effective_ppt) <- colnames(PPT_Growing_Season)
for (i in 3:ncol(Effective_ppt)) {
  c <-  as.data.frame(PPT_Growing_Season[,i]) %>%
    mutate(tot = lag(PPT_Growing_Season[,i], 1, default = 0) + PPT_Growing_Season[,i],
           runoff = case_when(tot >= 38.5 ~  tot-38.5,
                              tot <  38.5 ~ 0))
  c$runoff2 <- ifelse(c$`PPT_Growing_Season[, i]` == 0, 0, c$runoff)
  
  c$ppt <- c$`PPT_Growing_Season[, i]`-c$runoff2
  ppt <- ifelse(c$ppt < 0, 0, c$ppt)
  
  Effective_ppt[,i] <- ppt
  rm(c)
}

Effective_ppt[,1] <- PPT_Growing_Season[,1]
Effective_ppt[,2] <- PPT_Growing_Season[,2]


Effective_ppt <- as.data.frame(Effective_ppt)

Monthly_Effective_PPT_Growing_Season <- Effective_ppt %>% 
  group_by(Year, Month) %>% 
  summarise(across(everything(), sum))


Yearly_Effective_PPT_Growing_Season <- Monthly_Effective_PPT_Growing_Season %>% 
  subset(select = - Month) %>% 
  group_by(Year) %>% 
  summarise(across(everything(), sum))


Effective_PPT_Mean <- t(Yearly_Effective_PPT_Growing_Season)
colnames(Effective_PPT_Mean) <- Effective_PPT_Mean[1,]

Effective_PPT_Mean <- as.data.frame(Effective_PPT_Mean[-c(1),])

counties <- c("Smyth","Roanoke", "Prince.Edward", "Sussex" ,"Alleghany", "Cumberland", "Northampton","Augusta","Caroline","Rappahannock","Loudoun","Shenandoah", "Essex")

Effective_PPT_Mean$Names <- rownames(Effective_PPT_Mean)

Effective_PPT_Mean <- Effective_PPT_Mean %>% 
  filter(Names %in% counties)
Effective_PPT_Mean$Names <- NULL

Effective_PPT_Mean2 <- colMeans(Effective_PPT_Mean)

Year  <- colnames(Effective_PPT_Mean)
Effective_PPT_Mean2 <- rbind.data.frame(Year,Effective_PPT_Mean2)
Effective_PPT_Mean2 <- t(Effective_PPT_Mean2)
rownames(Effective_PPT_Mean2) <- NULL
colnames(Effective_PPT_Mean2) <- c("Year", "PPT")
Effective_PPT_Mean2 <- as.data.frame(Effective_PPT_Mean2)

Effective_PPT_Mean2$PPT <- as.numeric(Effective_PPT_Mean2$PPT)
Effective_PPT_Mean2$Year <- as.numeric(Effective_PPT_Mean2$Year)

Effective_PPT_Mean2$PPT <- as.numeric(Effective_PPT_Mean2$PPT)/25.4

Effective_PPT_Mean2$PPT <- round(Effective_PPT_Mean2$PPT,1)

# Effective_PPT_Mean2 <- Effective_PPT_Mean2[-18,]
p2<-ggplot(Effective_PPT_Mean2, aes(x= Year, y=PPT)) +
  geom_line()+
  # geom_point()+
  labs(
    x = "Year",
    y = "Precip (in)",
    title = paste0("Effective Precipitation in VA (" , month.abb[Start_date], "-", month.abb[End_date], ")"))+
  scale_x_continuous(breaks= seq(2002,2019,2))+
  scale_y_continuous(limits = c(0,45),breaks= seq(0,45,5))+
  geom_text(aes(label = PPT),position = position_dodge(width = 1),
            vjust = -0.5, size = 4)
#
p2

p2<-p2+   theme_bw()+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position="none", 
        plot.title = element_text(color = "black", size = 22, face = "bold"),
        legend.text=element_text(size=22),
        axis.text=element_text(size=14, colour="black"),
        axis.title=element_text(size=16, colour="black"),
        axis.line = element_line(colour = "black", 
                                 size = 0.5, linetype = "solid"),
        axis.ticks = element_line(colour="black"),
        panel.grid.major=element_line(colour = "light grey"), 
        panel.grid.minor=element_blank(),
        strip.text = element_text(size = 14, color = "black" ),
        strip.background = element_rect(
          color="black", fill ="#FFE4C4", size=1, linetype="solid"))
p2

figure <- ggarrange(p, p2, 
                    ncol = 1, nrow = 2)
figure

ggsave(paste0(WUDR_github,"/PRISM DATA/", "PPT" , Start_date, "_", End_date, ".png"),plot = figure,height = 8,width = 8,units = "in")
