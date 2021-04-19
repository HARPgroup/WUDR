WUDR_github<-"F:/My Drive/WUDR/WUDR_Github/WUDR_local"
setwd(WUDR_github)

pacman::p_load(dplyr, tidyverse, rgdal, tmap, sqldf,ggpubr,expss )
options(scipen=999)
YEAR= 2017
summary_dat<-read.csv(paste0(WUDR_github,"/csv_files/" , YEAR, "Underthreshold_Summary.csv"))
summary_dat <- summary_dat[, -c(1)]

load(paste0(WUDR_github,"/dat_load/VA_shapefile.RData"))
county.codes <- read_csv(paste0(WUDR_github, "/csv_files/county_codes_census.csv")) 

df.summary <- summary_dat 


df.summary <- merge.data.frame(df.summary, county.codes, by.x = "County", by.y = "County_Name")
df.summary$Irr.Area.above.TH <- ifelse(df.summary$Total.Irri.Area == 0, df.summary$Irr.Area.above.TH == "NA", df.summary$Irr.Area.above.TH )
df.summary$Irr.Area.Under.TH <- ifelse(df.summary$Total.Irri.Area == 0, df.summary$Irr.Area.Under.TH == "NA", df.summary$Irr.Area.Under.TH )


VA_counties@data$Countycode = str_remove(VA_counties@data$COUNTYFP, "^0+")
plotdat<-sp::merge(VA_counties,df.summary, by.x = "Countycode", by.y = "County_Code")

#Check if merge was correct
sum(df.summary$Irr.Area.above.TH, na.rm = TRUE)
sum(plotdat@data$Irr.Area.above.TH, na.rm=TRUE)

p1<-tm_shape(plotdat)+
  tm_polygons("Irr.Area.Under.TH", title = "Irrigated Area ",
              # breaks = c(0,1,5,10,20),
              n=5,style="jenks",
              id="NAMELSAD",
              legend.hist = TRUE)+
  tm_layout(main.title = paste0(2017," Irrigated Area under threshold  (acres)"),
            legend.outside = TRUE,
            legend.title.size = 1.2,
            legend.text.size = 0.8,
            legend.position = c("left","top"),
            legend.bg.alpha = 1)
p1

tmap_save(p1, paste0(WUDR_github,"/plots/Irr.Area.Under.TH.png"),  width = 10, height = 5, units = 'in')
tmap_save(p1, paste0(WUDR_github,"/plots/Irr.Area.Under.TH.html"),  width = 10, height = 5, units = 'in')

p2<-tm_shape(plotdat)+
  tm_polygons("Pct.under.TH.of.total.Irr.area", title = "Percentage Under threshold ",
              # breaks = c(0,1,5,10,20),
              n=5,style="jenks",
              id="NAMELSAD",
              legend.hist = TRUE)+
  tm_layout(main.title = paste0(2017," Percentage of total irrigated area under threshold "),
            legend.outside = TRUE,
            legend.title.size = 1.2,
            legend.text.size = 0.8,
            legend.position = c("left","top"),
            legend.bg.alpha = 1)
p2

tmap_save(p2, paste0(WUDR_github,"/plots/Pct.under.TH.of.total.Irr.area.png"),  width = 10, height = 5, units = 'in')
tmap_save(p2, paste0(WUDR_github,"/plots/Pct.under.TH.of.total.Irr.area.html"),  width = 10, height = 5, units = 'in')

p3<-tm_shape(plotdat)+
  tm_polygons("Pct.under.TH.of.Irri.area.abv.TH", title = "Percentage Under threshold ",
              breaks = c(0,42,125,210,380,515, Inf),
              labels = c('0-42','42-125','125-210','210-380', '380-515', 
                         'All Irrigated area under threshhold'),
              # n=5,
              id="NAMELSAD",
              legend.hist = TRUE)+
  tm_layout(main.title = paste0(2017," Percentage Irrigated area under threshold of irrigated area above threshold "),
            legend.outside = TRUE,
            legend.title.size = 1.2,
            legend.text.size = 0.8,
            legend.position = c("left","top"),
            legend.bg.alpha = 1)
# 
#   tm_add_legend(
#     labels = c('0-42','42-125','125-210','210-380', '380-515', 
#                'All Irrigated area under threshhold'))

p3

tmap_save(p3, paste0(WUDR_github,"/plots/Pct.under.TH.of.Irri.area.abv.TH.png"),  width = 10, height = 5, units = 'in')
tmap_save(p3, paste0(WUDR_github,"/plots/Pct.under.TH.of.total.Irr.area.html"),  width = 10, height = 5, units = 'in')

p6<-ggplot(data = df.summary, aes(x=Irr.Area.Under.TH))+
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")+
  labs(subtitle = paste0(YEAR, " Density plot for irrigated Area under threshold"), 
       x= "Area under threshold", y="Density")+
  theme_light()
p6 

##########################
load(paste0(WUDR_github,"/dat_load/VA_shapefile.RData"))
county.codes <- read_csv(paste0(WUDR_github, "/csv_files/county_codes_census.csv")) 





load_data <- function(YEAR){
summary_dat<-read.csv(paste0(WUDR_github,"/csv_files/" , YEAR, "Underthreshold_Summary.csv"))
summary_dat <- summary_dat[, -c(1)]

df.summary <- summary_dat 


df.summary$Irr.Area.above.TH <- ifelse(df.summary$Total.Irri.Area == 0, df.summary$Irr.Area.above.TH == "NA", df.summary$Irr.Area.above.TH )
df.summary$Irr.Area.Under.TH <- ifelse(df.summary$Total.Irri.Area == 0, df.summary$Irr.Area.Under.TH == "NA", df.summary$Irr.Area.Under.TH )

df.summary <- merge.data.frame(df.summary, county.codes, by.x = "County", by.y = "County_Name")

df.summary <<-df.summary[,c(1,4)]
}

dat_2017 <- load_data(2017)
dat_2012 <- load_data(2012)
dat_2007 <- load_data(2007)
dat_2002 <- load_data(2002)

dat <- merge.data.frame(dat_2002, dat_2007, by = "County", all.x = TRUE, all.y = TRUE)
dat <- merge.data.frame(dat, dat_2012, by = "County", all.x = TRUE,  all.y = TRUE)
dat <- merge.data.frame(dat, dat_2017, by = "County", all.x = TRUE,  all.y = TRUE)

colnames(dat) <- c("County", "Year 2002", "Year 2007", "Year 2012", "Year 2017")

dat_long <- pivot_longer(dat, cols = c( "Year 2002", "Year 2007", "Year 2012", "Year 2017"))
p <- ggplot(dat_long, aes(x=value,  fill=name),
       alpha=0.5, adjust=2) +
  geom_density()+
  labs(
    x = "Irrigated area under threshold",
    y = "Density",
    title ="Density plot for irrigated area under threshold"
  ) +
  facet_wrap(~name,  ncol=1)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Scenarios"))
p<-p+   theme_bw()+ 
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
p
  
ggsave(paste0(WUDR_github,"/plots/PDC.png"), plot = p, width = 5, height = 8, units = "in")
