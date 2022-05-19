# Get PRISM data for all years from June to August

load(paste0(WUDR_github,"/dat_load/PPT_foundational_datset.RData")) 

 PPT_Growing_Season <- PPT_VA %>% 
  filter(between(Month, 6,8))

PPT_Growing_Season <- PPT_Growing_Season[, colSums(is.na(PPT_Growing_Season)) != nrow(PPT_Growing_Season)]

# Build loop for this 
ppt_df <- matrix(NA, nrow(PPT_Growing_Season), ncol(PPT_Growing_Season))
colnames(ppt_df) <- colnames(PPT_Growing_Season)


 for (i in 3:ncol(ppt_df)) {
   c <-  as.data.frame(PPT_Growing_Season[,i]) %>%
     mutate(tot = lag(PPT_Growing_Season[,i], 1, default = 0) + PPT_Growing_Season[,i],
            runoff = case_when(tot >= 38.5 ~  tot-38.5,
                               tot <  38.5 ~ 0))
   c$runoff2 <- ifelse(c$`PPT_Growing_Season[, i]` == 0, 0, c$runoff)
   
   c$ppt <- c$`PPT_Growing_Season[, i]`-c$runoff2
   ppt <- ifelse(c$ppt < 0, 0, c$ppt)
   
   ppt_df[,i] <- ppt
   rm(c)
 }

ppt_df[,1] <- PPT_Growing_Season[,1]
ppt_df[,2] <- PPT_Growing_Season[,2]

ppt_df <- as.data.frame(ppt_df)

Monthly_PPT_Growing_Season <- ppt_df %>% 
  group_by(Year, Month) %>% 
  summarise_each(funs(sum))


Yearly_PPT_Growing_Season <- Monthly_PPT_Growing_Season %>% 
  subset(select = - Month) %>% 
  group_by(Year) %>% 
  summarise_each(funs(sum))

ldata <-  pivot_longer(Yearly_PPT_Growing_Season, cols = (2:ncol(Yearly_PPT_Growing_Season)))
# ldata <- ldata[,-c(1)]
ldata <- ldata[complete.cases(ldata), ]
ldata$name <- toupper(ldata$name)
ldata$name <-gsub(".", " ", ldata$name , fixed=TRUE)
ldata$name <-gsub("1", "", ldata$name , fixed=TRUE)
ldata$name[ldata$name == "SUFFOLK"] <- "SUFFOLK CITY"
ldata$name[ldata$name == "VIRGINIA BEACH"] <- "VIRGINIA BEACH CITY"
ldata$name[ldata$name == "CHESAPEAKE"] <- "CHESAPEAKE CITY"
ldata$name[ldata$name == "FAIRFAX "] <- "FAIRFAX"
colnames(ldata)[3] <- "PPT"
ldata$PPT <- round(ldata$PPT, 2)
ppt_list_yearly <- split( ldata , f = ldata$Year)

save(ppt_list_yearly, file = paste0(WUDR_github,"/dat_load/PRECIP_JUNE_AUGUST_ALL_YEARS.RData"))