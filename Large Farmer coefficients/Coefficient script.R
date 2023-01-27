WUDR_github<-"F:/My Drive/WUDR/WUDR_Github/WUDR"
setwd(WUDR_github)

library(tidyverse) 
library(purrr)
load(paste0(WUDR_github,"/dat_load/LF_Coeff_both.RData"))

Rep_Coeff <- map_df(Large_DEQ_IRR_Coeff, ~as.data.frame(.x), .id="Year")
Rep_Coeff$SF_Unreported_Coefficient <- Rep_Coeff$SM_F_Unreported/Rep_Coeff$Facility_withdrawal_mg


Tot_Coeff <- map_df(Large_DEQ_Tot_area, ~as.data.frame(.x), .id="Year")
Tot_Coeff_Tot <- Tot_Coeff %>% 
  filter(Facility_withdrawal_mg  <=0)
  

Tot_Coeff_Tot$SF_Unreported_Coefficient <- round(Tot_Coeff_Tot$SM_F_Unreported/Tot_Coeff_Tot$Facility_All_Withdrawl_mg,3)

write.csv(Rep_Coeff, "Unreported coefficients- DEQ reported.csv")
write.csv(Tot_Coeff_Tot, "Unreported coefficients- Total Withdrawals.csv")
