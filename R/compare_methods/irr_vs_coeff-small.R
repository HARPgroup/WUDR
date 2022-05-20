library("sqldf")

small_f_vwuds = read.table(
  "https://raw.githubusercontent.com/HARPgroup/WUDR/main/Coefficient%20Tables/Timeseries_DEQ_Irr.csv"
  , header=TRUE, sep=","
)

small_f_precip = read.table(
  "https://raw.githubusercontent.com/HARPgroup/WUDR/main/Coefficient%20Tables/Timeseries_UnderTh_Deficit_Irr.csv"
  , header=TRUE, sep=","
)

cmp_data <- sqldf(
  "
    select a.County_Name,
      a.Year,
      a.Facility_withdrawal_mg,
      a.Mean_Coeff,
      a.Unreported_Coeff_Based,
      b.Unreported_Irrigation_based,
      b.Irrigation,
      b.PPT
    from small_f_vwuds as a
    left outer join small_f_precip as b
    on (
      a.COUNTYFP = b.County_Code
      and a.Year = b.Year

    )
  "
)

plot(
  Unreported_Coeff_Based ~ Unreported_Irrigation_based,
  data=cmp_data,
  ylim=c(0,3000),
  xlim=c(0,3000)
)
irrlm <- lm(
  Unreported_Coeff_Based ~ Unreported_Irrigation_based,
  data=cmp_data
)
summary(irrlm)
abline(irrlm, col="red")

# identify a couple outliers to ask about
test_case <- sqldf(
  "
  select * from cmp_data
  where County_Name = 'LOUDOUN'
  "
)
# If Loudoun has reported irrigation ave 23.2 mgy,
# and estimated irrigation on small acres
# how does the coefficient come out to > 100?
mean(test_case$Facility_withdrawal_mg)
mean(test_case$Unreported_Irrigation_based)
