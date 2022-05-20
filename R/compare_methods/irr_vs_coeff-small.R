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
    select a.*, b.*
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
