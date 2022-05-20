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
      b.PPT,
      Unreported_Irrigation_based / Facility_withdrawal_mg as yr_coeff
    from small_f_vwuds as a
    left outer join small_f_precip as b
    on (
      a.COUNTYFP = b.County_Code
      and a.Year = b.Year

    )
  "
)

cmp_data <- sqldf(
  "
    select a.*, b.med_coeff,
      b.med_coeff * a.Facility_withdrawal_mg as med_coeff_mg
    from cmp_data as a
    left outer join (
      select County_Name, median(yr_coeff) as med_coeff
      from cmp_data
      group by County_Name
    ) as b
    on (
      a.County_name = b.County_Name
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

# now do the median coefficient
plot(
  med_coeff_mg ~ Unreported_Irrigation_based,
  data=cmp_data,
  ylim=c(0,3000),
  xlim=c(0,3000)
)
irrlm <- lm(
  med_coeff_mg ~ Unreported_Irrigation_based,
  data=cmp_data
)
summary(irrlm)
abline(irrlm, col="red")


# cmp data that isreasonable, no > 1.0 coefficients
cmp_data_reas <- sqldf("select * from cmp_data where Mean_Coeff < 1.0")
plot(
  Unreported_Coeff_Based ~ Unreported_Irrigation_based,
  data=cmp_data_reas,
  ylim=c(0,500),
  xlim=c(0,500)
)
irrlm <- lm(
  Unreported_Coeff_Based ~ Unreported_Irrigation_based,
  data=cmp_data_reas
)
summary(irrlm)
abline(irrlm, col="red")

# cmp data that isreasonable, no > 1.0 coefficients
cmp_data_reas <- sqldf("select * from cmp_data where med_coeff < 1.0")
plot(
  med_coeff_mg ~ Unreported_Irrigation_based,
  data=cmp_data_reas,
  ylim=c(0,500),
  xlim=c(0,500)
)
irrlm <- lm(
  med_coeff_mg ~ Unreported_Irrigation_based,
  data=cmp_data_reas
)
summary(irrlm)
abline(irrlm, col="red")

# identify a couple outliers to ask about
test_case <- sqldf(
  "
  select *,
    Unreported_Irrigation_based / Facility_withdrawal_mg as yr_coeff
  from cmp_data
  where County_Name = 'LOUDOUN'
  and Year in (2002,2007,2012,2017)
  "
)
# If Loudoun has reported irrigation ave 23.2 mgy,
# and estimated irrigation on small acres
# how does the coefficient come out to > 100?
mean(test_case$Facility_withdrawal_mg)
mean(test_case$Unreported_Irrigation_based)
