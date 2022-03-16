#### DATA BASE
temp = c("DTR",
         "TN10p",
         "TN90p",
         "TNn",
         "TNx",
         "TX10p",
         "TX90p",
         "TXn",
         "TXx",
         "WSDI")


units_temp = c('%days', 'Celsius degrees', '%days','Celsius degrees','Celsius degrees','%days' ,'%days' , 'Celsius degrees', 'Celsius degrees', '%days' )

prec = c("CDD",
        "CWD",
        "PRCPTOT",
        "R10mm",
        "R20mm",
        "R95p",
        "R99p",
        "RX1day",
        "RX5day",
        "SDII")
                  

units_prec = c('days', 'days', 'mm', 'days', 'days', 'mm', 'mm', 'mm', 'mm', 'mm/day')


Index = c(temp, prec)

option = c(rep("Temperature", 10), rep("Precipitation",10))


text = c(
  "Annual mean difference between TX and TN",

  "Percentage of days when TN <10th percentile",

  "Percentage of days when TN >90th percentile % days",

  "Annual minimum value of daily minimum temperature",

  "Annual maximum value of daily minimum temp",

  "Percentage of days when TX <10th percentile",

  "Percentage of days when TN >90th percentile",

  "Annual minimum value of daily maximum temperature",

  "Annual maximum value of daily maximum temperature",

  "Annual count of days with at least 6 consecutive days when TX > 90th percentile",

  "Maximum number of consecutive days with daily rainfall < 1mm",

  "Maximum number of consecutive days with daily rainfall ≥ 1mm",

  "Annual total PRCP in wet days (RR >= 1mm)",

  "Annual count of days when precipitation >= 10mm",

  "Annual count of days when precipitation >= 20mm",

  "Annual total PRCP when RR >95th percentile",

  "Annual total PRCP when RR >99th percentile",

  "Annual maximum 1-day precipitation",

  "Annual maximum consecutive 5-day precipitation",

  "Annual total precipitation divided by the number of wet days in the year"
)


names_index = c(
                "Diurnal Temperatures range",
                "Cool Nights",
                "Warm Nights",
                "Min Tmin",
                "Max Tmin",
                "Cool Days",
                "Warm Days",
                "Min Tmax",
                "Max Tmax",
                "Warm Pell Duration Indicator",
                "Consecutive Dry Days", 
                "Consecutive Wet Days",
                "Annual Total Wet-Day Precipitation",
                "Number of heavy precipitation days",
                "Number of very heavy precipitation days",
                "Very Wet Days",
                "Extremely Wet Days",
                "Max 1-Day Precipitation Ammount",
                "Max 5-day Precipitation Amount",
                "Simple Daily Intesity Index")

# index_number = c(c(1:11) , c(1:10))
index_number = c(c(2:11) , c(1:10))

units = c(units_temp, units_prec)


data = data.frame(Index, option, text, names_index, index_number, units ,row.names = NULL, stringsAsFactors = FALSE)


data = data %>%  mutate(option, option2 = case_when(option == 'Temperature' ~ 'temp',
                                                    option == 'Precipitation' ~ 'prec'))


# Auxiliars
time = data.frame(c('year','bimonth', 'month'))
colnames(time) = 'period'
bimonth = c("DJF", "JA", "MA", "MJ", "SO")

bimonth_oficial = c('Dec-Jan-Feb', 'Jul-Aug', 'Mar-Apr', 'May-June', 'Sep-Oct')

month = c('jan', 'feb', 'mar', 'apr', 'may','jun', 'jul', 'ago', 'sep','oct', 'nov', 'dec')
month_oficial = c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December')
num_month = c(1:12)

number_months = data.frame(month , num_month, month_oficial)

date = data.frame(c(bimonth, month) ,c(bimonth_oficial,month_oficial), c(rep('bimonth', 5) , rep('month', 12)))
colnames(date) = c('when', 'when1' ,'period')

