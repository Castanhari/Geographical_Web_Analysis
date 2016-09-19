# Note: time_series is designed to work with the data set from chronos server (http://www.dpi.inpe.br/tws/wtss). It may not 
# work correctly with other data sets if their schema does not exactly match the schema with which this function is according

library(devtools)
library(wtss.R)
library(zoo)
library(lubridate)
library(bfast)
library(dtwSat)

AMOUNT_OF_SELECTED_ATTRIBUTES = 6 #TROCAR ISSO QUANDO O PLOT DO TWDTW ESTIVER FUNCIONANDO PARA QUALQUER QUANTIDADE DE ATRIBUTOS!!!

# Get time series attributes
get_attributes <- function(time_series)
{  time_series[[names(time_series)]]$attributes  }

# Apply BFAST 01
apply_bfast01 <- function(time_series)
{   # Time series in a ts object with all the original values
   ts_whole = ts(coredata(time_series[,1]), freq=365.25/(as.numeric(difftime(index(time_series[2]), index(time_series[1]), units="days"))), start=decimal_date(ymd(index(time_series[1]))))
   
   # Using BFAST for checking for one major break in the time series
   bfast01(ts_whole)
}

# Apply BFAST
apply_bfast <- function(time_series)
{  # Time series in a ts object without not available values
   ts_trim = ts(coredata(time_series[,1])[!is.na(coredata(time_series[,1]))], freq=365.25/(as.numeric(difftime(index(time_series[2]),index(time_series[1]),units = "days"))), start=decimal_date(ymd(index(time_series[1]))))
   
   # Using BFAST for an iterative break detection in seasonal and trend component of a time series
   bfast(ts_trim, max.iter=1)
}

# Apply BFAST monitor
apply_bfastmonitor <- function(time_series)
{  # Time series in a ts object with part of the original values
   ts_partial = ts(coredata(time_series[,1])[1:270], freq=365.25/(as.numeric(difftime(index(time_series[2]),index(time_series[1]),units = "days"))), start=decimal_date(ymd(index(time_series[1]))))
   
   # Using BFAST for monitoring disturbances in time series in near real-time
   bfastmonitor(ts_partial, start=time(ts_partial)[228], history=time(ts_partial)[1])
}

# Apply TWDTW
apply_twdtw <- function(time_series)
{  # Convert time series in a zoo object
   twdtw_ts = twdtwTimeSeries(time_series[,1:AMOUNT_OF_SELECTED_ATTRIBUTES])
   
   # In the region where the time serie was observed, we have soybean, cotton, and maize, whose typical temporal pattern
   pattern = twdtwTimeSeries(patterns.list)
   
   # Using the the temporal patterns to run the TWDTW analysis
   log_fun = logisticWeight(alpha=-0.1, beta=100) # Logistic time-weight
   twdtwApply(x=twdtw_ts, y=pattern, weight.fun=log_fun, keep=TRUE)
}

