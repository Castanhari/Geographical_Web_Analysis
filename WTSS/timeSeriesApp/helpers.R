# Note: time_series is designed to work with the data set from chronos server (http://www.dpi.inpe.br/tws/wtss). It may not 
# work correctly with other data sets if their schema does not exactly match the schema with which this function is according

library(wtss.R)
library(zoo)
library(lubridate)
library(bfast)
library(dtwSat)

#load("/home/osboxes/Desktop/Shiny/Apps/WTSS_v17/timeSeriesApp/yearly_12patterns_MT_Cerrado.RData")
load("/home/osboxes/Desktop/Shiny/Apps/WTSS_v17/timeSeriesApp/yearly_11_ndvi_evi_patterns_MT_Cerrado.RData")
#load("/home/shiny/shinyServerApps/components/timeSeriesApp/yearly_12patterns_MT_Cerrado.RData")
#load("/home/shiny/shinyServerApps/components/timeSeriesApp/yearly_11_ndvi_evi_patterns_MT_Cerrado.RData")

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
{  #signatures = yearly_patterns_mt
   signatures = yearly_patterns_MT_Cerrado
   
   # Remove time serires attributes not presented in the first class pattern
   keep = colnames(signatures@timeseries[[1]])
   time_series = time_series[, names(time_series) %in% keep, drop=FALSE]
   
   # Put time series and signatures into the same scale
   time_series = time_series*0.0001

   # Convert time series in a twdtwTimeSeries object if any attribute matches
   attrs = names(time_series)
   if(is.null(attrs))
   {  stop("There is no attribute matching within time series with dtwSat pattern!!")  }
   twdtw_ts = twdtwTimeSeries(time_series)
   
   # Build patterns according to the time series attributes
   patterns = lapply(
      signatures@timeseries,
      function(p)
      {  object_zoo = vector("list")
         dates = index(p)
         for(i in 1:length(attrs))
         {  object_zoo[[i]] = as.vector(coredata(p[,c(i)]))  }
         names(object_zoo) = attrs
         as.zoo(as.data.frame(object_zoo), dates)
      }
   )
   names(patterns) = names(signatures@timeseries)
   
   # Convert patterns in a twdtwTimeSeries object
   ts_patterns = twdtwTimeSeries(patterns)
   
   # Using the the temporal patterns to run the TWDTW analysis
   log_fun = logisticWeight(alpha=-0.1, beta=100) # Logistic time-weight
   twdtwApply(x=twdtw_ts, y=ts_patterns, weight.fun=log_fun, keep=TRUE)
}
