#load libraries and reading data
library(tseries)
library(forecast)
library(MASS)
#Plot and convert to ln format
rainfall <- read.csv("D:/rainfall1.csv")
print(rainfall)
lnrainfall=log(rainfall$Rainfall[1:312])
lnrainfall[which(!is.finite(lnrainfall))] <- 0
print(lnrainfall)
#Dickey-fuller test
adf.test(lnrainfall)
#Time series and seasonality
rainfalltimeseries <- ts(lnrainfall, frequency=12, start=c(1992))
print(rainfalltimeseries)
plot(rainfalltimeseries)
title("Rainfall")
components <- decompose(rainfalltimeseries)
 plot(components)
 #ARIMA Fitting
 fitlnrainfall<-auto.arima(rainfalltimeseries, trace=TRUE, test="kpss", ic="bic")
 fitlnrainfall
 confint(fitlnrainfall)
 exp(lnrainfall)
 #forecasting
 forecastedvalues_ln=forecast(fitlnrainfall,h=60)
forecastedvalues_ln
 plot(forecastedvalues_ln)
 forecastedvaluesextracted=as.numeric(forecastedvalues_ln$mean)
  finalforecastvalues=exp(forecastedvaluesextracted)
  finalforecastvalues
  #Ljung-Box
  Box.test(fitlnrainfall$resid, lag=20, type="Ljung-Box")
  Box.test(fitlnrainfall$resid, lag=25, type="Ljung-Box")
  Box.test(fitlnrainfall$resid, lag=15, type="Ljung-Box")
  #ACF,PACF
  acf(lnrainfall, lag.max=25)
  pacf(lnrainfall, lag.max=25)
  #percentage error
  df<-data.frame(rainfall$Rainfall[261:312],finalforecastvalues)
   col_headings<-c("Actual Weather","Forecasted Weather")
   names(df)<-col_headings
   attach(df)
  