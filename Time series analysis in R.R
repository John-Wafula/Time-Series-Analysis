#forecasting time series model for EC the next 48 months
#Forecasting EC
#store the time series data in  a time series object

null_ts<-ts(`null.(1)`$EC,start = c(2018),frequency = 365)
autoplot(null_ts)
#checking for stationarity
adf.test(null_ts,k=90)
#we can see that the p value is greater than the significance level of 1% its 0.6229
#null hypothesis-do not reject if p value is greater than 1%-our series is not stationary
# we perform a diffrencing on top of it
null_ts_df<-diff(null_ts,differences = 1)
adf.test(null_ts_df,k=90)
null_ts_df2<-diff(null_ts,differences = 2)
adf.test(null_ts_df2,k=90)
null_ts_df3<-diff(null_ts,differences = 3)
adf.test(null_ts_df3,k=90)

#the d term in the ARIMA(p,d,q)will be 1
autoplot(null_ts_df3)

#next we choose the p(AR or lag) term with PACF plot
pacf(null_ts_df3)

#our p-value would be 3
#next we choose our q or MA or moving average plot using ACF plot
#we use ACF
acf(null_ts_df3)
#our q value would be 6
#fitting the ARIMA model where since we have defined all the three factors
tsMod<-Arima(y=null_ts,order = c(3,0,6))
print(tsMod)
summary(tsMod)
forecast(tsMod,h=45)
#plotting the forecast time series
autoplot(forecast(tsMod,h=45))

