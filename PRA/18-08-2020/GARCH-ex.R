library ("rugarch")
library ("MTS")
library ("forecast")
library ("tseries")
options (scipen=99999)
setwd("D:/Analytics/R/PRA/18-08-2020")
data <- read.csv("senex-oil-exrt.csv")
df<-data
nrow(df)
ncol(df)
head(df)
View(df)

tsdisplay(df$ex, lag=50)   #Graph, ACF, PACF

##Unit Root Tests: Trend -----------
k = trunc(length(df$ex)-1)^(1/3)  #k= number of lags
k

adf.test(df$ex, alternative="stationary", k=5)  #Trend Non-stationarity
adf.test(df$ex, alternative="stationary", k=10)

##Identification: First Difference/Trend Differencing

tsdisplay(diff(df$ex,1), lag=50)   #Graph, ACF, PACF
k = trunc((length(diff(df$ex,1))-1)^(1/3))
k
adf.test(diff(df$ex,1), alternative="stationary", k=5)  
adf.test(diff(df$ex,5), alternative="stationary", k=5) 

##Identification: SEASONAL: Seasonal Differencing
tsdisplay(diff(df$ex,5), lag=50)   #Graph, ACF, PACF

##Estimation
fit <- Arima(df$ex, order=c(1,0,0), seasonal = list(order = c(0, 1, 1), period=5))
summary(fit)

##Diagnostics
Box.test(residuals(fit), lag=5, fitdf=3, type="Ljung")
window()
b2<-tsdiag(fit)
b2

#ARIMA Forecasting
fit.forecast<-forecast(fit,h=20)
fit.forecast
plot(fit.forecast, type="l")
plot(fit$residuals, col="green")

#Normality Test
jarque.bera.test(fit$residuals)

#GARCH ESTIMATION
#archTest
archTest(fit$residuals, lag=30) #long-term
archTest(fit$residuals, lag=10) #medium-term
archTest(fit$residuals, lag=5) #short-term
archTest(fit$residuals, lag=2)
archTest(fit$residuals, lag=1)
archTest(fit$residuals, lag=15)
archTest(fit$residuals, lag=20)


