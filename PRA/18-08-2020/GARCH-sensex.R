library ("rugarch")
library ("MTS")
library ("forecast")
library ("tseries")
options (scipen=99999)
setwd("D:/Analytics/R/PRA/18-08-2020")
data <- read.csv("sensex-oil-exrt.csv")
df<-data
nrow(df)
ncol(df)
head(df)
View(df)

tsdisplay(df$sensex, lag=50)   #Graph, ACF, PACF

##Unit Root Tests: Trend -----------
k = trunc(length(df$sensex)-1)^(1/3)  #k= number of lags
k

adf.test(df$sensex, alternative="stationary", k=5)  #Trend Non-stationarity
adf.test(df$sensex, alternative="stationary", k=10)

##Identification: First Difference/Trend Differencing

tsdisplay(diff(df$sensex,1), lag=50)   #Graph, ACF, PACF
k = trunc((length(diff(df$sensex,1))-1)^(1/3))
k
adf.test(diff(df$sensex,1), alternative="stationary", k=5)  
adf.test(diff(df$sensex,5), alternative="stationary", k=5) 

##Identification: SEASONAL: Seasonal Differencing
tsdisplay(diff(df$sensex,5), lag=50)   #Graph, ACF, PACF

##Estimation
fit <- Arima(df$sensex, order=c(1,0,0), seasonal = list(order = c(0, 1, 1), period=5))
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

#GARCH Specification
ug_spec<-ugarchspec()
ug_spec  #Default
ug_spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1), 
                                            submodel = "GARCH", external.regressors = NULL, variance.targeting = FALSE), 
                      mean.model = list(armaOrder=c(0,0), include.mean = FALSE), 
                      distribution.model ="std")

# For SGARCH
ug_spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1), 
                                            submodel = "NULL", external.regressors = NULL, variance.targeting = FALSE), 
                      mean.model = list(armaOrder=c(0,0), include.mean = FALSE), 
                      distribution.model ="std")
ug_spec

?ugarchspec

#Fit GARCH models

ug_fit<-ugarchfit(spec=ug_spec, data=fit$residuals)
ug_fit


#What slots are available for @model in ug_fit
ug_fit@model
names(ug_fit@model)

#What slots are available for @fit in ug_fit
ug_fit@fit
names(ug_fit@fit)

coeff<-ug_fit@fit$coef; coeff  #coeff

#conditional variance
ug_var<-ug_fit@fit$var; ug_var<-data.frame(ug_var); ug_var
ug_svar<-data.frame(sqrt(ug_var));ug_svar

ug_cvar<-ug_fit@fit$cvar; ug_cvar<-data.frame(ug_cvar); ug_cvar

ug_condH<-ug_fit@fit$condH; ug_conH<-data.frame(ug_condH); ug_condH

ug_sigma<-ug_fit@fit$sigma; ug_sigma<-data.frame(ug_sigma); ug_sigma

fit.values<-ug_fit@fit$fitted.values

fit.values<-data.frame(fit.values);fit.values


#residuals
ug_res<-ug_fit@fit$residuals  
ug_res<-data.frame(ug_res); ug_res 

#residual square
ug_res2<-(ug_res)^2           
ug_res2<-data.frame(ug_res2); ug_res2

#sigma<-sqrt(ug_res2); sigma

#Squared Residuals and Estimated Conditional Variance 
plot(ug_res2,typ="l")
lines(ug_var,col = "red")

# GARCH Model Forecasting: res and sE(res)
ug_fore<-ugarchforecast(ug_fit,n.ahead=20)
ug_fore

names(ug_fore@model)
names(ug_fore@forecast)

#ug_mr<-ug_fore@forecast$seriesFor
#plot(ug_mr,type="l")
ug_vr<-ug_fore@forecast$sigmaFor;ug_vr
plot(ug_vr,type="l")
lines (ug_vr,col="green")

#Last 20 obs

ug_var_t <- c(tail(ug_var,20),rep(NA,10))
ug_var_t
ug_res2_t <- c(tail(ug_res2,20),rep(NA,10))
ug_res2_t
ug_fore2 <-c(rep(NA,20),(ug_fore@forecast$sigmaFor)^2)
ug_fore2

plot  (ug_res2_t,type="l")
lines (ug_fore2,col="orange")
lines (ug_var_t,col="green")





