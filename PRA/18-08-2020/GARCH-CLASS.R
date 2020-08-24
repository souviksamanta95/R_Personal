# install.packages("rugarch")
# install.packages("MTS")
# install.packages("forecast")
# install.packages("tseries")
library ("rugarch")
library ("MTS")
library ("forecast")
library ("tseries")

##ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1), 
##                                 submodel = NULL, external.regressors = NULL, variance.targeting = FALSE), 
##           mean.model = list(armaOrder = c(1, 1), include.mean = TRUE, archm = FALSE, 
##                             archpow = 1, arfima = FALSE, external.regressors = NULL, archex = FALSE), 
##           distribution.model = "norm", start.pars = list(), fixed.pars = list(), ...)

# a standard specification
#spec1 = ugarchspec()
#spec1
# an example which keep the ar1 and ma1 coefficients fixed:
#spec2 = ugarchspec(mean.model=list(armaOrder=c(2,2), 
#                                   fixed.pars=list(ar1=0.3,ma1=0.3)))
#spec2
# an example of the EWMA Model
#spec3 = ugarchspec(variance.model=list(model="iGARCH", garchOrder=c(1,1)), 
#                   mean.model=list(armaOrder=c(0,0), include.mean=TRUE),  
#                   distribution.model="norm", fixed.pars=list(omega=0))

#Import Data
options (scipen=99999)
#data=read.csv("C:/Users/kakali/Desktop/PA/GARCH/IEX.csv")         # Read the 
data<-read.csv(file.choose(),header=TRUE)
df<-data
nrow(df)
ncol(df)
head(df)
View(df)

##ARIMA
fit <- arima(df$sensex, order=c(1,0,1), seasonal = list(order = c(1, 1, 1), period=5))
fit <- arima(df$Price, order=c(2,0,0), seasonal = list(order = c(1, 1, 1), period=24))
summary(fit)
windows()
tsdiag(fit)


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

#GARCH Specification
ug_spec<-ugarchspec()
ug_spec  #Default
ug_spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1), 
                                            submodel = "GARCH", external.regressors = NULL, variance.targeting = FALSE), 
                      mean.model = list(armaOrder=c(0,0), include.mean = FALSE), 
                      distribution.model ="std")
ug_spec

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



