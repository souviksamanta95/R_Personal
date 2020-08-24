## Time Series ARIMA and MSARIMA Models in R: Box Jenkin's Methodology
rm(list=ls())
cat("\014")
dev.off()

install.packages("tseries")
install.packages("forecast")
install.packages("plm")
install.packages("Formula")
install.packages("tcltk")
install.packages("uroot")
install.packages("pdR")
install.packages("stats")
install.packages("TSA")
install.packages("readxl")
install.packages("aTSA")
install.packages("rms")

library(tseries)
library(forecast)
library(plm)
library(Formula)
library (usdm)
library(tcltk)
library(uroot)
library(pdR)
library(stats)
library(TSA)
library (aTSA)
library (rms)

options (scipen=99999)


data<-read.csv(file.choose(),header=TRUE)
df<-data
nrow(df)
ncol(df)
head(df)
View(df)


df_nohold<- df[1:4000,]; View(df_nohold);
df_hold<-df[4001:8000,]; View (df_hold);


#sum(is.null(df))
#sum(is.na(df))
#y1 <- ts(data.matrix(df$pm2.5), frequency=24)
#y1


tsdisplay(dt$pm2.5,lag = 50)
k<- trunc((length(dt$pm2.5)-1)^(1/3))
k
adf.test((dt$pm2.5), alternative="stationary", k=12)

#Trend Differencing
tsdisplay(diff(dt$pm2.5,1),lag=50)
k = trunc((length(diff((dt$pm2.5),1)-1)^(1/3)))
k
adf.test(diff((dt$pm2.5),1), alternative="stationary", k=12)


#Seasonal Differencing
tsdisplay(diff(dt$pm2.5,24),lag=24)


covariate_xmatrix <- matrix(c(df_nohold$Iws, df_nohold$DEWP, df_nohold$TEMP, df_nohold$PRES, df_nohold$Is, df_nohold$Ir),nrow=4000,ncol=6)
covariate_xmatrix_hold <-matrix(c(df_hold$Iws, df_hold$DEWP, df_hold$TEMP, df_hold$PRES, df_hold$Is, df_hold$Ir), nrow=4000,ncol=6)
covariate_xmatrix_hold


#Iws: Cumulated wind speed (m/s)
#DEWP: Dew Point 
#TEMP: Temperature 
#PRES: Pressure (hPa)
#Is: Cumulated hours of snow
#Ir: Cumulated hours of rain

##Multicollinearity Check amongst independent variables
vif(df_hold[c(7,8,9,11)])

##Estimation
fit_arimax<-Arima(df_nohold$pm2.5, order=c(1,0,0),seasonal = list(order = c(0,1,1), period = 24),xreg=covariate_xmatrix,method = "ML")
summary(fit_arimax)
windows()
tsdiag(fit_arimax)

#Forecast
f_hold<-forecast(fit_arimax,xreg = covariate_xmatrix_hold)
f_hold
plot(f_hold)
##MAPE
f_hold_MAPE<-((abs(df_hold$pm2.5-f$mean)/df_hold$pm2.5))*100
mean(f_hold_MAPE)




