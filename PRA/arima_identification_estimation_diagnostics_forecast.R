     ## Time Series ARIMA and MSARIMA Models in R: Box Jenkin's Methodology
rm(list=ls())
cat("\014")

install.packages("tseries")
install.packages("forecast")
install.packages("plm")
install.packages("Formula")
install.packages("tcltk")
install.packages("uroot")
install.packages("pdR")
install.packages("stats")

library(tseries)
library(forecast)
library(plm)
library(Formula)
library(tcltk)
library(uroot)
library(pdR)
library(stats)
options (scipen=99999)

##data2=read.csv("C:/Users/kakali/Desktop/PA/ARIMA/CW/hourlydd.csv") # Read the file
##data3=read.csv("C:/Users/kakali/Desktop/PA/ARIMA/CW/sensex-oil-exrt.csv")# Read the file
##temp<-na.omit(df$Searchvolume)

##Identification Stage
#Plots: Graphs, ACF & PACF and Unit Root Tests

###GOOGLE SEARCH VOLUME WEEKLY
data<-read.csv(file.choose(),header=TRUE)
df<-data
nrow(df)
ncol(df)
head(df)
View(df)
#Plots: Graphs, ACF and PACF-------
##Identification: TREND: Level
windows()
tsdisplay(df$SearchVolume, lag=50)   #Graph, ACF, PACF

##Unit Root Tests: Trend -----------
k = trunc(length(df$SearchVolume)-1)^(1/3)  #k= number of lags
k

adf.test(df$SearchVolume, alternative="stationary", k=5)  #Trend Non-stationarity
adf.test(df$SearchVolume, alternative="stationary", k=12)

##Identification: First Difference/Trend Differencing

tsdisplay(diff(df$SearchVolume,1), lag=50)   #Graph, ACF, PACF
k = trunc((length(diff(df$SearchVolume,1))-1)^(1/3))
k
adf.test(diff(df$SearchVolume,1), alternative="stationary", k=5)  
adf.test(diff(df$SearchVolume,4), alternative="stationary", k=5)  

##Identification: SEASONAL: Seasonal Differencing
tsdisplay(diff(df$SearchVolume,4), lag=50)   #Graph, ACF, PACF

##Estimation
fit1 <- Arima(df$SearchVolume, order=c(0,0,1), seasonal = list(order = c(1, 1, 1), period=4))
fit1
summary(fit1)
fit2 <- Arima(df$SearchVolume, order=c(0,1,1), seasonal = list(order = c(1, 1, 1), period=4))
fit2
summary(fit2)

##Diagnostics
tsdisplay(res)
Box.test(residuals(fit1), lag=16, fitdf=4, type="Ljung")
b2<-tsdiag(fit1)
b2


##MONTHLY PEAK ELECTRICITY DEMAND
data<-read.csv(file.choose(),header=TRUE)
df<-data
nrow(df)
ncol(df)
head(df)
View(df)
#Plots: Graphs, ACF and PACF-------
##Identification: TREND: Level
windows()
tsdisplay(df$Demand, lag=50)   #Graph, ACF, PACF

##Unit Root Tests: Trend -----------
k = trunc(length(df$Demand)-1)^(1/3)
k

adf.test(df$Demand, alternative="stationary", k=4)  
adf.test(df$Demand, alternative="stationary", k=12)  

##Identification: First Difference/Trend Differencing
tsdisplay(diff(df$Demand,1), lag=50)   #Graph, ACF, PACF
k = trunc((length(diff(df$Demand,12))-1)^(1/3))
k
adf.test(diff(df$Demand,1), alternative="stationary", k=4)  
adf.test(diff(df$Demand,12), alternative="stationary", k=12)  
adf.test(diff(diff(df$Demand,12),1), alternative="stationary", k=4)  

##Identification: SEASONAL: Seasonal Differencing
tsdisplay(diff(df$Demand,12), lag=50)   #stationary

##Estimation
fit1 <- Arima(df$Demand, order=c(0,0,1), seasonal = list(order = c(1, 1, 1), period=12))
fit1
summary(fit1)
fit2 <- Arima(df$Demand, order=c(3,0,1), seasonal = list(order = c(0, 1, 1), period=12))
fit2
summary(fit2)

##Diagnostics
tsdisplay(res)
Box.test(residuals(fit1), lag=16, fitdf=4, type="Ljung")
b2<-tsdiag(fit1)
b2


##MONTHLY FOREIGN TOURIST DEMAND
data<-read.csv(file.choose(),header=TRUE)
df<-data
nrow(df)
ncol(df)
head(df)
View(df)
#Plots: Graphs, ACF and PACF-------
##Identification: TREND: Level
tsdisplay(df$FTD, lag=50)   #Graph, ACF, PACF

##Unit Root Tests: Trend -----------
k = trunc(length(df$FTD)-1)^(1/3)
k

adf.test(df$FTD, alternative="stationary", k=4)  
adf.test(df$FTD, alternative="stationary", k=12)  

##Identification: First Difference/Trend Differencing
tsdisplay(diff(df$FTD,1), lag=50)   #Graph, ACF, PACF
k = trunc((length(diff(df$FTD,1))-1)^(1/3))
k

adf.test(diff(df$FTD,1), alternative="stationary", k=4)  
adf.test(diff(df$FTD,12), alternative="stationary", k=24)  
adf.test(diff(diff(df$FTD,12),1), alternative="stationary", k=24)  

##Identification: SEASONAL: Seasonal Differencing
tsdisplay(diff(df$FTD,12), lag=50)   #Graph, ACF, PACF

tsdisplay(diff(diff(df$FTD,12),1), lag=50)   #stationary


######DAILY SENSEX PRICE-------------------
data<-read.csv(file.choose(),header=TRUE)
df<-data
nrow(df)
ncol(df)
head(df)
View(df)

#Plots: Graphs, ACF and PACF-------
##Identification: TREND: Level
tsdisplay(df$sensex, lag=50)   #Graph, ACF, PACF
tsdisplay(df$oil, lag=50)      #Graph, ACF, PACF
tsdisplay(df$ex, lag=50)       #Graph, ACF, PACF

##Unit Root Tests: Trend -----------
k = trunc(length(df$sensex)-1)^(1/3)
k

adf.test(df$sensex, alternative="stationary", k=6)  
adf.test(df$sensex, alternative="stationary", k=15)

##Identification: First Difference/Trend Differencing
tsdisplay(diff(df$sensex,1), lag=50)   #Graph, ACF, PACF
k = trunc((length(diff(df$sensex,1))-1)^(1/3))
k
adf.test(diff(df$sensex,1), alternative="stationary", k=5)  
adf.test(diff(df$sensex,5), alternative="stationary", k=5)  
adf.test(diff(diff(df$sensex,5),1), alternative="stationary", k=20)  

##Identification: SEASONAL: Seasonal Differencing
tsdisplay(diff(df$sensex,5), lag=50)          
tsdisplay(diff(diff(df$sensex,5),1),lag=50)   

###US Electricity Sales
data<-read.csv(file.choose(),header=TRUE)
df<-data
nrow(df)
ncol(df)
head(df)
View(df)
#Plots: Graphs, ACF and PACF-------
##Identification: TREND: Level
windows()
tsdisplay(df$Sales, lag=50)   #Graph, ACF, PACF

##Unit Root Tests: Trend -----------
k = trunc(length(df$Sales)-1)^(1/3)  #k= number of lags
k

adf.test(df$Sales, alternative="stationary", k=7)  #Trend Non-stationarity
adf.test(df$Sales, alternative="stationary", k=12)

##Identification: First Difference/Trend Differencing

#tsdisplay(diff(df$Sales,1), lag=50)   #Graph, ACF, PACF
k = trunc((length(diff(df$Sales,1))-1)^(1/3))
k
#adf.test(diff(df$Sales,1), alternative="stationary", k=6)  
adf.test(diff(df$Sales,12), alternative="stationary", k=12)  

##Identification: SEASONAL: Seasonal Differencing
tsdisplay(diff(diff(df$Sales,12),1), lag=50)   #Graph, ACF, PACF

##Estimation
fit1 <- Arima(df$Sales, order=c(1,1,1), seasonal = list(order = c(1, 1, 1), period=12))
fit1
summary(fit1)
fit2 <- Arima(df$searchvolume, order=c(3,0,1), seasonal = list(order = c(0, 1, 1), period=4))
fit2
summary(fit2)

##Diagnostics
tsdisplay(residuals(fit1))
Box.test(residuals(fit1), lag=5, fitdf=2, type="Ljung")
Box.test(residuals(fit1), lag=10, fitdf=2, type="Ljung")
Box.test(residuals(fit1), lag=20, fitdf=2, type="Ljung")
windows()
b2<-tsdiag(fit1)


##Simle Forecast
fcast.fit1 <- forecast(fit1, h=30)
fcast.fit1
plot(fcast.fit1)

##Out-of-Sample Forecast
hold <- window(ts(df$Sales), start=277); 
fit1_no_holdout <- Arima(ts(df$Sales[-c(277:288)]), order=c(1,1,1),  seasonal = list(order = c(1, 1, 1), period = 12))
fit1_fcast_no_holdout <- forecast(fit1_no_holdout,h=12)
plot(fit1_fcast_no_holdout, main=" ")
lines(ts(df$Sales))
summary(fit1_fcast_no_holdout)

##Out-of-sample forecast accuracy
fcst_hold<-((abs(hold-fit1_fcast_no_holdout$mean)/hold))*100
mean(fcst_hold)

##auto.arima
fit1.auto<-auto.arima(df$Sales, d=NA, D=NA, seasonal = TRUE)
fit1.auto
summary(fit1.auto)

##Diagnostics
res <- residuals(fit1.auto)
tsdisplay(res)
Box.test(res, lag=16, fitdf=5, type="Ljung")
windows()
b1<-tsdiag(fit1.auto)
b1

fcast.auto <- forecast(fit1.auto, h=30)
plot(fcast.auto)


###HOURLY ELECTRICITY DEMAND------------------

data<-read.csv(file.choose(),header=TRUE)
df<-data
nrow(df)
ncol(df)
head(df)
View(df)


#Plots: Graphs, ACF and PACF-------
##Identification: TREND: Level

tsdisplay(df$MW, lag=50)   #stationary

##Unit Root Tests: Trend -----------
k = trunc(length(df$MW)-1)^(1/3)
k

adf.test(df$MW, alternative="stationary", k=9)  
##If lshort is TRUE, then the truncation lag parameter is set to 
##trunc(4*(n/100)^0.25), otherwise trunc(12*(n/100)^0.25) is used.
pp.test(df$MW, alternative="stationary", lshort=TRUE)




fit1 <- Arima(df$MW, order=c(1,0,1), seasonal = list(order = c(1, 0 ,1), period=24))
fit1

summary(fit1)

##Diagnostics
tsdisplay(residuals(fit1))
Box.test(residuals(fit1), lag=5, fitdf=4, type="Ljung") # fitdf = no of parameters in model
Box.test(residuals(fit1), lag=10, fitdf=4, type="Ljung")
Box.test(residuals(fit1), lag=20, fitdf=4, type="Ljung")
windows()
b2<-tsdiag(fit1)
