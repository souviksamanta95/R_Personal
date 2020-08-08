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



#Plots: Graphs, ACF and PACF-------
###GOOGLE SEARCH VOLUME------------------ This is a weekly data
data <- read.csv("D:/Analytics/R/PRA/Datasets/Google.csv", header = T)

df<-data
nrow(df)
ncol(df)
head(df)
View(df)
#Plots: Graphs, ACF and PACF-------
##Identification: TREND: Level
tsdisplay(df$SearchVolume, lag=50)   #stationary

##Unit Root Tests: Trend -----------
k = trunc(length(df$SearchVolume)-1)^(1/3)
k

adf.test(df$SearchVolume, alternative="stationary", k=4)  
adf.test(df$SearchVolume, alternative="stationary", k=12)

##Identification: First Difference/Trend Differencing

tsdisplay(diff(df$SearchVolume,1), lag=50)   #stationary
k = trunc((length(diff(df$SearchVolume,1))-1)^(1/3))
k
adf.test(diff(df$SearchVolume,1), alternative="stationary", k=5)  
adf.test(diff(df$SearchVolume,4), alternative="stationary", k=8)  

##Identification: SEASONAL: Seasonal Differencing
tsdisplay(diff(df$SearchVolume,4), lag=50)   #stationary

##Unit Root Tests: Seasonal-----------
y1 <- ts(data.matrix(df$SearchVolume), start = as.Date("2014-04-06"), end = as.Date ("2017-04-30"), frequency=7)
wts<-y1
is.ts(y1)
str(y1)
hegy.test(y1, deterministic = c(1,1,1), lag.method = "AIC", maxlag = 12)

dy1<-diff(y1,4); dy1
is.ts(dy1)
str(dy1)
hegy.test(dy1, deterministic = c(1,1,1), lag.method = "AIC", maxlag = 12)


##MONTHLY PEAK ELECTRICITY DEMAND
data<-read.csv(file.choose(),header=TRUE)
df<-data
nrow(df)
ncol(df)
head(df)
View(df)
#Plots: Graphs, ACF and PACF-------
##Identification: TREND: Level
tsdisplay(df$Demand, lag=50)   #stationary

##Unit Root Tests: Trend -----------
k = trunc(length(df$Demand)-1)^(1/3)
k

adf.test(df$Demand, alternative="stationary", k=4)  
adf.test(df$Demand, alternative="stationary", k=36)  
##If lshort is TRUE, then the truncation lag parameter is set to 
##trunc(4*(n/100)^0.25), otherwise trunc(12*(n/100)^0.25) is used.
pp.test(df$Demand, alternative="stationary", lshort=TRUE)

##Identification: First Difference/Trend Differencing
tsdisplay(diff(df$Demand,1), lag=50)   #stationary
k = trunc((length(diff(df$Demand,12))-1)^(1/3))
k
adf.test(diff(df$Demand,1), alternative="stationary", k=4)  
pp.test(diff(df$Demand,1), alternative="stationary", lshort=TRUE)
adf.test(diff(df$Demand,12), alternative="stationary", k=15)  
adf.test(diff(diff(df$Demand,12),1), alternative="stationary", k=4)  

##Identification: SEASONAL: Seasonal Differencing
tsdisplay(diff(df$Demand,12), lag=50)   #stationary

##Unit Root Tests: Seasonal-----------
y1 <- ts(data.matrix(df$Demand), start = as.Date("2000-04-01"), end = as.Date ("2007-02-01"), frequency=12)
y1
is.ts(y1)
str(y1)
hegy.test(y1, deterministic = c(1,0,0), lag.method = "BIC", maxlag = 4)
HEGY.TEST

dy1<-diff(y1,12); dy1
is.ts(dy1)
str(dy1)
hegy.test(dy1, deterministic = c(1,1,1), lag.method = "AIC", maxlag = 12)


##MONTHLY FOREIGN TOURIST DEMAND
data<-read.csv(file.choose(),header=TRUE)
df<-data
nrow(df)
ncol(df)
head(df)
View(df)
#Plots: Graphs, ACF and PACF-------
##Identification: TREND: Level
tsdisplay(df$FTD, lag=50)   #stationary

##Unit Root Tests: Trend -----------
k = trunc(length(df$FTD)-1)^(1/3)
k

adf.test(df$FTD, alternative="stationary", k=4)  
##If lshort is TRUE, then the truncation lag parameter is set to 
##trunc(4*(n/100)^0.25), otherwise trunc(12*(n/100)^0.25) is used.
pp.test(df$FTD, alternative="stationary", lshort=TRUE)
adf.test(df$FTD, alternative="stationary", k=12)  

##Identification: First Difference/Trend Differencing
tsdisplay(diff(df$FTD,1), lag=50)   #stationary
k = trunc((length(diff(df$FTD,1))-1)^(1/3))
k
adf.test(diff(df$FTD,1), alternative="stationary", k=4)  
pp.test(diff(df$FTD,1), alternative="stationary", lshort=TRUE)
adf.test(diff(df$FTD,12), alternative="stationary", k=12)  
adf.test(diff(diff(df$FTD,12),1), alternative="stationary", k=12)  

##Identification: SEASONAL: Seasonal Differencing
tsdisplay(diff(df$FTD,12), lag=50)   #stationary

tsdisplay(diff(diff(df$FTD,12),1), lag=50)   #stationary

##Unit Root Tests: Seasonal-----------
y1 <- ts(data.matrix(df$FTD), start = as.Date("1998-01-01"), end = as.Date ("2007-06-01"), frequency=12)
y1
is.ts(y1)
str(y1)
hegy.test(y1, deterministic = c(1,0,1), lag.method = "BIC", maxlag = 12)

dy1<-diff(y1,12); dy1
is.ts(dy1)
str(dy1)
hegy.test(dy1, deterministic = c(1,1,1), lag.method = "AIC", maxlag = 12)

######DAILY SENSEX PRICE-------------------
data<-read.csv(file.choose(),header=TRUE)
df<-data
nrow(df)
ncol(df)
head(df)
View(df)

#Plots: Graphs, ACF and PACF-------
##Identification: TREND: Level
tsdisplay(df$sensex, lag=50)   #stationary
tsdisplay(df$oil, lag=50)      #stationary
tsdisplay(df$ex, lag=50)       #stationary

##Unit Root Tests: Trend -----------
k = trunc(length(df$sensex)-1)^(1/3)
k

adf.test(df$sensex, alternative="stationary", k=5)  
##If lshort is TRUE, then the truncation lag parameter is set to 
##trunc(4*(n/100)^0.25), otherwise trunc(12*(n/100)^0.25) is used.
pp.test(df$sensex, alternative="stationary", lshort=TRUE)

##Identification: First Difference/Trend Differencing
tsdisplay(diff(df$sensex,1), lag=50)   #stationary
k = trunc((length(diff(df$sensex,1))-1)^(1/3))
k
adf.test(diff(df$sensex,1), alternative="stationary", k=5)  
pp.test(diff(df$sensex,1), alternative="stationary", lshort=TRUE)

##Identification: SEASONAL: Seasonal Differencing
tsdisplay(diff(df$sensex,5), lag=50)   #stationary

##Unit Root Tests: Seasonal-----------
y1 <- ts(data.matrix(df$sensex), start = as.Date("2008-03-03"), end = as.Date ("2008-11-28"), frequency=5)
y1
is.ts(y1)
str(y1)
hegy.test(y1, deterministic = c(1,1,1), lag.method = "BIC", maxlag = 20)

dy1<-diff(y1,5); dy1
is.ts(dy1)
str(dy1)
hegy.test(dy1, deterministic = c(1,1,1), lag.method = "AIC", maxlag = 12)


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
