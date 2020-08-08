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
#data <- read.csv("D:/Analytics/R/PRA/Datasets/Google.csv", header = T)
data <- read.csv("D:/Analytics/R/PRA/Datasets/sensex-oil-exrt.csv", header = T)

df<-data
nrow(df)
ncol(df)
head(df)
View(df)
#Plots: Graphs, ACF and PACF-------
##Identification: TREND: Level
tsdisplay(df$sensex, lag=50)   #stationary

##Unit Root Tests: Trend -----------
k = trunc(length(df$sensex)-1)^(1/3)
k

adf.test(df$sensex, alternative="stationary", k=4)  
adf.test(df$sensex, alternative="stationary", k=12)

##Identification: First Difference/Trend Differencing

tsdisplay(diff(df$sensex,1), lag=50)   #stationary
k = trunc((length(diff(df$sensex,1))-1)^(1/3))
k

adf.test(diff(df$sensex,1), alternative="stationary", k=5)  
adf.test(diff(df$sensex,4), alternative="stationary", k=8)  
