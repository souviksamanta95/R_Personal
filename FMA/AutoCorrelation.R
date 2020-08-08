# Session 5&6 - 18/07/2020
# More stationarity tests

setwd("D:/Analytics/R/FMA/Datasets")
rm(list=ls()) # Remove all variables from environment

data = read.csv("Nifty 50 Index.csv", header = T)
head(data)
tail(data)
nrow(data)
ncol(data)
data[,2] <- as.numeric(data[,2])
str(data)

# Selecting Price removing missing values
price <- data[,2]

sum(is.na(price))

# ADF Test for stationarity  ---- LOW power test - good size property
library(tseries)
adf.test(price)
# Comes out to be stationary

# Phillips-Perron(PP) Test
pp.test(price)
# Comes out to be non stationary  ---- HIGH power test - small size property

# KPSS test 
kpss.test(price)


# Estimating log returns and check stationarity
ret <- diff(log(price))
adf.test(ret)

# Test for auto correlation - Ljung Box test
Box.test(ret)
# p < 0.05 ----> Reject H0, i.e. There is auto correlation >> Predictable


# Simulate a white noise with mean 0 and SD 1 ---> resembles Return
y <- rnorm(1000,0,1)
plot(y, type = "l")

# Tests on white noise (WN)
adf.test(y)               # p-value will be < 0.05  --> Stationary
jarque.bera.test(y)       # p-value will be > 0.05  --> Normal
Box.test(y)               # p-value will be > 0.05  --> No auto-correlation

# Random walk : a cumulative sum of white noise ---> resembles Price
x <- cumsum(y)
plot(x, type = "l")

# Tests on Random walk (RW)
adf.test(x)               # p-value will be > 0.05  --> Non stationary












