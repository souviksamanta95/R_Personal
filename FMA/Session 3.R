setwd("D:/Analytics/R/FMA/Datasets")

# Session 3 - Date - 11/07/2020 - 10:15 am

rm(list=ls()) # Remove all variables from environment

data = read.csv("Nifty 50 Index.csv", header = T)
head(data)
tail(data)
nrow(data)
ncol(data)

price_nifty <- data[,2]

library(tseries)

adf.test(price_nifty)
# This is not stationary, so taking 1st difference i.e. log returns

# Estimating log returns
ret_nifty <- diff(log(price_nifty))

adf.test(ret_nifty)
# This comes out to be stationary as p value is less than 0.05

# Plot prices and returns
plot(price_nifty, type = "l")
plot(ret_nifty, type = "l")

# Statistical tests
library(fBasics)
basicStats(ret_nifty)

# Test for normality - Jrque Bera Test
jarque.bera.test(ret_nifty)
# P is less than 0.05 ----- reject H0 -- not normally distributed


# For ITC Data --

rm(list=ls()) # Remove all variables from environment

data = read.csv("ITC.csv", header = T)
head(data)
tail(data)
nrow(data)
ncol(data)

price_nifty <- data[,2]

library(tseries)

adf.test(price_nifty)
# This is not stationary, so taking 1st difference i.e. log returns

# Estimating log returns
ret_nifty <- diff(log(price_nifty))

adf.test(ret_nifty)
# This comes out to be stationary as p value is less than 0.05

# Plot prices and returns
plot(price_nifty, type = "l")
plot(ret_nifty, type = "l")

# Statistical tests
library(fBasics)
basicStats(ret_nifty)

# Test for normality - Jrque Bera Test
jarque.bera.test(ret_nifty)
# P is less than 0.05 ----- reject H0 -- not normally distributed

