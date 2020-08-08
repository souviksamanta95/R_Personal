setwd("D:/Analytics/R/FMA/Datasets")
rm(list=ls()) # Remove all variables from environment

data = read.csv("S&P 500 Index.csv", header = T)
head(data)
tail(data)
nrow(data)
ncol(data)
data[,2] <- as.numeric(data[,2])
str(data)

# Selecting Price removing missing values
null <- c(which(is.na(data[,2])))
price <- data[-null,2]
price <- data[,2]

sum(is.na(price))

# ADF Test for stationarity
library(tseries)
adf.test(price)

# Estimating log returns and check stationarity
ret <- diff(log(price))
adf.test(ret)

# Plot prices and returns
plot(price, type = "l")
plot(ret, type = "l")

# Statistical tests for Returns
library(fBasics)
res <- basicStats(ret)

write.csv(res, "out.csv")
# Test for normality - Jarque Bera Test
jarque.bera.test(ret)

