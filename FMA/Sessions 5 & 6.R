rm(list=ls()) 

setwd("D:/Analytics/R/FMA/Datasets")

data=read.csv("Nifty 50 Index.csv",header = T)

price_n = data[,2]
ret_n = diff(log(price_n))

#All three tests of stationarity for the price series
library(tseries)
adf.test(price_sp)
pp.test(price_sp)
kpss.test(price_sp)

#Autocorrelation
Box.test(ret_n)


#Simulate a WN with mean 0 and SD 1
y <- rnorm(1000,0,1) #White Noise
plot(y,type="l")

adf.test(y)
jarque.bera.test(y)
Box.test(y)

#RW
x <- cumsum(y)
plot(x,type="l")

adf.test(x)








