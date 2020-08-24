rm(list=ls()) 
setwd("D:/Analytics/R/FMA/Datasets")
data=read.csv("Power Grid.csv",header = T)


# Replacing null values with last 5 day's average price
str(data)
data$Price <- as.numeric(data$Price)
null <- c(which(is.na(data[,2])))
for (i in null){
  data$Price[i] <- mean(c(data$Price[i-5],data$Price[i-4],data$Price[i-3],data$Price[i-2],data$Price[i-1]), na.rm = TRUE)
}
sum(is.na(data))

price_Powergrid = data[,2]

ret_Powergrid = diff(log(price_Powergrid))

l <- length(ret_Powergrid)
actual <- ret_Powergrid[c(seq((l-18),l))]
ret_Powergrid <- ret_Powergrid[-c(seq((l-18),l))]

# Test for autocorrelation
Box.test(ret_Powergrid,lag=10,type = "Ljung-Box")

library(forecast)

#model_1 <- arima(ret_Powergrid, order = c(6,0,0), fixed = c(0,0,NA,NA,0,NA,0))
model_1 <- arima(ret_Powergrid, order = c(2,0,1), include.mean = FALSE)


#ARCH Test
library(FinTS)
arch=ArchTest(model_1$residuals,10)
arch
acf(model_1$residuals^2,10)

#GARCH Model Estimation
library(rugarch)
#GARCH MODEL
#Use the residual series

## sGARCH Model-----------------------------------------------------------------
spec1=ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                 mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
                 distribution.model = "norm")
garch_fit_1 = ugarchfit(spec = spec1, data = model_1$residuals)
garch_fit_1

spec1=ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                 mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
                 distribution.model = "std")
garch_fit_1 = ugarchfit(spec = spec1, data = model_1$residuals)
garch_fit_1

spec1=ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                 mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
                 distribution.model = "sstd")
garch_fit_1 = ugarchfit(spec = spec1, data = model_1$residuals)
garch_fit_1

## gjrGARCH Model-----------------------------------------------------------------
spec1=ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1)),
                 mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
                 distribution.model = "norm")
garch_fit_1 = ugarchfit(spec = spec1, data = model_1$residuals)
garch_fit_1

spec1=ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1)),
                 mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
                 distribution.model = "std")
garch_fit_1 = ugarchfit(spec = spec1, data = model_1$residuals)
garch_fit_1

spec1=ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1)),
                 mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
                 distribution.model = "sstd")
garch_fit_1 = ugarchfit(spec = spec1, data = model_1$residuals)
garch_fit_1

## eGARCH Model-----------------------------------------------------------------
spec1=ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1, 1)),
                 mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
                 distribution.model = "norm")
garch_fit_1 = ugarchfit(spec = spec1, data = model_1$residuals)
garch_fit_1

spec1=ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1, 1)),
                 mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
                 distribution.model = "std")
garch_fit_1 = ugarchfit(spec = spec1, data = model_1$residuals)
garch_fit_1

spec1=ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1, 1)),
                 mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
                 distribution.model = "sstd")
garch_fit_1 = ugarchfit(spec = spec1, data = model_1$residuals)
garch_fit_1
# ------------------------------------------------------------------------------

# Diagnostics
std_res_1 <- as.numeric(residuals(garch_fit_1, standardize=T))
ArchTest(std_res_1, lags = 10)
# no ARCH effect now.. that's good

# Recursive and Rolling window test -


