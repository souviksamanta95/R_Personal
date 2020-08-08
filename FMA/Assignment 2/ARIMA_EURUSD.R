rm(list=ls())
setwd("D:/Analytics/R/FMA/Datasets")
data=read.csv("EURUSD.csv",header = T)


# Replacing null values with last 5 day's average price
str(data)
data$Price <- as.numeric(data$Price)
null <- c(which(is.na(data[,2])))
for (i in null){
  data$Price[i] <- mean(c(data$Price[i-5],data$Price[i-4],data$Price[i-3],data$Price[i-2],data$Price[i-1]), na.rm = TRUE)
}
sum(is.na(data))

price_EURUSD = data[,2]
ret_EURUSD = diff(log(price_EURUSD))

l <- length(ret_EURUSD)
actual <- ret_EURUSD[c(seq((l-18),l))]
ret_EURUSD <- ret_EURUSD[-c(seq((l-18),l))]

# Test for autocorrelation
Box.test(ret_EURUSD,lag=10,type = "Ljung-Box")
library(forecast)

# AR Model
pacf(ret_EURUSD,10)   # 1,2,7,8 are significant
auto.arima(ret_EURUSD,max.p = 10,max.d = 0, max.q = 0, ic="aic")   # AR(2) - zero mean
auto.arima(ret_EURUSD,max.p = 10,max.d = 0, max.q = 0, ic="bic")   # AR(2) - zero mean

# MA Model
acf(ret_EURUSD,10)    # 1,7,9 are significant
auto.arima(ret_EURUSD,max.p = 0,max.d = 0, max.q = 10, ic="aic")   # MA(1) - zero mean
auto.arima(ret_EURUSD,max.p = 0,max.d = 0, max.q = 10, ic="bic")   # MA(1) - zero mean

# ARMA Model
auto.arima(ret_EURUSD,max.p = 10,max.d = 0, max.q = 10, ic="aic")   # ARMA(0,1) - zero mean
auto.arima(ret_EURUSD,max.p = 10,max.d = 0, max.q = 10, ic="bic")   # ARMA(0,1) - zero mean

# Model Estimation
model_1 <- arima(ret_EURUSD, order = c(8,0,0), fixed = c(NA,NA,0,0,0,0,NA,NA,0))
model_2 <- arima(ret_EURUSD, order = c(2,0,0), include.mean = FALSE)

model_3 <- arima(ret_EURUSD, order = c(0,0,9), fixed = c(NA,0,0,0,0,0,NA,0,NA,0))
model_4 <- arima(ret_EURUSD, order = c(0,0,1), include.mean = FALSE)

# Model diagnostics - 
Box.test(model_1$residuals, 10, type = "Ljung-Box")     # p > 0.05  ---> no correlation
Box.test(model_2$residuals, 10, type = "Ljung-Box")     # p < 0.05  ---> AUTO-correlation
Box.test(model_3$residuals, 10, type = "Ljung-Box")     # p > 0.05  ---> no correlation
Box.test(model_4$residuals, 10, type = "Ljung-Box")     # p < 0.05  ---> AUTO-correlation

# Forecasting - 
predict(model_1, 1)         #  
predict(model_3, 1)         #  

# Recursive Prediction - one week ahead
predict(model_1, 5)         # 
predict(model_3, 5)         # 

# Rolling window or moving window
l <- length(ret_EURUSD) - 19
pred1 <- 0
pred2 <- 0

for (i in 1:19){
  model_1_1 <- arima(ret_EURUSD[i:(l+i-1)], order = c(8,0,0), fixed = c(NA,NA,0,0,0,0,NA,NA,0))
  pre1 <- predict(model_1_1,1)
  pred1[i] <- pre1$pred[1]
}
for (i in 1:19){
  model_3_1 <- arima(ret_EURUSD[i:(l+i-1)], order = c(0,0,9), fixed = c(NA,0,0,0,0,0,NA,0,NA,0))
  pre1 <- predict(model_3_1,1)
  pred2[i] <- pre1$pred[1]
}

pred = data.frame(actual,pred1,pred2)
write.csv(pred, "pred_EURUSD.csv")
