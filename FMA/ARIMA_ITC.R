rm(list=ls()) 
setwd("D:/Analytics/R/FMA/Datasets")
data=read.csv("ITC.csv",header = T)
price_ITC = data[,2]

ret_ITC = diff(log(price_ITC))

# Test for autocorrelation
Box.test(ret_ITC,lag=10,type = "Ljung-Box")

library(forecast)

# AR Model
pacf(ret_ITC,10)   # 1,2,3,4 are significant
auto.arima(ret_ITC,max.p = 10,max.d = 0, max.q = 0, ic="aic")   # AR(4) - non-zero mean
auto.arima(ret_ITC,max.p = 10,max.d = 0, max.q = 0, ic="bic")   # AR(3) - zero mean

# MA Model
acf(ret_ITC,10)    # 1,2 are significant
auto.arima(ret_ITC,max.p = 0,max.d = 0, max.q = 10, ic="aic")   # MA(3) - non-zero mean
auto.arima(ret_ITC,max.p = 0,max.d = 0, max.q = 10, ic="bic")   # MA(2) - zero mean

# ARMA Model
auto.arima(ret_ITC,max.p = 10,max.d = 0, max.q = 10, ic="aic")   # ARMA(2,1) - non-zero mean
auto.arima(ret_ITC,max.p = 10,max.d = 0, max.q = 10, ic="bic")   # ARMA(1,1) - zero mean

# Model Estimation
model_1 <- arima(ret_ITC, order = c(4,0,0), include.mean = TRUE)
model_2 <- arima(ret_ITC, order = c(3,0,0), include.mean = FALSE)
model_3 <- arima(ret_ITC, order = c(0,0,3), include.mean = TRUE)
model_4 <- arima(ret_ITC, order = c(0,0,2), include.mean = FALSE)
model_5 <- arima(ret_ITC, order = c(2,0,1), include.mean = TRUE)
model_6 <- arima(ret_ITC, order = c(1,0,1), include.mean = FALSE)

# Model diagnostics - 
Box.test(model_1$residuals, 10, type = "Ljung-Box")     # p > 0.05  ---> no correlation
Box.test(model_2$residuals, 10, type = "Ljung-Box")     # p > 0.05  ---> no correlation
Box.test(model_3$residuals, 10, type = "Ljung-Box")     # p > 0.05  ---> no correlation
Box.test(model_4$residuals, 10, type = "Ljung-Box")     # p > 0.05  ---> no correlation
Box.test(model_5$residuals, 10, type = "Ljung-Box")     # p > 0.05  ---> no correlation
Box.test(model_6$residuals, 10, type = "Ljung-Box")     # p > 0.05  ---> no correlation

# Forecasting - 
predict(model_1, 1)         #  0.00084
predict(model_2, 1)         # -0.000157
predict(model_3, 1)         #  0.0008059
predict(model_4, 1)         # -0.00009905
predict(model_5, 1)         # 0.001168
predict(model_6, 1)         # 0.0002345

# Recursive Prediction - one week ahead
predict(model_1, 5)         # PPPPP
predict(model_2, 5)         # NNPNN
predict(model_3, 5)         # PPPPP
predict(model_4, 5)         # NPPPP
predict(model_5, 5)         # PPPPP
predict(model_6, 5)         # PPPPP

# Rolling window or moving window
l <- length(ret_ITC) - 19
pred1 <- 0

for (i in 1:19){
  model_1_1 <- arima(ret_ITC[i:(l+i-1)], order = c(2,0,0), include.mean = FALSE)
  pre1 <- predict(model_1_1,1)
  pred1[i] <- pre1$pred[1]
}
write.csv(pred1, "pred1_ITC.csv")




