rm(list=ls()) 
setwd("D:/Analytics/R/FMA/Datasets")
data=read.csv("Nifty 50 Index.csv",header = T)
price_NIFTY = data[,2]
ret_NIFTY = diff(log(price_NIFTY))
l <- length(ret_NIFTY)
actual <- ret_NIFTY[c(seq((l-18),l))]
ret_NIFTY <- ret_NIFTY[-c(seq((l-18),l))]

# Test for autocorrelation
Box.test(ret_NIFTY,lag=10,type = "Ljung-Box")

library(forecast)

# AR Model
pacf(ret_NIFTY,10)   # 1,3 are significant
auto.arima(ret_NIFTY,max.p = 10,max.d = 0, max.q = 0, ic="aic")   # AR(5)(1,3) - zero mean
auto.arima(ret_NIFTY,max.p = 10,max.d = 0, max.q = 0, ic="bic")   # AR(0) - zero mean

# MA Model
acf(ret_NIFTY,10)    # 1,3,4,5 are significant
auto.arima(ret_NIFTY,max.p = 0,max.d = 0, max.q = 10, ic="aic")   # MA(1) - zero mean
auto.arima(ret_NIFTY,max.p = 0,max.d = 0, max.q = 10, ic="bic")   # MA(0) - zero mean

# ARMA Model
auto.arima(ret_NIFTY,max.p = 10,max.d = 0, max.q = 10, ic="aic")   # ARMA(2,3) - zero mean
auto.arima(ret_NIFTY,max.p = 10,max.d = 0, max.q = 10, ic="bic")   # ARMA(0,0) - zero mean

# Model Estimation
model_1 <- arima(ret_NIFTY, order = c(3,0,0), fixed = c(NA,0,NA,0))
model_2 <- arima(ret_NIFTY, order = c(0,0,5), fixed = c(NA,0,NA,NA,NA,0))
model_3 <- arima(ret_NIFTY, order = c(0,0,1), include.mean = FALSE)

# Model diagnostics - 
Box.test(model_1$residuals, 10, type = "Ljung-Box")     # p > 0.05  ---> no correlation
Box.test(model_2$residuals, 10, type = "Ljung-Box")     # p > 0.05  ---> no correlation
Box.test(model_3$residuals, 10, type = "Ljung-Box")     # p < 0.05  ---> AUTOcorrelation

# Forecasting - 
predict(model_1, 1)         #  0.00084
predict(model_2, 1)         # -0.000157

# Recursive Prediction - one week ahead
predict(model_1, 5)         # PPPPP
predict(model_2, 5)         # NNPNN

# Rolling window or moving window
l <- length(ret_NIFTY) - 19
pred1 <- 0
pred2 <- 0

for (i in 1:19){
  model_1_1 <- arima(ret_NIFTY[i:(l+i-1)], order = c(3,0,0), fixed = c(NA,0,NA,0))
  pre1 <- predict(model_1_1,1)
  pred1[i] <- pre1$pred[1]
}
for (i in 1:19){
  model_2_1 <- arima(ret_NIFTY[i:(l+i-1)], order = c(0,0,5), fixed = c(NA,0,NA,NA,NA,0))
  pre1 <- predict(model_2_1,1)
  pred2[i] <- pre1$pred[1]
}

pred = data.frame(actual,pred1,pred2)
write.csv(pred, "pred_NIFTY.csv")



