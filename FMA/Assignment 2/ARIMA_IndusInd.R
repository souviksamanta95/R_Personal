rm(list=ls()) 
setwd("D:/Analytics/R/FMA/Datasets")
data=read.csv("IndusInd.csv",header = T)
l <- length(data$Price)
data <- data[-c(seq((l-18),l)),]
price_IndusInd = data[,2]

ret_IndusInd = diff(log(price_IndusInd))

# Test for autocorrelation
Box.test(ret_IndusInd,lag=10,type = "Ljung-Box")

library(forecast)

# AR Model
pacf(ret_IndusInd,10)   # 1,2,4,5,7 are significant
auto.arima(ret_IndusInd,max.p = 10,max.d = 0, max.q = 0, ic="aic")   # AR(5)(1,2,4,5) - non-zero mean
auto.arima(ret_IndusInd,max.p = 10,max.d = 0, max.q = 0, ic="bic")   # AR(2) - zero mean

# MA Model
acf(ret_IndusInd,10)    # 1,2,4,5,7 are significant
auto.arima(ret_IndusInd,max.p = 0,max.d = 0, max.q = 10, ic="aic")   # MA(1) - non-zero mean
auto.arima(ret_IndusInd,max.p = 0,max.d = 0, max.q = 10, ic="bic")   # MA(1) - zero mean

# ARMA Model
auto.arima(ret_IndusInd,max.p = 10,max.d = 0, max.q = 10, ic="aic")   # ARMA(5,0) - non-zero mean
auto.arima(ret_IndusInd,max.p = 10,max.d = 0, max.q = 10, ic="bic")   # ARMA(2,0) - zero mean

# Model Estimation
model_1 <- arima(ret_IndusInd, order = c(7,0,0), fixed = c(NA,NA,0,NA,NA,0,NA,0))
model_2 <- arima(ret_IndusInd, order = c(5,0,0), include.mean = TRUE)
model_3 <- arima(ret_IndusInd, order = c(2,0,0), include.mean = FALSE)

model_4 <- arima(ret_IndusInd, order = c(0,0,1), include.mean = TRUE)
model_5 <- arima(ret_IndusInd, order = c(0,0,1), include.mean = FALSE)

# Model diagnostics - 
Box.test(model_1$residuals, 10, type = "Ljung-Box")     # p > 0.05  ---> no correlation
Box.test(model_2$residuals, 10, type = "Ljung-Box")     # p > 0.05  ---> no correlation
Box.test(model_3$residuals, 10, type = "Ljung-Box")     # p < 0.05  ---> AUTO-correlation
Box.test(model_4$residuals, 10, type = "Ljung-Box")     # p < 0.05  ---> AUTO-correlation
Box.test(model_5$residuals, 10, type = "Ljung-Box")     # p < 0.05  ---> AUTO-correlation

# Forecasting - 
predict(model_1, 1)         #  0.00084
predict(model_2, 1)         # -0.000157

# Recursive Prediction - one week ahead
predict(model_1, 5)         # PPPPP
predict(model_2, 5)         # NNPNN

# Rolling window or moving window
l <- length(ret_IndusInd) - 19
pred1 <- 0

for (i in 1:19){
  model_1_1 <- arima(ret_IndusInd[i:(l+i-1)], order = c(2,0,0), include.mean = FALSE)
  pre1 <- predict(model_1_1,1)
  pred1[i] <- pre1$pred[1]
}
write.csv(pred1, "pred1_IndusInd.csv")




