rm(list=ls()) 
setwd("D:/Analytics/R/FMA/Datasets")
data=read.csv("USDINR.csv",header = T)

# Replacing null values with last 5 day's average price
str(data)
data$Price <- as.numeric(data$Price)
null <- c(which(is.na(data[,2])))
for (i in null){
  data$Price[i] <- mean(c(data$Price[i-5],data$Price[i-4],data$Price[i-3],data$Price[i-2],data$Price[i-1]), na.rm = TRUE)
}
sum(is.na(data))

price_USDINR = data[,2]

ret_USDINR = diff(log(price_USDINR))
l <- length(ret_USDINR)
actual <- ret_USDINR[c(seq((l-18),l))]
ret_USDINR <- ret_USDINR[-c(seq((l-18),l))]

# Test for autocorrelation
Box.test(ret_USDINR,lag=10,type = "Ljung-Box")

library(forecast)

# AR Model
pacf(ret_USDINR,10)   # 1,5 are significant
auto.arima(ret_USDINR,max.p = 10,max.d = 0, max.q = 0, ic="aic")   # AR(1) - non-zero mean
auto.arima(ret_USDINR,max.p = 10,max.d = 0, max.q = 0, ic="bic")   # AR(1) - zero mean

# MA Model
acf(ret_USDINR,10)    # 1,2,5 are significant
auto.arima(ret_USDINR,max.p = 0,max.d = 0, max.q = 10, ic="aic")   # MA(2) - non-zero mean
auto.arima(ret_USDINR,max.p = 0,max.d = 0, max.q = 10, ic="bic")   # MA(1) - zero mean

# ARMA Model
auto.arima(ret_USDINR,max.p = 10,max.d = 0, max.q = 10, ic="aic")   # ARMA(2,1) - non-zero mean
auto.arima(ret_USDINR,max.p = 10,max.d = 0, max.q = 10, ic="bic")   # ARMA(1,0) - zero mean

# Model Estimation
model_1 <- arima(ret_USDINR, order = c(5,0,0), fixed = c(NA,0,0,0,NA,0))
model_2 <- arima(ret_USDINR, order = c(1,0,0), include.mean = TRUE)
model_3 <- arima(ret_USDINR, order = c(1,0,0), include.mean = FALSE)

model_4 <- arima(ret_USDINR, order = c(0,0,5), fixed = c(NA,NA,0,0,NA,0))
model_5 <- arima(ret_USDINR, order = c(0,0,2), include.mean = TRUE)
model_6 <- arima(ret_USDINR, order = c(0,0,1), include.mean = FALSE)

model_7 <- arima(ret_USDINR, order = c(2,0,1), include.mean = TRUE)


# Model diagnostics - 
Box.test(model_1$residuals, 10, type = "Ljung-Box")     # p > 0.05  ---> no correlation
Box.test(model_2$residuals, 10, type = "Ljung-Box")     # p > 0.05  ---> no correlation
Box.test(model_3$residuals, 10, type = "Ljung-Box")     # p > 0.05  ---> no correlation
Box.test(model_4$residuals, 10, type = "Ljung-Box")     # p > 0.05  ---> no correlation
Box.test(model_5$residuals, 10, type = "Ljung-Box")     # p > 0.05  ---> no correlation
Box.test(model_6$residuals, 10, type = "Ljung-Box")     # p < 0.05  ---> AUTO-correlation
Box.test(model_7$residuals, 10, type = "Ljung-Box")     # p > 0.05  ---> no correlation

# Forecasting - 
predict(model_1, 1)         #  0.00084
predict(model_2, 1)         # -0.000157
predict(model_3, 1)         #  0.0008059
predict(model_4, 1)         # -0.00009905
predict(model_5, 1)         # 0.001168
predict(model_6, 1)         # 0.0002345
predict(model_7, 1)         # 0.0002345

# Recursive Prediction - one week ahead
predict(model_1, 5)         # PPPPP
predict(model_2, 5)         # NNPNN
predict(model_3, 5)         # PPPPP
predict(model_4, 5)         # NPPPP
predict(model_5, 5)         # PPPPP
predict(model_6, 5)         # PPPPP
predict(model_7, 5)         # PPPPP

# Rolling window or moving window
l <- length(ret_USDINR) - 19
pred1 <- 0
pred2 <- 0
pred3 <- 0
pred4 <- 0
pred5 <- 0
pred6 <- 0

for (i in 1:19){
  model_1_1 <- arima(ret_USDINR[i:(l+i-1)], order = c(5,0,0), fixed = c(NA,0,0,0,NA,0))
  pre1 <- predict(model_1_1,1)
  pred1[i] <- pre1$pred[1]
}
for (i in 1:19){
  model_3_1 <- arima(ret_USDINR[i:(l+i-1)], order = c(1,0,0), include.mean = TRUE)
  pre1 <- predict(model_3_1,1)
  pred2[i] <- pre1$pred[1]
}
for (i in 1:19){
  model_1_1 <- arima(ret_USDINR[i:(l+i-1)], order = c(1,0,0), include.mean = FALSE)
  pre1 <- predict(model_1_1,1)
  pred3[i] <- pre1$pred[1]
}
for (i in 1:19){
  model_3_1 <- arima(ret_USDINR[i:(l+i-1)], order = c(0,0,5), fixed = c(NA,NA,0,0,NA,0))
  pre1 <- predict(model_3_1,1)
  pred4[i] <- pre1$pred[1]
}
for (i in 1:19){
  model_1_1 <- arima(ret_USDINR[i:(l+i-1)], order = c(0,0,2), include.mean = TRUE)
  pre1 <- predict(model_1_1,1)
  pred5[i] <- pre1$pred[1]
}
for (i in 1:19){
  model_3_1 <- arima(ret_USDINR[i:(l+i-1)], order = c(2,0,1), include.mean = TRUE)
  pre1 <- predict(model_3_1,1)
  pred6[i] <- pre1$pred[1]
}


pred = data.frame(actual,pred1,pred2,pred3,pred4,pred5,pred6)
write.csv(pred, "pred_USDINR.csv")




