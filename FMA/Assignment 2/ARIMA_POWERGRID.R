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

# AR Model
pacf(ret_Powergrid,10)   # 3,4,6 are significant
auto.arima(ret_Powergrid,max.p = 10,max.d = 0, max.q = 0, ic="aic")   # AR(4)(2,3,4) - zero mean
auto.arima(ret_Powergrid,max.p = 10,max.d = 0, max.q = 0, ic="bic")   # AR(4)(2,3,4) - zero mean

# MA Model
acf(ret_Powergrid,10)    # 2,3,4,6 are significant
auto.arima(ret_Powergrid,max.p = 0,max.d = 0, max.q = 10, ic="aic")   # MA(0) - zero mean
auto.arima(ret_Powergrid,max.p = 0,max.d = 0, max.q = 10, ic="bic")   # MA(0) - zero mean

# ARMA Model
auto.arima(ret_Powergrid,max.p = 10,max.d = 0, max.q = 10, ic="aic")   # ARMA(1,1) - zero mean
auto.arima(ret_Powergrid,max.p = 10,max.d = 0, max.q = 10, ic="bic")   # ARMA(2,1) - zero mean

# Model Estimation
model_1 <- arima(ret_Powergrid, order = c(6,0,0), fixed = c(0,0,NA,NA,0,NA,0))
model_2 <- arima(ret_Powergrid, order = c(4,0,0), fixed = c(0,NA,NA,NA,0))

model_3 <- arima(ret_Powergrid, order = c(1,0,1), include.mean = FALSE)
model_4 <- arima(ret_Powergrid, order = c(2,0,1), include.mean = FALSE)

# Model diagnostics - 
Box.test(model_1$residuals, 10, type = "Ljung-Box")     # p > 0.05  ---> no correlation
Box.test(model_2$residuals, 10, type = "Ljung-Box")     # p > 0.05  ---> no correlation
Box.test(model_3$residuals, 10, type = "Ljung-Box")     # p < 0.05  ---> AUTO-correlation
Box.test(model_4$residuals, 10, type = "Ljung-Box")     # p > 0.05  ---> no correlation

# Forecasting - 
predict(model_1, 1)         #  0.00084
predict(model_2, 1)         # -0.000157
predict(model_3, 1)         #  0.0008059
predict(model_4, 1)         # -0.00009905

# Recursive Prediction - one week ahead
predict(model_1, 5)         # PPPPP
predict(model_2, 5)         # NNPNN
predict(model_3, 5)         # PPPPP
predict(model_4, 5)         # NPPPP

# Rolling window or moving window
l <- length(ret_Powergrid) - 19
pred1 <- 0
pred2 <- 0
pred3 <- 0

for (i in 1:19){
  model_1_1 <- arima(ret_Powergrid[i:(l+i-1)], order = c(6,0,0), fixed = c(0,0,NA,NA,0,NA,0))
  pre1 <- predict(model_1_1,1)
  pred1[i] <- pre1$pred[1]
}
for (i in 1:19){
  model_2_1 <- arima(ret_Powergrid[i:(l+i-1)], order = c(4,0,0), fixed = c(0,NA,NA,NA,0))
  pre1 <- predict(model_2_1,1)
  pred2[i] <- pre1$pred[1]
}
for (i in 1:19){
  model_2_1 <- arima(ret_Powergrid[i:(l+i-1)], order = c(2,0,1), include.mean = FALSE)
  pre1 <- predict(model_2_1,1)
  pred3[i] <- pre1$pred[1]
}

pred = data.frame(actual,pred1,pred2,pred3)
write.csv(pred, "pred_Powergrid.csv")




