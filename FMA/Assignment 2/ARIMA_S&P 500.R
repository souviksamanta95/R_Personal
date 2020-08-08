rm(list=ls()) 
setwd("D:/Analytics/R/FMA/Datasets")
data=read.csv("S&P 500 Index.csv",header = T)

price_snp = data[,2]

ret_snp = diff(log(price_snp))
l <- length(ret_snp)
actual <- ret_snp[c(seq((l-18),l))]
ret_snp <- ret_snp[-c(seq((l-18),l))]

# Test for autocorrelation
Box.test(ret_snp,lag=10,type = "Ljung-Box")

library(forecast)

# AR Model
pacf(ret_snp,10)   # 1,4,6,7,9 are significant
auto.arima(ret_snp,max.p = 10,max.d = 0, max.q = 0, ic="aic")   # AR(1) - zero mean
auto.arima(ret_snp,max.p = 10,max.d = 0, max.q = 0, ic="bic")   # AR(1) - zero mean

# MA Model
acf(ret_snp,10)    # 1,7,9 are significant
auto.arima(ret_snp,max.p = 0,max.d = 0, max.q = 10, ic="aic")   # MA(1) - zero mean
auto.arima(ret_snp,max.p = 0,max.d = 0, max.q = 10, ic="bic")   # MA(1) - zero mean

# ARMA Model
auto.arima(ret_snp,max.p = 10,max.d = 0, max.q = 10, ic="aic")   # ARMA(2,1) - zero mean
auto.arima(ret_snp,max.p = 10,max.d = 0, max.q = 10, ic="bic")   # ARMA(1,0) - zero mean

# Model Estimation
model_1 <- arima(ret_snp, order = c(9,0,0), fixed = c(NA,0,0,NA,0,NA,0,0,NA,0))
model_2 <- arima(ret_snp, order = c(1,0,0), include.mean = FALSE)

model_3 <- arima(ret_snp, order = c(0,0,9), fixed = c(NA,0,0,0,0,0,NA,0,NA,0))
model_4 <- arima(ret_snp, order = c(0,0,1), include.mean = FALSE)

model_5 <- arima(ret_snp, order = c(2,0,1), include.mean = TRUE)
model_6 <- arima(ret_snp, order = c(1,0,0), include.mean = FALSE)

# Model diagnostics - 
Box.test(model_1$residuals, 10, type = "Ljung-Box")     # p > 0.05  ---> no correlation
Box.test(model_2$residuals, 10, type = "Ljung-Box")     # p < 0.05  ---> AUTO-correlation
Box.test(model_3$residuals, 10, type = "Ljung-Box")     # p > 0.05  ---> no correlation
Box.test(model_4$residuals, 10, type = "Ljung-Box")     # p < 0.05  ---> AUTO-correlation
Box.test(model_5$residuals, 10, type = "Ljung-Box")     # p < 0.05  ---> AUTO-correlation
Box.test(model_6$residuals, 10, type = "Ljung-Box")     # p < 0.05  ---> AUTO-correlation

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
l <- length(ret_snp) - 19
pred1 <- 0
pred2 <- 0

for (i in 1:19){
  model_1_1 <- arima(ret_snp[i:(l+i-1)], order = c(9,0,0), fixed = c(NA,0,0,NA,0,NA,0,0,NA,0))
  pre1 <- predict(model_1_1,1)
  pred1[i] <- pre1$pred[1]
}
for (i in 1:19){
  model_3_1 <- arima(ret_snp[i:(l+i-1)], order = c(0,0,9), fixed = c(NA,0,0,0,0,0,NA,0,NA,0))
  pre1 <- predict(model_3_1,1)
  pred2[i] <- pre1$pred[1]
}

pred = data.frame(actual,pred1,pred2)
write.csv(pred, "pred_snp.csv")




