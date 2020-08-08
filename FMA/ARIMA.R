### Session 5&6

## STEP - 1
# Model Identification

rm(list=ls()) 

setwd("D:/Analytics/R/FMA/Datasets")

data=read.csv("spindex.csv",header = T)

price_sp = data[,2]

ret_sp = diff(log(price_sp))

# Test for autocorrelation
Box.test(ret_sp,lag=10,type = "Ljung-Box")

library(forecast)

# AR Model
pacf(ret_sp,10)   # 1,2,5 are significant
auto.arima(ret_sp,max.p = 10,max.d = 0, max.q = 0, ic="aic")   # 1,2 are significant
auto.arima(ret_sp,max.p = 10,max.d = 0, max.q = 0, ic="bic")   # 1,2 are significant

# MA Model
acf(ret_sp,10)    # 1,2,3,5 are significant
auto.arima(ret_sp,max.p = 0,max.d = 0, max.q = 10, ic="aic")   # 1,2,3,4,5 are significant
auto.arima(ret_sp,max.p = 0,max.d = 0, max.q = 10, ic="bic")   # 1,2 are significant

# ARMA Model
auto.arima(ret_sp,max.p = 10,max.d = 0, max.q = 10, ic="aic")   # AR1,AR2,MA1 are significant
auto.arima(ret_sp,max.p = 10,max.d = 0, max.q = 10, ic="bic")   # AR1,AR2 are significant

### End of session 5&6


### Start of session 7&8 -------------------------------------------------------

## STEP - 2
# Model Estimation
model_1 <- arima(ret_sp, order = c(2,0,0), include.mean = FALSE)
# It will include ar1, ar2 and also indicating mean = 0

model_2 <- arima(ret_sp, order = c(5,0,0), fixed = c(NA,NA,0,0,NA,0))
# It will exclude ar3 and ar4; last one of "fixed" denotes mean which is already "0"
# Which AR values we want to estimate, will be processed for NA inside "fixed"

model_3 <- arima(ret_sp, order = c(0,0,5), fixed = c(NA,NA,NA,0,NA,0))
# It will take ma1, ma2, ma3, ma5, intercept or mean = 0

model_4 <- arima(ret_sp, order = c(0,0,5), include.mean = FALSE)
# It will take all ma1...ma5 with mean 0

model_5 <- arima(ret_sp, order = c(0,0,2), include.mean = FALSE)
# It will take ma1, ma2

model_6 <- arima(ret_sp, order = c(2,0,1), include.mean = FALSE)
# It will take ar1, ar2, ma1

# for ARMA(3,2) where lag2 = 0, it will be - 
# "order = c(3,0,2), fixed = c(NA,0,NA,NA,NA,0)"

## STEP - 3
# Model diagnostics - 

# Error term should not have auto-correlation, otherwise it will have
# some information which is not yet captured.
# We will test epsilon(t) or error term is not auto-correlated

Box.test(model_1$residuals, 10, type = "Ljung-Box")     # p > 0.05  ---> no correlation
Box.test(model_2$residuals, 10, type = "Ljung-Box")     # p > 0.05  ---> no correlation
Box.test(model_3$residuals, 10, type = "Ljung-Box")     # p > 0.05  ---> no correlation
Box.test(model_4$residuals, 10, type = "Ljung-Box")     # p > 0.05  ---> no correlation
Box.test(model_5$residuals, 10, type = "Ljung-Box")     # p > 0.05  ---> no correlation
Box.test(model_6$residuals, 10, type = "Ljung-Box")     # p > 0.05  ---> no correlation

# Whichever has p > 0.05, we will take all of them forward.

## STEP - 4
# Forecasting - 
# Recursive Prediction -
predict(model_1, 1)     # 1 means, one step ahead
# it gives return of positive 0.0000879
predict(model_2, 1)         # 0.00008489
predict(model_3, 1)         # 0.00007123
predict(model_4, 1)         # -0.0000618   (negative return)
predict(model_5, 1)         # 0.00005836
predict(model_6, 1)         # 0.00006504
# Where actual is positive


# Recursive Prediction - one week ahead
predict(model_1, 5)         # PNPPN     - P is Positive sign, N is negative.
predict(model_2, 5)         # PNPPN
predict(model_3, 5)         # PNPPN
predict(model_4, 5)         # NNPPN
predict(model_5, 5)         # PNPPN
predict(model_6, 5)         # PNPNN
# Where actual is PNPPN

## STEP - 5
# Forecast evaluation and back-testing - 
# Rolling window or moving window
l <- length(ret_sp) - 19
pred1 <- 0

for (i in 1:19){
  model_1_1 <- arima(ret_sp[i:(l+i-1)], order = c(2,0,0), include.mean = FALSE)
  pre1 <- predict(model_1_1,1)
  pred1[i] <- pre1$pred[1]
}

write.csv(pred1, "pred1_sp.csv")




