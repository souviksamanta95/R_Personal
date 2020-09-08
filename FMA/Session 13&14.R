rm(list=ls()) 
setwd("C:\\Users\\faisal\\Desktop\\TEACHING\\Financial Market Analytics")
data=read.csv("spindex.csv",header = T)
price_sp = data[,2]
ret_sp = diff(log(price_sp))
Box.test(ret_sp,lag=10,type = "Ljung-Box")
library(forecast)

#Model Identification - Already Done

#Model Estimation
model_1=arima(ret_sp,order=c(5,0,0),fixed=c(NA,NA,0,0,NA,0))

#Model Diagnostics
Box.test(model_1$residuals,10,type="Ljung-Box")

model_1$residuals

#ARCH Test
library(FinTS)
arch=ArchTest(model_1$residuals,10)

#If there is autocorrelation in the return series, perform ARCH test as following
arch2=ArchTest(ret_sp,10)

#GARCH Model Estimation
library(rugarch)
#GARCH MODEL
#Use the residual series
spec1=ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                 mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
                 distribution.model = "norm")
garch_fit_1 = ugarchfit(spec = spec1, data = model_1$residuals)
garch_fit_1

#Use the return series
spec10=ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                 mean.model = list(armaOrder = c(5, 0), include.mean = FALSE),
                 distribution.model = "norm")
garch_fit_10 = ugarchfit(spec = spec10, data = ret_sp)
garch_fit_10

#Change the distribution (t-distribution)
spec2=ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                 mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
                 distribution.model = "std")
garch_fit_2 = ugarchfit(spec = spec2, data = model_1$residuals)
garch_fit_2

#Change the distribution (skewed t-distribution)
spec3=ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                 mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
                 distribution.model = "sstd")
garch_fit_3 = ugarchfit(spec = spec3, data = model_1$residuals)
garch_fit_3

#eGARCH MODEL
#Use the residual series
spec4=ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1, 1)),
                 mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
                 distribution.model = "norm")
garch_fit_4 = ugarchfit(spec = spec4, data = model_1$residuals)
garch_fit_4

#Change the distribution (t-distribution)
spec5=ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1, 1)),
                 mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
                 distribution.model = "std")
garch_fit_5 = ugarchfit(spec = spec5, data = model_1$residuals)
garch_fit_5

#Change the distribution (skew t)
spec6=ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1, 1)),
                 mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
                 distribution.model = "sstd")
garch_fit_6 = ugarchfit(spec = spec6, data = model_1$residuals)
garch_fit_6

#gjrGARCH MODEL
#Use the residual series
spec7=ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1)),
                 mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
                 distribution.model = "norm")
garch_fit_7 = ugarchfit(spec = spec7, data = model_1$residuals)
garch_fit_7

#Change the distribution (t-distribution)
spec8=ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1)),
                 mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
                 distribution.model = "std")
garch_fit_8 = ugarchfit(spec = spec8, data = model_1$residuals)
garch_fit_8

#Change the distribution (sstd)
spec9=ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1)),
                 mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
                 distribution.model = "sstd")
garch_fit_9 = ugarchfit(spec = spec9, data = model_1$residuals)
garch_fit_9


#Test if the GARCH Model is a good fit or not
std_res_1=as.numeric(residuals(garch_fit_1,standardize=TRUE))
ArchTest(std_res_1,lags = 10)

#Test it for all the 9 models
std_res_2=as.numeric(residuals(garch_fit_2,standardize=TRUE))
ArchTest(std_res_2,lags = 10)


#Recursive Prediction
forc_1 = ugarchforecast(garch_fit_1, n.ahead=19)
forc_1

#Rolling Window - Model return separately and Volatility separately
l=length(model_1$residuals)-19
pred=0
for(i in 1:19){
  spec1=ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                   mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
                   distribution.model = "norm")
  garch_fit_1 = ugarchfit(spec = spec1, data = model_1$residuals[i:(l+i-1)])
  forc_1 = ugarchforecast(garch_fit_1, n.ahead=1)
  pred[i]=forc_1@forecast$sigmaFor[[1]]
}
write.csv(pred,"Pred_sGARCH_1.csv")

#Rolling Window - Model return and volatility together
l=length(ret_sp)-19
pred1=0
pred2=0
for(i in 1:19){
  spec2=ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                   mean.model = list(armaOrder = c(5, 0), include.mean = FALSE),
                   distribution.model = "norm")
  garch_fit_2 = ugarchfit(spec = spec2, data = ret_sp[i:(l+i-1)])
  forc_2 = ugarchforecast(garch_fit_2, n.ahead=1)
  pred1[i]=forc_2@forecast$seriesFor[[1]]
  pred2[i]=forc_2@forecast$sigmaFor[[1]]
}
pred_all=cbind(pred1,pred2)
write.csv(pred_all,"Pred_AR_sGARCH.csv")












