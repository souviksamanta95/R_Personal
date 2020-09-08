#What slots are available for @model in ug_fit
ug_fit@model
names(ug_fit@model)

#What slots are available for @fit in ug_fit
ug_fit@fit
names(ug_fit@fit)
ug_fit@fit$coef   #coeff



##GARCH Model Forecasting: res and sE(res)
ug_fore <- ugarchforecast(ug_fit,n.ahead=10)

ug_fore@forecast$sigmaFor     # square root of forecast residual square
ug_fore@forecast$seriesFor    # arima part, for mean = false, it should be zero
#names(ug_fore@model)
#names(ug_fore@forecast)

ug_STER<-ug_fore@forecast$sigmaFor;
plot(ug_STER,type="l")
lines (ug_STER,col="green")

#Last 20 obs
ug_var_t <- c(tail(ug_var,10),rep(NA,10))
#ug_var_t
ug_res2_t <- c(tail(ug_res2,10),rep(NA,10))
#ug_res2_t
ug_fore2 <-c(rep(NA,10),(ug_fore@forecast$sigmaFor)^2)
#ug_fore2

plot (ug_res2_t,type="l")     #Actual Variance
lines (ug_fore2,col="orange") #Out-of sample Forecast Variance
lines (ug_var_t,col="green")  #Estimated Variance
