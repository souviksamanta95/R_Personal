## Time Series ARIMA and MSARIMA Models in R: Box Jenkin's Methodology
rm(list=ls())
cat("\014")
dev.off()

install.packages("tseries")
install.packages("forecast")
install.packages("plm")
install.packages("Formula")
install.packages("tcltk")
install.packages("uroot")
install.packages("pdR")
install.packages("stats")
install.packages("TSA")
install.packages("readxl")
install.packages("aTSA")
install.packages("rms")

library(tseries)
library(forecast)
library(usdm)
library(plm)
library(Formula)
library(tcltk)
library(uroot)
library(pdR)
library(stats)
library(TSA)
#library (aTSA)
#library (rms)

options (scipen=99999)

data<-read.csv(file.choose(),header=TRUE)
df<-data
nrow(df)
ncol(df)
head(df)
View(df)

df_nohold<- df[1:76,]; 
View(df_nohold);
df_hold<-df[77:86,]; 
View (df_hold);

#sum(is.null(df))
#sum(is.na(df))
#y1 <- ts(data.matrix(df$TS), frequency=12)
#y1

##Multicollinearity Check amongst independent variables
vif(df_hold[c(4,5,6)])

covariate_xmatrix <- matrix(c(df_nohold$agri_cdt, df_nohold$WPIT, df_nohold$CPIAL, df_nohold$IIP, df_nohold$RainDev),nrow=76,ncol=5)
covariate_xmatrix
covariate_xmatrix_hold <-matrix(c(df_hold$agri_cdt, df_hold$WPIT, df_hold$CPIAL, df_hold$IIP, df_hold$RainDev), nrow=10,ncol=5)
covariate_xmatrix_hold

##dimnames = list(c("agri_cdt", "WPIT","CPIAL", "IIP","RainDev")))


covariate_xmatrix <- matrix(c(df_nohold$agri_cdt, df_nohold$CPIAL),nrow=76,ncol=2)
covariate_xmatrix


###ARIMAX ESTIMATION: THE STEPS OF ARIMA ESTIMATION ARE NOT SHOWN HERE####
fit.arimax<-Arima(df_nohold$TS, order=c(1,0,0),  seasonal = list(order = c(0,1,1), period = 12),
                                xreg=covariate_xmatrix, method = "ML")
summary(fit.arimax)

fit.arimax<-Arima(df_nohold$TS, order=c(1,0,0),  seasonal = list(order = c(0,1,1), period = 12),
                  xreg=covariate_xmatrix, method = "ML")
summary(fit.arimax)


           ###Diagnostics
windows()
tsdiag(fit.arimax)

#Box.test(residuals(m1), lag= 10, type="Ljung")
#Box.test(residuals(m1), lag= 24, type="Ljung")
#Box.test(residuals(m1), lag= 50, type="Ljung")
#res <- residuals(fit.arimax)
#tsdisplay(res)

####Forecast: In case the forecast function doesn't work, 
####please reinstall the forecast package, before running the following lines

f_hold<-forecast(fit.arimax,xreg = covariate_xmatrix_hold)
f_hold
plot(f_hold)
##MAPE
f_hold_MAPE<-((abs(df_hold$TS-f_hold$mean)/df_hold$TS))*100
mean(f_hold_MAPE)



