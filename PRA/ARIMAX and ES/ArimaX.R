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
library(plm)
library(Formula)
library(tcltk)
library(uroot)
library(pdR)
library(stats)
library(TSA)
library (readxl)
library (aTSA)
library (rms)

options (scipen=99999)

#data=read.csv("C:/Users/kakali/Desktop/PA/ARIMA-X/ts.csv")# Read the file
data<-read.csv(file.choose(),header=TRUE)
df<-data
nrow(df)
ncol(df)
head(df)
View(df)

#hold<-data[77:86,]
df<- data[1:76,]
View(df)


sum(is.null(df))
sum(is.na(df))


#y1 <- ts(data.matrix(df$TS), frequency=12)
#y1

covariate_xmatrix <- matrix(c(df$agri_cdt, df$WPIT,df$CPIAL, df$IIP,df$RainDev), nrow=76, ncol=5)
##dimnames = list(c("agri_cdt", "WPIT","CPIAL", "IIP","RainDev")))



###ARIMAX ESTIMATION: THE STEPS OF ARIMA ESTIMATION ARE NOT SHOWN HERE####

fit.arimax<-Arima(df$TS, order=c(1,0,0),  seasonal = list(order = c(0,1,1), period = 12),xreg=covariate_xmatrix, method = "ML")
summary(fit.arimax)
fit.arimax$fitted

###Diagnostics
tsdiag(fit.arimax)

#Box.test(residuals(m1), lag= 10, type="Ljung")
#Box.test(residuals(m1), lag= 24, type="Ljung")
#Box.test(residuals(m1), lag= 50, type="Ljung")

res <- residuals(fit.arimax)
tsdisplay(res)

####Forecast: In case the forecast function doesn't work, 
####please reinstall the forecast package, before running the following lines

f<-forecast(fit.arimax, h=10, xreg = covariate_xmatrix)
f
plot(f)


