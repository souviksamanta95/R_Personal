rm(list=ls())
cat("\014")
dev.off()

#install.packages("vars")
#install.packages("forecast")
#install.packages("tseries")
#install.packages("quantmod")
#install.packages("rugarch")
#install.packages("rmgarch")
#install.packages("xts")

library ("vars")
library ("forecast")
library("tseries")
library ("quantmod")
library ("rugarch")
library ("rmgarch")
library("xts")

options (scipen=99999)
#Import Data
startDate=as.Date("2016-04-01")
endDate=as.Date("2019-12-31")

getSymbols("IBM",  from=startDate, to=endDate)
getSymbols("GOOG", from=startDate, to=endDate)
getSymbols("BP",   from=startDate, to=endDate)

rIBM<-monthlyReturn(IBM)
rBP<-monthlyReturn(BP)
rGOOG<-dailyReturn(GOOG)


tsdisplay(rIBM)
tsdisplay(rBP)

rIBM<-diff(IBM,5)
tsdisplay(rIBM)
tsdisplay(diff(IBM,5),lag=30)

rBP<-diff(BP,5)
rGOOG<-diff(GOOG,5)

rX <-data.frame(rIBM, rBP, rGOOG)

names (rX) [1] <-"rIBM"
names (rX) [2] <-"rBP"
names (rX) [3] <-"rGOOG"
head(rX)
plot(rX$rIBM,type='l')
plot(rX$rBP,type='l')
plot(rX$rGOOG,type='l')

ndiffs(rX[,"rIBM"], alpha=0.05, test=c("adf"))
ndiffs(rX[,"rBP"], alpha = 0.05, test = c("adf"))
ndiffs(rX[,"rGOOG"], alpha = 0.05, test = c("adf"))

#VARselect(d_df, lag.max = 10, type = "both") [["selection"]]
VARselect(rX[,1:3],lag.max=9,type="both") [["selection"]]
var_est <- VAR(rX[,1:3], p=1, type = "const")
summary(var_est)

RES_rIBM <-var_est$varresult$rIBM$residuals
RES_rBP  <-var_est$varresult$rBP$residuals
RES_rGOOG<-var_est$varresult$rGOOG$residuals

#Diagnostics
#Test for serial autocorrelation using the Portmanteau test
#Rerun var model with other suggested lags if H0 is rejected at 0.05
serial.test(var_est, lags.pt = 10, type = "PT.asymptotic")
#ARCH test (Autoregressive conditional heteroscedasdicity)
arch.test(var_est, lags.multi = 4)
#Test for normality
normality.test(var_est, multivariate.only = TRUE)

RES_rX <-data.frame(RES_rIBM, RES_rBP, RES_rGOOG)

##Dynamic Conditional Correlation Model (DCC) GARCH: Multivariate GARCH
##Model Setup DCC 
## Replicate AR(1)-GARCH(1,1) model for three assets

uspec.n <- multispec(replicate(3, ugarchspec(mean.model=list(armaOrder=c(0,0))
                                                 ,distribution.model ="std")))
##Estimate these Univariate GARCH models using 'multifit' command

multf<-multifit(uspec.n,RES_rX)
multf

##Specify correlation specification using 'dccspec'

spec1<-dccspec(uspec=uspec.n, dccOrder=c(1,1), distribution='mvnorm')

##Estimate using 'dccfit'
#VAR.fit=var_est
fit1<-dccfit(spec1, data=RES_rX, fit.control=list(eval.se=TRUE), fit=multf)
fit1

##Time Varying Covariance and Correlation Matrices

cov1<-rcov(fit1)  #covariance
dim(cov1)
cor1<-rcor(fit1)  #correlation
dim(cor1)

cor1[,,dim(cor1)[3]] ##Last day correlation matrix

cor_BG <- cor1[2,3,] ##Row 2, Col 3 for all days
cor_BG<-as.xts(cor_BG)
cor_BG

##Plot Time Varying Correlation between BP and Google
plot(cor_BG)

##Plotting all three correlations between the three assets
dev.off()
par(mfrow=c(3,1))  ##creating a frame of three windows to be filled by plots
plot(as.xts(cor1[1,2,]), main = "IBM and BP")
plot(as.xts(cor1[1,3,]), main = "IBM and Google")
plot(as.xts(cor1[2,3,]), main = "BP and Google")


##Kroner and Sultan Time Varying Hedging Ratio/Eqn#6
beta12<-cov1[1,2,]/cov1[2,2,]
beta12<-as.xts(beta12)
plot(beta12)
beta12_avg<-mean(beta12)
beta12_avg

beta13<-cov1[1,3,]/cov1[3,3,]
beta13<-as.xts(beta13)
beta13_avg<-mean(beta13)
beta13_avg
plot(beta13)

beta21<-cov1[1,2,]/cov1[2,2,]
beta21<-as.xts(beta21)
beta21_avg<-mean(beta21)
beta21_avg
plot(beta21)

beta23<-cov1[2,3,]/cov1[2,2,]
beta23<-as.xts(beta23)
beta23_avg<-mean(beta23)
plot(beta23)

beta31<-cov1[3,1,]/cov1[3,3,]
beta31<-as.xts(beta31)
plot(beta31)

beta32<-cov1[3,2,]/cov1[3,3,]
beta32<-as.xts(beta32)
plot(beta32)

beta<-data.frame(beta12,beta13,beta21,beta23,beta31,beta32)
write.csv (beta, "beta.csv")

##Kroner and Ng Portfolio Weights/Eqn#7

w12<-(cov1[2,2,]-cov1[1,2,])/(cov1[1,1,]-2*cov1[1,2,]+cov1[2,2,])
w12<-as.xts(w12)
plot(w12)

w13<-(cov1[3,3,]-cov1[1,3,])/(cov1[1,1,]-2*cov1[1,3,]+cov1[3,3,])
w13<-as.xts(w13)
plot(w13)

w21<-(cov1[1,1,]-cov1[2,1,])/(cov1[2,2,]-2*cov1[2,1,]+cov1[1,1,])
w21<-as.xts(w21)
plot(w21)

wgt<-data.frame(w12, w13, w21)
write.csv (wgt, "weight.csv")

##Similarly for w13, w21, w23, w31, w32

##Forecasts of Covariance and Correlation
dccf1<-dccforecast(fit1, n.ahead=10)
plot(dccf1)

##Actual forecasts for the correlation 
names (dccf1@mforecast)
Rhf<-dccf1@mforecast$H #covariance
Rf <-dccf1@mforecast$R #correlation

##To extract forecasts for all three assets and then plot

corf_IB<-Rf[[1]][1,2,] ##IBM and BP 
plot(corf_IB, type='l');
corf_IG<-Rf[[1]][1,3,] ##IBM and Google
plot(corf_IG, type='l')
corf_BG<-Rf[[1]][2,3,] ##BP and Google

#Last in-sample estimates of correlation

par(mfrow=c(3,1))
c_IB<-c(tail(cor1[1,2,],20), rep(NA,10)) #getting last 20 correlation
c_IB
cf_IB<-c(rep(NA, 20), corf_IB) #getting the last 10 forecasts
cf_IB
plot(c_IB, type="l", main ="cor IBM and BP")
lines (cf_IB, type="l", col="orange")

##Similarly for other two C_IG and C_BG

