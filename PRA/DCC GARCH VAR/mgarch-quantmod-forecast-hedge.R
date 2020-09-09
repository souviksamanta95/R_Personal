install.packages("quantmod")
install.packages("rugarch")
install.packages("rmgarch")

library (quantmod)
library (rugarch)
library (rmgarch)

options (scipen=99999)

#Import Data
startDate=as.Date("2016-04-01")
endDate=as.Date("2019-12-31")

getSymbols("IBM",  from=startDate, to=endDate)
getSymbols("GOOG", from=startDate, to=endDate)
getSymbols("BP",   from=startDate, to=endDate)

rIBM<-dailyReturn(IBM)
rBP<-dailyReturn(BP)
rGOOG<-dailyReturn(GOOG)

rX <-data.frame(rIBM, rBP, rGOOG)

names (rX) [1] <-"rIBM"
names (rX) [2] <-"rBP"
names (rX) [3] <-"rGOOG"


##Dynamic Conditional Correlation Model (DCC) GARCH: Multivariate GARCH
##Model Setup DCC 
## Replicate AR(1)-GARCH(1,1) model for three assets

uspec.n <- multispec(replicate(3, ugarchspec(mean.model=list(armaOrder=c(1,0)))))

##Estimate these Univariate GARCH models using 'multifit' command

multf<-multifit(uspec.n,rX)
multf

##Specify correlation specification using 'dccspec'

spec1<-dccspec(uspec=uspec.n, dccOrder=c(1,1), distribution='mvnorm')

##Estimate using 'dccfit'

fit1<-dccfit(spec1, data=rX,fit.control=list(eval.se=TRUE), fit=multf)
fit1

names(fit1@mfit)


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


##Kroner and Sultan Time Varying Hedging Ratio/equation #6
beta12<-cov1[1,2,]/cov1[2,2,]
beta12<-as.xts(beta12)
plot(beta12)
beta12_avg<-mean(beta12)
beta12_avg

beta13<-cov1[1,3,]/cov1[3,3,]
beta13<-as.xts(beta13)
plot(beta13)

beta21<-cov1[1,2,]/cov1[2,2,]
beta21<-as.xts(beta21)
beta21_avg<-mean()
plot(beta21)

beta23<-cov1[2,3,]/cov1[3,3,]
beta23<-as.xts(beta23)
plot(beta23)

beta31<-cov1[3,1,]/cov1[3,3,]
beta31<-as.xts(beta31)
plot(beta31)

beta32<-cov1[3,2,]/cov1[3,3,]
beta32<-as.xts(beta32)
plot(beta32)

beta<-data.frame(beta12,beta13,beta21,beta23,beta31,beta32)
write.csv (beta, "beta.csv")

##Kroner and Ng Portfolio Weights: eqn#7

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
dccf1

##Actual forecasts for the correlation 
names (dccf1@mforecast)
Rhf<-dccf1@mforecast$H #covariance
Rf <-dccf1@mforecast$R #correlation

##To extract forecasts for all three assets and then plot

corf_IB<-Rf[[1]][1,2,] ##IBM and BP
corf_IG<-Rf[[1]][1,3,] ##IBM and Google
corf_BG<-Rf[[1]][2,3,] ##BP and Google

#Last in-sample estimates of correlation

par(mfrow=c(3,1))
c_IB<-c(tail(cor1[1,2,],20), rep(NA,10)) #getting last 20 correlation 
cf_IB<-c(rep(NA, 20), corf_IB) #getting the last 10 forecasts
plot(c_IB, type="l", main ="cor IBM and BP")
lines (cf_IB, type="l", col="orange")

##Similarly for other two C_IG and C_BG
