rm(list=ls())
cat("\014")
dev.off()

setwd("/media/souvik/Analytics/R/PRA/Assignment/VAR")

library ("vars")
library ("forecast")
library("tseries")
library ("rugarch")
library ("rmgarch")
library("xts")

df <- read.csv("dataset_VAR.csv")
df$Gold <- as.numeric(df$Gold)
names(df)
str(df)

rSNP <- diff(log(df$Snp))
rGOLD <- diff(log(df$Gold))
rBTC <- diff(log(df$BTC))
rOIL <- diff(log(df$Oil))

plot(rSNP, type = "l")
plot(rGOLD, type = "l")
plot(rBTC, type = "l")
plot(rOIL, type = "l")

rX <-data.frame(rSNP, rGOLD, rOIL)

ndiffs(rX[,"rSNP"], alpha=0.05, test=c("adf"))
ndiffs(rX[,"rGOLD"], alpha = 0.05, test = c("adf"))
ndiffs(rX[,"rOIL"], alpha = 0.05, test = c("adf"))

VARselect(rX[,1:3],lag.max=9,type="both") [["selection"]]
var_est <- VAR(rX[,1:3], p=1, type = "const")
summary(var_est)

RES_rSNP <- var_est$varresult$rSNP$residuals
RES_rGOLD <- var_est$varresult$rGOLD$residuals
RES_rOIL <- var_est$varresult$rOIL$residuals

RES_rX <-data.frame(RES_rSNP, RES_rGOLD, RES_rOIL)

#Diagnostics
#Test for serial autocorrelation using the Portmanteau test
#Rerun var model with other suggested lags if H0 is rejected at 0.05
serial.test(var_est, lags.pt = 10, type = "PT.asymptotic")
#ARCH test (Autoregressive conditional heteroscedasdicity)
arch.test(var_est, lags.multi = 4)
arch.test(var_est, lags.multi = 10)
arch.test(var_est, lags.multi = 15)
arch.test(var_est, lags.multi = 20)
#Test for normality
normality.test(var_est, multivariate.only = TRUE)

#Dynamic Conditional Correlation Model (DCC) GARCH: Multivariate GARCH
##Model Setup DCC
## Replicate AR(1)-GARCH(1,1) model for three assets

uspec.n <- multispec(replicate(3, ugarchspec(mean.model=list(armaOrder=c(0,0))
                                                 ,distribution.model ="std")))
##Estimate these Univariate GARCH models using 'multifit' command

multf<-multifit(uspec.n,RES_rX)
multf

##Specify correlation specification using 'dccspec'

spec1 <- dccspec(uspec=uspec.n, dccOrder=c(1,1), distribution='mvnorm')

##Estimate using 'dccfit'
#VAR.fit=var_est
fit1 <- dccfit(spec1, data=RES_rX, fit.control=list(eval.se=TRUE), fit=multf)
fit1

##Time Varying Covariance and Correlation Matrices
cov1<-rcov(fit1)  #covariance
dim(cov1)
cor1<-rcor(fit1)  #correlation
dim(cor1)

cor1[,,dim(cor1)[3]] ##Last day correlation matrix

cor_BG <- cor1[2,3,] ##Row 2, Col 3 for all days
cor_BG <- as.xts(cor_BG)
cor_BG

##Plot Time Varying Correlation between SNP and OIL
plot(cor_BG)

##Plotting all three correlations between the three assets
#dev.off()
par(mfrow=c(3,1))  ##creating a frame of three windows to be filled by plots
plot(as.xts(cor1[1,2,]), main = "Gold and SNP")
plot(as.xts(cor1[1,3,]), main = "SNP and Oil")
plot(as.xts(cor1[2,3,]), main = "Gold and Oil")

# CORRELATION is done till now

##Kroner and Sultan Time Varying Hedging Ratio/Eqn#6
beta12 <- cov1[1,2,]/cov1[2,2,]
beta12 <- as.xts(beta12)
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
beta23_avg
plot(beta23)

beta31<-cov1[3,1,]/cov1[3,3,]
beta31<-as.xts(beta31)
plot(beta31)

beta32<-cov1[3,2,]/cov1[3,3,]
beta32<-as.xts(beta32)
plot(beta32)

beta <- data.frame(beta12,beta13,beta21,beta23,beta31,beta32)
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
dccf1 <- dccforecast(fit1, n.ahead=10)
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












































