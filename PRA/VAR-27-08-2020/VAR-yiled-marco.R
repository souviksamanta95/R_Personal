rm(list=ls())
cat("\014")
dev.off()

install.packages("vars")
install.packages("forecast")
install.packages("tseries")
install.packages("tvReg")

library ("vars")
library ("forecast")
library("tseries")
library("tvReg")

#Estimation of a VAR by utilising OLS per equatio
#VAR(y, p = 1, type = c("const", "trend", "both", "none"),
#    season = NULL, exogen = NULL, lag.max = NULL,
#    ic = c("AIC", "HQ", "SC", "FPE"))
## S3 method for class 'varest'
#print(x, digits = max(3, getOption("digits") - 3), ...)

##summary, plot, coef, residuals, fitted, predict, irf, fevd, Phi, Psi, 
##normality.test, arch.test, serial.test, VARselect, logLik

#Import Data
options (scipen=99999)
data<-read.csv(file.choose(),header=TRUE)
df<-data
df<-data[-c(8,9)]
nrow(df)
ncol(df)
head(df)
View(df)

windows()
plot.ts(df)
summary(df)

#Train
df.train<-df[1:69,]
df.test<-df[-c(1:69),];
df.test

#Run Augmented Dickey-Fuller tests to determine stationarity and 
#order of integration or number of differences to achieve stationarity.

ndiffs(df[,"level"], alpha=0.05, test=c("adf"))
ndiffs(df[,"slope"], alpha = 0.05, test = c("adf"))
ndiffs(df[,"curv"], alpha = 0.05, test = c("adf"))
ndiffs(df[,"growth"], alpha = 0.05, test = c("adf"))
ndiffs(df[,"inflation"], alpha = 0.05, test = c("adf"))
ndiffs(df[,"CMR"], alpha = 0.05, test = c("adf"))

##Follwoing pieces of code also helps to execute ADF test/Unit Root test
k <- trunc((length(df$CMR)-1)^(1/3)); k
adf.test(df$CMR, k=4)
adf.test(diff(df$CMR,1), k=4)
pp.test(df$CMR, alternative="stationary")  


#Difference to achieve stationarity
d_CMR    <- diff(df[, "CMR"], differences = 1); 
data.frame(d_CMR)
d_level  <- diff(df[, "level"], differences = 1); 
data.frame(d_level)

df<-df[2:79,]
df <- cbind(df, d_CMR, d_level)
df<-df[-c(1,2,7)]
nrow(df)
ncol(df)
head(df)
View(df)
windows()
plot.ts(df)


#Lag optimisation
#VARselect(d_df, lag.max = 10, type = "both") [["selection"]]
VARselect(df[,1:6],lag.max=9,type="both") [["selection"]]
VARselect(df[,1:6],lag.max=9,type="const") [["selection"]]

#Vector autoregression with lags set according to results of lag optimisation. 
var_est <- VAR(df[,1:6], p=1, type = "const")
summary(var_est)
fevd(var_est, n.ahead=5)

#Forecasting
prd <- predict(var_est, n.ahead = 12, ci = 0.95, dumvar = NULL)
print(prd)
plot(prd, "single")

#Granger Causality test
#Does x2 granger cause x1?
#grangertest(d.x1 ~ d.x2, order = 2) 

#Does d_CMR granger cause d_level?
grangertest(df$d_level ~ df$d_CMR, order = 1)
grangertest(df$d_CMR ~ df$d_level, order = 1)

grangertest(df$slope ~ df$d_CMR, order = 1)
grangertest(df$d_CMR ~ df$slope, order = 1)

grangertest(df$inflation ~ df$d_CMR, order = 1)
grangertest(df$d_CMR ~ df$inflation, order = 1)

grangertest(df$d_level ~ df$inflation, order = 1)
grangertest(df$inflation ~ df$d_level, order = 1)

grangertest(df$slope ~ df$inflation, order = 1)
grangertest(df$inflation ~ df$slope, order = 1)

grangertest(df$curv ~ df$d_CMR, order = 1)
grangertest(df$d_CMR ~ df$curv, order = 1)

grangertest(df$curv ~ df$d_CMR, order = 1)
grangertest(df$d_CMR ~ df$curv, order = 1)

# Calculate impulse response
var_est_IR <- irf(var_est,impulse="d_level",response="inflation",n.ahead = 20,ortho = FALSE,
                  cumulative = TRUE)

# Plot
plot(var_est_IR)


#Diagnostics
#Test for serial autocorrelation using the Portmanteau test
#Rerun var model with other suggested lags if H0 is rejected at 0.05
serial.test(var_est, lags.pt = 10, type = "PT.asymptotic")

#ARCH test (Autoregressive conditional heteroscedasdicity)
arch.test(var_est, lags.multi = 4)

#Test for normality
normality.test(var_est, multivariate.only = TRUE)

var_est$varresult$d_level$residuals
jarque.bera.test(var_est$varresult$d_level$residuals)
