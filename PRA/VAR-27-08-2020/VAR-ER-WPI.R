


# 
# install.packages("vars")
# install.packages("forecast")
# install.packages("tseries")
# install.packages("tvReg")

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
nrow(df)
ncol(df)
head(df)
View(df)
#y<-df[c(2,3,4,5)]

plot.ts(df)
summary(df)

#Train
df.train<-df[1:120,]
df.train <-data.frame(df.train);df.train
df.test<-df[-c(1:120),];
df.test

#Run Augmented Dickey-Fuller tests to determine stationarity and 
#order of integration or number of differences to achieve stationarity.

ndiffs(df[,"ER"], alpha=0.05, test=c("adf"))
ndiffs(df[,"MS"], alpha = 0.05, test = c("adf"))
ndiffs(df[,"WPI"], alpha = 0.05, test = c("adf"))
ndiffs(df[,"FER"], alpha = 0.05, test = c("adf"))


##Follwoing pieces of code also helps to execute ADF test/Unit Root test
k <- trunc((length(df$ER)-1)^(1/3)); k
adf.test(df$ER, k=5)
adf.test(diff(df$ER,1), k=5)
pp.test(df$ER, alternative="stationary")  
pp.test(diff((df$ER),1), alternative="stationary")

#Difference to achieve stationarity
d_ER  <- diff(df[, "ER"], differences = 1)
d_MS  <- diff(df[, "MS"], differences = 1)
d_WPI <- diff(df[, "WPI"], differences = 1)
d_FER  <- diff(df[, "FER"], differences = 1)

d_df <- cbind(d_ER,d_MS,d_WPI, d_FER)
plot.ts(d_df)
View(d_df)

#Lag optimisation
#VARselect(d_df, lag.max = 10, type = "both") [["selection"]]
VARselect(d_df[,1:3],lag.max=9,type="both") [["selection"]]

#Vector autoregression with lags set according to results of lag optimisation. 
var_est <- VAR(d_df[,1:3], p=1, type = "const", season=NULL) # p = num of lags
summary(var_est)
fevd(var_est, n.ahead=50)

summary(var_est)
var_est$varresult$d_ER$fitted.values

#Forecasting
prd <- predict(var_est, n.ahead = 10, ci = 0.95, dumvar = NULL)
print(prd)
plot(prd, "single")

#Granger Causality test
#Does x2 granger cause x1?
#grangertest(d.x1 ~ d.x2, order = 2)

#Does d_ER granger cause d_WPI?
grangertest(d_WPI ~ d_ER, order = 1) # here order 2 is also significant  i.e. long memory
grangertest(d_ER ~ d_WPI, order = 1)

grangertest(d_WPI ~ d_MS, order = 1)
grangertest(d_MS ~ d_WPI, order = 1)

grangertest(d_MS ~ d_FER, order = 1)
grangertest(d_FER ~ d_MS, order = 1)


# Calculate impulse response
var_est_IR <- irf(var_est,impulse="d_pgas",response="d_pgdp",n.ahead = 20,ortho = FALSE,
                  cumulative = TRUE)

# Plot
plot(var_est_IR)



#Diagnostics
#Test for serial autocorrelation using the Portmanteau test
#Rerun var model with other suggested lags if H0 is rejected at 0.05
serial.test(var_est, lags.pt = 3, type = "PT.asymptotic")

#ARCH test (Autoregressive conditional heteroscedasdicity)
arch.test(var_est, lags.multi = 4)

#Test for normality
normality.test(var_est, multivariate.only = TRUE)

