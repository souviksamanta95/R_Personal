#Import Data
options (scipen=99999)
data=read.csv("C:/Users/kakali/Desktop/PA/ES/Data/sensex-oil-exrt.csv")
df<-data[-c(2)]
View(df)

#ses(y, h = 10, level = c(80, 95), fan = FALSE,
#    initial = c("optimal", "simple"), alpha = NULL, lambda = NULL,
#    biasadj = FALSE, x = y, ...)

#holt(y, h = 10, damped = FALSE, level = c(80, 95), fan = FALSE,
#     initial = c("optimal", "simple"), exponential = FALSE,
#     alpha = NULL, beta = NULL, phi = NULL, lambda = NULL,
#     biasadj = FALSE, x = y, ...)

#hw(y, h = 2 * frequency(x), seasonal = c("additive", "multiplicative"),
#   damped = FALSE, level = c(80, 95), fan = FALSE,
#   initial = c("optimal", "simple"), exponential = FALSE,
#   alpha = NULL, beta = NULL, gamma = NULL, phi = NULL,
#   lambda = NULL, biasadj = FALSE, x = y, ...)


#Exponential Smoothing
#Install and load forecast package 
if(!require(forecast)) {
  install.packages("forecast", dependencies = T)
  library(forecast) }

if(!require(graphics)) {
  install.packages("graphics", dependencies = T)
  library(graphics) }


if(!require(stats)) {
  install.packages("stats", dependencies = T)
  library(stats) }

##Fit simple exponential smoothing model 
fit_ses <- ses(df$ex)
summary(fit_ses)
#Plot the forecasted values
plot(fit_ses)


##Fit Holt exponential smoothing model 
fit_holt <- holt(df$ex)
summary(fit_holt)
#Plot the forecasted values
plot(fit_holt)


##Fit Holt-Winters exponential smoothing model 
##Converting the data as Time Series
y1 <- ts(data.matrix(df$ex), start = as.Date("2008-03-03"), end = as.Date ("2008-11-28"), frequency=5)
y1
is.ts(y1)
str(y1)

##Fit with hw
fit_hw <- hw(y1, h = 10, seasonal = c("additive","multiplicative"))
summary(fit_hw)
#Plot the forecasted values
plot(fit_hw)

##Fit with HoltWinters
fit_hw <- HoltWinters(y1, seasonal = "mult")
fit_hw
pred <- predict(fit_hw, 50, prediction.interval = TRUE)
plot(fit_hw, pred)


#Fit automated exponential smoothing model 
fit_auto <- forecast(df$ex, h = 10)

summary(fit_auto)
#Plot the forecasted values
plot(forecast(fit_auto))
