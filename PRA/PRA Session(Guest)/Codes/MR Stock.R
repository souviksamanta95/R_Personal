datanew  <- read.csv (file.choose(),header=TRUE)  
data<-datanew[-c(1,2)] #removing serial no and Date
summary(data)

#Checking missing values
sum(is.na(data))

#Creating graphs
library(ggplot2)
options(scipen=9999)
attach(data)

#### SCATTER PLOT- To understand the expected coefficient signs
ggplot(data = data, mapping = aes(x =MACD , y = Close)) +
  geom_point(size=3, shape=20)+ggtitle("Close VS MACD") 

ggplot(data = data, mapping = aes(x =X14D_RSI , y = Close)) +
  geom_point(size=3, shape=20)+ggtitle("Close VS RSI")

ggplot(data = data, mapping = aes(x=X14D_StochOsc , y = Close)) +
  geom_point(size=3, shape=20)+ggtitle("Close VS Stochastic Oscillator")

ggplot(data = data, mapping = aes(x=sentiment , y = Close)) +
  geom_point(size=3, shape=20)+ggtitle("Close VS Market Sentiment")

#Linear Regression
reg<-lm(Close~data$MACD,data=data)
summary(reg)

reg<-lm(Close~data$X14D_RSI,data=data)
summary(reg)

reg<-lm(Close~data$X14D_StochOsc,data=data)
summary(reg)

data<-data[-4]#removing Stochastic Oscillator as input

reg<-lm(Close~.,data=data)
summary(reg)

#checking correlation to avoid multicollinearity
library(usdm)
vif(data[-c(1)])
cor(data[,-c(1)])  #correlation matrix