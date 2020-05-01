# Bike sharing data from UCI (Local file)
library(readxl)
bs <- read_excel("C:/Users/souvi/Documents/R/IM507/Bike-Sharing-Dataset/DPA assignment.xlsx")

#View(bs)
dim(bs)
nrow(bs)
ncol(bs)
names(bs)
class(bs)
str(bs)
summary(bs)

# Most data are factors, but they are in numerical form which needs to be converted to factor

# Changing date as date if it's not automatic...

bs$dteday <- as.Date(bs$datetime, "%d-%m-%y")
typeof(bs$datetime)

# Conversion to factors :

bs$season <- factor(bs$season,levels = c("1","2","3","4"), labels = c("Winter", "Spring", "Summer", "Fall"))
bs$holiday <- factor(bs$holiday,levels = c("0","1"), labels = c("Not Holiday", "Holiday"))
bs$workingday <- factor(bs$workingday, levels = c("0","1"), labels = c("Not workingday", "Workingday"))
bs$weather <- factor(bs$weather, levels = c("1","2","3","4"), labels = c("Clear", "Cloudy", "Light rain", "Heavy rain"))

#View(bs)
table(bs$season)

# Extracting month and year from the date : using **lubridate**
#install.packages("lubridate")
library(lubridate)
bs$month <- month(bs$datetime)
bs$year <- year(bs$datetime)
str(bs)
table(bs$month)


# Converting normal values to actual valuefor temperature :


bs$temp <- bs$temp*41
bs$atemp <- bs$atemp*50
bs$hum <- bs$hum*100
bs$windspeed <- bs$windspeed*67

str(bs$windspeed)
# checking missing values :

sum(is.na(bs))


# Boxplotting continuous variables - 

boxplot(bs$temp)
boxplot(bs$atemp)
boxplot(bs$humidity)
boxplot(bs$windspeed)

outval <- boxplot.stats(bs$windspeed)$out
outval

length(outval)
hist(outval, labels = T, xlim = c(30,60))

# Using ggplot2

boxplot_windspeed <- ggplot(bs, aes(x=season,y=windspeed,color=season))+
                      geom_boxplot()+theme_classic()
boxplot_windspeed

library(dplyr)

bs %>%
  ggplot(aes(x=season, y=windspeed, fill=factor(year)))+
  geom_boxplot()+theme_classic()

bs %>%
  ggplot(aes(x=season, y=windspeed, fill=factor(month)))+
  geom_boxplot()+theme_classic()

bs %>%
  ggplot(aes(x=season, y=windspeed, fill=factor(month)))+
  geom_boxplot()+theme_classic() + facet_wrap(~year)

ggplot(bs, aes(x=season, y=windspeed, fill=factor(month)))+
  geom_boxplot()+theme_classic() + facet_wrap(~year)


ggplot(bs, aes(temp,count))+
  geom_point()+theme_classic()

ggplot(bs, aes(temp,count))+
  geom_point(aes(color=season, shape=season))+theme_classic()

ggplot(bs, aes(temp,count))+
  geom_point(aes(color=season, shape=season))+
  geom_smooth(aes(color=season, fill=season), method = "lm")+theme_classic()



x <- ggplot(bs, aes(temp,count))

x + geom_point(aes(color=season))+theme_classic()

#CVP

cvp <- bs%>% select(temp,atemp,humidity,windspeed,count)
cvp<-data.frame(cvp)
cvp

# Correlation plotting
#install.packages("corrplot")
library("corrplot")
m <- cor(cvp)
m

cvp_plot <- cor(cvp)
class(cvp_plot)
corrplot(cvp_plot)
corrplot(cvp_plot,method="number") #correlation between the various parameters

# Building models

# Linear univariate model -
lm1 <- lm(count~temp,bs)
summary(lm1)
# Predicting from model -
newdata <- data.frame(temp=c(30,20,10,35))
predict(lm1,newdata)

