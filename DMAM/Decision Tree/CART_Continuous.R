getwd()
data<-read.csv("golf1.csv")
data$Outlook<-factor(data$Outlook)
data$Temp<-factor(data$Temp)
data$Windy<-factor(data$Windy)
data$Play.Golf<-factor(data$Play.Golf)
str(data)
head(data)
summary(data)



#install.packages("rpart")
library(rpart)

#rpart(formula, data=, method='')
#arguments:			
# formula: The function to predict
# data: Specifies the data frame
# method: "class" for a classification tree ,"anova" for a regression tree	

fit <- rpart(Play.Golf ~ Outlook + Temp + Humidity + Windy, method="class", data=data,
             control=rpart.control(minsplit=1))
summary(fit)
print(fit)

#install.packages("rpart.plot")	
library(rpart.plot)
rpart.plot(fit, extra = 101)
