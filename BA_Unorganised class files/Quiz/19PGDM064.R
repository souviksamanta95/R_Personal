getwd()
setwd("C:/Users/souvi/Documents/R/BA/Quiz")

data <- read.csv("Test.csv")

head(data)
summary(data)
# Finding the most correlated value
cor(data)
cor(data, use="complete.obs")
symnum(cor(data,use="complete.obs"))     # Production of wheat is mostly related to year

# Getting the location of missing values---
ind<- function(t)
{
  x<- dim(length(t))
  x[which(!is.na(t))]=1
  x[which(is.na(t))]=0
  return(x)
}
data$Ind <- ind(data$Production.of.wheat..MT.)
data$Ind

names(data)
# Fitting the Linear model of y on x 

lm(Production.of.wheat..MT.~ï..Year,data=data)
summary(lm(Production.of.wheat..MT.~ï..Year,data=data))

# y = -3107410 + 1588*x
# Imputation of missing values
for(i in 1:nrow(data))
{
  if(data$Ind[i]==0)
  {
    data$Production.of.wheat..MT.[i]=-3107410 + 1588*data$ï..Year[i]
  }
}

summary(data)
data$Ind <- ind(data$Production.of.wheat..MT.)


# Imputing missing values by kNN----

library(VIM)

data <- kNN(data)
data <- subset(data, select = ï..Year:Quality.of.fertilizer)

summary(data)


# Question 2 :
# Finding the strongest relation by coefficient of determination i.e. R squared value
summary(lm(Production.of.wheat..MT.~ï..Year,data=data)) # R squared = 0.9824
summary(lm(Production.of.wheat..MT.~Amount.of.rainfall,data=data)) # R squared = 0.0004333
summary(lm(Production.of.wheat..MT.~Qulaity.of.Soil,data=data)) # R squared = 0.004594
summary(lm(Production.of.wheat..MT.~Quality.of.fertilizer,data=data)) # R squared = 0.01269

# So Year is the strongest related parameter in this case


# Multiple linear model ---
summary(lm(Production.of.wheat..MT.~.,data=data))

# The Equation will be >> -3116000 + 1593year - 1.743Amount of rainfall-322.9Qulaity of Soil -37.23Quality of fertilizer

# For Year = 2020, Amount of rainfall=585,  Qulaity of Soil=6.5, Quality of fertilizer=7 ; Production of wheet in MT =

prod_2020 <- -3116000 + 1593*2020 - 1.743*585 - 322.9*6.5 - 37.23*7
prod_2020 # = 98480.88(ANS)

