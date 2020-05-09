# Linear and multiple linear regression

setwd("D:/Analytics/R/BA/Dataset")

lung <- read.csv("LungCapData2.csv")

attach(lung)

# Regression between LungCap and Age

names(lung)
class(lung$Age)

library(ggplot2)

ggplot(lung, aes(x=LungCap, y=Age))+
  geom_point()+geom_smooth()

cor(lung$Age,lung$LungCap)

?lm

# Modelling linear regression 

mod <- lm(lung$LungCap ~ lung$Age)  # Always put the dependent variable inthe front

# Checking the summary of the model 

summary(mod)

# Checking attributes of the model 

attributes(mod)

#extracting the attrubutes
mod$coefficients
coef(mod)  #alternatively
#checking the plot of the model
plot(Age, LungCap, main = "Scatterplot")
abline(mod, col= "red", lwd =3)

#creating confidence level
confint(mod)
#charging the level of significance
confint(mod, level = 0.99)
#summary model
summary(mod)
#analysis of variance for the model
anova(mod)

# Regression Diagnostics
# Four Assumptions of regression
# The Y-value (or the arrears,"e") are independent
# The Y- value can be expressed as the linear function of the x variable
# Variation ofObservations around the regression line (the residual SE) is constant (Homoscedasticity)
# For given value of X,Y value(or the arrear)are normally distributed


mform = c(2,2)
plot(mod)
# Multiple Regression
model1 <- lm(lung$LungCap ~ lung$Age + lung$Height)
summary(model1)
plot(model1)
cor(lung$Age,lung$Height)
model2 <- lm(lung$LungCap ~ .,data = lung)
summary(model2)

# Stepwise regression
model3 <- lm(lung$LungCap ~ 1,data = lung) # 1 means only intercept
summary(model3)
step(model3, direction = 'forward', scope = formula(model2))
step(model2, direction = "backward")
hist(lung$LungCap)
data1 <- log(lung$LungCap)




