#Linear Regression Model

attach(LungCapData2)
#Between Lung capacity(Y) and Age(X)
#Time is the only independent variable

names(LungCapData2) # Gives the coloumn names

#Checking the type of variable
class(Age)
class(LungCap)
#Output variable is binary - classfication or logistic regression

plot(Age,LungCap,main="Scatterplot")
cor(Age,LungCap)

# between (-0.4,0.4) very less regression, ignore the variables 0.4-0.6 = moderate
#0.6-0.8 =strong, greater than 0.8 = very strong r^r around 50%, age is able to explain
#50% change in lungcap

help(lm)#Shows the 
?lm

mod = lm(LungCap~Age)
summary(mod)

#Residuals:
 # Min      1Q  Median      3Q     Max 
#-4.7262 -1.0370 -0.1497  0.9637  6.3836
#Residuals, the distance between best fit line and the actaual points, min residual

#p value - less than 0.05 significant
#residual standard error - 
#degree of freedom is relevant in sample data less than 30 varibles
#not in population
#Degree of freedom no of information required to gain the information, 
#since in large sample size, n is already large, so n or n+1 doesnot make huge significance
#but in sample size less than 30, n or n+1 is big, say 3/5 and 3/4 will have 
#huge variance
#but 3/30,3/31 not much difference
#Fischer statistic, p value again less than 0.05, model is significant and of importance
#df = no of samples -1

attributes(mod)
#qr - quartile
mod$coefficients

#plotting model and regression line
plot(Age,LungCap,main = "Scatterplot")
abline(mod,col="red",lwd=3)#lwd line width = 3

confint(mod)
confint(mod, level = 0.99)
#

anova(mod)

#Four assumptions of regression
#The y values (or the errors e) are independent
#Y value can be expressed as a linear function of x variable
#Variation of observations around the regression line (the residual SE)
#is constant(Homoscedasticity - on both side of the line 
#the errors should be same, not a skewed data)
#For given value of X,Y values(or the error) are normally distributed

mfrow = c(2,2)
plot(mod)

#Residuals vs leverage, till some distance the linear line can be maintained but after
#some time, it deviates from straight line as skewed


#Multiple Regression
cor(Age,Height)
model1 = lm(LungCap ~ Age+Height)
summary(model1)

model2 = lm(LungCap~ .,data=LungCapData2)
summary(model2)

#Step wise regression
model3 = lm(LungCap~1, data = LungCapData2) #1 means only intercept
summary(model3)
step(model3, direction = "forward", scope = formula(model2))
#If aic goes higher, the model is wekaer,
#As the new varaibles are added, the AIC is lower so better predictability

step(model2, direction = "backward")#new study forward model, modification backward
