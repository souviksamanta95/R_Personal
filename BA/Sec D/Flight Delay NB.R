#### Table 8.4

library(e1071)
FlightDelays<- read.csv(file.choose(), header= T)
delays.df <-FlightDelays

# change numerical variables to categorical first
delays.df$DAY_WEEK <- factor(delays.df$DAY_WEEK)
delays.df$DEP_TIME <- factor(delays.df$DEP_TIME)

# create hourly bins departure time 
delays.df$CRS_DEP_TIME <- factor(round(delays.df$CRS_DEP_TIME/100))

# Create training and validation sets.
selected.var <- c(10, 1, 8, 4, 2, 13)
set.seed(123)
train.index <- sample(c(1:dim(delays.df)[1]), dim(delays.df)[1]*0.6)  
train.df <- delays.df[train.index, selected.var]
valid.df <- delays.df[-train.index, selected.var]

# run naive bayes
delays.nb <- naiveBayes(Flight_Status ~ ., data = train.df)
delays.nb

#### Table 8.6

## predict probabilities
pred.prob <- predict(delays.nb, newdata = valid.df, type = "raw")
head(pred.prob)
## predict class membership
pred.class <- predict(delays.nb, newdata = valid.df)
head(pred.class)

df <- data.frame(actual = valid.df$Flight_Status, predicted = pred.class, pred.prob)
head(df)

#### Table 8.7

library(caret)
library(ggplot2)

# training
pred.class <- predict(delays.nb, newdata = train.df)
confusionMatrix(pred.class, train.df$Flight_Status)

# validation
pred.class1 <- predict(delays.nb, newdata = valid.df)
confusionMatrix(pred.class1, valid.df$Flight_Status)

install.packages("MLmetrics")
library(MLmetrics)
F1_Score(y_true=train.df$Flight_Status, y_pred=pred.class)


# New Data (FlightDelays New Data.Csv)
FlightDelays_new<- read.csv(file.choose(), header= T)
str(FlightDelays_new)
FlightDelays_new$CRS_DEP_TIME<-factor(FlightDelays_new$CRS_DEP_TIME)
FlightDelays_new$DAY_WEEK<-factor(FlightDelays_new$DAY_WEEK)

New_data<-FlightDelays_new

pred.class2 <- predict(delays.nb, newdata = New_data)
pred.class2

pred.prob2 <- predict(delays.nb, newdata = New_data, type="raw")
pred.prob2

result_New_data<-cbind(pred.prob2,pred.class2)
result_New_data
