# CART regression
car.df <- read.csv("ToyotaCorolla.csv")
#car.df<-read.csv(file.choose(), header= T)
str(car.df)

# preprocess
set.seed(1)  
train.index <- sample(c(1:dim(car.df)[1]), dim(car.df)[1]*0.6)  
valid.index <- setdiff(c(1:dim(car.df)[1]), train.index)  
train.df <- car.df[train.index, ]
valid.df <- car.df[valid.index, ]

# regression tree:
tr <- rpart(Price ~  Age_08_04 + KM + Fuel_Type + 
              HP + Automatic + Doors + Quarterly_Tax + 
              Mfr_Guarantee + Guarantee_Period + Airco + 
              Automatic_airco + CD_Player + Powered_Windows + 
              Sport_Model + Tow_Bar, data = train.df, 
            method = "anova", minbucket = 1, maxdepth = 30, cp = 0.001)
prp(tr)

# errors
library(forecast)
library(ggplot2)
accuracy(predict(tr, train.df), train.df$Price)
accuracy(predict(tr, valid.df), valid.df$Price)

# shallower tree
tr.shallow <- rpart(Price ~  Age_08_04 + KM + Fuel_Type + 
                      HP + Automatic + Doors + Quarterly_Tax + 
                      Mfr_Guarantee + Guarantee_Period + Airco + 
                      Automatic_airco + CD_Player + Powered_Windows + 
                      Sport_Model + Tow_Bar, data = train.df, 
                    method = "anova")
prp(tr.shallow)
accuracy(predict(tr.shallow, train.df), train.df$Price)
accuracy(predict(tr.shallow, valid.df), valid.df$Price)

############################################
#Classification Tree
#Model for categorical price
bins <- seq(min(car.df$Price), 
            max(car.df$Price), 
            (max(car.df$Price) - min(car.df$Price))/20)
Binned_Price <- .bincode(car.df$Price, 
                         bins, 
                         include.lowest = TRUE)
Binned_Price <- as.factor(Binned_Price)
train.df$Binned_Price <- Binned_Price[train.index]
valid.df$Binned_Price <- Binned_Price[valid.index]

tr.binned <- rpart(Binned_Price ~  Age_08_04 + KM + Fuel_Type + 
                     HP + Automatic + Doors + Quarterly_Tax + 
                     Mfr_Guarantee + Guarantee_Period + Airco + 
                     Automatic_airco + CD_Player + Powered_Windows + 
                     Sport_Model + Tow_Bar, data = train.df)
prp(tr.binned)

# predict price
new.record <- data.frame(Age_08_04 = 77, 
                         KM = 117000, 
                         Fuel_Type = "Petrol", 
                         HP = 110, 
                         Automatic = 0, 
                         Doors = 5, 
                         Quarterly_Tax = 100, 
                         Mfr_Guarantee = 0, 
                         Guarantee_Period = 3, 
                         Airco = 1, 
                         Automatic_airco = 0, 
                         CD_Player = 0, 
                         Powered_Windows = 0, 
                         Sport_Model = 0, 
                         Tow_Bar = 1)
# regression model 
price.tr <- predict(tr, newdata = new.record)

# classification model 
price.tr.bin <- bins[predict(tr.binned, newdata = new.record, type = "class")]