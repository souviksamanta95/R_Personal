
# Guest Lecture on 08/12/19

# Big data : 3V's - Value, velocity, variety   and also VERASITY (4th V)

test <- read.csv("C:/Users/souvi/Documents/R/IM507/Test_Super.txt", header = T, stringsAsFactors = F)
train <- read.csv("C:/Users/souvi/Documents/R/IM507/Train_Super.txt", header = T, stringsAsFactors = F)

View(train)
View(test)     # one column will be less as no sales data

dim(train)
dim(test)

str(train)
summary(train)

# Univariate and bi-variate analysis : ggplot

library(ggplot2)

# Univariate

ggplot(train, aes(Outlet_Establishment_Year))+
  geom_histogram(binwidth = 1.0, color="black", fill = "grey")

# Bi-variate

ggplot(train, aes(x=Item_Visibility, y=Item_Outlet_Sales))+
  geom_point(size=2.5, color="navy")

ggplot(train, aes(Outlet_Identifier, Item_Outlet_Sales))+
  geom_bar(stat = "identity", color="purple") + ggtitle("Outlet Vs Total Sales")

ggplot(train, aes(Item_Type, Item_Outlet_Sales))+
  geom_bar(stat = "identity", color="purple") + ggtitle("Item type Vs Total Sales")

ggplot(train, aes(Item_Type, Item_MRP))+
  geom_boxplot(color="black", fill = "light blue") + ggtitle("Item Type Vs MRP")


table(is.na(train))
colSums(is.na(train))


## Isuues - Missing values in Item weights, visibility=0, Fat content same type, Blank values in Outlet size, in test, dummy column for prediction
#                   **Median**               **Median**         **2types**          **Others**                      * Dummy Column

# Categorical data sets -

table(train$Item_Fat_Content)     # same type, but typing errors
table(train$Item_Type)
table(train$Outlet_Location_Type)
table(train$Outlet_Type)
table(train$Outlet_Size)    # Blank values

test$Item_Outlet_Sales=1    # mean(train$Item_Outlet_Sales)

combi <- rbind(train, test)

# Treating missing values -----

combi$Item_Weight[is.na(combi$Item_Weight)] <- median(combi$Item_Weight, na.rm = T)

table(is.na(combi$Item_Weight))

combi$Item_Visibility[combi$Item_Visibility==0.0] <- median(combi$Item_Visibility)

summary(combi$Item_Visibility)

# Get from PPT
