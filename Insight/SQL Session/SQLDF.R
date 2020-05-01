# SQL Session by Insight---- Part 1 ----

#install.packages("sqldf")

library(readxl)
getwd()
setwd("C:/Users/souvi/Documents/R/Insight/SQL Session/")
library(sqldf)
excel_sheets("P1-SuperStoreUS-2015.xlsx")
Orders <- read_excel("P1-SuperStoreUS-2015.xlsx",sheet="Orders")
Returns <- read_excel("P1-SuperStoreUS-2015.xlsx",sheet="Returns")
Users = read_excel("P1-SuperStoreUS-2015.xlsx",sheet="Users")

sqldf("Select distinct `State or Province`, Country from Orders
      where `State or Province` like 'A%m%'
      order by `State or Province` asc")

df=sqldf("Select `Product Sub-Category`, `Order ID`, `Customer Segment` from Orders
      where `Customer Segment` in ('Small Business', 'Corporate' and 'Orders') and `Order ID`>87000 order by `Order ID`")

head(df)

head(sqldf("Select Profit from Orders where Profit not in (Select max(Profit) from Orders)"))

write.csv(df,"data1.csv")


# Joins : Left, Inner, Right, outer       ** similar to merge()

# To get order ID, Product Name where status is "returned"

sqldf("SELECT Orders.`Order ID`, Orders.`Product Name`
      FROM Orders
      INNER JOIN Returns
      ON Orders.`Order ID`= Returns.`Order ID`")

# To get customer id and their corresponding manager name

sqldf("SELECT Orders.`Customer ID`, Users.Manager
      FROM Orders
      LEFT JOIN Users
      ON Orders.Region = Users.Region")

# After adding another manager as my name -----

sqldf("SELECT count (Orders.`Customer ID`), Users.Manager
      FROM Orders
      LEFT JOIN Users
      ON Orders.Region = Users.Region
      GROUP BY Users.Manager")
# No right join in R so, change the sequence

# "as" is user so as to name it as a header name

sqldf("SELECT Users.Manager as Manager_Name, count (Orders.`Customer ID`) as Count_of_Customers
      FROM Users
      LEFT JOIN Orders
      ON Orders.Region = Users.Region
      GROUP BY Users.Manager")

# To get the list of number of customers from each country

sqldf("SELECT Orders.Region as Region, count(Orders.`Customer ID`) as Number_of_Customer
      FROM Orders
      GROUP BY Orders.Region")


