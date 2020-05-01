# *************************Group Assignment for Group - 11 **********************************

# Question 1 : Importing Data 1 ----------------------------------------------------

library(readxl)

excel_sheets("C:/Users/souvi/Documents/R/IM507/DPA assignment.xlsx")
data1 <- read_excel("C:/Users/souvi/Documents/R/IM507/DPA assignment.xlsx", sheet = "Data1")

# Question 2 : Understanding the data ----------------------------------------------

View(data1)
dim(data1)
names(data1)
class(data1)
summary(data1)


# Question 3 : Structure of the dataset --------------------------------------------

str(data1)

# Question 4 : Conversion for proper datatype --------------------------------------

data1$Education <- factor(data1$Education,levels = c("1","2","3"), labels = c("Undergrad", "Graduate", "Advanced"))
data1$`Securities Account` <- factor(data1$`Securities Account`,levels = c("0","1"), labels = c("Don't Have","Have"))
data1$`CD Account` <- factor(data1$`CD Account`,levels = c("0","1"), labels = c("Don't Have","Have"))
data1$Online <- factor(data1$Online,levels = c("0","1"), labels = c("Don't Have","Have"))
data1$CreditCard <- factor(data1$CreditCard,levels = c("0","1"), labels = c("Don't Have","Have"))

# Question 5 : Joining Data 1 and Data 2 and creating Data 3------------------------

data2 <- read_excel("C:/Users/souvi/Documents/R/IM507/DPA assignment.xlsx", sheet = "Data2")

names(data1)
names(data2)
data3 <- merge(data1,data2, by.x = "ID", by.y = "ID")
View(data3)

# Converting new column to a factor -

data3$`Personal Loan` <- factor(data3$`Personal Loan`,levels = c("0","1"), labels = c("Don't Have","Have"))

# Question 6 : Joining Data 3 and Data 4 and creating Data 5-------------------------

data4 <- read_excel("C:/Users/souvi/Documents/R/IM507/DPA assignment.xlsx", sheet = "Data4")

data4$Education <- factor(data4$Education,levels = c("1","2","3"), labels = c("Undergrad", "Graduate", "Advanced"))
data4$`Securities Account` <- factor(data4$`Securities Account`,levels = c("0","1"), labels = c("Don't Have","Have"))
data4$`CD Account` <- factor(data4$`CD Account`,levels = c("0","1"), labels = c("Don't Have","Have"))
data4$Online <- factor(data4$Online,levels = c("0","1"), labels = c("Don't Have","Have"))
data4$CreditCard <- factor(data4$CreditCard,levels = c("0","1"), labels = c("Don't Have","Have"))
data4$`Personal Loan` <- factor(data4$`Personal Loan`,levels = c("0","1"), labels = c("Don't Have","Have"))


names(data3)
names(data4)
dim(data3)
dim(data4)

data5 <- rbind(data3,data4)

dim(data5)


# Question 7 : Exploration of data5 ----------------------------------------------------

View(data5)
dim(data5)
nrow(data5)
ncol(data5)
names(data5)
class(data5)
str(data5)
summary(data5)

# Question 8 : Check for missing values ------------------------------------------------

# Total missing values in the data frame -

sum(is.na(data5))

# Missing values in variables -

sapply(data5, function(x)sum(is.na(x)))

# Age, CCAvg, Mortgage have missing values

# Replacing missing values by the means of the particular variable -

data5$Age[is.na(data5$Age)] <- mean(data5$Age, na.rm = T)
data5$CCAvg[is.na(data5$CCAvg)] <- mean(data5$CCAvg, na.rm = T)
data5$Mortgage[is.na(data5$Mortgage)] <- mean(data5$Mortgage, na.rm = T)

sum(is.na(data5))


# Question 9 : Creating Age group variable ------------------------------------------

summary(data5$Age)

# Min age = 23 ; max age = 67   Let us take 5 age groups like-
#   Group A : 20+ to <= 30
#   Group B : 30+ to <= 40
#   Group C : 40+ to <= 50
#   Group D : 50+ to <= 60
#   Group E : 60+

data5$Age_group <- cut(data5$Age,c(20,30,40,50,60,Inf),labels = c("A","B","C","D","E"))

# Question 10 : Creating income group variable --------------------------------------

summary(data5$Income)

# Min age = 8 ; max age = 224   Let us take 5 age groups like-
#   Group A : 0 to <= 50
#   Group B : 50+ to <= 100
#   Group C : 100+ to <= 150
#   Group D : 150+ to <= 200
#   Group E : 200+

data5$income_group <- cut(data5$Income,c(0,50,100,150,200,Inf),labels = c("A","B","C","D","E"))


# Question 11 :  Listing customer id, Mortgage details of the customers who belong to age group (61-65), have family size 3 or above, have education level = 3 and have accepted the loan offer

library(dplyr)

data5 %>% filter(Age>=61,Age<=65,Family>=3,Education=="Advanced",`Personal Loan`=="Have") %>% select(ID,Mortgage)

# Question 12 :  Creating a graph to display the distribution of customers based on education level-----------------

library(ggplot2)

ggplot(data5, aes(Education))+
  geom_bar(color="black", fill = "light blue") + 
  xlab("Education Level") +
  ylab("Count")+
  ggtitle("Distribution of customers based on education level")+
  theme_classic()

# Question 13 :  Creating a graph to display the distribution of customers based on income group-----------------

ggplot(data5, aes(income_group))+
  geom_bar(color="black", fill = "light blue") + 
  xlab("Income Group") +
  ylab("Count")+
  ggtitle("Distribution of customers based on Income Group")+
  theme_classic()


# Question 14 :  Creating a graph to display the distribution of customers who accepted or rejected the loan offer based on family size-----------------

ggplot(data5, aes(Family, fill=`Personal Loan`))+
  geom_bar(color="black") + 
  xlab("Family size") +
  ylab("Count")+
  ggtitle("Distribution of customers accepted loan based on Family size")+
  theme_classic()

#--------------------------------------------------------------------------------------------------------------------------------------------------------
