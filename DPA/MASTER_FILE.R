############################### Mother script for all ################################

# Package install - 
install.packages(c("dplyr","tidyr","tidyverse","readr","readxl","RCurl","Amelia","Rcpp","Hmisc","stringr","lubridate","ggplot2","imputeTS"))

# Activating libraries -
library("dplyr")
library("tidyr")
library("tidyverse")
library("readr")
library("readxl")
library("RCurl")
library("Amelia")
library("Rcpp")
library("Hmisc")
library("stringr")
library("lubridate")
library("ggplot2")
library("imputeTS")

###################################################################################
# CLASS OF 18/10/2019 -------------------------------------------------------------
###################################################################################

getwd()
setwd("C:/Users/souvi/Documents/R/IM507")
a<-50
typeof(a)
b<-6
a%%b
x<-2
y<-6*x^2+5*x+2
y
x<-2
y<-3
z<-(((x+y)^2)/2)^0.5
z
k<-"Ha ha ha!!"
l<-"Ghatiya joke !"
paste(k,l)
d1<-as.Date("2018-11-10")
typeof(d1)
#User Input programs 

a<- readline(prompt = "Enter a: ")
a
a<- as.integer(readline(prompt = "Enter a: "))
print(a)

# Data frame

q1 <- c(12,45,25,26,28,35,36,27,19,35)
class(q1)
length(q1)
q2 <- c(14,23,56,37,27,41,37,28,30,32)
q3 <- q1+q2
q3
min(q3)
mean(q3)
median(q3)
sd(q3)
sum(q3)
sort(q3)
sort(q3, decreasing = TRUE)
rank(q3)
order(q3)
order(sort(q3))

# Assigning levels

l <- c("e1","e2","e3","e4","e5","e6","e7","e8","e9","e10")
names(q3) <- l
q3


QS <- c(20,50,40,35)
PPU <- c(20.5,30,28,29.5)
Region <- c("N","E","S","W")
Amount <- QS*PPU
names(Amount) <- Region
Amount

q3[5]
q3[3:7]
q3[c(2:4,7)]
q3[-5]
q3[q3>50]
q3[q3>50&q3<80]

# Conversion to a factor

r1 <- c("N","E","S","W","N")
r1
class(r1)
typeof(r1)

r2 <- factor(r1,levels = c("N","E","S","W")) # Its still nominal
r2
class(r2)
typeof(r1)

# For ordinal :

emp1 <- c("SE","SM","SE","RM","SM")
emp2 <- factor(emp1, levels = c("SE","SM","RM"), ordered = TRUE)
emp2
typeof(emp2)
class(emp2)


# Check Null Values :

is.na(q3)
sum(is.na(q3))


###################################################################################
# CLASS OF 25/10/2019 -------------------------------------------------------------
###################################################################################

Q1<-c(12,45,15,26,35,36,27,19,25)
class(Q1)
length(Q1)
typeof(Q1)
Q2<-c(14,23,34,45,56,67,78,89,98)
Q3<-Q1+Q2
Q3

min(Q3)
max(Q3)
mean(Q3)
median(Q3)
sd(Q3)
sum(Q3)
sort(Q3)
rank(Q3)

Q3[Q3>50]
Q3[Q3>40 & Q3<80]

r2<-c("N","S","N","E","W")
R1<-factor(r2,levels = c("N","E","S","W"), ordered= TRUE)
typeof(R1)
class(R1)
R1


#CLASS 3
v1<-c(2,8,10,15)
class(v1)
#Combine has to be small C
v3<-c(5,10,"A",6)
class(v3)
#character will take precedence and all values will be converted to character
v4<-c(5,10,15,TRUE)
typeof(v4)
v5<-c(5,10,15,TRUE,"A")
class(v5)
#If character is present then that will take precedence over all the types.
v1<-c(15L,14L,16L,18L,19L,20L)
typeof(v1)
v2<-c(13,14,15,16,17,19)
typeof(v2)
v3<-as.Date(c("2012-09-26","2013-09-26","2014-09-26","2015-09-26"))
class(v3)
v4<-c(TRUE,FALSE,TRUE,FALSE,TRUE,FALSE)
class(v4)

# Summing up after removing elements

v5<-c(35,76,98,NA,789)
sum(is.na(v5))
mean(v5,na.rm = TRUE)


v6<-c(30,40,50,60,53,43)
rank(v6)
order(v6)

#Rank is based on the ranking of the numbers in ascending order
#Order will be on the numbers in whichever order position the minimum number is

v6[3:6]

X<-c(20,19,37,53,82,64,45,93,61,20,38,45,37,64,11)
typeof(X)
class(X)
length(X)
max(X)
min(X)
mean(X)
sort(X,decreasing = TRUE)
X[5]
X[X>80|X<15]

Y<-c(2,5,4)
Z<-c(X,Y)
Z

X4<-X+Y
X4

Customer_category<-c("Silver","gold","Diamond","Silver","gold","Diamond")
Customer<-factor(Customer_category)


Customer<-factor(cust.levels = c("Silver","gold","Diamond"),ordered = TRUE)
Customer

D= as.integer(readline(prompt = "enter distance"))
S= as.integer(readline(prompt = "enter Speed"))
T<-(D/S)
T

print(paste("total time taken to cover=",T))

#SESSION -2 

vect2 <- c(1:100)
vect2

#Matrix

# Default is by column filling
mat1 <- matrix(c(1:20),nrow=2,ncol = 5)
mat1

# If we want to fill by row
mat2 <- matrix(c(1:20),nrow=2,ncol = 5, byrow = TRUE)
mat2

mat3 <- matrix(c(1:30), nrow = 3, ncol = 5)
mat3

mat1+mat2
mat2+mat3     #Not possible to add here like vectors


# Putting names to the row and column names by "dimnames"

m1 <- matrix(sample(10:100,25), nrow = 5, ncol = 5, byrow = TRUE,
             dimnames = list(c("r1","r2","r3","r4","r5"),c("c1","c2","c3","c4","c5")))
m1

# Using set.seed for controlling randomisation of sample

set.seed(12345)
m2 <- matrix(sample(10:100,25), nrow = 5, ncol = 5, byrow = TRUE,
             dimnames = list(c("r1","r2","r3","r4","r5"),c("c1","c2","c3","c4","c5")))
m2

m2[2,4]
m2[2:4,3:5]

# Array  -- 3 Dimensions

ar1 <- array(sample(10:80,27),dim=c(3,3,3))
ar1   # output is shown in three levels

# Data frame

empid <- c("emp01","emp02","emp03","emp04","emp05")
employee <- c("Shakshi", "Muskaan", "Aishwariya", "Abhinav", "Paridhi")
salary <- c(5000,2000,3500,200,500)
designation <- factor(c("SE","SM","RM","SE","RM"),levels = c("SE","SM","RM"),ordered = TRUE)

empframe <- data.frame(empid,employee,salary,designation)
empframe

# Structure of a data frame

str(empframe)

# to prevent the data entry from being factor, rather take it as string :

empframe <- data.frame(empid,employee,salary,designation,stringsAsFactors = FALSE)
empframe

str(empframe)
class(empframe)
dim(empframe)

ename1 <- empframe$employee
ename1

# adding quantity sold as a column

qs <- c(20,45,43,35,68)
empdf1 <- cbind(empframe,qs)
empdf1

# adding another entry using rbind

new1 <- list("emp06","Souvik", 700, "SE", 76) 
empdf2 <- rbind(empdf1,new1)
empdf2

typeof(empdf2[6,3])

###################################################################################
# CLASS OF 15/11/2019 -------------------------------------------------------------
###################################################################################

# Managing Data frames

Name <- c("Amit","Kiran","Tarun","Neelesh","Sonam")
Gender <- c("M","F","M","M","F")
Age <- c(32,21,24,45,36)
df1 <- data.frame(Name,Gender,Age,stringsAsFactors = FALSE)

summary(df1)

Name<- c("Tarun","Amit","Kiran","Sonam","Neelesh")
Working <- c("Yes","No","Yes","No","Yes")
df2 <- data.frame(Name,Working,stringsAsFactors = FALSE)

df1
df2
Name
# Using cbind, it will not work
df3 <- cbind(df1,df2)
# Using merge function
df4 <- merge(df1,df2, by.x = "Name", by.y = "Name")
df4
# Sort function

df5 <- df4[order(df4$Age),]
df5

# Converting continuous variable into groups
# 1. Cut the data you want to make group for, using breaks

df6 <- cut(df4$Age, breaks = 3)
df6

# 2. Using custom bins

AgeGroup <- cut(df4$Age,c(20,30,40,Inf),labels = c("A","B","C")) # lower limit is "less than", upper limit is "less than equals" ! !

df7 <- cbind(df4,AgeGroup)
df7

# Subsetting

df8 <- subset(df4,Working=="Yes")
df8

df8 <- subset(df4,Working=="Yes"& Age>30)
df8


# Control Statements ------ (IF)  (IF ELSE)  (Nested if --> ELSE IF)

temp <- 34

# IF

if(temp>40){
  print("Hot")
}

# IF ELSE

if(temp>40){
  print("Hot")
}else {print("Normal")}

# ELSE IF

if(temp>40){
  print("Hot")
}else if(temp<0){
  print("Cold")
}else{print("Normal")}

# User defined function :

temp1 <- function(temp=0){
  if(temp>40)
  {
    print("Hot")
  }
  else if(temp<0)
  {
    print("Cold")
  }
  else{print("Normal")}
}
temp1(-10)
temp1(32)
temp1(46)
temp1()

# Price vs quantity discount

prc <- function(q=1,pr=100){
  if(q>100)
  {
    pr=pr*0.85 # 15% discount
  }
  else if(q>80)
  {
    pr=pr*0.9 # 10% discount
  }
  else if(q>40)
  {
    pr=pr*0.95 # 5% discount
  }
  print(paste("Price =",pr))
}

prc(45,100)
prc(85,100)
prc(105,100)
prc()


# Ifelse

temp <- -30
ifelse(temp<0,"Cold","Normal")

# Switch

a <- 10
b <- 5
operator <- readline(prompt = "Enter Operator :")
switch(operator,
       "+" = a+b,
       "-" = a-b,
       "/" = a/b)

# While loop

c <- 0
while(c<5){
  print("Hello")
  c=c+1
}

# Even number generator upto 20

c <- 2
while(c<=20){
  print(c)
  c=c+2
}  


# For loop

for (i in 1:5){
  print("Hello")
}


x <- c(2,5,6,8,3,9)
l <- length(x)
for (i in 1:l){
  v <- ifelse(x[i]>5,"Yes","No")
  print(v)
}


# Packages and libraries

s <- c("Laptop","Printer","Mouse")
str_to_upper(s)  

# Installing package "stringr"

install.packages("stringr")
library("stringr")
str_to_upper(s)  
str_to_lower(s)  

data()
View(airquality)


data=airquality
data

dim(data)
str(data)
head(data,n=10)
sum(is.na(data))
sum(is.na(data$Ozone))
sum(is.na(data$Solar.R))
sum(is.na(data$Wind))
sum(is.na(data$Temp))
summary(data)
summary(data$Ozone)

# Replacing missing values by mean values

data$Ozone[is.na(data$Ozone)] <- mean(data$Ozone,na.rm = T)
data

###################################################################################
# CLASS OF 22/11/2019 -------------------------------------------------------------
###################################################################################

# Basic graphs

aq <- airquality

# For plotting whole dataset
plot(aq)

# Histogram
hist(aq$Ozone)
?hist()
hist(aq$Ozone, ylim = c(0,60))
hist(aq$Ozone, main = "Ozone Histogram",
     xlab = "Ozone",
     col = "lightblue",
     labels = T,
     breaks = 10,
     ylim = c(0,60))
hist(aq$Ozone, main = "Ozone Histogram",
     xlab = "Ozone",
     col = "lightblue",
     labels = T,
     breaks = 5,
     ylim = c(0,60))

plot(aq$Ozone, type = "l")
plot(aq$Ozone, type = "h")

# Boxplot
boxplot(aq$Ozone)


plot(aq$Month,aq$Ozone)

# Splitting data monthwise

s<- split(aq, aq$Month)
s
typeof(s)
class(s)

# lapply function

lapply(s,function(x){
  colMeans(x[,c("Ozone","Wind")],na.rm = T)
})

# sapply function

sapply(s,function(x){
  colMeans(x[,c("Ozone","Wind")],na.rm = T)
})

# tapply function

tapply(aq$Temp,aq$Month,mean)  # Note, variable should be categorical otherwise error will be there

# dplyr and tidyverse package  - advanced case

#install.packages("dplyr")
#install.packages("tidyverse")

library(dplyr)
library(tidyverse)

# dplyr :
# mutate()  select()  filter()  summerise()   arrange()


select(aq,Ozone,Wind)

aq %>% select(Ozone,Wind,Temp,Month)

aq %>% filter(Month==7|Month==9)

aq %>% mutate(Temp_Cat=ifelse(Temp>90,"H","N"))

aq %>% group_by(Month) %>%
  summarize(mean(Ozone))


aq %>% select(Ozone,Wind,Temp,Month) %>%
  filter(Month==7|Month==9)%>%
  mutate(Temp_Cat=ifelse(Temp>90,"H","N"))%>%
  group_by(Month)%>%
  summarize(Mean_Ozone=mean(Ozone))%>%
  arrange(Mean_Ozone)




###################################################################################
# CLASS OF 29/11/2019 -------------------------------------------------------------
###################################################################################


# Packages :  readr   readxl     RCurl     Amelia     Rcpp     Hmisc

#install.packages("readr")
#install.packages("readxl")
#install.packages("RCurl")
#install.packages("Amelia")
#install.packages("Rcpp")
#install.packages("Hmisc")

library(readr)
library(readxl)
library(RCurl)
library(Amelia)
library(Rcpp)
library(Hmisc)


# Importing data :

getwd()
setwd("C:/Users/souvi/Documents/R/IM507")

# Read CSV files :
d1 <- read.csv("C:/Users/souvi/Documents/R/IM507/airtravel.csv", stringsAsFactors = F)
d1
View(d1)
str(d1)

# Read TXT files :

d2 <- read.delim("C:/Users/souvi/Documents/R/IM507/airtravel 1.txt", stringsAsFactors = F)
d2

# Read table :

d3 <- read.table("C:/Users/souvi/Documents/R/IM507/airtravel 2.txt", header = T, sep = "/", stringsAsFactors = F)
d3


#-----------------------------------------------------------------------------------


# Same can be done by readr package :

read_csv("C:/Users/souvi/Documents/R/IM507/airtravel.csv")
read_tsv("C:/Users/souvi/Documents/R/IM507/airtravel 1.txt")

#-----------------------------------------------------------------------------------

# Reading xls files : readxl

excel_sheets("C:/Users/souvi/Documents/R/IM507/urbanpop.xls")       # see multiple sheetnames
read_excel("C:/Users/souvi/Documents/R/IM507/urbanpop.xls")         # default = 1st sheet
read_excel("C:/Users/souvi/Documents/R/IM507/urbanpop.xls", sheet = "1967-1974")

# From environment section, > import dataset > browse ----> GUI approach

# Use UCI ML repository for most of the data sets

# importing datasets directly from UCI URL

d4 <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data")
               ,header = F,
               col.names = c("Sepal.length", "Sepal.width","petal.length","petal.width","species"))

head(d4)

# Export data from R to the working directory :

write.csv(airquality,file = "aq.csv")

# While importing from excel you can change the time of data at the point of importing from the column dropdown


###################################################################################
# BIKE SHARING DATA -------------------------------------------------------------
###################################################################################



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


###################################################################################
# AIR QUALITY DATA -------------------------------------------------------------
###################################################################################

# Write  R code to call the (built-in) dataset airquality. 

aq <- airquality

# Check whether it is a data frame or not? 

is.data.frame(aq)

# How many records and variables are there in the dataset?

dim(aq)

# Identify the datatypes of each variable.

str(aq)

# Are there any missing values in the data set? If yes, then identify which variable has missing value.

summary(aq)     # Ozone and Solar.R shows NA values, i.e. missing data

# Total number of missing values

sum(is.na(aq))

# Replace the missing values with the mean of the variable. How many values are you replacing for each variable and with what value of mean?

aq$Ozone[is.na(aq$Ozone)] <- mean(aq$Ozone,na.rm = T)
aq$Solar.R[is.na(aq$Solar.R)] <- mean(aq$Solar.R,na.rm = T)

# Identify min, maximum, mean, median standard deviation, variance, and NA values for each variable. Explain each variable with the help of these values.

summary(aq)

# Order the entire data frame by Temp and Wind column.

aq1 <- aq[order(aq$Temp),]
aq2 <- aq[order(aq$Wind),]
aq1
aq2

# Create a new dataframe by removing the variables 'Solar.R' and 'Wind' and display the new data frame.

aq
aq3 <- aq[c(-2,-3)]
aq3

# Write a code to display all the values of temperature variable only.

aq$Temp


aqs <- subset(aq,aq$Month==6&aq$Temp>80)
aqs

###################################################################################
# MISSING VALUE DATA -------------------------------------------------------------
###################################################################################


# R approach of treating missing values -
aq <- airquality

sum(is.na(aq))
# Find out were is the missing data -
sapply(aq, function(x)sum(is.na(x)))

# row wise check how many missing values are there 
rowSums(is.na(aq))

# Graphical representation of missing values -
# Libraries - Amelia and Rcpp
library(Amelia)
library(Rcpp)

missmap(aq, main = "Missing Value Map")

# These missing values are to be omitted or replaced by mean, or some specific values

# using Hmisc library for treating missing values-
#install.packages("Hmisc")

library(Hmisc)

# Replacing values approach -
aq$Ozone <- with(aq, impute(Ozone,9)) #Replacing missing values with specific value i.e. 9 (say)
missmap(aq, main = "Missing Value Map")

# Omitting records approach -

aq2 <- airquality
sum(is.na(aq))
dim(aq2)

aq2 <- na.omit(aq2)
dim(aq2) # As some records have been removed, dimension will change


# Identifying outliers and treating them ----
# Visualizing by boxplot ---
library(ggplot2)

boxplot()



###################################################################################
# GGPLOT2 IN DEPTH -------------------------------------------------------------
###################################################################################

#------------ Aesthetics

library(ggplot2)

ggplot(data=movies, aes(x=CriticRating, y=AudienceRating))

#------------ Geometry

ggplot(data=movies, aes(x=CriticRating, y=AudienceRating)) +
  geom_point()

#------------ Color

ggplot(data=movies, aes(x=CriticRating, y=AudienceRating,
                        colour=Genre)) +
  geom_point()

#------------ add size

ggplot(data=movies, aes(x=CriticRating, y=AudienceRating,
                        colour=Genre, size=Genre)) +
  geom_point()

#------------ add size in a better way

ggplot(data=movies, aes(x=CriticRating, y=AudienceRating,
                        colour=Genre, size=BudgetMillions)) +
  geom_point()

#>>> This is #1 (We will improve it)

p <- ggplot(data=movies, aes(x=CriticRating, y=AudienceRating,
                             colour=Genre, size=BudgetMillions))
# Points
p + geom_point()
# Lines
p + geom_line()
# Multiple Lines
p + geom_line() + geom_point()

# Overriding Aesthetics

q <- ggplot(data=movies, aes(x=CriticRating, y=AudienceRating,
                             colour=Genre, size=BudgetMillions))
# ad geom layer

q + geom_point()

#Oveerriding aes
#Ex1
q + geom_point(aes(size=CriticRating))

#Ex2

q + geom_point(aes(color=BudgetMillions))

#Ex3

q + geom_point(aes(x=BudgetMillions)) +
  xlab("Budget Millions $$$") # For renaming the axis name from the original q

#Ex4

p + geom_line() + geom_point()

# Reduce line size

p + geom_line(size=1) + geom_point()

# Mapping vs Setting

#Mapping
p + geom_point(aes(color=Genre))

#Setting
p + geom_point(color="DarkGreen")  #We can't put aes for setting

#Error
p + geom_point(aes(color="DarkGreen"))


# Histograms and Density charts

s <- ggplot(data=movies, aes(x=BudgetMillions))
s + geom_histogram(binwidth = 5)

# Add Color
# Setting Color
s + geom_histogram(binwidth = 5, fill="green")

# Mapping Color
s + geom_histogram(binwidth = 5, aes(fill=Genre))

# Adding border to the colors

s + geom_histogram(binwidth = 5, aes(fill=Genre), color="Black")

#>>> 3 (We will improve it)

# Density Charts 
s + geom_density(aes(fill=Genre)) # Overlapping

s + geom_density(aes(fill=Genre), position = "stack") # Stacking

# Starting layer tips

t <- ggplot(data=movies, aes(x=AudienceRating))
t + geom_histogram(binwidth = 40,
                   fill="White", color="blue")
# Another way
t <- ggplot(data=movies)
t + geom_histogram(binwidth = 40,
                   aes(x=AudienceRating),
                   fill="White", color="blue")

# Statistical Transformation

u <- ggplot(data=movies, aes(x=CriticRating,
                             y=AudienceRating, color=Genre))
u + geom_point() + geom_smooth(fill=NA)

# Boxplot

u <- ggplot(data=movies, aes(x=Genre,
                             y=AudienceRating, color=Genre))
u + geom_boxplot()
u + geom_boxplot(size=1.2)
u + geom_boxplot(size=1.2) + geom_point()

# Trick / hack

u + geom_boxplot(size=1.2) + geom_jitter()

# Another way 

u + geom_jitter() + geom_boxplot(size=1.2, alpha=0.5)

# Using facets

v <- ggplot(data=movies, aes(x=BudgetMillions))
v + geom_histogram(binwidth=10, aes(fill=Genre),
                   color="black")
# Facets
v + geom_histogram(binwidth=10, aes(fill=Genre),
                   color="black") + 
  facet_grid(Genre~., scales = "free")

# Scatterplots

w <- ggplot(data=movies, aes(x=CriticRating,
                             y=AudienceRating, color=Genre))
w + geom_point(size=3)

# Facets

w + geom_point(size=3) + facet_grid(Genre~.)

w + geom_point(size=3) + facet_grid(.~Year)

w + geom_point(size=3) + facet_grid(Genre~Year)

w + geom_point(size=3) +
  geom_smooth() +facet_grid(Genre~Year)

w + geom_point(aes(size=BudgetMillions)) +
  geom_smooth() +facet_grid(Genre~Year)

# Coordinates : (Limits, Zoom)

m <- ggplot(data=movies, aes(x=CriticRating, y=AudienceRating,
                             size=BudgetMillions, color=Genre))
m + geom_point()

m + geom_point() +
  xlim(50,100) +
  ylim(50,100)

#Won't work always

n <- ggplot(data=movies, aes(x=BudgetMillions))
n + geom_histogram(binwidth=10, aes(fill=Genre),
                   color="black")+
  ylim(0,50)

#Instead - zoom
n + geom_histogram(binwidth=10, aes(fill=Genre),
                   color="black")+
  coord_cartesian(ylim=c(0,50))

# Improved graph

w + geom_point(aes(size=BudgetMillions)) +
  geom_smooth() +facet_grid(Genre~Year) +
  coord_cartesian(ylim = c(0,100))

# Themes

o <- ggplot(data=movies, aes(x=BudgetMillions))
h <- o + geom_histogram(binwidth = 10, aes(fill=Genre), color="Black")
h
# Axis Lebel

h +
  xlab("Money Axis") +
  ylab("Number of Movies")

# lebel formatting

h +
  xlab("Money Axis") +
  ylab("Number of Movies") +
  theme(axis.title.x = element_text(color = "darkgreen", size = 30) ,
        axis.title.y = element_text(color = "darkred", size = 30) )

# Tick mark formatting

h +
  xlab("Money Axis") +
  ylab("Number of Movies") +
  theme(axis.title.x = element_text(color = "darkgreen", size = 30),
        axis.title.y = element_text(color = "darkred", size = 30),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))

# Legend formatting

h +
  xlab("Money Axis") +
  ylab("Number of Movies") +
  theme(axis.title.x = element_text(color = "darkgreen", size = 30),
        axis.title.y = element_text(color = "darkred", size = 30),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        
        legend.title = element_text(size = 30),
        legend.text = element_text(size = 20),
        legend.position = c(1,1),
        legend.justification = c(1,1))


# Title to the Plot :

h +
  xlab("Money Axis") +
  ylab("Number of Movies") +
  ggtitle("Movie Budget Distribution")+
  theme(axis.title.x = element_text(color = "darkgreen", size = 30),
        axis.title.y = element_text(color = "darkred", size = 30),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        
        legend.title = element_text(size = 30),
        legend.text = element_text(size = 20),
        legend.position = c(1,1),
        legend.justification = c(1,1),
        
        plot.title = element_text(color = "Darkblue",
                                  size = 40,
                                  family = "courier"))

###################################################################################
# ASSIGNMENT WORK ----------------------------------------------------------------
###################################################################################

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


###################################################################################
# PROJECT WORK ----------------------------------------------------------------
###################################################################################

# Importing the dataset -----------------------------------------------------------

d1 <- read.csv("C:/Users/souvi/Documents/R/IM507/apy.csv")

View(d1)
head(d1)
dim(d1)
names(d1)
summary(d1)
str(d1)
class(d1$Production)
# Class of Production has been taken as factor which needs to be converted to double
d1$Production <- as.double(d1$Production)
str(d1)
class(d1$Production)
typeof(d1$Production)

#Check for missing values -----
sum(is.na(d1))

# No missing values in the raw data. So, initially no need for treatment.

# Analysis of data -----

library(dplyr)
library(tidyr)

# Rearrenging data state wise and year wise

d_year <- d1 %>% group_by(Crop_Year,State_Name)%>%
  summarize(Total_Production = sum(Production))

yrr <- spread(d_year, key = "State_Name", value = "Total_Production" )

yrr

# yrr shows the missing records, also 2005 has all records. So, let us subset all data of 2005 to analyse crop-wise.
# Subsetting 2005 ---

y_2005 <- d1 %>% filter(Crop_Year==2005)

# Finding out highest produced crop ---

y_2005_crop <- y_2005 %>% group_by(Crop = Crop) %>% 
  summarize(Total = sum(Production)) %>% arrange(desc(Total))

head(y_2005_crop)
# Rice is the highest produced crop for 2005. So, let us analyse the data for Rice for all states and crop years.

# Subsetting data for Rice ---

rice <- d1 %>% filter(Crop=="Rice")

# Rearrenging data year and state wise ---

rice_state <- rice %>% group_by(Crop_Year,State_Name)%>%
  summarize(Total_Production = sum(Production))

yrr_rice <- spread(rice_state, key = "State_Name", value = "Total_Production" )

dim(yrr_rice)

# Check for missing values ---

sum(is.na(yrr_rice))

# Year wise missing values - in the data we can see so many missing values in 2015, so we should remove record for 2015 which is the last row
nrow(yrr_rice)

yrr_rice <- yrr_rice[-19,]

# State wise missing values ---
sapply(yrr_rice, function(x)sum(is.na(x)))


# There are records for 18 years where Andaman Nikobar and Jharkhand has more than 50% missing values
# Removing columns for Andaman Nikobar and Jharkhand

rice_new <- yrr_rice %>% select(-`Andaman and Nicobar Islands`,-Jharkhand)

sum(is.na(rice_new))
sapply(rice_new, function(x)sum(is.na(x)))

# Filling missing values with mean 

#install.packages("imputeTS")

library(imputeTS)
rice_new <- na_mean(rice_new)

sum(is.na(rice_new))

# Now, missing values have been treated 

# Plotting using ggplot2

library(ggplot2)

# Restructuring the data for further analysis -

rice_str <- gather(rice_new, key = "State_Name", value = "Production",c(-Crop_Year))

# Plotting rice_new (Treated dataset) for further analysis -
# Plotting using ggplot2

# Total production of top 5 states in rice production throughout years -

# Finding out top 5 rice producers -

rice_str %>% group_by(State = State_Name) %>%
  summarize(Total = sum(Production)) %>% arrange(desc(Total))

# Bihar, Uttar Pradesh, Assam, Odisha, Karnataka are the top 5

# Year-wise rice production for Bihar - 
rice_str %>% filter(State_Name=="Bihar") %>%
  ggplot(aes(Crop_Year,Production))+
  geom_bar(stat="identity", color="black", fill = "light blue") +
  ggtitle("Year-wise rice production for Bihar") +
  theme_classic()

# Year-wise rice production for Uttar Pradesh - 
rice_str %>% filter(State_Name=="Uttar Pradesh") %>%
  ggplot(aes(Crop_Year,Production))+
  geom_bar(stat="identity", color="black", fill = "light blue") +
  ggtitle("Year-wise rice production for Uttar Pradesh") +
  theme_classic()

# Year-wise rice production for Assam - 
rice_str %>% filter(State_Name=="Assam") %>%
  ggplot(aes(Crop_Year,Production))+
  geom_bar(stat="identity", color="black", fill = "light blue") +
  ggtitle("Year-wise rice production for Assam") +
  theme_classic()

# Year-wise rice production for Odisha - 
rice_str %>% filter(State_Name=="Odisha") %>%
  ggplot(aes(Crop_Year,Production))+
  geom_bar(stat="identity", color="black", fill = "light blue") +
  ggtitle("Year-wise rice production for Odisha") +
  theme_classic()

# Year-wise rice production for Karnataka - 
rice_str %>% filter(State_Name=="Karnataka") %>%
  ggplot(aes(Crop_Year,Production))+
  geom_bar(stat="identity", color="black", fill = "light blue") +
  ggtitle("Year-wise rice production for Karnataka") +
  theme_classic()


# Finding out top 5 highest produced crops for 2005 and graphically finding its relation to season

# IN 2005 most produced crops are - 
head(y_2005_crop)

# Top 5 produced crops are - Rice, Maize, Moong(Green Gram), Sesamum, Urad

# yrr_rice shows that 2004, 2005 has data for all the states. So let us plot for 2004 and 2005

# Finding relation between produced crop with respect to season -

# Production of Rice -
d1 %>% filter(Crop_Year==c(2004,2005),Crop=="Rice") %>%
  ggplot(aes(Production))+
  geom_histogram(color="black",aes(fill=Season)) + theme_classic()+
  facet_grid(Crop_Year~., scales = "free")

# Production of Maize -
d1 %>% filter(Crop_Year==c(2004,2005),Crop=="Maize") %>%
  ggplot(aes(Production))+
  geom_histogram(color="black",aes(fill=Season)) + theme_classic()+
  facet_grid(Crop_Year~., scales = "free")

# Production of Moong(Green Gram) -
d1 %>% filter(Crop_Year==c(2004,2005),Crop=="Moong(Green Gram)") %>%
  ggplot(aes(Production))+
  geom_histogram(color="black",aes(fill=Season)) + theme_classic()+
  facet_grid(Crop_Year~., scales = "free")

# Production of Urad -
d1 %>% filter(Crop_Year==c(2004,2005),Crop=="Urad") %>%
  ggplot(aes(Production))+
  geom_histogram(color="black",aes(fill=Season)) + theme_classic()+
  facet_grid(Crop_Year~., scales = "free")

