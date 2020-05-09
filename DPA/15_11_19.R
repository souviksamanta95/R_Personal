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
