# Understanding basic distribution of data using histogram
#cars data - mpg

#########
cars
head(cars)
names(cars)

datasets::iris
head(iris)




##HANDLING MISSING DATA

#Inserting missing values
cars[2,2]=cars[4,4]=NA
head(cars)

#Replacing with mean (for numeric values)
cars[2,2]=mean(na.omit(cars$cylinders))

#Replacing with mode (for both numeric and character)
cars[2,2]=max(na.omit(cars$cylinders))
cars[4,4]=max(cars$brand)

#starwars dataset (inbulit) - Identify and Insert appropriate values in NA
# in each column
rm(list = ls())
library(quantmod)
getSymbols("NTPC.NS",from="2010-11-21",to="2019-11-21")

head(NTPC.NS)

price=NTPC.NS$NTPC.NS.Adjusted

price[which(is.na(price))]=mean(na.omit(price))

wage=Gender_Wage_Gap$lwage
female=Gender_Wage_Gap$female
exper=Gender_Wage_Gap$exper
exper_sqr=Gender_Wage_Gap$expersq

summary(lm(wage~female+exper+exper_sqr))


which(is.na(price1))

head(NTPC.NS)

mean(close)

close=NTPC.NS$NTPC.NS.Adjusted
which(is.na(close))
close[which(is.na(close))]=mean(na.omit(close))
mean(close)
##
summary(lm(Gender_Wage_Gap$lwage~Gender_Wage_Gap$female+Gender_Wage_Gap$exper+Gender_Wage_Gap$expersq))

#SMOKING DATASET

library(tseries)
get.hist.quote(c("MSFT","IBM"),start = "2010-11-11",end="2019-11-21",
               compression = "w",retclass = "zoo",quote = "AdjClose")


disp=mtcars$disp
hist(disp,freq = FALSE)
curve(dnorm(x,mean = mean(disp),sd=sd(disp)),
      add=TRUE)

qqnorm(disp)
qqline(disp)

library(fBasics)
basicStats(disp)
tickers=c("MSFT","IBM")
get.hist.quote(instrument=tickers,start = "2000-01-01",end="2019-11-11",auto.assign=FALSE,
               compression = "w",retclass = "zoo",quote = "Close",method = "auto")
###




cig_price=SMOKING$cigpric
hist(cig_price,freq = FALSE)
curve(dnorm(x,mean = mean(cig_price),sd=sd(cig_price)),
      add=TRUE)
qqnorm(cig_price)
qqline(cig_price)

##################################################################################
#Download NTPC.NS price from 21 Nov 2010 till date and identify missing values and 
#replace them with mean

# Refer to Gender Wage data - Analyse when lnwage = f(female, exper, exper^2)

# Data Distribution
#
#Histogram and qq plots

#PDF and CDF

disp=mtcars$disp
hist(disp,freq = FALSE)
lines(density(disp))
curve(dnorm(x,mean = mean(disp),sd=sd(disp)),
      add=TRUE)

qqnorm(disp)
qqline(disp)


hist(mtcars$disp,freq = FALSE)
lines(density(mtcars$disp))
curve(dnorm(x,mean = mean(mtcars$disp),sd=sd(mtcars$disp)),
      add=TRUE)
plot(ecdf(mtcars$disp),cex=0)

#qq plots
qqnorm(mtcars$disp)
qqline(mtcars$disp)
#

#Download Microsoft Closing prices and plot the histogram and normal curve of prices
library(quantmod)
getSymbols("MSFT",from="2010-11-11",to="2019-11-11")
price=MSFT$MSFT.Adjusted
hist(price,freq = F)
curve(dnorm(x,mean=mean(price),sd=sd(price)),add=T)

##QQ Plots
qqnorm(price)
qqline(price)
# add a normal distribution line in histogram


hist(iris$Petal.Length, freq=FALSE)
curve(dnorm(x, mean=mean(iris$Petal.Length), sd=sd(iris$Petal.Length)), add=TRUE, col="red") #line
##
#Understanding Basic Statistics of data

library(fBasics)
basicStats(cig_price)

library(fBasics)
basicStats(mtcars$disp)
#
?get.hist.quote



#################################################
# DATA TRANSFORMATION
disp
qqnorm(disp)
min_max=(disp-min(disp))/range(disp)
qqnorm(min_max)
qqline(min_max)

Z_score=(disp-mean(disp))/sd(disp)

max(abs(disp))
decimal=disp/10^3
qqnorm(decimal)
qqline(decimal)

#





max(abs(cig_price))

decimal=cig_price/10^2

z_score=(cig_price-mean(cig_price))/sd(cig_price)

log123=log(cig_price)


disp=mtcars$disp


#########################################################
#OUTLIERS and its identification
ol=c(400,412,410,405,416,1000,2)
qqnorm(ol)
qqline(ol)

#Let's plot it against a series from 1 to 7 (length of ol)
x_axis=(1:length(ol))
plot(x_axis,ol)
identify(x_axis,ol) #Visual identification (not suitable for large datasets)
##
## In order to remove and store new variable
remover=function(x)
{
  x_axis=c(1:length(x))
  plot(x_axis,x)
  outliers=identify(x_axis,x)
  x1=x[-outliers]
  qqnorm(x1)
  qqline(x1)
  return(x1)
  
}
remover(ol)
#store the above in a new variable
v=remover(ol)
v
par("mar")
par(mar=c(2,2,2,2))
#Use any dataset to identify and remove outliers using above method

###
#Another way to deal with outliers is IQR
#Create a random sample
rm(list=ls())
data=c(sample(1:20,size = 40,replace = TRUE))
x_axis=c(1:length(data))

data
#Let's add two outliers above
data=c(sample(1:20,size = 40,replace = TRUE),65,150)
data

qqnorm(data)
qqline(data)
basicStats(data)

bench1=5.25-1.5*IQR(data)
bench2=17+1.5*IQR(data)

new_data=data[data>bench1 & data<bench2]
new_data
summary(data)
#Suppose you want to remove all variables above the IQR
benchmark=16+1.5*IQR(data)
benchmark
data[data<benchmark]

data1=data[data<benchmark]
data1
qqnorm(data1)
qqline(data1)
##
summary(data)
bench1=6.25-1.5*IQR(data)
bench1

data2=data[data<benchmark & data>bench1]
data2
qqnorm(data2)
qqline(data2)




#1. Running R Programme- www.cran.r-project.org
#Latest version 3.6.0
#Installing packages - from CRAN and from local files (C:, D: etc)
#Creating your own directory
#Datasets in R
#1. mtcars
#2. AirPassengers
#3. carData::Arrests
#4. freeny
#5. JohnsonJohnson (Qtrly sales)

mtcars



#Using "head" and "tail" commands


#Exercise: Use "cars" dataset and find the head and tail of the data
#Using the same data, find the elements of first 7 rows and all columns
#Find the elements of rows between 7 and 11 and all columns
#Find the elements of 2, 5 and 10 row

freeny$
  
  
  
  
  
  
  ###### R As a calculator
  
  
  3+6
3-9
1/5

##To test whether a data has numerical value




###Logical
TRUE*5
FALSE*5

#Vectors
c(1,2,4,7)
c(1:7)
c("Mango","Apple")

#Exercise: Test whether c(1,2,4,7) is a numeric or not
#Test whether c("Mango","Apple") and c(1:7) are numeric

c(1,2,3)*3

which(is.na(c(1,2,NA,3,5,NA)))

missing=c(1,2,NA,3,5,NA)
which(is.na(missing))

clean=na.omit(missing)
rm(list=ls())





#You can perform mathematical operations on a vector
c(1,2,4,7)*3
c(1,2,4,7)+3
c(1,2,4,7)/3


length(c(10:100))



## With two or more vectors
c(1,2,4,7)+c(2,5,8,11)

c(1,2,4,7)+c(2,5,8,11) - c(11,19,11,87)

length(c(1,2,4,7))
#Checking length of a vector
length(c(1,2,4,7)+c(2,5,8,11))

#Vectors
#Storing of variables
vec1=c(1,2,4,7)
vec2=c(1:7)
vec1
rm(i)
rm(list=ls())
#Removing variables from list - one var and more than one var




#When you need to define a criteria in the vector
c(1:7, by=2) # This will not yield the result

seq(from=1, to=6, by=2)
seq(1,3, by=0.1)
seq(1,10, by=2)

#Mathematical operations on a vector
vec1*3
vec1+3
vec1/3

#Vectorised expressions
sum(1:4)
mean(1:4)

#Exercise: Create a vector containing 1 to 15 and multiply and 
#divide each of them with 3

#On the sane vector add and subtract 3
#Create a vetcor 10:100 and jump by 2 integers staring 10
#Create a vetcor 10:100 and jump by 2 integers staring 11
#Create one numeric and one character vector


mtcars[c(2,5,7),]

head(mtcars)
disp=mtcars$disp



############ Matrices
x1=c(1,2,3,4)
x2=c(3,6,5,7)
x3=c(11,12,14,16)

x4=cbind(x1,x2,x3)

x4

matrix(c(1:6),nrow = 3)


matrix(c(2,4,3,1,5,7),nrow = 3)
matrix(c(2,4,3,1,5,7),ncol = 3,byrow = T)
matrix(c(2,4,3,1,5,7),nrow = 3)

#You can also combine 2 or more vetors to get a matrix (Similar elements)
x1=c(1,2,3,4)
x2=c(5,4,3,7)
x3=c(11,12,13,16)
cbind(x1,x2,x3)
mtcars
displacement=mtcars$disp
displacement
##Class exercise
#1. Create a matrix with 10 elements (5 by 2) and (2 by 5)
#2 Create a matric with 6 characters (3 by 2) and (2 by 3)
#3. Create a matrix consisting of elements "Hello", "Morning",
#"Class", "DPA", "IMI Delhi", "India" in 2 by 3 and 3 by 2 matrix

#4. Create two vectors and combine them - in the first 
#include any 5 companies and in the secondinclude their profits for a particular year

## Missing Values and removing missing values
missing=c(1,3,4,6,7,NA,8,7,NA)

which(is.na(missing))

na.omit(missing)





#Storing of data in variables and performing analysis on that variable
x=c(1:6)
x

y=seq(from=1, to=20, by=4)
y
x^2

#1. Create two vectors a and b of same length and add them/ subtract them
#2. Create two vectors of sales of two companies for past 5 years from the same industry and calculate:
#a. Combine them in a common matrix 
#b.The average sales 
#c. Total sales






#ARRAYS
A=c(1,2,3,4)
arry1=array(A,dim = c(2,2))
arry1

B=array(1:20,dim = c(2,5,2))
B


JohnsonJohnson





#In arrays, you can break this matrix into two or more
C=array(1:20,dim = c(2,5,2))
C


#Exercise: Create an array of 10:40 in two different matrices with 2 rows each
#Create an array of 10:54 in three different matrices with 3 rows each

E=array(10:40,dim = c(2,5,3))
E

write.csv(E,"RESULTS.csv")





write.csv(E,"RESULTS.csv")



#Saving and importing your results
vector=c(1:20)
write.csv(vector,"Results.csv")


######################################
#DATA FRAMES

x=c(1:3)
y=c("Delhi","Bhubaneswar","Kolkata")

data.frame(x,y)


z=data.frame(x,y)
z
cbind(x,y)
#You can also select a specified element(s)in the data frame

z
#suppose you want to find row 3, col 2, which is Kolkata
z[3,2]

#Both columns of row 2
z[2,]

#Both rows of col 2
z[,2]


#You can also create a n by 3 dataframe
a=c("Ramesh","Kapil","Ravi","Kavita")
b=c("Maths","Arts","Sports","Science")#Subjects liked
c=c(13,15,11,9) #Age




d=data.frame(a,b,c)
d
#You can also find the dimensions of the dataframe
nrow(d)
ncol(d)
dim(d)

#You can extract elements by mentioning their row and col nos.- use SQUARE BRACKET
d[2,3]
d[,3]
d[2,]
d[2,1:2]


#Class Exercise: 
#1. Find if a,b,c are numeric
#2. Create a data frame of four companies of your choice with their sales, number of employees,
#Profit/loss and salary expenses
#2. Find the sales of all four companies
#3. Find the sales, no. of employees and salary of the 3rd company

rm(list=ls())

########################################################
#2nd Visit
#Factors
rm(list=ls())

head(mtcars)
summary(lm(mtcars$mpg~mtcars$hp+mtcars$cyl))



gender=factor(c("male","male","female","male"))
gender

nlevels(gender)

#
data = c("East","West","East","North","North","East","West","West",
         "West","East","North")

data=factor(c("East","West","East","North","North","East","West","West",
              "West","East","North"))

data
direction=factor(c("East","West","East","North","North","East","West","West","West","East","North"))
direction
is.factor(direction)

# Apply the factor function.
factor_data = factor(data)

print(factor_data)
print(is.factor(factor_data))
nlevels(factor_data)


#On creating any data frame with a column of text data, R treats the text column as categorical data 
#and creates factors on it.

# Create the vectors for data frame.
height = c(132,151,162,139,166,147,122)
weight = c(48,49,66,53,67,52,40)
gender = c("male","male","female","female","male","female",
           "male")
all=data.frame(height,weight,gender)
all

summary(lm(all$height~all$gender))

all=data.frame(height,weight,gender)
all



summary(lm(all$weight~all$gender))

all=data.frame(height,weight,gender)
lm(weight~gender)
summary(lm(mtcars$mpg~mtcars$cyl+mtcars$disp+mtcars$hp))

head(mtcars)
# Create the data frame.
input_data = data.frame(height,weight,gender)
print(input_data)

# Test if the gender column is a factor.
print(is.factor(input_data$gender))

# Print the gender column so see the levels.
print(input_data$gender)

rm(list=ls())

#Create a factor for 5 different colors


months = c("March","April","January","November","January","September",
           "October","September","November","August","November",
           "February","April")

months = factor(months)
table(months)

#Although "months" have an order starting January, this is not reflected in output.
#You need to define "ordered=TRUE" under levels as shown:

months = factor(months,levels=c("January","February","March","April","May","June","July","August","September",
                                "October","November","December"),ordered=TRUE)
table(months)
#The nos. denote frequencies

#Another example
id=c("1","2","3","4")
age=c(42,31,26,22)
gender=c(1,1,0,1)
skin_col=c("white" ,"Black" ,"white" ,"Black")

all=data.frame(age,gender,skin_col)
summary(lm(all$age~all$skin_col))

#Here "Skin col" is a Factor. 
# In the data frame command, factors have been assigned

all=data.frame(id,age,gender,skin_col)
all

lm(age~skin_col)
ls.str(all)  # This will print the structure of the data

a = factor (c(1, 2, 3, 2, 3, 2, 1), levels=1:4, labels=c("Small", "Medium", "Large", "Huge")) # Create it
a

b= factor (c(1, 2, 3, 2, 3, 2, 4), levels=1:4, labels=c("Small", "Medium", "Large", "Huge")) # Create it
b



# Factors help you to do statistical modelling of "categorical variables"
#However, mean, median etc. should not be calculated as they do not make any sense

#An example
cars
ls.str(cars)
data.frame(cars)
lm(cars$mpg~cars$brand)

##LISTS
#Lists are the R objects which contain elements of different types like 
#??? numbers, strings, vectors and another list inside it. 
#A list can also contain a matrix or a function as its elements. 
#List is created using list() function.

#Create a list containing strings, numbers, vectors and a logical
# values.
list_data = list("Red", "Green", c(21,32,11), TRUE, 
                 51.23, 119.1)

list_data

list_data = list(c("Jan","Feb","Mar"), matrix(c(3,9,5,1,-2,8), 
                                              nrow = 2),list("green",12.3))

list_data

# Give names to the elements in the list.
names(list_data) = c("1st Quarter", "A_Matrix", "A Inner list")
list_data

#Manipulating List Elements
#We can add, delete and update list elements as shown below. 
#We can add and delete elements only at the end of a list. 
#But we can update any element.

list_data[4] <- "New element"
print(list_data[4])
list_data

#Merging Lists
#You can merge many lists into one list by placing all the lists 
#inside one list() function.

# Create two lists.
list1 = list(1,2,3)
list2 = list("Sun","Mon","Tue")

# Merge the two lists.
merged.list = c(list1,list2)

# Print the merged list.
print(merged.list)

#Converting List to Vector
#A list can be converted to a vector so that the elements of the vector 
#can be used for further manipulation. All the arithmetic operations on
#vectors can be applied after the list is converted into vectors. 
#To do this conversion, we use the unlist() function. 
#It takes the list as input and produces a vector.

# Create lists.

list(1,3,5)
list(c(1,3,5))
list1 <- list(1:5)
print(list1)

list2 <-list(10:14)
print(list2)


list1+list2


# Convert the lists to vectors.
v1 <- unlist(list1)
v2 <- unlist(list2)

print(v1)
print(v2)

x1=c(1,2,3,4)
y1=c(11,13,16,16)

both=cbind(x1,y1)
both
colnames(both)=c("Col 1","Col 2")
rownames(both)=c("Row 1","Row 2","Row 3","Row 4")


# Now add the vectors
result <- v1+v2
print(result)

#list within list
list(c("a","b"),c(1:5),450,list(c(1:5),"XYZ"))


x1=c(1,2,3,4)
y1=c(11,12,13,14)







both=cbind(x1,y1)
both
colnames(both)=c("Col 1","Col 2")



##Renaming of column and row names
z=c(1:3)
x=c(2:4)

both=cbind(z,x)
colnames(both)=c("Col 1", "Col 2")
rownames(both)=c("Row 1","Row 2","Row3")

both



z1=cbind(z,x)
colnames(z1)=c("integers","Number")
z1
rownames(z1)=c("Row 1","Row 2","Row 3")
z1
###########################################################################
#CONTROL OR CONDITIONAL STATEMENTS

1==1
1>1
1>=1
1!=1

for( i in 1:5){
  print(i^2)
}

##Exercise: Write a for loop to print square of 1,2,5,11,18

x1=1
y1=2

if (x1==y1){
  print("Equal")
} else {
  print("Not Equal")
}

# Multiply each no.from 1 to 10 with 5
for (i in 1:10){
  print(5*i)
}

#Using While loops
#
A=0
while(A<10){
  print("Will Not Play")
  A=A+1
}

#Exercise: Let there are TWO teams A and B playing football. 
#Write a conditional statement
#1. to declare that team A wins
#2. to declare that if A wins, then it will play the finals, 
#else it will be B
#Hint: Give no. of goals scored by each team
A=11
B=10
if(A>10){
  print("Play")
}else{
  print("Will Not")
}


#Suppose you want that if it scores more than 10, it will play
A=0
while(A<=10){
  if(A<10){
    print("NOT PLAY")
  }else{
    print("Will Play")
  }
  A=A+1
}

head(cars)
cars$mpg[2]=NA
cars$brand[2]=NA
head(cars)
cars$mpg[2]=mean(na.omit(cars$mpg))
cars$brand[2]=max(na.omit(cars$brand))
head(cars)


rm(list=ls())
head(cars)



data()

bondyield
library(quantmod)
getSymbols(c("MSFT","IBM"),from="2010-11-15",to="2019-11-15")
IBM
write.csv(IBM,"IBM Stock Price.csv")
