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

#e<-c("e1","e2","e3","e4","e5","e6","e7","e8","e9")
#names(Q3)<-e
#Q3


#Qs<-c(20,50,40,35)
#PPU<-c(20.5,30,28,29.5)
#Region<-c("N","E","S","W")

#p1<-(Qs*PPU)
#p1
#names(Region)<-p1
#Region


Q3[Q3>50]
Q3[Q3>40 & Q3<80]



r2<-c("n","s","n","e","w")
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


data()
