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
