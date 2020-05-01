#Removing the outliers
data<- c(sample(x=1:20,size=40, replace=TRUE),65,80) 
data
summary(data)
length(data)
data1<-(data)
length(data1)
boxplot(data1)
bench<- 16+1.5*IQR(data1) #interquartile range
bench
data1<-data[data<31]
data1
summary(data1)
boxplot(data1)

#winsorizing the outliers
#replacing outliers with bench values
data
length(data)
summary(data)
boxplot(data)
bench<-16+1.5*IQR(data)
data[data>bench]
data[data>bench]=bench
summary(data)
boxplot(data)
length(data)
#variable transform
log_data<-log(data)


#treatment of missing values using regression
x<-1:10
y<-c(11,12,18,14,17,NA,NA,19,NA,27)
z<-c(19,11,2,14,20,4,9,10,18,1)
w<-c(1,4,7,10,3,5,7,6,6,9)

data<-data.frame(x,y,z,w)
data
#step 1: finding the most correlated value
cor(data)
cor(data, use="complete.obs")
#sysnum
symnum(cor(data,use="complete.obs"))
# 0 ' ' 0.3 '.' 0.6 ',' 0.8 '+' 0.9 '*' 0.95 'B' 1 -> will indicated how strong 
#is the correlation

#which command : sees where the missing value is and where there is no 
#missing value
#in whatever rows pair of x y w z , there is NA we have an indicator value I 
#which will write 0 for presence of NA values and 1 for no NA values
#So, once we know this, we will only execute regression for rows which have 1
ind<- function(t)
{
  x<- dim(length(t))
  x[which(!is.na(t))]=1
  x[which(is.na(t))]=0
  return(x)
}

#Step 3: Fitting the Linear model of y on x 
lm(y~x,data=data)
summary(lm(y~x,data=data))
#y=9.743 +1.509*x
#Step 4: Imputation of missing values
for(i in 1:nrow(data))
{
  if(data$I[i]==0)
  {
    data$y[i]=9.743 +1.509*data$x[i]
  }
}

datasummary(data)
data$I<-ind(data$y)
data
