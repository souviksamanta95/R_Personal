#MBA - Business Analytics Class - Demonstrated on 07-Mar-2020
#Implementing KNN using R
####################################################\
prc = read.csv("Disease_Identification.csv")
#first 6 rows and structure
head(prc)
str(prc)

####################################################
#Data Preprocessing
#Removing first column from the dataset
prc=prc[,-1]
head(prc)
View(prc)


#Exploratory Analysis
#Frequency distributiion for the diagnosis
table(prc$diagnosis_result)
pie(table(prc$diagnosis_result))
prop.table(table(prc$diagnosis_result))

#Creating new column
prc$diagnosis= factor(prc$diagnosis_result, levels = c("B","M"), labels = c("Benign","Maligna"))
head(prc)
round(prop.table(table(prc$diagnosis))*100,digits = 1)

#Normalise data or standardise the data (can be done using z scores)

#Creating a normalizing function
normalize=function(x){
  return((x - min(x))/(max(x)-min(x)))}

prc_n=as.data.frame(lapply(prc[2:9], normalize))
View(prc)
View(prc_n)

dim(prc_n)
str(prc_n)
View(prc)
View(prc_n)
summary(prc$radius)
summary(prc_n$radius)


#Creating the training and test data sets - partitioning
prc_train = prc_n[1:65,]
prc_test=prc_n[66:100,]

#We have not included target variable in dignosis_result 
str(prc_train)

#Creating the labels
prc_train_labels=prc[1:65,1]
prc_test_labels=prc[66:100,1]
View(prc_train_labels)

install.packages("class")
library(class)

#Model Creation

#choosing K -> k= square root of the number (This is one of the way to choose k but there can be different ways too)
k=sqrt(nrow(prc))
k

#Don't take decimals for K and Don't consider even numbers - pick the nearest odd numbers

#KNN model
prc_test_pred = knn(train = prc_train, test = prc_test,cl=prc_train_labels,k=11)

#Check prediction
prc_test_pred

#Check test
prc_test_labels

table(prc_test_labels)
table(prc_test_pred)

#Change the K to 9 and see the prediction
prc_test_pred = knn(train = prc_train, test = prc_test,cl=prc_train_labels,k=9)

#confusion matrix
#original data on horizonatal- rows , predicted data - columns (x,y)

table(prc_test_labels,prc_test_pred)

#Accuracy= (sum of diagonal elements (left to right)/Total)*100
((8+16)/35)*100

##################################################################################
