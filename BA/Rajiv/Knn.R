#When the data cannot be imputed, say binary digits, average cannot be used, data is too
#large use average, social sciences data, it does not make much difference
#Dealing with small datasets sample less than 30, knn, by default k is set as 5

setwd("/Users/risha/Desktop/Subjects Courses/Term III/Analytics/Bhupendra Soam")
movies=read.csv("movies.csv")
movies
str(movies)
summary(movies)
install.packages("VIM")
library(VIM)
?kNN()
movie1 = kNN(movies,variable = "Genre","Profitability",k=6)
summary(movie1)

movies2=kNN(movies)
summary(movies2)
head(movies2)
#Collection of well defined distinct objects - set

movies2 = subset(movies2,select=Film:Year)
movies2
#Assignment - Pick a random set, delete some values and replace with kNN