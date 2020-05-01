setwd("/Users/risha/Desktop/Subjects Courses/Term III/Analytics/Bhupendra Soam")
x1 = read.csv("movies_exercise1.csv")
x1
summary(x1)
x2=kNN(x1)
x2
summary(x2)
x2=subset(x2,select = Film:Year)
head(x2)
