getwd()
setwd("/media/souvik/SSD2/Analytics/R/BA/Dataset")

movies <-  read.csv("movies.csv")
str(movies)
head(movies)
summary(movies)
View(movies)
# KNN Imputation
# KNN is present in VIM Package
install.packages("VIM")
library("VIM")
?kNN()
# Impute the missing values in some variables
movie1 <- kNN(movies, variable = "Genre", "Profitability", k=6)
summary(movie1)

# Imputing all issing values at once

movie2 <- kNN(movies)
summary(movie2)
head(movie2)

# Clening additional variables (Junk)
movie2 <- subset(movie2, select = Film:Year)
head(movie2)

