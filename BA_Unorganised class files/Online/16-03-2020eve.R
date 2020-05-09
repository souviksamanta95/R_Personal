# Association Rule - market basket analysis

install.packages("arules")
install.packages("arulesViz")
install.packages("grid")
install.packages("Matrix")

library(arules)
library(arulesViz)
library(grid)
library(Matrix)

# Loading the groceries dataset
setwd("D:/Analytics/R/BA/Online")

groceries <- read.csv("groceries.csv")

summary(groceries)

rule1 <- apriori(groceries, parameter = list(support=0.002, confidence=0.5))

inspect(head(rule1,5))
