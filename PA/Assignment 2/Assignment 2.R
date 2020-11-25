# Importing Data
data <- read.csv("data.csv")
str(data)
output <- data$Y
input <- data[,c(-1,-2)]

# Loading Libraries
# install.packages("rDEA")
library(rDEA)
library(dplyr)

model <- dea(XREF = input, YREF = output, X = input[,], Y = output, model = "input", RTS = "constant")
rnk <- rank(-model$thetaOpt, ties.method = c("average", "first", "last", "random", "max", "min"))
efficiency <- data.frame(cbind(data[,1], model$thetaOpt, rnk))
colnames(efficiency) <- c("DMU", "Efficiency", "Rank")

sorted_data <- arrange(efficiency, Rank)


