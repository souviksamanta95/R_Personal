# Importing Data
data <- read.csv("data2.csv")
str(data)
output <- data[3]
input <- data[c(4,5,6,7,8)]
price <- data[c(9,10,11,12,13)]

# Loading Libraries
# install.packages("rDEA")
library(rDEA)
library(dplyr)

out_model <- dea(XREF = input, YREF = output, X = input[,], Y = output[,], model = "output", RTS = "variable")
in_model <- dea(XREF = input, YREF = output, X = input[,], Y = output[,], model = "input", RTS = "variable")

# RTS = variable -----------this is needed for different input and output efficiencies
# RTS = Return to Scale

out_rank <- rank(-out_model$thetaOpt)
in_rank <- rank(-in_model$thetaOpt)
efficiency <- data.frame(cbind(data[,1], out_model$thetaOpt, out_rank, in_model$thetaOpt, in_rank))
colnames(efficiency) <- c("DMU", "Output_Efficiency", "Output_Rank", "Inout_Efficiency", "In_Rank")
print(efficiency)

