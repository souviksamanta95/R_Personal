
data <- read.csv(file.choose())
summary(data)
str(data)
library(ggplot2)
a <- ggplot(data=data, aes(x=Rev_Million, fill=State), color="Black")
a + geom_histogram(binwidth = 5) +
  xlim(0,100)

b <- ggplot(data=data, aes(x= State, y=Rev_Million, color=Product), color="Black")
b + geom_boxplot()+ylim(0,100)
