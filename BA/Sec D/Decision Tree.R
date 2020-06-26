library(rpart)
install.packages("rpart.plot")
library(rpart.plot)
data<-read.csv(file.choose(), header= T)
str(data)


deeper.ct <- rpart(Return ~ ., data = data, method = "class", cp = 0, minsplit = 1)
# count number of leaves
length(deeper.ct$frame$var[deeper.ct$frame$var == "<leaf>"])

# plot tree
prp(deeper.ct, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10, 
    box.col=ifelse(deeper.ct$frame$var == "<leaf>", 'gray', 'white'))  
rpart.plot(deeper.ct)

install.packages("rattle")
library(rattle)	
fancyRpartPlot(deeper.ct)
