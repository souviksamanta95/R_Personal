
df <- read.csv("placement.csv",header=T)




head(df)

# load library
require(neuralnet)

# fit neural network
nn=neuralnet(Placed~.,data=df, hidden=c(2,3),act.fct = "logistic",
             linear.output = FALSE)

# plot neural network
plot(nn)

# creating test set
TKS=c(30,40,85,100,20,30,40,60,45,67,90,25,50)
CSS=c(85,50,40,20,80,30,40,75,80,60,30,45,50)
test=data.frame(TKS,CSS)

placed_test <- c(1,1,0,0,1,0,0,1,1,1,0,0,1)

## Prediction using neural network
Predict=compute(nn,test)
Predict$net.result

# Converting probabilities into binary classes setting threshold level 0.5
prob <- Predict$net.result
pred <- ifelse(prob>0.5, 1, 0)
pred

table(placed_test,pred)

