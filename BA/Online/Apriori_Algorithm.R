#Association is an unsupervised learning - (use Apriori Algorithm)
#Install Package for Apriori Algorithm

install.packages("arules")
install.packages("arulesViz")
install.packages("grid")
install.packages("Matrix")

library(arules)
library(arulesViz)

#Load Data Set - Groceries

data("Groceries")


summary(Groceries)

#Rule - 1

apriori(Groceries,parameter = list(support=0.002,confidence=0.5)) -> rule1

#Suport - 0.2% of all the orders have both the consequent and antecedent
#Confidence: Out of all the antecedent there need to be 50% of them should also have consequent

#Inspection of Rule - 1
inspect(head(rule1,5))

#Sort by Lift Values
inspect(head(sort(rule1,by="lift"),5))

#Plot the rules
plot(rule1)

#- Y - Axis - Confidence and X- Axis we have support on the right side we have heat map for Lift
#Dark Red dots are mostly occuring with low support value

#Let's plot other way
plot(rule1, method = "grouped")

#Items as LHS and RHS - Size of the bubble represent the  support and color of the bubble is lift
#Larger the bubbble greater is the support, darker the bubble greater is the lift


#Rule 2 - Changing the length of antecedent to 5

apriori(Groceries,parameter = list(support=0.002,confidence=0.5, minlen=5)) -> rule2

#Inspection of Rule - 2

inspect(head(rule2,5))

plot(rule2, method = "grouped")

#Rule 3 - Changing the length of antecedent to 5

apriori(Groceries,parameter = list(support=0.007,confidence=0.6)) -> rule3

inspect(head(rule3,4))
plot(rule3, method = "grouped")



#### Other functions

# remove first column and convert to matrix
fp.mat <- as.matrix(fp.df[, -1])

# convert the binary incidence matrix into a transactions database
fp.trans <- as(fp.mat, "transactions")


