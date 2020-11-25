install.packages("arules")
install.packages("arulesViz")
library(arules)
library(arulesViz)



library(datasets)

# Load the data set
data(Groceries)
head(Groceries)
Groceries<-Groceries@itemInfo
Groceries<-as.data.frame(Groceries)
str(Groceries)
typeof(Groceries)
# Create an item frequency plot for the top 20 items
itemFrequencyPlot(Groceries,topN=20,type="absolute")

###########
#Apriori
#############

# Get the rules
rules <- apriori(Groceries, parameter = list(supp = 0.001, conf = 0.8))

# Show the top 5 rules, but only 2 digits
options(digits=2)
inspect(rules[1:150])
