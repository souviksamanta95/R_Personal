#### Table 14.4
library(arules)
#fp.df <- read.csv("Faceplate.csv")
fp.df<- read.csv(file.choose(), header= T)
# remove first column and convert to matrix
fp.mat <- as.matrix(fp.df)

# convert the binary incidence matrix into a transactions database
fp.trans <- as(fp.mat, "transactions")
inspect(fp.trans)

## get rules
# when running apriori(), include the minimum support, minimum confidence, and target
# as arguments. 
rules <- apriori(fp.trans, parameter = list(supp = 0.6, conf = 0.5, target = "rules"))

# inspect the first six rules, sorted by their lift
inspect(head(sort(rules, by = "lift"), n = 6))
inspect((sort(rules, by = "lift")))






#### Table 14.8

#all.books.df <- read.csv("CharlesBookClub.csv")
all.books.df<- read.csv(file.choose(), header= T)
# create a binary incidence matrix
count.books.df <- all.books.df[, 8:18]
incid.books.df <- ifelse(count.books.df > 0, 1, 0)
incid.books.mat <- as.matrix(incid.books.df[, -1])

#  convert the binary incidence matrix into a transactions database
books.trans <- as(incid.books.mat, "transactions")
inspect(books.trans)

# plot data
itemFrequencyPlot(books.trans)

# run apriori function
rules <- apriori(books.trans, 
                 parameter = list(supp= 200/4000, conf = 0.5, target = "rules"))
summary(rules)
# inspect rules
inspect(sort(rules, by = "lift"))



