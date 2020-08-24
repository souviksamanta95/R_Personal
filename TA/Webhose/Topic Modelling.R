setwd("D:/Analytics/R/TA/Webhose")
textdata <- read.csv("combined.csv")

library(dplyr)
library(rvest)
library(stringr)
library(tm)
library(tidytext)
library(dplyr)

colnames(textdata) <- "text"
head(textdata)

myCorpus <- VCorpus(VectorSource(textdata$text))


writeLines(as.character(myCorpus[[1]]))

# Tokenization

# textdata <- mutate(textdata, text = as.character(text))

textdata %>%
  unnest_tokens(word,text) %>%
  count(word,sort=TRUE)

# Data pre-processing 

# Convert to lower case
myCorpus <- tm_map(myCorpus, content_transformer(tolower))

# Remove punctuation
myCorpus <- tm_map(myCorpus, removePunctuation) 

# Remove numbers
myCorpus <- tm_map(myCorpus, removeNumbers)

# Remove stop-words from corpus with default in built stopwords
stopwords()
myCorpus <- tm_map(myCorpus,removeWords,stopwords("english"))

# add extra stop words: 'fy' within the inbuilt list
myStopwords <- c(stopwords("english"), "also", "can", "will", "said", "like", "one")

# remove stopwords from corpus
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)

# Remove urls
removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeURL)) 

# Removing white spaces (extra spaces)
myCorpus <- tm_map(myCorpus,stripWhitespace)

writeLines(as.character(myCorpus[[1]]))

# Creating Document Term Matrix
Dtm <- DocumentTermMatrix(myCorpus)
inspect(Dtm)
Dtm2 <- removeSparseTerms(Dtm, sparse=0.98)
inspect(Dtm2)

#load topic models library
#install.packages("topicmodels")
library(topicmodels)


#Set parameters for Gibbs sampling
#set burn in
burnin <- 4000
#set iterations
iter <- 2000
#thin the spaces between samples
thin <- 500
#use random integers as seed 
seed <-list(2003,5,63,100001,765)
#set random starts at 5
nstart <- 5
# return the highest probability as the result
best <- TRUE

#Number of topics
k <- 5

#Run LDA using Gibbs sampling
ldaOut <-LDA(Dtm2,k, method="Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))

#view the top 6 terms for each of the 5 topics, create a matrix and write to csv
terms(ldaOut,6)


ldaOut.terms <- as.matrix(terms(ldaOut,6))



#view the topic assignment for each document
topics(ldaOut)


##################
#write out results
#docs to topics
ldaOut.topics <- as.matrix(topics(ldaOut))
setwd("D:/Analytics/R/TA/Webhose/Outputs")

write.csv(ldaOut.topics,file=paste("LDAGibbs",k,"DocsToTopics.csv"))


#top 6 terms in each topic
ldaOut.terms <- as.matrix(terms(ldaOut,6))
write.csv(ldaOut.terms,file=paste("LDAGibbs",k,"TopicsToTerms.csv"))

#probabilities associated with each topic assignment
topicProbabilities <- as.data.frame(ldaOut@gamma)
write.csv(topicProbabilities,file=paste("LDAGibbs",k,"TopicProbabilities.csv"))
