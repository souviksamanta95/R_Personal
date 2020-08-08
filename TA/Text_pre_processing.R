# Session 3 - 10/07/2020 - 2:30 pm

# Text pre processing
library(tm)
library(NLP)
library(textstem)
library(SnowballC)
library(reshape2)
library(ggplot2)

textdata = data.frame(readLines("D:/Analytics/R/TA/textdata2.txt"))
names(textdata) = "text"
View(textdata)
str(textdata)
# textdata <- data.frame(textdata)

# create corpus
myCorpus<-VCorpus(VectorSource(textdata$text))


for(i in 1:4){
  writeLines(as.character(myCorpus[[i]]))
}

# tokenization 
# install.packages("tidytext")
library(tidytext)
library(dplyr)
# textdata <- mutate(textdata, text = as.character(text))

textdata %>%
  unnest_tokens(word,text) %>%
  count(word,sort=TRUE)

## data preprocessing 

# convert to lower case

myCorpus <- tm_map(myCorpus, content_transformer(tolower))
#myCorpus <- tm_map(myCorpus, tolower)

# remove punctuation
myCorpus <- tm_map(myCorpus, removePunctuation) 

# remove numbers
myCorpus <- tm_map(myCorpus, removeNumbers)


# remove stopwords from corpus with default in built stopwords
stopwords()
myCorpus <- tm_map(myCorpus,removeWords,stopwords("english"))

# add extra stop words: 'fy' within the inbuilt list
myStopwords <- c(stopwords("english"), "fy")

# remove stopwords from corpus
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)

# Remove urls
removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeURL)) 

# Removing white spaces (extra spaces)
myCorpus <- tm_map(myCorpus,stripWhitespace)

# Stemming
library(SnowballC)
myCorpus <- tm_map(myCorpus, stemDocument)

# Lemmatization
library(textstem)
lemmatize_strings(myCorpus)


# Replacing specific pattern
myCorpus <- tm_map(myCorpus, content_transformer(gsub), pattern = c("indias"), replacement = c("india"))
myCorpus <- tm_map(myCorpus, content_transformer(gsub), pattern = c("chinas"), replacement = c("china"))

# Basic pre processing ends here -
# The document term matrix ----- Data exploration (Top 10)

dtm <- DocumentTermMatrix(myCorpus)

dtm.matrix <- as.matrix(dtm)
wordcount <- colSums(dtm.matrix)
topten <- head(sort(wordcount, decreasing=TRUE), 10)
topten  # Top ten words with counts

# Plotting the data -
# For top 10 -
dtm <- DocumentTermMatrix(myCorpus)


dtm.matrix <- as.matrix(dtm)
wordcount <- colSums(dtm.matrix)
topten <- head(sort(wordcount, decreasing=TRUE), 10)
library(reshape2)
library(ggplot2)

dfplot <- as.data.frame(melt(topten))
dfplot$word <- dimnames(dfplot)[[1]]
dfplot$word <- factor(dfplot$word,
                      levels=dfplot$word[order(dfplot$value,
                                               decreasing=TRUE)])

fig <- ggplot(dfplot, aes(x=word, y=value)) + geom_bar(stat="identity")
fig <- fig + xlab("Word in Corpus")
fig <- fig + ylab("Count")
print(fig)

### For all the data
# data Exploration 
dtm <- DocumentTermMatrix(myCorpus)

dtm.matrix <- as.matrix(dtm)
wordcount <- colSums(dtm.matrix)
topten <- sort(wordcount, decreasing=TRUE)

library(reshape2)
library(ggplot2)

dfplot <- as.data.frame(melt(topten))
dfplot$word <- dimnames(dfplot)[[1]]
dfplot$word <- factor(dfplot$word,
                      levels=dfplot$word[order(dfplot$value,
                                               decreasing=TRUE)])

fig <- ggplot(dfplot, aes(x=value, y=word)) + geom_bar(stat="identity")
fig <- fig + xlab("Count")
fig <- fig + ylab("Word in Corpus")
print(fig)
