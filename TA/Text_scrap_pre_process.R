

library(dplyr)
library(rvest)
library(stringr)

article_html <- read_html("https://www.thehindu.com/opinion/editorial/none-gains-the-hindu-editorial-on-us-withdrawal-from-who/article32035308.ece")
article_body <- html_nodes(article_html,xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "drop-caps", " " ))]')

article_body
article_body_text <- html_text(article_body)
article_body_text

textdata = data.frame(article_body_text)
names(textdata) = "text"
#View(textdata)
str(textdata)
# textdata <- data.frame(textdata)

# create corpus
library(tm)
myCorpus<-VCorpus(VectorSource(textdata$text))

writeLines(as.character(myCorpus[[1]]))

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

writeLines(as.character(myCorpus[[1]]))



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
