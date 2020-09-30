rm(list=ls())

library(dplyr)
library(rvest)
library(stringr)

func <- function(htm, xp){
  article_html <- read_html(htm)
  article_body <- html_nodes(article_html,xpath=xp)
  
  article_body_text <- html_text(article_body)
  
  textdata = data.frame(article_body_text)
  names(textdata) = "text"
  
  textdata <- data.frame(textdata)
  return(textdata)
}

htm1 <- "https://www.ndtv.com/india-news/amid-standoff-with-china-video-shows-army-tanks-near-lac-2301703"
xp1 <- '//p'

htm2 <- "https://www.news18.com/news/india/india-china-standoff-with-no-thaw-as-winter-sets-in-why-hitting-pause-was-the-only-logical-option-2902225.html"
xp2 <- '//p'

htm3 <- "https://www.thehindu.com/news/national/ladakh-standoff-new-army-talks-positive-but-no-breakthrough-on-de-escalation/article32667313.ece"
xp3 <- '//p'

htm4 <- "https://www.financialexpress.com/defence/india-china-standoff-no-loss-of-ground-says-indian-army/2085460/"
xp4 <- '//p'

htm5 <- "https://www.ndtv.com/india-news/india-will-retaliate-if-its-defences-on-heights-are-breached-top-sources-2292908"
xp5 <- '//p'

df <- data.frame()

# Keeping relevant rows for final data frame
df1 <- data.frame(func(htm1, xp1)[1:13,1])
df2 <- data.frame(func(htm2, xp2))
df3 <- data.frame(func(htm3, xp3)[1:16,1])
df4 <- data.frame(func(htm4, xp4)[1:25,1])
df5 <- data.frame(func(htm5, xp5)[1:16,1])

# Changing the names
names(df1) <- "text"
names(df2) <- "text"
names(df3) <- "text"
names(df4) <- "text"
names(df5) <- "text"

df <- rbind(df, df1, df2, df3, df4, df5)
names(df) <- "text"


# Create corpus
library(tm)
myCorpus <- VCorpus(VectorSource(df$text))

# Viewing the corpus
for(i in 1:83){
  writeLines(as.character(myCorpus[[i]]))
}

## data pre-processing

# convert to lower case
myCorpus <- tm_map(myCorpus, content_transformer(tolower))

# remove punctuation
myCorpus <- tm_map(myCorpus, removePunctuation) 

# remove numbers
myCorpus <- tm_map(myCorpus, removeNumbers)

# Removing stop-words
myStopwords <- c(stopwords("english"), "india", "china", "indian", "chinese")
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)

# Removing white spaces (extra spaces)
myCorpus <- tm_map(myCorpus,stripWhitespace)


# Term document matrix
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



# N-gram analysis -------------

textdata <- data.frame(text = sapply(myCorpus, as.character), stringsAsFactors = FALSE)

library(tidytext)
library(dplyr)
uni_gram <- as.data.frame(textdata%>%
                            unnest_tokens(word,text) %>%
                            count(word,sort=TRUE))
uni_gram[1:50,]
str(uni_gram)


data_bigrams <- textdata %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)%>%
  count(bigram,sort=TRUE)

data_bigrams[1:50,]
str(data_bigrams)
