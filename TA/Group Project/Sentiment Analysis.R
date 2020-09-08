setwd("D:/Analytics/R/TA/Group Project")
textdata = read.csv("MIReviews.csv", header = TRUE)
library(SnowballC)
library(textstem)
library(tm)
library(NLP)
library(reshape2)
library(ggplot2)
library(tidyr)
library(igraph)
library(ggraph)
library(tidytext)
library(wordcloud)
library(dplyr)

colnames(textdata)
View(textdata)
str(textdata)

# Load the data as a corpus
docs <- VCorpus(VectorSource(textdata$Review))
View(docs)

#Text transformation
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

#Cleaning the text
# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))

# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)

for(i in 1:100){
  writeLines(as.character(docs[[i]]))
}

# Text stemming
# docs <- tm_map(docs, stemDocument)

#Build a term-document matrix
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

#Generate the Word cloud
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

names(textdata$Review) = "text"
textdata<-mutate(textdata,text= as.character(Review))
data_bigrams <- textdata %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)%>%
  count(bigram,sort=TRUE)
data_bigrams

### deleting stop words
bigrams_separated <- data_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# new bigram counts:
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

bigram_counts

## combine words
bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")
#SENTIMENT ANALYSIS

# Tokenise
library(tidytext)

textdata$Review<-as.character(textdata$Review)
review_tidy <- textdata %>% unnest_tokens(word, Review)
View(textdata)
View(review_tidy)

# Count
review_tidy %>% count(word) %>% arrange(desc(n))

# Remove stop words -
# http://snowball.tartarus.org/algorithms/english/stop.txt
# http://www.lextek.com/manuals/onix/stopwords1.html

review_tidy2 <- review_tidy %>% anti_join(stop_words)

# Count
word_count<-review_tidy2 %>% count(word) %>% arrange(desc(n))


# Visualization
library(ggplot2)
dev.off()
ggplot(word_count, aes(x = word, y = n)) + geom_col()

# Count
word_count2 <- review_tidy2 %>% count(word) %>% filter(n>200) %>% arrange(desc(n))
ggplot(word_count2, aes(x = word, y = n)) + geom_col() + ggtitle("Word Counts")

# SENTIMENT Analysis
# Using BING(Binary), AFINN(Ordinal), LOUGHRAN(Other meanings), NRC(tidytext) dictionaries

# BING
get_sentiments("bing")
install.packages("textdata")
library(textdata)
get_sentiments("afinn")
get_sentiments("loughran")
get_sentiments("nrc")

sentiment_review1 <- review_tidy2 %>% inner_join(get_sentiments("bing"))
sentiment_review1<-sentiment_review1 %>% count(sentiment)%>% filter(n>200)%>% arrange(desc(n))

sentiment_review2 <- review_tidy2 %>% inner_join(get_sentiments("loughran"))
sentiment_review2<-sentiment_review2 %>% count(sentiment)%>% filter(n>200)%>% arrange(desc(n))

sentiment_review3 <- review_tidy2 %>% inner_join(get_sentiments("nrc"))
sentiment_review3<-sentiment_review3 %>% count(sentiment)%>% filter(n>200)%>% arrange(desc(n))

sentiment_review3 %>% count(sentiment) %>% arrange(desc(n))
pos_neg <- sentiment_review3 %>% count(sentiment) %>% filter(sentiment %in% c("positive", "negative"))
pos_neg
library(dplyr)
ggplot(sentiment_review1, aes(x = n, y = sentiment)) + geom_col() + ggtitle("BING library results") + xlab("Counts")
ggplot(sentiment_review2, aes(x = n, y = sentiment)) + geom_col() + ggtitle("Loughran library results") + xlab("Counts")
ggplot(sentiment_review3, aes(x = n, y = sentiment)) + geom_col() + ggtitle("NRC library results") + xlab("Counts")
