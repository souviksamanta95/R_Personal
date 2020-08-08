# Session 6 - n-grams
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


textdata <- data.frame(readLines("Zara Review.txt"))
names(textdata) = "text"
View(textdata)
str(textdata)

# tokenization 

library(tidytext)
library(dplyr)
uni_gram<-as.data.frame(textdata%>%
                          unnest_tokens(word,text) %>%
                          count(word,sort=TRUE))
uni_gram
str(uni_gram)


data_bigrams <- textdata %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)%>%
  count(bigram,sort=TRUE)

data_bigrams
str(data_bigrams)

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


######################## for any error - 
# textdata <- mutate(textdata, text = as.character(text))


# Tri-grams
tri_grams<-textdata %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word) %>%
  count(word1, word2, word3, sort = TRUE)

tri_grams

## Visualization
library(igraph)

# original counts
bigram_counts


# filter for only relatively common combinations
bigram_graph <- bigram_counts %>%
  filter(n > 0) %>%
  graph_from_data_frame()

bigram_graph

library(ggraph)
set.seed(2017)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)


## combine words
bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

bigrams_united

##bar chart
ggplot(head(bigrams_united,20), aes(reorder(bigram,n), n)) +
  geom_bar(stat = "identity") + coord_flip() +
  xlab("Bigrams") + ylab("Frequency") +
  ggtitle("Most frequent bigrams")










