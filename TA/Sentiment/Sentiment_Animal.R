# Guest lecture - Date - 28/08/2020 - 2:30pm - ASSIGNMENT

library(tidyverse)
setwd("D:/Analytics/R/TA/Sentiment")
animal_reviews <- read.csv('animal_farm.csv', stringsAsFactors=FALSE)
str(animal_reviews)
View(animal_reviews)

animal_reviews %>% group_by((Product)) %>%
  summarise((number_rows = n()))

# Average star ratings productwise
animal_reviews %>% group_by((Product)) %>%
  summarise((star_mean = mean(Stars)))

# Tokenise
library(tidytext)

animal_tidy <- animal_reviews %>% unnest_tokens(word, Review)

View(animal_tidy)

# Count
animal_tidy %>% count(word) %>% arrange(desc(n))

# Remove stop words -
# http://snowball.tartarus.org/algorithms/english/stop.txt
# http://www.lextek.com/manuals/onix/stopwords1.html

animal_tidy2 <- animal_tidy %>% anti_join(stop_words)

# Count
animal_tidy2 %>% count(word) %>% arrange(desc(n))

# Remove stop words (local)

local_dict <- tribble(~word, ~lexicon, "animal", "my_dict", "2", "my_dict", "ê", "my_dict")

#Combining local dict with global dict
dict <- stop_words %>% bind_rows(local_dict)

animal_tidy2 <- animal_reviews %>% unnest_tokens(word, Review) %>% anti_join(dict)

# Count
word_count <- animal_tidy2 %>% count(word) %>% arrange(desc(n))

# Visualization
library(ggplot2)

ggplot(word_count, aes(x = word, y = n)) + geom_col()

# Count
word_count2 <- animal_tidy2 %>% count(word) %>% filter(n>500) %>% arrange(desc(n))
ggplot(word_count2, aes(x = word, y = n)) + geom_col() + ggtitle("Word Counts")

# SENTIMENT Analysis
# Using BING(Binary), AFINN(Ordinal), LOUGHRAN(Other meanings), NRC(tidytext) dictionaries

# BING
get_sentiments("bing")
#install.packages("textdata")
library(textdata)
get_sentiments("afinn")
get_sentiments("loughran")
get_sentiments("nrc")

sentiment_review <- animal_tidy2 %>% inner_join(get_sentiments("bing"))
sentiment_review %>% count(sentiment)

sentiment_review <- animal_tidy2 %>% inner_join(get_sentiments("loughran"))
sentiment_review %>% count(sentiment)

sentiment_review <- animal_tidy2 %>% inner_join(get_sentiments("nrc"))
sentiment_review %>% count(sentiment)

sentiment_review %>% count(sentiment) %>% arrange(desc(n))
pos_neg <- sentiment_review %>% count(sentiment) %>% filter(sentiment %in% c("positive", "negative"))
pos_neg