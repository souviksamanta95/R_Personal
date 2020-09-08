# Guest lecture - Date - 28/08/2020 - 2:30pm

#install.packages("tidyverse")
#install.packages("tidytext")
library(tidyverse)
setwd("D:/Analytics/R/TA/Sentiment")
roomba_reviews <- read.csv('Roomba Reviews.csv', stringsAsFactors=FALSE)
str(roomba_reviews)
View(roomba_reviews)

roomba_reviews %>% group_by((Product)) %>%
  summarise((number_rows = n()))

# Average star ratings productwise
roomba_reviews %>% group_by((Product)) %>%
  summarise((star_mean = mean(Stars)))

# Tokenise
library(tidytext)

roomba_tidy <- roomba_reviews %>% unnest_tokens(word, Review)

View(roomba_tidy)

# Count
roomba_tidy %>% count(word) %>% arrange(desc(n))

# Remove stop words -
# http://snowball.tartarus.org/algorithms/english/stop.txt
# http://www.lextek.com/manuals/onix/stopwords1.html

roomba_tidy2 <- roomba_tidy %>% anti_join(stop_words)

# Count
roomba_tidy2 %>% count(word) %>% arrange(desc(n))

# Remove stop words (local)

local_dict <- tribble(~word, ~lexicon, "roomba", "my_dict", "2", "my_dict", "ê", "my_dict")

#Combining local dict with global dict
dict <- stop_words %>% bind_rows(local_dict)

roomba_tidy2 <- roomba_reviews %>% unnest_tokens(word, Review) %>% anti_join(dict)

# Count
word_count <- roomba_tidy2 %>% count(word) %>% arrange(desc(n))

# Visualization
library(ggplot2)

ggplot(word_count, aes(x = word, y = n)) + geom_col()

# Count
word_count2 <- roomba_tidy2 %>% count(word) %>% filter(n>500) %>% arrange(desc(n))
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

sentiment_review <- roomba_tidy2 %>% inner_join(get_sentiments("bing"))
sentiment_review %>% count(sentiment)

sentiment_review <- roomba_tidy2 %>% inner_join(get_sentiments("loughran"))
sentiment_review %>% count(sentiment)

sentiment_review <- roomba_tidy2 %>% inner_join(get_sentiments("nrc"))
sentiment_review %>% count(sentiment)

sentiment_review %>% count(sentiment) %>% arrange(desc(n))
pos_neg <- sentiment_review %>% count(sentiment) %>% filter(sentiment %in% c("positive", "negative"))
pos_neg

ggplot(sentiment_review, aes(x = word, y = sentiment)) + geom_col()


# Regular expression
text <- c("Ania's favorite two colors are blue and red.", "Ania's favorite number is 1111.", "Ania lives at Dwarka, 42 Sub Way, Delhi", "She is 5 feet tall" , "Ania has visited 30 countries" , "Ania has ten fingers." , "Ania has worked at eleven different jobs" , " She can speak 3 languages" , "Ania's favorite food is pizza" , "Ania can name 10 facts about herself.")
str(text)

grep(pattern = "\\d\\s", x = text)

clean_text <- gsub(pattern = "Ania", replacement = "She", text)

clean_text
