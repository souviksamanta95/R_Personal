# Assignment 1 --- Date - 18/07/2020 --- ROOM -1
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

shrutika_htm <- "https://www.dw.com/en/coronavirus-triggers-mental-health-crisis-in-india/a-54011738"
shrutika_xp <- '//p[(((count(preceding-sibling::*) + 1) = 28) and parent::*)] | //p[(((count(preceding-sibling::*) + 1) = 32) and parent::*)] | //p[(((count(preceding-sibling::*) + 1) = 33) and parent::*)] | //p[(((count(preceding-sibling::*) + 1) = 31) and parent::*)] | //p[(((count(preceding-sibling::*) + 1) = 30) and parent::*)] | //p[(((count(preceding-sibling::*) + 1) = 27) and parent::*)] | //p[(((count(preceding-sibling::*) + 1) = 25) and parent::*)] | //p[(((count(preceding-sibling::*) + 1) = 24) and parent::*)] | //*[(@id = "bodyContent")]//p[(((count(preceding-sibling::*) + 1) = 1) and parent::*)]'

souvik_htm <- "https://www.newindianexpress.com/lifestyle/health/2020/may/17/61-indians-suffering-from-mental-health-issues-during-lockdown-survey-2144506.html"
souvik_xp <- '//*[(@id = "storyContent")] | //*[(@id = "content_head")]'

amandeep_htm <- "https://indianexpress.com/article/lifestyle/life-style/matters-of-the-mind-hold-the-line-to-collectively-beat-the-crisis-6508614/"
amandeep_xp <- '//*[@id="section"]/div/div[3]/div[1]/div/div/div/div/p[1] | //*[@id="section"]/div/div[3]/div[1]/div/div/div/div/p[2] | //*[@id="section"]/div/div[3]/div[1]/div/div/div/div/p[3] | //*[@id="section"]/div/div[3]/div[1]/div/div/div/div/p[4] | //*[@id="section"]/div/div[3]/div[1]/div/div/div/div/p[6] | //*[@id="section"]/div/div[3]/div[1]/div/div/div/div/p[7] | //*[@id="section"]/div/div[3]/div[1]/div/div/div/div/p[9] | //*[@id="section"]/div/div[3]/div[1]/div/div/div/div/p[11]'

shatakshi_htm <- "https://www.edexlive.com/opinion/2020/jun/25/mental-health-matters-why-young-india-is-depressed-and-anxious-about-life-in-a-post-covid-world-12875.html"
shatakshi_xp <- '//*[(@id = "storyContent")]//p[(((count(preceding-sibling::*) + 1) = 1) and parent::*)]'

shriya_htm <- "https://www.downtoearth.org.in/blog/health/covid-19-lockdown-why-india-must-brace-for-a-mental-health-crisis-71889"
shriya_xp <- '//*[@id="box_0"]/div[2]/p[27]'

shubham_htm <- "https://fit.thequint.com/mind-it/why-is-mental-healthcare-is-hard-to-get-in-india"
shubham_xp <- '//*[@id="8b899606-c515-4b6e-98c4-275339b9eff2"]/div[3]/div/h2 | //*[@id="8b899606-c515-4b6e-98c4-275339b9eff2"]/div[3]/div/p[1] | //*[@id="8b899606-c515-4b6e-98c4-275339b9eff2"]/div[3]/div/p[2] | //*[@id="8b899606-c515-4b6e-98c4-275339b9eff2"]/div[3]/div/p[3]'

apurva_htm <- "https://theprint.in/india/a-huge-mental-health-crisis-awaits-india-post-covid-but-only-the-power-of-community-will-help/427146/"
apurva_xp <- '//*[contains(concat( " ", @class, " " ), concat( " ", "st__content-block--text", " " ))]//*[contains(concat( " ", @class, " " ), concat( " ", "st__content-block--text", " " ))]//*[contains(concat( " ", @class, " " ), concat( " ", "st__content-block--text", " " )) and (((count(preceding-sibling::*) + 1) = 4) and parent::*)]//p'

df <- data.frame()

df <- rbind(df, func(souvik_htm,souvik_xp))
df <- rbind(df, func(shrutika_htm,shrutika_xp))
df <- rbind(df, func(amandeep_htm,amandeep_xp))
df <- rbind(df, func(shriya_htm,shriya_xp))
df <- rbind(df, func(shatakshi_htm,shatakshi_xp))
df <- rbind(df, func(shubham_htm,shubham_xp))
df <- rbind(df, func(apurva_htm, apurva_xp))

# Eleminating 11th row as it is irrelevant
df2 <- data.frame(df[-11,])
names(df2) <- "text"


# Create corpus
library(tm)
myCorpus <- VCorpus(VectorSource(df2$text))

# Viewing the corpus
for(i in 1:33){
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
myStopwords <- c(stopwords("english"), "ians", "â", 'cent', "per")
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

# Tri-grams
tri_grams<-textdata %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word) %>%
  count(word1, word2, word3, sort = TRUE)

head(tri_grams,10)

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

# Visualising Tri-grams

# original counts
tri_grams


# filter for only relatively common combinations
trigram_graph <- tri_grams %>%
  filter(n > 0) %>%
  graph_from_data_frame()

trigram_graph

ggraph(trigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)
