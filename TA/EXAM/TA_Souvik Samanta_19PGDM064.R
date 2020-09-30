# Clearing environment
rm(list=ls())

# Setting Working Directory
setwd("D:/Analytics/R/TA/EXAM")

# Loading libraries
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(e1071)        
library(caret)  
library(dplyr)

# Importing Dataset
textdata <- read.csv("Question1 (2).csv")

#View the first few lines of the dataset and the structure
head(textdata)
str(textdata)

# Defining the categories as factors
textdata$sentiment <- factor(textdata$sentiment)

# Check the number of positive and negative sentiments
table(textdata$sentiment)
prop.table(table(textdata$sentiment))

# Create corpus
txt_corpus <- VCorpus(x = VectorSource(textdata$review))

# Check the text in some rows
lapply(txt_corpus[5:8], as.character)

# Creating new corpus for cleaning
corpus_clean <- txt_corpus

# Remove Numbers
corpus_clean <- tm_map(x = corpus_clean, FUN = removeNumbers)

# Transform all letters to lower case
corpus_clean <- tm_map(x = corpus_clean, FUN = content_transformer(tolower))

# Remove punctuation
corpus_clean <- tm_map(x = corpus_clean, FUN = removePunctuation)

# Remove stop words
corpus_clean <- tm_map(x = corpus_clean, FUN = removeWords, stopwords())

# add extra stop words: 'fy' within the inbuilt list
myStopwords <- c(stopwords("english"), "the", "one")

# remove stopwords from corpus
corpus_clean <- tm_map(corpus_clean, removeWords, myStopwords)

# Remove extra white spaces
corpus_clean <- tm_map(x = corpus_clean, FUN = stripWhitespace)

# Rechecking the corpus
for (i in c(5:8)){
  writeLines(as.character(corpus_clean[[i]]))
}

# Create Document Term Matrix
DTM <- DocumentTermMatrix(x = corpus_clean)

dtm.matrix <- as.matrix(DTM)
wordcount <- colSums(dtm.matrix)
topten <- head(sort(wordcount, decreasing=TRUE), 10)
topten  # Top ten words with counts

## Create training and test set

# Create Training Set
DTM_train <- DTM[1:round(nrow(DTM)*0.80, 0), ]

# Create Test Set
DTM_test <- DTM[(round(nrow(DTM)*0.80, 0)+1):nrow(DTM), ]

# Create vectors with labels for the training and test set
train_labels <- textdata[1:round(nrow(textdata)*0.80, 0), ]$sentiment
test_labels <- textdata[(round(nrow(textdata)*0.80, 0)+1):nrow(DTM), ]$sentiment

# Check proportion of ham and spam is similar on the training and test set
prop.table(table(train_labels))

prop.table(table(test_labels))


library(wordcloud)

# Create wordcloud for the whole dataset
wordcloud(words = corpus_clean,
          min.freq = 100, # minimum number of times a word must be present before it appears
          random.order = FALSE, # Arrange most frequent words to be in the center of the word cloud
          color = (colors = c("#4575b4","#74add1","#abd9e9","#e0f3f8","#fee090","#fdae61","#f46d43","#d73027")) # Colour gradient for the font
)

##
threshold <- 0.1
min_freq = round(DTM$nrow*(threshold/100),0) # calculate minimum frequency
min_freq


# Create vector of most frequent words
frequent_words <- findFreqTerms(x = DTM, lowfreq = min_freq)

str(frequent_words)

# Filter DTM to only have most frequent words
DTM_train_most_frequent <- DTM_train[, frequent_words]
DTM_test_most_frequent <- DTM_test[, frequent_words]

# Check dimension of DTM
dim(DTM_train_most_frequent)



# Create function  that converts numeric values to "Yes" or "No" if word is present or absent in document
is_present <- function(x) {
  x <- ifelse(test = x > 0, yes = "Yes", no = "No")
}

# Apply is_present() function to training and test DTM
DTM_train_most_frequent <- apply(X = DTM_train_most_frequent,
                                 MARGIN = 2, # Apply function to columns
                                 FUN = is_present) # Specify function to be used

DTM_test_most_frequent <- apply(X = DTM_test_most_frequent,
                                MARGIN = 2, # Apply function to columns
                                FUN = is_present) # Specify function to be used


library(e1071)

# Create model from the training dataset
spam_classifier <- naiveBayes(x = DTM_train_most_frequent, y = train_labels)
train_predictions <- predict(object = spam_classifier, newdata = DTM_train_most_frequent)

# Create confusion matrix
confusionMatrix(data = train_predictions, reference = train_labels, positive = "positive", dnn = c("Prediction", "Actual"))

## Make predictions on test set
test_predictions <- predict(object = spam_classifier, newdata = DTM_test_most_frequent)

## Create confusion matrix
N
# install.packages("caret")

library(caret)

# Create confusion matrix
confusionMatrix(data = test_predictions, reference = test_labels, positive = "positive", dnn = c("Prediction", "Actual"))





















