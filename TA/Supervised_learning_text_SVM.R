library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(e1071)        
library(caret)  
library(readr)

#Import data
sms_raw <- read.csv("spam.csv")


#View the first few lines of the dataset
head(sms_raw)

#Select & rename appropriate columns of the dataset
sms_raw <- sms_raw[,1:2]
colnames(sms_raw) <- c("Type", "Text")
sms_raw$Type <- factor(sms_raw$Type)
str(sms_raw)


# Check the number of spam and ham messages
table(sms_raw$Type)


prop.table(table(sms_raw$Type))

# Create corpus
sms_corpus <- VCorpus(x = VectorSource(sms_raw$Text))

# Print corpus
sms_corpus

# Check the text in some messages and their type
lapply(sms_corpus[5:8], as.character)

sms_raw$Type[5:8]

corpus_clean <- sms_corpus

# Remove Numbers
corpus_clean <- tm_map(x = corpus_clean, FUN = removeNumbers)

# Remove punctuation
corpus_clean <- tm_map(x = corpus_clean, FUN = removePunctuation)

# Remove stop words
corpus_clean <- tm_map(x = corpus_clean, FUN = removeWords, stopwords())

# Remove extra white spaces
corpus_clean <- tm_map(x = corpus_clean, FUN = stripWhitespace)

# Create Document Term Matrix
DTM <- DocumentTermMatrix(x = corpus_clean)

## Create training and test set

# Create Training Set
DTM_train <- DTM[1:round(nrow(DTM)*0.80, 0), ]

# Create Test Set
DTM_test <- DTM[(round(nrow(DTM)*0.80, 0)+1):nrow(DTM), ]

# Create vectors with labels for the training and test set
train_labels <- sms_raw[1:round(nrow(sms_raw)*0.80, 0), ]$Type
test_labels <- sms_raw[(round(nrow(sms_raw)*0.80, 0)+1):nrow(DTM), ]$Type

# Check proportion of ham and spam is similar on the training and test set
prop.table(table(train_labels))

prop.table(table(test_labels))


# Create wordcloud for the whole dataset
wordcloud(words = corpus_clean,
          min.freq = 50, # minimum number of times a word must be present before it appears
          random.order = FALSE, # Arrange most frequent words to be in the center of the word cloud
          color = (colors = c("#4575b4","#74add1","#abd9e9","#e0f3f8","#fee090","#fdae61","#f46d43","#d73027")) # Colour gradient for the font
)

##
threshold <- 0.1
min_freq = round(DTM$nrow*(threshold/100),0) # calculate minimum frequency
min_freq


# Create vector of most frequent words
frequent_words <- findFreqTerms(x = DTM, lowfreq = min_freq)



# Filter DTM to only have most frequent words
DTM_train_most_frequent <- DTM_train[, frequent_words]
DTM_test_most_frequent <- DTM_test[, frequent_words]

# Check dimension of DTM
dim(DTM_train_most_frequent)



# Create function  that converts numeric values to either 1 or 0 if word is present or absent in document
is_present <- function(x) {
  x <- ifelse(test = x > 0, 1, 0)
}

# Apply is_present() function to training and test DTM
DTM_train_most_frequent <- apply(X = DTM_train_most_frequent,
                                 MARGIN = 2, # Apply function to columns
                                 FUN = is_present) # Specify function to be used

DTM_test_most_frequent <- apply(X = DTM_test_most_frequent,
                                MARGIN = 2, # Apply function to columns
                                FUN = is_present) # Specify function to be used


library(e1071)



#SVM

svm_model<-svm(formula=as.factor(train_labels)~.,data=DTM_train_most_frequent)
svm_model
svm_train_predictions <- predict(object = svm_model, newdata = DTM_train_most_frequent)
confusionMatrix(data = svm_train_predictions, reference = as.factor(train_labels), positive = "spam", dnn = c("Prediction", "Actual"))

svm_test_predictions <- predict(object = svm_model, newdata = DTM_test_most_frequent)
confusionMatrix(data = svm_test_predictions, reference = as.factor(test_labels), positive = "spam", dnn = c("Prediction", "Actual"))
