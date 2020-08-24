# Date - 21/08/2020 - 2:30 pm - Guest Lecture - Sentiment Analysis

setwd("D:/Analytics/R/TA/Sentiment")

# Load library
library (tm)
## Loading required package: NLP
library (stringr)
library (rvest)
## Loading required package: xml2
library (SnowballC)
trip = read.csv ("tripadvisor_reviews.csv")
N
# sentiment, echo=FALSE}
# Sentiment Analysis of Hotel Reviews on Trip Advisor - JW Marriott
hotel_reviews <- as.character (trip $ review)
# Load the positive and negative lexicon data and explore
positive_lexicon <- read.csv ("positive-lexicon.txt")
negative_lexicon <- read.csv ("negative-lexicon.txt")
# Load the stop words text file and explore
stop_words <- read.csv ("stopwords_en.txt")

vector_source_reviews = VectorSource (hotel_reviews)

reviews_corpus <- Corpus (vector_source_reviews)
#inspect(reviews_corpus)
reviews_corpus[[2]] $ content
# Remove stop words
filtered_corpus_no_stopwords <- tm_map (reviews_corpus, removeWords, stopwords ('english'))
## Warning in tm_map.SimpleCorpus(reviews_corpus, removeWords,
## stopwords("english")): transformation drops documents
#inspect(filtered_corpus_no_stopwords)
# Remove Punctuation
filtered_corpus_no_puncts <- tm_map (filtered_corpus_no_stopwords, removePunctuation)
## Warning in tm_map.SimpleCorpus(filtered_corpus_no_stopwords, removePunctuation):
## transformation drops documents
#inspect(filtered_corpus_no_puncts)
# Remove Numbers
filtered_corpus_no_numbers <- tm_map (filtered_corpus_no_puncts, removeNumbers)
## Warning in tm_map.SimpleCorpus(filtered_corpus_no_puncts, removeNumbers):
## transformation drops documents
#inspect(filtered_corpus_no_numbers)
# Remove unwanted white spaces
filtered_corpus_no_whitespace <- tm_map (filtered_corpus_no_numbers, stripWhitespace)
## Warning in tm_map.SimpleCorpus(filtered_corpus_no_numbers, stripWhitespace):
## transformation drops documents
#inspect(filtered_corpus_no_whitespace)
# Make all words to lowercase
filtered_corpus_to_lower <- tm_map (filtered_corpus_no_whitespace, content_transformer (tolower))
## Warning in tm_map.SimpleCorpus(filtered_corpus_no_whitespace,
## content_transformer(tolower)): transformation drops documents
#inspect(filtered_corpus_to_lower)

# Remove stop words of the external file from the corpus and whitespaces again and inspect
stopwords_vec <- as.data.frame (stop_words)
final_corpus_no_stopwords <- tm_map (filtered_corpus_to_lower, removeWords, stopwords_vec[,1])
## Warning in tm_map.SimpleCorpus(filtered_corpus_to_lower, removeWords,
## stopwords_vec[, : transformation drops documents
#inspect(final_corpus_no_stopwords)
final_corpus <- tm_map (final_corpus_no_stopwords, stripWhitespace)
## Warning in tm_map.SimpleCorpus(final_corpus_no_stopwords, stripWhitespace):
## transformation drops documents
#inspect(final_corpus)

# Character representation of the corpus of first review
final_corpus[[1]] $ content
## [1] " significant travel challenge day visit hotel hotel handled staff members chris jennifer expectations chris welcoming remembered time jennifer handled difficult situation professionalism thoroughness hotel lovely common"
hotel_reviews[1]
## [1] "I had a significant travel challenge during my 5 day visit to this hotel and the hotel could not have handled it better. Staff members, Chris and Jennifer went beyond my expectations. Chris was especially welcoming and remembered my name each time I saw him. Jennifer handled my difficult situation with professionalism and thoroughness.\nThe hotel itself was lovely. Common..."

# Stem the words to their root of all reviews present in the corpus
stemmed_corpus <- tm_map (final_corpus, stemDocument)
## Warning in tm_map.SimpleCorpus(final_corpus, stemDocument): transformation drops
## documents
stemmed_corpus[[1]] $ content
## [1] "signific travel challeng day visit hotel hotel handl staff member chris jennif expect chris welcom rememb time jennif handl difficult situat profession thorough hotel love common"

TDM_corpus <- TermDocumentMatrix (stemmed_corpus)
# terms occurring with a minimum frequency of 5
findFreqTerms (TDM_corpus, 5)

total_pos_count <- 0
total_neg_count <- 0
pos_count_vector <- c ()
neg_count_vector <- c ()
size <- length (stemmed_corpus)
for (i in 1 : size){
  corpus_words<- list ( strsplit (stemmed_corpus[[i]] $ content, split = " "))
  #print(intersect(unlist(corpus_words), unlist(positive_lexicon))) ## positive words in current review
  pos_count <- length ( intersect ( unlist (corpus_words), unlist (positive_lexicon)))
  #print(intersect(unlist(corpus_words), unlist(negative_lexicon))) ## negative words in current review
  neg_count <- length ( intersect ( unlist (corpus_words), unlist (negative_lexicon)))
  if (pos_count > neg_count){
    #print("It's a positive review")
  } else {
    #print("It's a negative review")
  }
  total_count_for_current_review <- pos_count + neg_count ## current positive and negative count
  pos_percentage <- (pos_count * 100) / total_count_for_current_review
  neg_percentage <- (neg_count * 100) / total_count_for_current_review
  #print(pos_percentage) ## current positive percentage
  #print(neg_percentage) ## current negtive percentage
  total_pos_count <- total_pos_count + pos_count ## overall positive count
  total_neg_count <- total_neg_count + neg_count ## overall negative count
  pos_count_vector <- append (pos_count_vector, pos_count)
  neg_count_vector <- append (neg_count_vector, neg_count)
}
print (pos_percentage) ## current positive percentage
## [1] 71.42857
print (neg_percentage) ## current negtive percentage
## [1] 28.57143

# Sentiment score of each review and visualizing using boxplot
counts <- data.frame (pos_count_vector, neg_count_vector)
sentiment <- data.frame ( c (1 : size),(pos_count_vector - neg_count_vector) / (pos_count_vector + neg_count_vector))
names (sentiment)= c ('review_id','SentimentScore')
boxplot (sentiment $ SentimentScore[0 : 5] ~ sentiment $ review_id[0 : 5])

# Visualiztion of positive and negative count of single review
singe_review <- c (counts $ pos_count_vector[8], counts $ neg_count_vector[8])
barplot ( t ( as.data.frame (singe_review)), ylab = "Count", xlab = "Positve v/s Negative", main = "Positive")

# Calculating overall percentage of positive and negative words of all the reviews
total_pos_count ## overall positive count

total_count <- total_pos_count + total_neg_count
overall_positive_percentage <- (total_pos_count * 100) / total_count
overall_negative_percentage <- (total_neg_count * 100) / total_count
overall_positive_percentage ## overall positive percentage

overall_negative_percentage ## overall negative percentage
# Visualization of positive and negative word count for all the reviews
review_count_frame <- data.frame ( matrix ( c (pos_count_vector, neg_count_vector), nrow = 100, ncol = 2))
colnames (review_count_frame) <- c ("Positive Word Count", "Negative Word Count")
barplot (review_count_frame $ `Positive Word Count`, ylab = "Positive Word Count", xlab = "Reviews from 1 ")

# Visualization of Overall positive and negative reviews
percent_vec <- c (overall_positive_percentage, overall_negative_percentage)
percent_frame <- as.data.frame (percent_vec)
rownames (percent_frame) <- c ("Positive Reviews","Negative Reviews")
colnames (percent_frame) <- c ("Percentage")
percentage <- t (percent_frame)
barplot (percentage, ylab = "Percentage", main = "Sentiment Analysis of JW Marriot Reviews on TripAdvisor")

library (wordcloud)
## Loading required package: RColorBrewer
set.seed (1234) # for reproducibility
textM = as.matrix (TDM_corpus)
#textM[0:10]
textFreqs <- rowSums (textM)
freqTab <- data.frame (term = names (textFreqs), num = textFreqs)
write.csv (freqTab, "freq.csv", row.names = FALSE)
wordcloud (freqTab $ term, freqTab $ num, max.words = 500, color = "green")

wordcloud (words = freqTab $ term, freq = freqTab $ num, min.freq = 4,
           max.words=500, random.order=FALSE, rot.per=0.35,
           colors= brewer.pal (8, "Dark2"))

freqTab = read.csv ('freq.csv', stringsAsFactors = FALSE)
dim (freqTab)
head (freqTab)
library (wordcloud2)
#wordcloud2(data=freqTab)
wordcloud2 (data=freqTab[0 : 100,], size=1.6, color='random-dark')
wordcloud2 (data=freqTab[100 : 200,], size = 0.7, shape = 'pentagon')


