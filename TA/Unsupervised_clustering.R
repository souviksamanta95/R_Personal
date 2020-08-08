# Unsupervised learning - 31/07/2020

library(tm)

#Import data
sms_raw<-read.csv("spam2.csv")


# Create corpus
sms_corpus <- VCorpus(VectorSource(sms_raw$V2))

# print corpus
inspect(sms_corpus)


corpus_clean <- sms_corpus

# Remove Numbers
corpus_clean <- tm_map(x = corpus_clean, FUN = removeNumbers)

# Remove punctuation
corpus_clean <- tm_map(x = corpus_clean, FUN = removePunctuation)

# Remove stop words
corpus_clean <- tm_map(x = corpus_clean, FUN = removeWords, stopwords("english"))

# Remove extra white spaces
corpus_clean <- tm_map(x = corpus_clean, FUN = stripWhitespace)

# Create Document Term Matrix
DTM <- DocumentTermMatrix(x = corpus_clean)
inspect(DTM[1:10, 1001:1010])

#TF-IDF
DTM_tfidf<-weightTfIdf(na.omit(DTM))
inspect(DTM_tfidf[1:10, 1001:1010])

# Clustering
m<-as.matrix(na.omit(DTM_tfidf))
rownames(m)<-1:nrow(m)

norm_eucl<-function(m)
  m/apply(m, 1, function(x) sum(x^2)^.5)

m_norm<-norm_eucl(m)
results<-kmeans(na.omit(m_norm),20)
results$size
results$withinss
results$betweenss

clusters <- 1:20
for (i in clusters) {
  cat("Cluster " , i, ":", findFreqTerms(DTM_tfidf[results[['cluster']]==i,], 2),"\n\n")
}
