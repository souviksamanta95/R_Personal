# Session 5 : Date - 17/07/2020 -- 2:30 pm
# TF-IDF ------

library(tm)
library(proxy)
library(dplyr)

doc <- c( "The sky is blue.", "The Sun is bright today.",
          "The sun in the sky is bright.", "We can see the shining sun, the bright sun." )

# create term frequency matrix using functions from tm library
doc_corpus <- Corpus( VectorSource(doc) )
control_list <- list(removePunctuation = TRUE, stopwords = TRUE, tolower = TRUE)
tdm <- TermDocumentMatrix(doc_corpus, control = control_list)
stopwords()
# print
( tf <- as.matrix(tdm) )


# idf
( idf <- log( ncol(tf) / ( 1 + rowSums(tf != 0) ) ) )

# diagonal matrix
( idf <- diag(idf) )

tf_idf <- crossprod(tf, idf)
colnames(tf_idf) <- rownames(tf)
View(tf_idf)

# Note that normalization is computed "row-wise"
tf_idf / sqrt( rowSums( tf_idf^2 ) )

doc




