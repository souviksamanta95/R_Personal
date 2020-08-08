rm(list=ls())
## Required package
list.of.packages <- c("ggplot2", "Rcpp","twitteR","ROAuth", "RCurl", "tm", 
                      "devtools", "base64enc","httr", "wordcloud","qdap" ,
                      "translateR", "arabicStemR","stringr","RColorBrewer",
                      "translateR","RWeka", "dplyr", "syuzhet")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
repo='http://nbcgib.uesc.br/mirrors/cran/'
if(length(new.packages)) install.packages(new.packages, repo = repo)
lapply(list.of.packages, require, character.only = TRUE)

text_clean <- function(input_data) {
  clean_text = gsub("&amp", " ", input_data)
  clean_text = gsub("@\\w+", " ", clean_text)
  clean_text = gsub("[[:punct:]]", " ", clean_text)
  clean_text = gsub("[[:digit:]]", " ", clean_text)
  clean_text = gsub("http\\w+", "", clean_text)
  clean_text = gsub("[ \t]{2,}", " ", clean_text)
  clean_text = gsub("^\\s+|\\s+$", " ", clean_text)
  clean_text = gsub('[^[:graph:]]', " ",clean_text) ## removes graphic characters  #like emoticons 
  clean_text = gsub('[[:cntrl:]]', " ", clean_text) # removes control characters
  clean_text = gsub('\\d+', " ", clean_text) # removes numbers
  clean_text=str_replace_all(clean_text,"[^[:graph:]]", " ") 
  clean_text <- str_replace_all(clean_text," "," ") #get rid of unnecessary spaces
  clean_text <- str_replace_all(clean_text, "https://t.co/[a-z,A-Z,0-9]*","") # Get rid of URLs
  clean_text <- str_replace_all(clean_text, "http://t.co/[a-z,A-Z,0-9]*","") # Get rid of URLs
  clean_text <- str_replace_all(clean_text,"#[a-z,A-Z]*","") # Get rid of hashtags
}

#ET <- read.csv("C:\\Users\\Meghana Mittal\\Downloads\\ET_Stock_Sentiment_Input.csv")
#BL <- read.csv("C:\\Users\\Meghana Mittal\\Downloads\\BL_Stock_Sentiment_Input.csv")
data_file <- read.csv (file.choose(),header=TRUE) 
news<- as.data.frame(data_file, stringasfactor=FALSE)
#View(data)

#news <- full_join(ET, BL)

news_sentiment <- function(file) {
  formatted_text <- text_clean(file$Headlines)
  
  file_sentiment <- file %>%
    select(Date, Headlines) %>%
    mutate(sentiment = get_sentiment(formatted_text))
  
  day_sentiment <- file_sentiment %>%
    select(Date, sentiment) %>%
    group_by(Date) %>%
    summarise_all(mean)
  
  return(day_sentiment)
}

sentiment <- news_sentiment(news)
stock_sentiment <- inner_join(file, sentiment, by = "Date")
write.csv(stock_sentiment, file = "stock_sentiment.csv")






