# Session 2 - Date - 05/07/2020 - 6:00pm-7:30pm

#install.packages("rvest")
library(dplyr)
library(rvest)
library(stringr)


google <- read_html("https://news.google.com")


headline_all <- google %>% html_nodes("article") %>% html_text("span") %>%
  str_split("(<=[a-z0-9!?\\.])(?=[A-Z])")

headline_all <- sapply(headline_all, function(x) x[1])

headline_all[1:10]




# Scrapping news article
article_html <- read_html("https://zeenews.india.com/india/iafs-sukhoi-su-30mkis-apache-patrol-lac-in-ladakh-send-a-strong-message-to-china-2293693.html")
article_body <- html_nodes(article_html,xpath="//p")
# xpath can be fetched using selectorgadget extension in chrome
article_body # this is in html format which is having <p> for paragraphs
article_body_text <- html_text(article_body) # Filtering only text part removing <p> part
article_body_text


#
article_html <- read_html("https://in.finance.yahoo.com/quote/%5ENSEI/history?p=%5ENSEI")
article_body <- html_nodes(article_html,xpath="//*[contains(concat( " ", @class, " " ), concat( " ", "smartphone_Px\(20px\)", " " ))]")
