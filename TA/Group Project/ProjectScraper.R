library(dplyr)
library(rvest)
library(stringr)
library(tidyverse)
library(dplyr)

# Defining scraper function
scrape <- function(url){
flk <- read_html(url)

flk %>% html_nodes("[class='_2xg6Ul']") %>% html_text() %>% gsub("[^\x01-\x7F]", "", .) -> ttl

flk %>% html_nodes("[class='qwjRop']") %>%  html_text() %>% gsub("READ MORE", "", .)  %>% gsub("[^\x01-\x7F]", "", .)-> rv

rvw <- data.frame("Title" = ttl, "Review" = rv)
return(rvw)
}

# Defining blank data frame for capturing data
rvw_master <- data.frame(Title=character(), Review=character())

#-------------------------------------------------------------------------------
url <- c()
for (i in seq(1,5)){
  ur <- paste("https://www.flipkart.com/mobiles/mi~brand/pr?sid=tyy%2C4io&otracker=nmenu_sub_Electronics_0_Mi&page=",i) %>% gsub(" ", "", .)
  url <- c(url,ur)
}

url_list <- c()
for (i in url)
{
  flk <- read_html(i)
  ff <- flk %>% html_nodes("[class='_31qSD5']") %>% html_attr("href")
  j <- 1
  nn <- c()
  for (i in ff)
  {
    nn[j] <- paste("https://www.flipkart.com",i) %>% gsub(" ", "", .)
    j <- j + 1
  }
  url_list <- c(url_list, nn)
}
#-------------------------------------------------------------------------------

# Running scraper
for (i in url_list){
rvw_master <- rbind(rvw_master,scrape(i))}

write.csv(rvw_master, "MIReviews.csv", row.names = FALSE)
