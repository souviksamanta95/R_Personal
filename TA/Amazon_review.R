# Session 6 - Amazon review

library(tidyverse)
library(rvest)

scrape_amazon <- function(ASIN, page_num){
  
  url_reviews <- paste0("https://www.amazon.in/product-reviews/",ASIN,"/?pageNumber=",page_num)
  
  doc <- read_html(url_reviews) # Assign results to `doc`
  
  trim <- function (x) gsub("^\\s+|\\s+$", "", x)
  
  # Review Title
  doc %>% 
    html_nodes("[class='a-size-base a-link-normal review-title a-color-base review-title-content a-text-bold']") %>%
    html_text()  %>% gsub("\n", "", .) %>% trim() -> review_title
  
  # Review Text
  doc %>% 
    html_nodes("[class='a-size-base review-text review-text-content']") %>%
    html_text() %>% gsub("\n", "", .) %>% trim()-> review_text
  
  # Number of stars in review
  doc %>%
    html_nodes("[data-hook='review-star-rating']") %>%
    html_text() -> review_star
  
  
  # Return a tibble
  tibble(review_title,
         review_text,
         review_star,
         page = page_num) %>% return()
}


scrape_amazon(ASIN = "B07VT1Z7DV", page_num = 1) ->outputlist1
scrape_amazon(ASIN = "B07VT1Z7DV", page_num = 2) ->outputlist2
#head()
outputlist1$review_text
outputlist1$review_title


review_data<-as.data.frame(rbind(outputlist1,outputlist2))
View(review_data)
write.csv(review_data,"review.csv")

