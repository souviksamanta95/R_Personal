#################################################################################
###################### Scraping the news articles ######################
#################################################################################

######### Install Packages #########
RequiredPackages <- c("tidyverse","rvest", "stringi", "xml2", "dplyr" )
for (i in RequiredPackages) { #Installs packages if not yet installed
  if (!require(i, character.only = TRUE)) install.packages(i)
}
library(tidyverse)
library(rvest)
library(stringi)
library(xml2)
library(dplyr)

######### Reading the links from csv #########
data_headline<-read.csv(file.choose(),header = T,stringsAsFactors = F)
links<-data_headline$URLs

######### Initial Setup #########

data <- data.frame(Date<-c(),
                   Headline <- c(),
                   Article <- c(),
                   Link<-c()) 
v<-c()

######### Extracting Data from website #########

for(i in 1:nrow(data_headline)) {
  #Save URL
  save<- read_html(links[i])
  
  #downloading article and saving as single vector
  article <- save %>% html_nodes('div .inf-body')%>% html_nodes("p") %>% html_text()
  for(j in 1:length(article)){
    v[i]<-paste0(v[i],article[j])}
  
  #saving as data frame
  Date[i]<- data_headline$Date
  Headline[i]<-data_headline$Headlines[i]
  Link<-links[i]
  Article[i]<-v[i]
  data<-cbind(Date,Headline,Link,Article)
  
  #Adding Lag Time
  lag <- runif(1,2,4)
  Sys.sleep(lag)
  
  #incrementing value of i
  i<-i+1
}

######### Saving the articles in a csv file #########

write.csv(data,"Articles.csv")
#write.csv(data,"Govt_Arti_2019.csv")  

######### Closing all connections #########
closeAllConnections()
remove(x)