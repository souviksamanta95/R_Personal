# Packages :  readr   readxl     RCurl     Amelia     Rcpp     Hmisc

#install.packages("readr")
#install.packages("readxl")
#install.packages("RCurl")
#install.packages("Amelia")
#install.packages("Rcpp")
#install.packages("Hmisc")

library(readr)
library(readxl)
library(RCurl)
library(Amelia)
library(Rcpp)
library(Hmisc)


# Importing data :

getwd()
setwd("C:/Users/souvi/Documents/R/IM507")

# Read CSV files :
d1 <- read.csv("C:/Users/souvi/Documents/R/IM507/airtravel.csv", stringsAsFactors = F)
d1
View(d1)
str(d1)

# Read TXT files :

d2 <- read.delim("C:/Users/souvi/Documents/R/IM507/airtravel 1.txt", stringsAsFactors = F)
d2

# Read table :

d3 <- read.table("C:/Users/souvi/Documents/R/IM507/airtravel 2.txt", header = T, sep = "/", stringsAsFactors = F)
d3


#-----------------------------------------------------------------------------------


# Same can be done by readr package :

read_csv("C:/Users/souvi/Documents/R/IM507/airtravel.csv")
read_tsv("C:/Users/souvi/Documents/R/IM507/airtravel 1.txt")

#-----------------------------------------------------------------------------------

# Reading xls files : readxl

excel_sheets("C:/Users/souvi/Documents/R/IM507/urbanpop.xls")       # see multiple sheetnames
read_excel("C:/Users/souvi/Documents/R/IM507/urbanpop.xls")         # default = 1st sheet
read_excel("C:/Users/souvi/Documents/R/IM507/urbanpop.xls", sheet = "1967-1974")

# From environment section, > import dataset > browse ----> GUI approach

# Use UCI ML repository for most of the data sets

# importing datasets directly from UCI URL

d4 <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data")
               ,header = F,
               col.names = c("Sepal.length", "Sepal.width","petal.length","petal.width","species"))

head(d4)

# Export data from R to the working directory :

write.csv(airquality,file = "aq.csv")

# While importing from excel you can change the time of data at the point of importing from the column dropdown

