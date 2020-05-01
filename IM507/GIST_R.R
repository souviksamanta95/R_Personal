# GIST ~ Moral of the story >>>> RStudio <<<<<   !!! Don't RUN !!! Just go through

install.packages(c("dplyr","tidyr","tidyverse","readr","readxl","RCurl","Amelia","Rcpp","Hmisc","stringr","lubridate","ggplot2","imputeTS"))

# Activating libraries -
library("dplyr")
library("tidyr")
library("tidyverse")
library("readr")
library("readxl")
library("RCurl")
library("Amelia")
library("Rcpp")
library("Hmisc")
library("stringr")
library("lubridate")
library("ggplot2")
library("imputeTS")

# Quick notes ---------------------------------------------------------------------

a<- readline(prompt = "Enter a: ")

emp2 <- factor(emp1, levels = c("SE","SM","RM"), ordered = TRUE)

set.seed(12345)
m1 <- matrix(sample(10:100,25), nrow = 5, ncol = 5, byrow = TRUE,
             dimnames = list(c("r1","r2","r3","r4","r5"),c("c1","c2","c3","c4","c5")))

aq$Ozone[is.na(aq$Ozone)] <- mean(aq$Ozone,na.rm = T)

library(imputeTS)
rice_new <- na_mean(rice_new)

df4 <- merge(df1,df2, by.x = "Name", by.y = "Name")

AgeGroup <- cut(df4$Age,c(20,30,40,Inf),labels = c("A","B","C")) # lower limit is "less than", upper limit is "less than equals" ! !

df8 <- subset(df4,Working=="Yes"& Age>30)

operator <- readline(prompt = "Enter Operator :")
switch(operator,
       "+" = a+b,
       "-" = a-b,
       "/" = a/b)


