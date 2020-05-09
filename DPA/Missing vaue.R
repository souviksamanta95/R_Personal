# R approach of treating missing values -
aq <- airquality

sum(is.na(aq))
# Find out were is the missing data -
sapply(aq, function(x)sum(is.na(x)))

# row wise check how many missing values are there 
rowSums(is.na(aq))

# Graphical representation of missing values -
# Libraries - Amelia and Rcpp
library(Amelia)
library(Rcpp)

missmap(aq, main = "Missing Value Map")

# These missing values are to be omitted or replaced by mean, or some specific values

# using Hmisc library for treating missing values-
#install.packages("Hmisc")

library(Hmisc)

# Replacing values approach -
aq$Ozone <- with(aq, impute(Ozone,9)) #Replacing missing values with specific value i.e. 9 (say)
missmap(aq, main = "Missing Value Map")

# Omitting records approach -

aq2 <- airquality
sum(is.na(aq))
dim(aq2)

aq2 <- na.omit(aq2)
dim(aq2) # As some records have been removed, dimension will change


# Identifying outliers and treating them ----
# Visualizing by boxplot ---
library(ggplot2)

boxplot()


