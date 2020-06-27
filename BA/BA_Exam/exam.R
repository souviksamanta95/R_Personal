# Setting working directory
setwd("C:/BA")

# Reading the data from csv file
data1 <- read.csv("Western_Music_Lover.csv", header = T)
head(data1)

# Normalizing the data
data1.norm <- sapply(data1[,-1], scale)

# Applying k-means clustering for 3 clusters
library(caret)
km <- kmeans(data1.norm, 3)
km
table(km$cluster)

library(factoextra)
library(ggpubr)
fviz_cluster(km, data = data1.norm)

plot(c(0), xaxt = 'n', xlab = "", ylab = "", type = "l",
     ylim = c(min(km$centers), max(km$centers)), xlim = c(0,17))
axis(1, at = c(1:17), labels = names(data1[,-1]))
for (i in c(1:3)){
  lines(km$centers[i,], lty = i, lwd = 2,
        col = ifelse(i %in% c(1,3,5), "black", "dark grey"))}
text(x = 0.5, y = km$centers[, 1], labels = paste("Cluster", c(1:3)))
