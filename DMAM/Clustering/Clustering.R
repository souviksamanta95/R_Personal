library(dplyr)
data("USArrests")
str(USArrests)
summary(USArrests)
my_data <- USArrests %>%
  na.omit() %>%          # Remove missing values (NA)
  scale()                # Scale variables

# View the firt 3 rows
head(my_data, n = 3)

pkgs <- c("factoextra",  "NbClust")
install.packages(pkgs)

library(factoextra)
library(NbClust)

# Elbow method
fviz_nbclust(my_data, kmeans, method = "wss") +
    labs(subtitle = "Elbow method")

set.seed(123)
?kmeans
kmc <- kmeans(my_data, 4, nstart = 25)

# Print the results
print(kmc)

# Cluster size
kmc$size
# Cluster means
kmc$centers
kmc$betweenss
kmc$totss
kmc$cluster

# Visualize
library("factoextra")
fviz_cluster(kmc, data = my_data,
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal())



#############################################
# Compute hierarchical clustering
res.hc <- USArrests %>%
  scale() %>%                    # Scale the data
  dist(method = "euclidean") %>% # Compute dissimilarity matrix
  hclust(method = "complete")     # Compute hierachical clustering

# Visualize using factoextra
# Cut in 4 groups and color by groups
fviz_dend(res.hc, k = 4, # Cut in four groups
          cex = 0.5, # label size
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE # Add rectangle around groups
)

##########################################
# Compute hierarchical clustering
res.hc <- USArrests %>%
  scale() %>%                    # Scale the data
  dist(method = "euclidean") %>% # Compute dissimilarity matrix
  hclust(method = "average")     # Compute hierachical clustering

# Visualize using factoextra
# Cut in 4 groups and color by groups
fviz_dend(res.hc, k = 4, # Cut in four groups
          cex = 0.5, # label size
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE # Add rectangle around groups
)

#############library("fpc")
set.seed(123)
db <- fpc::dbscan(my_data, eps = 0.7, MinPts = 3)

# Plot DBSCAN results
library("factoextra")
fviz_cluster(db, data = my_data, stand = FALSE,
             ellipse = FALSE, show.clust.cent = FALSE,
             geom = "point",palette = "jco", ggtheme = theme_classic())

################################

################################################
install.packages("fpc")

install.packages("dbscan")
library(dbscan)
dbscan::kNNdistplot(my_data, k = 3)
