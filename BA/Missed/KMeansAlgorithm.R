#K Means Clustering
#K Means Clustering is an unsupervised leaning
#The purpose if to  group similar items together and form clusters

#Read the dataset
Cluster_Data <- read.csv("Utilities.csv")
View(Cluster_Data)
head(Cluster_Data)

#Normalize
normalize=function(x){
  return((x - min(x))/(max(x)-min(x)))}

Cluster_Data_N=as.data.frame(lapply(Cluster_Data[2:9], normalize))

head(Cluster_Data_N)
View(Cluster_Data_N)

#Build the model
results.Utilities =kmeans(Cluster_Data_N,4)

results.Utilities

#To see Size of each cluster -  How many records are in one cluster
results.Utilities$size

#What are the clusters - Element wise cluster is created
results.Utilities$cluster

#Center of clusters - 
results.Utilities$centers


#Assigning Cluster and adding column in the dataset
library(dplyr)
clu=Cluster_Data%>%
  mutate(Cluster_Assigned=results.Utilities$cluster)
View(clu)

#Example of Plotting the clusters
plot(Cluster_Data_N$Sales~Cluster_Data_N$Cost,col=results.Utilities$cluster)
  