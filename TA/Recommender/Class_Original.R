#The dataset includes "5000 users from the anonymous rating data from the Jester Online Joke Recommender System."
#The ratings contained in the data frame range between -10.00 and 10.00.
#Each user included has rated at least 36 jokes.
#The dataset is stored as an item of class realRatingMatrix


library(recommenderlab)
data(Jester5k)
typeof(Jester5k)
jester <- as.vector(Jester5k@data)
jester <- matrix(data = jester, nrow = Jester5k@data@Dim[1], ncol = Jester5k@data@Dim[2])
jester <- as.data.frame(jester)
dim(jester)
head(jester)
rownames(jester) <- Jester5k@data@Dimnames[[1]]
names(jester) <- Jester5k@data@Dimnames[[2]]


#The goal of the recommender system will be to provide 5 recommended jokes per user for the first twenty users in the dataset.
#Based on the information available and the dimensions of the dataset.

#Creating the Model
premade_model <- Recommender(data = Jester5k, method = "UBCF", parameter = list(method = "Cosine", k = 20))

#Applying the Model
#The model is then applied to the dataset to obtain five recommendations each for the first twenty users.
#Here, the dataset is again subsetted to exclude those with fewer than 20 unrated jokes.
Jester5k_rec <- Jester5k[100 - rowCounts(Jester5k) > 20]
premade_allrecs <- predict(object = premade_model, newdata = Jester5k_rec, n = 5)


#Results
premade_recs <- data.frame(matrix(nrow = 20, ncol = 5))
rownames(premade_recs) <- names(premade_allrecs@items)[1:20]
for(i in 1:20) {
  for (j in 1:5) {
    premade_recs[i, j] <- paste0("j", premade_allrecs@items[[i]][j])
  }
}
names(premade_recs) <- as.character(1:5)
premade_recs