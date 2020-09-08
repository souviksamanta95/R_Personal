library(tidyverse)


setwd("D:/Analytics/R/PRA/Datasets")

edge_list <- tibble(from = c(1, 2, 2, 3, 4), to = c(2, 3, 4, 2, 1))
node_list <- tibble(id = 1:4)
edge_list
node_list

letters <- read.csv("correspondence.csv")
letters

sources <- letters %>%
  distinct(source) %>%
  rename(label = source)

destinations <- letters %>%
  distinct(destination) %>%
  rename(label = destination)

nodes <- full_join(sources, destinations, by = "label")
nodes

nodes <- nodes %>% rowid_to_column("id")
nodes

per_route <- letters %>%  
  group_by(source, destination) %>%
  summarise(weight = n()) %>% 
  ungroup()
per_route


edges <- per_route %>% 
  left_join(nodes, by = c("source" = "label")) %>% 
  rename(from = id)

edges <- edges %>% 
  left_join(nodes, by = c("destination" = "label")) %>% 
  rename(to = id)

edges

edges <- per_route %>% 
  left_join(nodes, by = c("source" = "label")) %>% 
  rename(from = id)

edges <- edges %>% 
  left_join(nodes, by = c("destination" = "label")) %>% 
  rename(to = id)

edges


edges <- select(edges, from, to, weight)
edges 

#install.packages("network")
library(network)
routes_network <- network(edges, vertex.attr = nodes, matrix.type = "edgelist", ignore.eval = FALSE)
class(routes_network)

routes_network


plot(routes_network, vertex.cex = 3)

# Using igraph library

detach(package:network) 
rm(routes_network) 

#install.packages("igraph")
library(igraph)

routes_igraph <- graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)
routes_igraph

plot(routes_igraph, edge.arrow.size = 0.2)


plot(routes_igraph, layout = layout_with_graphopt, edge.arrow.size = 0.2)


# Tutorial 2 -----

actors <- read_csv("Actors.csv") 
movies <- read_csv("Movies.csv")
actorNetwork <- graph_from_data_frame(d=movies, vertices=actors, directed=F)
plot(actorNetwork)

E(actorNetwork)$color <- ifelse(E(actorNetwork)$Movie == "Forest Gump", "green", ifelse(E(actorNetwork)$Movie == "Apollo 13", "black", ifelse(E(actorNetwork)$Movie == "The Rock", "orange", "red"))) 

V(actorNetwork)$color <- ifelse(V(actorNetwork)$Gender == "Male", "lightblue", "pink") 

#Replot the network
plot(actorNetwork)
legend("topleft", c("Male","Female"), pch=21, col="#777777", pt.bg=c("lightblue","pink"), pt.cex=2, cex=.8) 

legend("bottomright", c("Forest Gump","Apollo 13", "The Rock", "Titanic"), col=c("green","black","orange","red"), lty=1, cex=.8)
degree(actorNetwork, mode="all")

closeness(actorNetwork, mode="all", weights=NA, normalized=T)
betweenness(actorNetwork, directed=F, weights=NA, normalized = T)
distances(actorNetwork, v=V(actorNetwork)["Kevin Bacon"], to=V(actorNetwork), weights=NA)























