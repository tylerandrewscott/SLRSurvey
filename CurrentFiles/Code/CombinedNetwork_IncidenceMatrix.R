setwd("C:/Users/kyras/OneDrive/Desktop/SLRSurvey/SLRSurvey/CurrentFiles")
library(igraph)
library(netrankr)
library(data.table)
library(tidyverse)

#Load and Format Incidence Matrix-----------------------------
adjacency <- read_csv("C:/Users/kyras/OneDrive/Desktop/SLRSurvey/SLRSurvey/CurrentFiles/Data/AdjMatrix_MinOtherRecode.csv")

adjacencydf <- adjacency[ , -c(1, 43)]
str(adjacencydf)
adjacencydf <- as.data.frame(adjacencydf)
str(adjacencydf)
adjacencydf$rowsum <- rowSums(adjacencydf[ , 1:41])
rownames(adjacencydf) <- adjacency$ResponseId


adjacencydf<- adjacencydf[adjacencydf$rowsum>0, ]
adjacencydf <- adjacencydf[ , -c(42)]
adjmat <- as.matrix(adjacencydf)


#Create Network--------------
?graph.incidence
  
net <- graph.incidence(adjmat, add.names=TRUE)
igraph::list.vertex.attributes(net)

vertex_attr(net, index=V(net))

#list of attribute names to match to their number for group ID
attributes <- get.vertex.attribute(net, "TRUE", index=V(net))
attributes

#Louvain Clustering Algorithm------------------------------------
louvain_communities <- igraph::cluster_louvain(net)
igraph::groups(louvain_communities)
louvain_communities
#8 groups with modularity 0.15

#FastGreedy (Newmans) Clustering Algorithm------------------------
fastgreedycommunity <- cluster_fast_greedy(net)
fastgreedycommunity
igraph::groups(fastgreedycommunity)
#6 groups with mod 0.18

#InfoMap Clustering Algorithm--------------
#some articles say can't Use InfoMap on Bipartite network (the random walk is periodic)
infomapcommunity <- cluster_infomap(net)
infomapcommunity
#when run it get one single group with 0 modularity

#Walk Trap Clustering Algorithm-------------------
walktrapcommunity <- cluster_walktrap(net)
walktrapcommunity
#739 groups with modularity 0 (all single vertex communities)

#Edge Betweenness---------------
edgebetween <- cluster_edge_betweenness(net, directed=FALSE)
edgebetween
class(edgebetween)
#gives one single group with 0 modularity 
