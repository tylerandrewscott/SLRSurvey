setwd("C:/Users/kyras/OneDrive/Desktop/SLRSurvey/SLRSurvey/CurrentData")
library(igraph)
library(netrankr)
library(data.table)
library(sna)
library(ergm.count)
library(ergm.rank)
library(latentnet)

#Create All Pairs Edgelist--------------------------
#Create edgelist with all policies, concerns, barrier pairs in 2 columns
CombinedEdge_Raw <- read.csv("AllCombined_Policy_Concern_Barriers.csv")
combo <-CombinedEdge_Raw #names my 3 column spreadsheet as object x
idcombo <- t(combn(seq_along(combo), 2))
comboedgelist <- lapply(1:nrow(idcombo), function(i) combo[, c(idcombo[i, 1], idcombo[i, 2])]) 
comboedgelist <- lapply(comboedgelist, setNames, c("I1","I2")) #names columns 1 and 2
comboedgelist <- do.call(rbind, comboedgelist)
comboedgelist <- comboedgelist[rowSums(is.na(comboedgelist))==0, ]

#remove any pairs that are missing one or both in a pair for final edge list
fincomboedge<-comboedgelist[!(comboedgelist$I1==""|comboedgelist$I2==""), ]

#Create weighted edgelist------------------------------------------------------------
#create data table for policies from existing edge list
combined.data.table <- data.table(fincomboedge)
#create new spreadsheet with number of occurences for each node pair
weightedcomboedge <- combined.data.table[ , .N, by=.(combined.data.table$I1, combined.data.table$I2)]

#rename data table columns for use as weighted edgelist
setnames(weightedcomboedge, "N", "weight")
setnames(weightedcomboedge, "combined.data.table", "I1")
setnames(weightedcomboedge, "combined.data.table.1", "I2")

#Load vertex attribute--------------------------------------------------------------------
attrib <- read.csv("SLR_Combined_VectorTypes.csv")

####try to create edge attribute for type of edge inside weighted edgelist----------------
weightedcomboedge$Type1 <- attrib$Type[match(weightedcomboedge$I1, attrib$Vector)]

weightedcomboedge$Type2 <- attrib$Type[match(weightedcomboedge$I2, attrib$Vector)]

weightedcomboedge$edgetype <- "NA"

  weightedcomboedge[weightedcomboedge$Type1=="Policy" & weightedcomboedge$Type2=="Policy"]$edgetype <- "Policy-Policy"
  weightedcomboedge[weightedcomboedge$Type1=="Concern" & weightedcomboedge$Type2=="Concern"]$edgetype <- "Concern-Concern"
  weightedcomboedge[weightedcomboedge$Type1=="Barrier" & weightedcomboedge$Type2=="Barrier"]$edgetype <- "Barrier-Barrier"
  weightedcomboedge[weightedcomboedge$Type1=="Policy"& weightedcomboedge$Type2=="Concern"]$edgetype <- "Policy-Concern"
  weightedcomboedge[weightedcomboedge$Type1=="Policy"& weightedcomboedge$Type2=="Barrier"]$edgetype <- "Policy-Barrier"
  weightedcomboedge[weightedcomboedge$Type1=="Barrier"& weightedcomboedge$Type2=="Concern"]$edgetype <- "Barrier-Concern"
  weightedcomboedge[weightedcomboedge$Type1=="Barrier"& weightedcomboedge$Type2=="Policy"]$edgetype <- "Policy-Barrier"
  weightedcomboedge[weightedcomboedge$Type1=="Concern"& weightedcomboedge$Type2=="Barrier"]$edgetype <- "Barrier-Concern"
  weightedcomboedge[weightedcomboedge$Type1=="Concern"& weightedcomboedge$Type2=="Policy"]$edgetype <- "Policy-Concern"

#create color edge type attribute in weighted edge list
weightedcomboedge$edgecolor <- "NA"  
weightedcomboedge[weightedcomboedge$edgetype=="Policy-Policy"]$edgecolor <- "#dcdcdc"
weightedcomboedge[weightedcomboedge$edgetype=="Barrier-Barrier"]$edgecolor <- "#dcdcdc"
weightedcomboedge[weightedcomboedge$edgetype=="Concern-Concern"]$edgecolor <- "#dcdcdc"
weightedcomboedge[weightedcomboedge$edgetype=="Policy-Concern"]$edgecolor <- "#808080"
weightedcomboedge[weightedcomboedge$edgetype=="Policy-Barrier"]$edgecolor <- "#808080"
weightedcomboedge[weightedcomboedge$edgetype=="Barrier-Concern"]$edgecolor <- "#808080"
  
####create network-----------------------------------------------------------
set.seed(2)
combinednet=igraph::graph_from_data_frame(weightedcomboedge, directed=FALSE)
combode=igraph::degree(combinednet)


####create vertex attributes------------------------------------
combinednet <- igraph::set.vertex.attribute(combinednet, "Name", index=attrib$Vector, value=attrib$Vector)
combinednet <- igraph::set.vertex.attribute(combinednet, "Type", index=attrib$Vector, attrib$Type)
combinednet <- igraph::set.vertex.attribute(combinednet, "Color", index=attrib$Vector, value=attrib$Color)

?set.vertex.attribute()

igraph::list.vertex.attributes(combinednet)

####plot with vertex color using attributes-----------------------------------
plot(combinednet, layout=layout_with_graphopt, vertex.size=combode/4, edge.width=weightedcomboedge$weight/10, vertex.color=V(combinednet)$Color, vertex.label.cex=0.5, edge.color=weightedcomboedge$edgecolor)

plot(combinednet, layout=layout_with_fr, vertex.size=combode/4, edge.width=weightedcomboedge$weight/10, vertex.color=V(combinednet)$Color, vertex.label.cex=0.5, edge.color=weightedcomboedge$edgecolor)

plot(combinednet, layout=layout_with_drl, vertex.size=combode/4, edge.width=weightedcomboedge$weight/20, vertex.color=V(combinednet)$Color, vertex.label.cex=0.5, edge.color=weightedcomboedge$edgecolor)


####try and remove edges below certain weight----------------------------

tenedgethreshold <- combinednet
tenedgethreshold<- delete.edges(tenedgethreshold, which(E(tenedgethreshold)$weight <10)-1)
tende=igraph::degree(tenedgethreshold)
tenedgethreshold <- delete.vertices(tenedgethreshold, V(tenedgethreshold)[degree(tenedgethreshold)==0])
plot(tenedgethreshold, layout=layout_with_graphopt, vertex.size=tende/4, edge.width=E(tenedgethreshold)$weight/15, vertex.color=V(tenedgethreshold)$Color, vertex.label.cex=0.5, edge.color=E(tenedgethreshold)$edgecolor)

####try to do community detection--------------------------------------
#louvain method
louvain_communities <- igraph::cluster_louvain(combinednet, weights=E(combinednet)$weight)
combinednet$community <- louvain_communities$membership
unique(combinednet$community)

plot(combinednet, vertex.color=rainbow(3, alpha=0.6)[louvain_communities$membership], vertex.label.cex=0.25)

louvain_communities$membership

louvain_communities

#Newman's Modularity
simple_combinednet = simplify(combinednet)
fastgreedycommunity <- cluster_fast_greedy(simple_combinednet, weights=E(combinednet)$weight)

fastgreedycommunity
sizes(fastgreedycommunity)
membership(fastgreedycommunity)

plot(fastgreedycommunity, simple_combinednet)

#Edge Betweenness
cluster_edge_betweenness(combinednet, weights=E(combinednet)$weight, directed=FALSE)
#only gives one group with modularity 0 so not worth using this method

#Info Map
?cluster_infomap
infomapcommunity <- cluster_infomap(combinednet, e.weights=E(combinednet)$weight)
infomapcommunity
membership(infomapcommunity)
plot(infomapcommunity, simple_combinednet)

