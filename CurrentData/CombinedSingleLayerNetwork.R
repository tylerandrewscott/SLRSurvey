setwd("C:/Users/kyras/OneDrive/Desktop/SLRSurvey/SLRSurvey/CurrentData")
library(igraph)
library(netrankr)
library(data.table)

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

#Create Network--------------------------------------------------------------------
#Load in vertex attribute
attrib <- read.csv("SLR_Combined_VectorTypes.csv")

#try to create edge attribute for type of edge inside weighted edgelist
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
  
#create network
set.seed(2)
combinednet=graph_from_data_frame(weightedcomboedge, directed=FALSE)
combode=igraph::degree(combinednet)

#create vertex attributes
combinednet <- set.vertex.attribute(combinednet, "Name", attrib$Vector, attrib$Vector)
combinednet <- set.vertex.attribute(combinednet, "Type", index=attrib$Vector, attrib$Type)
combinednet <- set.vertex.attribute(combinednet, "Color", index=attrib$Vector, value=attrib$Color)

list.vertex.attributes(combinednet)
get.vertex.attribute(combinednet)

list.edge.attributes(combinednet)


#plot with vertex color using attributes
plot(combinednet, layout=layout_with_graphopt, vertex.size=combode/4, edge.width=weightedcomboedge$weight/10, vertex.color=V(combinednet)$Color, vertex.label.cex=0.5, edge.color=weightedcomboedge$edgecolor)

plot(combinednet, layout=layout_with_fr, vertex.size=combode/4, edge.width=weightedcomboedge$weight/10, vertex.color=V(combinednet)$Color, vertex.label.cex=0.5, edge.color=weightedcomboedge$edgecolor)

#try and remove edges below certain weight

tenedgethreshold <- combinednet
tenedgethreshold<- delete.edges(tenedgethreshold, which(E(tenedgethreshold)$weight <10)-1)
tende=igraph::degree(tenedgethreshold)
tenedgethreshold <- delete.vertices(tenedgethreshold, V(tenedgethreshold)[degree(tenedgethreshold)==0])
plot(tenedgethreshold, layout=layout_with_graphopt, vertex.size=tende/4, edge.width=E(tenedgethreshold)$weight/15, vertex.color=V(tenedgethreshold)$Color, vertex.label.cex=0.5, edge.color=E(tenedgethreshold)$edgecolor)

