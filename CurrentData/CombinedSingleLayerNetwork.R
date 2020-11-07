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

#Match vector attributes

weightedcomboedge$TypeI1=factor(attrib[match(weightedcomboedge$I1, attrib$Vector), "Type"])

weightedcomboedge$TypeI2=factor(attrib[match(weightedcomboedge$I2, attrib$Vector), "Type"])

weightedcomboedge <- weightedcomboedge[ , c("I1", "I2", "weight", "TypeI1", "TypeI2")]

weightedcomboedge$colorI1=c("darkseagreen", "goldenrod", "slateblue")[as.numeric(weightedcomboedge$TypeI1)]

weightedcomboedge$colorI2=c("darkseagreen", "goldenrod", "slateblue")[as.numeric(weightedcomboedge$TypeI2)]

#create network
set.seed(2)
combinednet=graph_from_data_frame(weightedcomboedge, directed=FALSE)
combode=igraph::degree(combinednet)

plot(combinednet, layout=layout_with_graphopt, vertex.size=combode, edge.width=weightedcomboedge$weight, edge.color="black", )
