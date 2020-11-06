setwd("C:/Users/kyras/OneDrive/Desktop/SLRSurvey/SLRSurvey/CurrentData")
library(igraph)
library(netrankr)

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

#Create weighted edgelist
library(data.table)
#create data table for policies from existing edge list
combined.data.table <- data.table(fincomboedge)
#create new spreadsheet with number of occurences for each node pair
weightedcomboedge <- combined.data.table[ , .N, by=.(combined.data.table$I1, combined.data.table$I2)]

#Create Network
