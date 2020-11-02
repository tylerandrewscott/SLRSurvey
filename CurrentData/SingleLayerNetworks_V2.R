setwd("C:/Users/kyras/OneDrive/Desktop/SLRSurvey/SLRSurvey/CurrentData")
library(igraph)
library(netrankr)
library(sna)

#Concerns to concerns edgelist----------------------------------------------------
Concerns_Raw <- read.csv("SLR_raw_concerns.csv")
Concerns_Raw[555,3]="Ecosystem" #one typo in dataset

#create edgelist that is only 2 columns for concerns
x<-Concerns_Raw #names my 3 column spreadsheet as object x
idx <- t(combn(seq_along(x), 2)) #generates all combinations of the 3 columns
concernedgelist <- lapply(1:nrow(idx), function(i) x[, c(idx[i, 1], idx[i, 2])]) 
concernedgelist <- lapply(concernedgelist, setNames, c("C1","C2")) #names columns C1 and C2
concernedgelist <- do.call(rbind, concernedgelist)
concernedgelist <- concernedgelist[rowSums(is.na(concernedgelist))==0, ]

#remove any pairs that are missing one or both concerns
finconcernedge<-concernedgelist[!(concernedgelist$C1==""|concernedgelist$C2==""), ]

#policies to policies edgelist--------------------------------------------------------

#create edgelist that is only 2 columns for policy priorities
Policies_Raw <- read.csv("SLR_raw_policies.csv")
y <-Policies_Raw #names my 3 column spreadsheet as object y
idy <- t(combn(seq_along(y), 2)) #generates all combinations of the 3 columns
policyedgelist <- lapply(1:nrow(idy), function(i) y[, c(idy[i, 1], idy[i, 2])]) 
policyedgelist <- lapply(policyedgelist, setNames, c("P1","P2")) #names columns P1 and P2
policyedgelist <- do.call(rbind, policyedgelist)
policyedgelist <- policyedgelist[rowSums(is.na(policyedgelist))==0, ]

#remove any pairs that are missing one or both policies
finpolicyedge<-policyedgelist[!(policyedgelist$P1==""|policyedgelist$P2==""), ]

#barriers to barriers edgelist-----------------------------------------------------

#create edgelist that is only 2 columns for barriers
Barriers_Raw <- read.csv("SLR_raw_barriers.csv")
z <-Barriers_Raw #names my 3 column spreadsheet as object z
idz <- t(combn(seq_along(z), 2)) #generates all combinations of the 3 columns
barrieredgelist <- lapply(1:nrow(idz), function(i) z[, c(idz[i, 1], idz[i, 2])]) 
barrieredgelist <- lapply(barrieredgelist, setNames, c("B1","B2")) #names columns B1 and B2
barrieredgelist <- do.call(rbind, barrieredgelist)
barrieredgelist <- barrieredgelist[rowSums(is.na(barrieredgelist))==0, ]

#remove any pairs that are missing one or both barriers
finbarrieredge<-barrieredgelist[!(barrieredgelist$B1==""|barrieredgelist$B2==""), ]

#Create weighted edgelists------------------------------
install.packages("data.table")
library(data.table)
#create data table for policies from existing edge list
policy.data.table <- data.table(finpolicyedge)
#create new spreadsheet with number of occurences for each node pair
weightedpolicies <- policy.data.table[ , .N, by=.(policy.data.table$P1, policy.data.table$P2)]
#---------------------------------------------------
#do the same thing for concerns
#create data table for concerns from existing edge list
concern.data.table <- data.table(finconcernedge)
#create new spreadsheet with number of occurrences for each node pair
weightedconcerns <- concern.data.table[ , .N, by=.(concern.data.table$C1, concern.data.table$C2)]
#-------------------------------------------------------------
#do the same thing for barriers
#create data table for barriers from existing edge list
barrier.data.table <- data.table(finbarrieredge)
#create new spreadsheet with number of occurences for each node pair
weightedbarriers <- barrier.data.table[ , .N, by=.(barrier.data.table$B1, barrier.data.table$B2)]

#rename policy data table columns for use as weighted edgelist
setnames(weightedpolicies, "N", "weight")
setnames(weightedpolicies, "policy.data.table", "P1")
setnames(weightedpolicies, "policy.data.table.1", "P2")

#rename concerns data table for use as weighted edgelist

setnames(weightedconcerns, "N", "weight")
setnames(weightedconcerns, "concern.data.table", "C1")
setnames(weightedconcerns, "concern.data.table.1", "C2")
#rename barriers data table for use as weighted edgelist

setnames(weightedbarriers, "N", "weight")
setnames(weightedbarriers, "barrier.data.table", "B1")
setnames(weightedbarriers, "barrier.data.table.1", "B2")

#create policy network--------------------------
set.seed(2)
policiesnet=graph_from_data_frame(weightedpolicies, directed=FALSE)
policiesnet
plot(policiesnet, edge.width=E(policiesnet)$weight, layout=layout_with_fr)

#create degree to size nodes and replot with new layout
polde=igraph::degree(policiesnet)
plot(policiesnet,vertex.size=polde, edge.width=E(policiesnet)$weight/10, layout=layout_with_graphopt)
plot(policiesnet,vertex.size=polde, edge.width=E(policiesnet)$weight/10, layout=layout_with_dh)
plot(policiesnet,vertex.size=polde, edge.width=E(policiesnet)$weight/10, layout=layout_with_fr)

#create concerns network---------------------------
set.seed(2)
concernsnet=graph_from_data_frame(weightedconcerns, directed=FALSE)

#create degree to size nodes and replot with new layout
conde=igraph::degree(concernsnet)
plot(concernsnet,vertex.size=conde, edge.width=E(concernsnet)$weight/10, layout=layout_with_graphopt)
plot(concernsnet,vertex.size=conde, edge.width=E(concernsnet)$weight/10, layout=layout_with_fr)

#create barriers network---------------------------------------
set.seed(2)
barriersnet=graph_from_data_frame(weightedbarriers, directed=FALSE)

#create degree to size nodes and replot with new layout
barde=igraph::degree(barriersnet)
plot(barriersnet,vertex.size=barde, edge.width=E(barriersnet)$weight/10, layout=layout_with_graphopt)
plot(barriersnet,vertex.size=barde, edge.width=E(barriersnet)$weight/10, layout=layout_with_dh)
