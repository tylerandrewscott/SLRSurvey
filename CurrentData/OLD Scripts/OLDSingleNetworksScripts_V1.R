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

#policies to policies network-----------------------------------------------
#start trying to do the network for concerns
set.seed(2)
policiesnet=graph_from_data_frame(finpolicyedge, directed=FALSE) 
policiesnet

#try to visualize the policies
plot(policiesnet)
plot(policiesnet, layout=layout_with_fr(policiesnet))
dev.off()

#start looking into measuring policies network
degree(policiesnet) #degree centrality
betweenness(policiesnet) #betweenness centrality
closeness(policiesnet) #number steps required to access every node from a given node

#create dataset of node-level measures
names=V(policiesnet)$name
de=degree(policiesnet)
st=graph.strength(policiesnet)
be=betweenness(policiesnet, normalized=T)
nodedata_policies=data.frame(node.name=names, degree=de, strength=st, betweenness=be)
head(nodedata_policies)

#realized since unweighted network strength and degree will be same I think
plot(strength~degree, data=nodedata_policies)

#relationship between strength and betweenness in policies
plot(betweenness~strength, data=nodedata_policies)

#calculate number of nodes (16) and edges (1937) for policies
nodenum_policies=vcount(policiesnet)
edgesnum_policies=ecount(policiesnet)

nodenum_policies
edgesnum_policies

#calculate the density
#dyads_policies=(nodenum_policies*(nodenum_policies -1))/2
#density_policies=edgesnum_policies/dyads_policies
#density_policies
#can also just use function which is easier
edge_density(policiesnet)

#plot the degree distribution (statistical distribution of node degrees in network) for policies
hist(degree(policiesnet), breaks=10, col="gray")

#plot probability densities of each degree (proportion of nodes with degree 1, 2, etc.) for policies network
plot(degree.distribution(policiesnet), pch=19)

#paths for policies network- results are 1 or 2 for most since all policies tend to be picked once by at least one person
paths_policies=distances(policiesnet, algorithm="unweighted")
paths_policies

#community detection
eb_policies=edge.betweenness.community(policiesnet)
eb_policies
length(eb_policies) #number of communiteis
modularity(eb_policies) #modularity
membership(eb_policies) #assignment of nodes to communities
plot(eb_policies, policiesnet)

#concerns network-------------------------------------------------------------------------


set.seed(2)
concernsnet=graph_from_data_frame(finconcernedge, directed=FALSE) 
concernsnet

#try to visualize the concerns
plot(concernsnet)
plot(concernsnet, layout=layout_with_fr(concernsnet))
dev.off()

#start looking into measuring concerns network
?detach
detach(package:sna, unload=FALSE) #detached sna package so that it would run these functions with igraph since I have igraph object
degree(concernsnet) #degree centrality
betweenness(concernsnet) #betweenness centrality
closeness(concernsnet) #number steps required to access every node from a given node

#create dataset of node-level measures
namesconcern=V(concernsnet)$name
deconcern=degree(concernsnet)
stconcern=graph.strength(concernsnet)
beconcern=betweenness(concernsnet, normalized=T)
nodedata_concerns=data.frame(node.name=namesconcern, degree=deconcern, strength=stconcern, betweenness=beconcern)
head(nodedata_concerns)

#realized since unweighted network strength and degree will be same I think
plot(strength~degree, data=nodedata_concerns)

#relationship between strength and betweenness in concerns
plot(betweenness~strength, data=nodedata_concerns)

#density
edge_density(concernsnet)

#plot the degree distribution (statistical distribution of node degrees in network) for concerns
hist(degree(concernsnet), breaks=10, col="gray")

#plot probability densities of each degree (proportion of nodes with degree 1, 2, etc.) for concerns network
plot(degree.distribution(concernsnet), pch=19)

#paths for concerns
paths_concerns=distances(concernsnet, algorithm="unweighted")
paths_concerns

#community detection
eb_concerns=edge.betweenness.community(concernsnet)
eb_concerns
length(eb_concerns) #number of communities
modularity(eb_concerns) #modularity
membership(eb_concerns) #assignment of nodes to communities
plot(eb_concerns, concernsnet)

#Counting possible combinations that occur in each single network-------------------------

#Trying to get at the number of times different direct connections occur between two nodes in policies edgelist to see which are commonly selected together
#double check how many different policies there are
unique(finpolicyedge$P1)
unique(finpolicyedge$P2)

library(dplyr)

policycombos <- finpolicyedge %>% count(P1, P2)

#do this for concerns and barriers too
concerncombos <- finconcernedge %>% count(C1, C2)
barriercombos <- finbarrieredge %>% count(B1, B2) 

#output these to excel and will need to combine the identical reverse connections (water and transpo with transpo and water in only concerns spreadsheet)

library(writexl)
#write_xlsx(policycombos, "policycombos.xlsx")
#write_xlsx(concerncombos, "concerncombos.xlsx")
#write_xlsx(barriercombos, "barriercombos.xlsx")
#already did this so won't run this in code again

#Creating barriers network--------------------------------------------------------------

set.seed(2)
barriersnet=graph_from_data_frame(finbarrieredge, directed=FALSE) 
concernsnet

#try to visualize the barrier
plot(barriersnet)
plot(barriersnet, layout=layout_with_fr(concernsnet))
dev.off()

#start looking into measuring barriers network
?degree
degree(barriersnet) #degree centrality
betweenness(barriersnet) #betweenness centrality
closeness(barriersnet) #number steps required to access every node from a given node

#create dataset of node-level measures
namesbarrier=V(barriersnet)$name
debarrier=degree(barriersnet)
stbarrier=graph.strength(barriersnet)
bebarrier=betweenness(barriersnet, normalized=T)
nodedata_barriers=data.frame(node.name=namesbarrier, degree=debarrier, strength=stbarrier, betweenness=bebarrier)
head(nodedata_barriers)

#realized since unweighted network strength and degree will be same I think
plot(strength~degree, data=nodedata_barriers)

#relationship between strength and betweenness in barriers
plot(betweenness~strength, data=nodedata_barriers)

#density
edge_density(barriersnet)

#plot the degree distribution (statistical distribution of node degrees in network) for barriers
hist(degree(barriersnet), breaks=10, col="gray")

#plot probability densities of each degree (proportion of nodes with degree 1, 2, etc.) for barriers network
plot(degree.distribution(barriersnet), pch=19)

#paths for barriers
paths_barriers=distances(barriersnet, algorithm="unweighted")
paths_barriers

#community detection
eb_barriers=edge.betweenness.community(barriersnet)
eb_barriers
length(eb_barriers) #number of communities
modularity(eb_barriers) #modularity
membership(eb_barriers) #assignment of nodes to communities
plot(eb_barriers, barriersnet)


#Using sna package-------------------------------------------------------------------------
library(sna)
snapolicyedge <- as.edgelist.sna(finpolicyedge)
gplot(snapolicyedge)
?gplot
policynetwork <- network(finpolicyedge, directed=FALSE)
#gplot(policynetwork, usearrows=FALSE, edge.lwd = 0.2,vertex.cex = degree(policynetwork)/100,interactive = TRUE) # Modify and save the exact coordinates to reproduce
#save to object coord to reproduce same graph 
#gplot(net, displaylabels = F,usearrows= F,edge.col = edgeColors,edge.lwd = 0.2,coord = coords) # Modify and save

