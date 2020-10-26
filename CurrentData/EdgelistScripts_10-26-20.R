setwd("C:/Users/kyras/OneDrive/Desktop/SLRSurvey/SLRSurvey/CurrentData")
library(igraph)
library(netrankr)

Concerns_Raw <- read.csv("SLR_raw_concerns.csv")

#create edgelist that is only 2 columns for concerns
x<-Concerns_Raw #names my 3 column spreadsheet as object x
idx <- t(combn(seq_along(x), 2)) #generates all combinations of the 3 columns
concernedgelist <- lapply(1:nrow(idx), function(i) x[, c(idx[i, 1], idx[i, 2])]) 
concernedgelist <- lapply(concernedgelist, setNames, c("C1","C2")) #names columns C1 and C2
concernedgelist <- do.call(rbind, concernedgelist)
concernedgelist <- concernedgelist[rowSums(is.na(concernedgelist))==0, ]

#remove any pairs that are missing one or both concerns
finconcernedge<-concernedgelist[!(concernedgelist$C1==""|concernedgelist$C2==""), ]

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

#start trying to do the network for policies

