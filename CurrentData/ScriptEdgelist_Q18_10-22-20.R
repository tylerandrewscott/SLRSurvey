setwd("C:/Users/kyras/OneDrive/Desktop/SLRSurvey/SLRSurvey/CurrentData")
library(igraph)
library(netrankr)

Top3Policies_Edgelist <- read.csv("Edgelist_Q18_10-22-20.csv")

?lapply

rm(idx)
?combn
#combn(x, m, FUN = NULL, simplify = TRUE, ...)
#x	is vector source for combinations, or integer n for x <- seq_len(n).

#m	 is number of elements to choose.

#FUN	is function to be applied to each combination; default NULL means the identity, i.e., to return the combination (vector of length m).

#simplify	is logical indicating if the result should be simplified to an array (typically a matrix); if FALSE, the function returns a list. Note that when simplify = TRUE as by default, the dimension of the result is simply determined from FUN(1st combination) (for efficiency reasons). This will badly fail if FUN(u) is not of constant length.

?seq_along
#Generate regular sequences. seq is a standard generic with a default method. seq.int is a primitive which can be much faster but has a few restrictions. seq_along and seq_len are very fast primitives for two common cases.

?t

#create new edgelist that is first all policies 1 with 2, then all policies 1 with 3, then policies 2 with 3 to create 2 column edgelist
x<-Top3Policies_Edgelist #names my 3 column spreadsheet as object x
idx <- t(combn(seq_along(x), 2)) #generates all combinations of the 3 columns
edgelist <- lapply(1:nrow(idx), function(i) x[, c(idx[i, 1], idx[i, 2])]) 
edgelist <- lapply(edgelist, setNames, c("P1","P2")) #names columns P1 and P2
edgelist <- do.call(rbind, edgelist)
edgelist <- edgelist[rowSums(is.na(edgelist))==0, ]
edgelist

#now can graph edgelist into network

