setwd("C:/Users/kyras/OneDrive/Desktop/SLRSurvey/SLRSurvey/CurrentFiles")
library(igraph)
library(netrankr)
library(data.table)
library(tidyverse)

#edges = data.table(numbers = 1:26,letters = letters)
#as.matrix(table(edges$numbers,edges$numbers))
#as.data.table(table(edges$numbers,edges$numbers))


#Load and Format Incidence Matrix-----------------------------
incidence <- read_csv("C:/Users/kyras/OneDrive/Desktop/SLRSurvey/SLRSurvey/CurrentFiles/Data/AdjMatrix_MinOtherRecode.csv")

incidencedf <- incidence[ , -c(1, 43)]
rownames(incidencedf)<-incidence$ResponseId
str(incidencedf)
incidencedf <- as.data.frame(incidencedf)
str(incidencedf)
incidencedf$rowsum <- rowSums(incidencedf)

incidencedf<- incidencedf[incidencedf$rowsum>0, ]
incidencedf <- incidencedf[ , -c(42)]
incidencemat <- as.matrix(incidencedf)

#need to figure out if this is actually an incidence matrix or how to make it that... incidence matrix would be saying that each concept is actually an edge and the vertices were people...

#Create Network--------------
network <- graph_from_incidence_matrix(incidencemat, directed=FALSE, mode='out', weighted=TRUE)



