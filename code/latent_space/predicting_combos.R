
packs =c('tidyverse','purrr','data.table','statnet','latentnet','bipartite')
need = packs[!sapply(packs,require,character.only=T)]
sapply(need,install.packages,type= 'source')
sapply(packs[need],require,character.only=T)

library(readxl)
orig = readxl::read_excel('Current Files/RawExcel/SLRSurvey_Full.xlsx')
orig[,grepl('^Q4',colnames(orig))]
orig$Q4[is.na(orig$Q4)]<-'Other'
#recode anything with fewer than 10 respondents as other
# see what we'd recode
as.data.table(table(orig$Q4))[order(-N),][N<10,]
orig$Q4[orig$Q4 %in% as.data.table(table(orig$Q4))[order(-N),][N<10,]$V1] <- 'Other'

incidence_dt = fread('Current Files/Data/NewAdjacencyMatrix_ColumnRenames.csv')
incidence_mat = as.matrix(incidence_dt[,-c('ResponseId','ResponseId_number','DK')])
rownames(incidence_mat)<-incidence_dt$ResponseId
#drop isolates
incidence_mat = incidence_mat[rowSums(incidence_mat)!=0,]

#create network object
bip_net = as.network(incidence_mat,matrix.type = 'incidence',bipartite = T,directed = F,loops = F)
#code actor types
bip_net %v% 'Actor_Type' <- orig$Q4[match(network.vertex.names(bip_net),orig$ResponseId)]
#code concept types
concept_types = fread('Current Files/Data/CombinedNetwork/Combined_VectorTypes_NoNewOther.csv')
bip_net %v% 'Concept_Type' <- concept_types$Type[match(network.vertex.names(bip_net), concept_types$Vector)]
set.vertex.attribute(bip_net,'Concept_Type',value = 'Person',v = which(is.na(bip_net %v% 'Concept_Type')))


install.packages('lvm4net')
library(lvm4net)
require(parallel)

seed = 24
#ccores = floor(detectCores()/1.25)
ccores = 50
control_custom = control.ergmm(threads = ccores,sample.size = ccores * 100,mle.maxit = 10,kl.threads = ccores)

d2fits<-lapply(0:6,function(g){print(g);ergmm(bip_net~euclidean(d=2,G=g),control = control_custom,verbose = T,tofit = c('mcmc'))})

Y = as.sociomatrix(bip_net)
lt2 = lta(Y,D = 2)
lt3 = lta(Y,D = 3)

mlttest = mlta(X = Y,G = 2,D = 2)


mlt_list = lapply(1:4,function(g) mlta(X = Y,G = g,D = 2))
plotY(Y)
goflsm(lt3)
mltd2g = mlta(Y,G = 2,D = 2)

round(mlttest$z,2)

mlttest$eta

plot(test2,Y)

test3$w

bip_net %v% 'Concept_Type' 

test3 = lta(as.sociomatrix(bip_net),D = 3)

library(plotly)
dim(mlttest$mu)

wh = round(mlttest$z)==1

tt = rbind(mlttest$mu[wh[,1],,1],
mlttest$mu[wh[,2],,2])


lt_for_plotly = data.table(tt)

ggplot()
#plot_ly(data = lt,x = ~ V1,y = ~V2, z = ~V3)

plot_ly(data = lt,x = ~ V1,y = ~V2)

head(lt)

test3$C
test3$b
test3$w


str(test2)
test2$BIC
test3$BIC


plot(test2)


summary(test)


sample.size = ccores * 100
threads = ccores
burnin = 7500
interval = 100


75 * 75
(sample.size%%threads || (burnin/interval)%%threads)


(burnin/interval)%%threads

(burnin/interval)%%threads

burnin/interval / threads

burnin/interval
threads

 sample.size%%threads
(burnin/interval)%%threads

control_custom

ergmm(bip_net~euclidean(d=2,G=g),control = control_custom)




computeModules(as.sociomatrix(bip_net))
require(bipartite)
test=computeModules(as.sociomatrix(bip_net))
class(test)
test@moduleWeb
bipartite::plotPAC(test@moduleWeb)
test@modules


, method="Beckett", deep = FALSE, deleteOriginalFiles = TRUE,
               tolerance = 1e-10, experimental = FALSE)


plot(test)
bip_net
str(test)
tt[1:10,1:10]
as.sociomatrix(bip_net)[1:10,1:10]

og = test@originalWeb
mw = test@moduleWeb

getModuleCoordinates(test)
devtools::install_github('biometry/bipartite/bipartite')

plotModuleWeb
plotModuleWeb(moduleWebObject = test,displayAlabels = F,displayBlabels = F,weighted = F,plotModules = T)
dim(test@modules)
test@modules[1:26,1:10]
class(test@modules)

test@modules

plotModuleWeb
ergmm(bip_net~ b1deg)

ergmm(bip_net~euclidean(d=2, G =2),control = control_custom,verbose = T)

ergmm(davis~)
b2factor('Concept_Type')
data(davis)

davis %v% 'factor' <- sample(letters[1:3],network.size(davis),replace = T)

# Fit a 2D 2-cluster fit and plot.
davis.fit<-ergmm(davis~euclidean(d=2,G=2)+b1factor('factor'))
plot(davis.fit,pie=TRUE,rand.eff="sociality")






#mod = ergmm(bip_net~euclidean(d=3, 4),tofit = c('mle'),control = control.ergmm(threads = 4))
#mod = readRDS('scratch/test_ergmm.rds')
#summary(mod,point.est = 'mle')

network.vertex.names(bip_net)

attrib <- read.csv("Current Files/Data/Combined Network/SLR_Combined_VectorTypes.csv")
attrib$Vector <- gsub('\\s','',attrib$Vector)
network.vertex.names(bip_net)[!network.vertex.names(bip_net) %in% attrib$Vector]

### workaround to match names to types
temp = (fread("Current Files/Data/Combined Network/AllCombined_Policy_Concern_Barriers.csv",na.strings = ''))
temp = gather(temp)
temp$key = str_remove_all(temp$key,'[0-9]{1,}')
temp = temp[!duplicated(temp),]
temp$value <- gsub("\\s",'',temp$value)


#match(network.vertex.names(bip_net),temp$value)
#network.vertex.names(bip_net)
control_custom = control.ergmm(threads = 1,sample.size = 500,interval = 10,mle.maxit = 30)

require(pbapply)
d2fits<-pblapply(1:4,function(g){print(g);ergmm(bip_net~euclidean(d=2,G=g),control = control_custom,verbose = T)},cl = 4)


  ergmm(bip_net~euclidean(d=2,G=2),control = control_custom),
  ergmm(bip_net~euclidean(d=2,G=3),control = control_custom),
  ergmm(bip_net~euclidean(d=2,G=4),control = control_custom),
  ergmm(bip_net~euclidean(d=2,G=5),control = control_custom),
  

d2G2 = ergmm(bip_net~euclidean(d=2,G=2),control = control_custom)
d2G3 = ergmm(bip_net~euclidean(d=2,G=3),control = control_custom)
d3G3 = ergmm(bip_net~euclidean(d=3,G=4),control = control_custom)
d3G6 = ergmm(bip_net~euclidean(d=3,G=6),control = control_custom)

summary(d2G3)
summary(d3G3)
plot(d3G3)
fits<-list(
# ergmm(bip_net~euclidean(d=2,G=1),control = control_custom),
  ergmm(bip_net~euclidean(d=2,G=2),control = control_custom),
  ergmm(bip_net~euclidean(d=2,G=3),control = control_custom),
  ergmm(bip_net~euclidean(d=2,G=4),control = control_custom),
  ergmm(bip_net~euclidean(d=2,G=5),control = control_custom),
 # ergmm(bip_net~euclidean(d=3,G=1),control = control_custom),
  ergmm(bip_net~euclidean(d=3,G=2),control = control_custom),
  ergmm(bip_net~euclidean(d=3,G=3),control = control_custom),
  ergmm(bip_net~euclidean(d=3,G=4),control = control_custom),
 ergmm(bip_net~euclidean(d=3,G=4),control = control_custom)
)

saveRDS(fits,'scratch/test_fit.rds')
# 
# ## Not run: 
# # Optionally, plot all fits.
# lapply(fits,plot)
# 
# ## End(Not run)
# 
# # Compute the BICs for the fits and plot them:
# (bics<-reshape(
#   as.data.frame(t(sapply(fits,
#                          function(x)c(G=x$model$G,unlist(bic.ergmm(x))[c("Y","Z","overall")])))),
#   list(c("Y","Z","overall")),idvar="G",v.names="BIC",timevar="Component",
#   times=c("likelihood","clustering","overall"),direction="long"
# ))
# 
# with(bics,interaction.plot(G,Component,BIC,type="b",xlab="Clusters", ylab="BIC"))
# 
# # Summarize and plot whichever fit has the lowest overall BIC:
# bestG<-with(bics[bics$Component=="overall",],G[which.min(BIC)])
# summary(fits[[bestG]])
# plot(fits[[bestG]])
# 
# 
# 
# par(mfrow = c(1, 2))
# # Extract a clustering
# 
# Z.K.ref <- summary(mod, point.est = "mle")
# # Plot one model, saving positions, using Z.K.ref to set reference clustering.
# Z.ref <- plot(samplk.d2G3, pie = TRUE, Z.K.ref = Z.K.ref)
# # Plot the other model, using Z.ref and Z.K.ref to ensure similar orientation and
# # coloring.
# plot(samplk.d2G3r, rand.eff = "receiver", pie = TRUE, Z.ref = Z.ref, Z.K.ref = Z.K.ref)
# dir.create('scratch/')
# saveRDS(mod,'scratch/test_ergmm.rds')
# install.packages('snowFT')
# mcmc.diagnostics(mod)
# str(mod)
# ergm(bip_net~edges)
# 
# 
# issue_edges = data.table(melt(crossprod(incidence_mat)))
# issue_edges = issue_edges[value>0&Var1!=Var2,]
# 
# 
# issue_edges[!paste(Var2,Var1) %in% paste(Var1,Var2),]
# 
# 
# setnames(issue_edges,c('value'),c('weights'))
# issue_graph = graph_from_data_frame(issue_edges,directed = F)
# 
# cluster_fast_greedy(issue_graph)
# 
# 
# bip_net = as.tnet(net = incidence_mat)
# plot(bip_net)
# betweenness_w(crossprod(incidence_mat))
# closeness_w(crossprod(incidence_mat))
# 
# compart(incidence_mat)
# 
# 
# igraph::closeness(issue_graph,weights = E(issue_graph)$weights)
# issue_graph
# ?graph_from_data_frame
# ?graph_from_edgelist
# 
# E(issue_graph)$weights
# 
# ?closeness
# 
# ?closeness
# igraph::cent
# igraph::betweenness(issue_graph)
# igraph::closeness(combinednet)
# igraph::edge_density(combinednet)
# 
# 
# 
# plot(issue_graph)
# 
# edge_dt = data.table(melt(incidence_mat))[value==1,]
# issue_net =igraph::graph_from_edgelist(as.matrix(edge_dt[,1:2]),directed = F)
# 
# plot(issue_net)
# ####create network-----------------------------------------------------------
# crossprod(incidence_mat)
# combinednet=igraph::graph_from_adjacency_matrix(crossprod(incidence_mat))
# 
# plot(combined)
# igraph::betweenness(combinednet)
# igraph::closeness(combinednet)
# igraph::edge_density(combinednet)
# 
# ####create vertex attributes------------------------------------
# combinednet <- igraph::set.vertex.attribute(combinednet, "Name", index=attrib$Vector, value=attrib$Vector)
# combinednet <- igraph::set.vertex.attribute(combinednet, "Type", index=attrib$Vector, attrib$Type)
# combinednet <- igraph::set.vertex.attribute(combinednet, "Color", index=attrib$Vector, value=attrib$Color)
# 
# 
# igraph::list.vertex.attributes(combinednet)
# 
# ####plot with vertex color using attributes-----------------------------------
# plot(combinednet, layout=layout_with_graphopt, vertex.size=combode/4, edge.width=weightedcomboedge$weight/10, vertex.color=V(combinednet)$Color, vertex.label.cex=0.5, edge.color=weightedcomboedge$edgecolor)
# 
# plot(combinednet, layout=layout_with_fr, vertex.size=combode/4, edge.width=weightedcomboedge$weight/10, vertex.color=V(combinednet)$Color, vertex.label.cex=0.5, edge.color=weightedcomboedge$edgecolor)
# 
# plot(combinednet, layout=layout_with_drl, vertex.size=combode/4, edge.width=weightedcomboedge$weight/20, vertex.color=V(combinednet)$Color, vertex.label.cex=0.5, edge.color=weightedcomboedge$edgecolor)
# 
# 
# 
# 
# 
# incidence_mat[,39]
# 
# net = as.network(incidence_mat,matrix.type = 'incidence',bipartite = T,directed = F)
# 
# issue_net = as.network(crossprod(incidence_mat),matrix.tye[ =])
# 
# plot(net)
# 
# clustering_tm(bip_net)
# 
# compar
# 
# 
# bip_net
# 
# require(igraph)
# 
# incidence_graph = graph_from_incidence_matrix(incidence_mat,directed = F)
# 
# 
# cluster_optimal(incidence_graph)
# 
# 
# plot(incidence_graph)
# make_bipartite_graph(incidence_mat,)
# V(incidence_graph)$name
# 
# incidence_mat
# ?from_incidence_matrix
# plot(incidence_graph)
# 
# set.seed(24)
# #using htis code as boilerplate https://uc-r.github.io/kmeans_clustering#optimal
# # function to compute total within-cluster sum of square 
# wss <- function(k,data) {
#   kmeans(data, k, nstart = 25,iter.max = 50)
# }
# # Compute and plot wss for k = 1 to k = 20
# k.values <- 1:20
# # extract wss for 1-20 clusters
# k_tests = lapply(k.values,wss,data = incidence_mat)
# 
# #if iters == iter.max, didn't converge
# iters = sapply(k_tests,'[[','iter')
# totwss = sapply(k_tests,'[[','tot.withinss')
# 
# plot(k.values, totwss,
#      type="b", pch = 19, frame = FALSE, 
#      xlab="Number of clusters K",
#      ylab="Total within-clusters sum of squares")
# 
# library(factoextra)
# set.seed(123)
# 
# fviz_nbclust(incidence_mat, kmeans, method = "wss",k.max = 50)
# fviz_nbclust(incidence_mat, kmeans, method = "silhouette",k.max = 50)
# 
# 
# map_dbl(k.values,wss,data = incidence_mat)
# 
# 
# test = kmeans(incidence_mat,20,nstart = 25)
# ?kmeans
# test$iter
# wss(2,incidence_mat)
# ?map_dbl
# wss_values <- map_dbl(k.values, wss)
# 
# wss(2)
# plot(k.values, wss_values,
#      type="b", pch = 19, frame = FALSE, 
#      xlab="Number of clusters K",
#      ylab="Total within-clusters sum of squares")
# 
# 
# 
# 
# k_tests = lapply(k_vals,function(k) kmeans(incidence_mat, centers = k, nstart = 25))
# 
# 
#                  
#                  
#                  str(k2)
# 
# 
#                  
#                  #library(igraph)
#                  library(netrankr)
#                  library(data.table)
#                  library(sna)
#                  library(ergm.count)
#                  library(ergm.rank)
#                  library(latentnet)
#                  
# bip_net = as.network(incidence_mat,matrix.type ='incidence',bipartite = T,directed = F)
# 
# 
# latent.fit<-ergmm(bip_net~euclidean(d = 3,G = 3))
# 
# 
# 
# plot(davis.fit,pie=TRUE,rand.eff="sociality")
# 
# 
# 
# latentnet::ergmm()
# 
# 
# 
# 
# 
# 
# edgelist = melt(incidence,id.vars = c('ResponseId','ResponseId_number'))[variable!='DK'&value==1,]
# 
# 
# rownames(incidence)<-incidence$ResponseId
# 
# incidence
# network(edgelist[,.(ResponseId,variable)],matrix.type = 'edgelist',bipartite = T)
# 
# 
# as.network(incidence,matrix.type = 'incidence',)
# 
# 
# #responses = fread('CurrentData/AllCombined_Policy_Concern_Barriers.csv')
# attrib <- read.csv("CurrentData/SLR_Combined_VectorTypes.csv")
# response_matrix = as.matrix(incidence[,-c('ResponseId','ResponseId_number','DK'),with = F]+1)
# lc = poLCA(response_matrix~1,data = data.frame(response_matrix),nclass = 3)
# round(lc$posterior,2)
# 
# 
# 
# 
# 
# class(response_matrix)
# 
# summary(response_matrix)
# 
# ?poLCA::poLCA()
# 
# attrib
# 
# #Create All Pairs Edgelist--------------------------
# #Create edgelist with all policies, concerns, barrier pairs in 2 columns
# CombinedEdge_Raw <- read.csv("CurrentData/AllCombined_Policy_Concern_Barriers.csv")
# combo <-CombinedEdge_Raw #names my 3 column spreadsheet as object x
# idcombo <- t(combn(seq_along(combo), 2))
# comboedgelist <- lapply(1:nrow(idcombo), function(i) combo[, c(idcombo[i, 1], idcombo[i, 2])]) 
# comboedgelist <- lapply(comboedgelist, setNames, c("I1","I2")) #names columns 1 and 2
# comboedgelist <- do.call(rbind, comboedgelist)
# comboedgelist <- comboedgelist[rowSums(is.na(comboedgelist))==0, ]
# 
# #remove any pairs that are missing one or both in a pair for final edge list
# fincomboedge<-comboedgelist[!(comboedgelist$I1==""|comboedgelist$I2==""), ]
# 
# #Create weighted edgelist------------------------------------------------------------
# #create data table for policies from existing edge list
# combined.data.table <- data.table(fincomboedge)
# #create new spreadsheet with number of occurences for each node pair
# weightedcomboedge <- combined.data.table[ , .N, by=.(combined.data.table$I1, combined.data.table$I2)]
# 
# #rename data table columns for use as weighted edgelist
# setnames(weightedcomboedge, "N", "weight")
# setnames(weightedcomboedge, "combined.data.table", "I1")
# setnames(weightedcomboedge, "combined.data.table.1", "I2")
# 
# #Load vertex attribute--------------------------------------------------------------------
# attrib <- read.csv("CurrentData/SLR_Combined_VectorTypes.csv")
# 
# ####try to create edge attribute for type of edge inside weighted edgelist----------------
# weightedcomboedge$Type1 <- attrib$Type[match(weightedcomboedge$I1, attrib$Vector)]
# 
# weightedcomboedge$Type2 <- attrib$Type[match(weightedcomboedge$I2, attrib$Vector)]
# 
# weightedcomboedge$edgetype <- "NA"
# 
#   weightedcomboedge[weightedcomboedge$Type1=="Policy" & weightedcomboedge$Type2=="Policy"]$edgetype <- "Policy-Policy"
#   weightedcomboedge[weightedcomboedge$Type1=="Concern" & weightedcomboedge$Type2=="Concern"]$edgetype <- "Concern-Concern"
#   weightedcomboedge[weightedcomboedge$Type1=="Barrier" & weightedcomboedge$Type2=="Barrier"]$edgetype <- "Barrier-Barrier"
#   weightedcomboedge[weightedcomboedge$Type1=="Policy"& weightedcomboedge$Type2=="Concern"]$edgetype <- "Policy-Concern"
#   weightedcomboedge[weightedcomboedge$Type1=="Policy"& weightedcomboedge$Type2=="Barrier"]$edgetype <- "Policy-Barrier"
#   weightedcomboedge[weightedcomboedge$Type1=="Barrier"& weightedcomboedge$Type2=="Concern"]$edgetype <- "Barrier-Concern"
#   weightedcomboedge[weightedcomboedge$Type1=="Barrier"& weightedcomboedge$Type2=="Policy"]$edgetype <- "Policy-Barrier"
#   weightedcomboedge[weightedcomboedge$Type1=="Concern"& weightedcomboedge$Type2=="Barrier"]$edgetype <- "Barrier-Concern"
#   weightedcomboedge[weightedcomboedge$Type1=="Concern"& weightedcomboedge$Type2=="Policy"]$edgetype <- "Policy-Concern"
# 
# #create color edge type attribute in weighted edge list
# weightedcomboedge$edgecolor <- "NA"  
# weightedcomboedge[weightedcomboedge$edgetype=="Policy-Policy"]$edgecolor <- "#dcdcdc"
# weightedcomboedge[weightedcomboedge$edgetype=="Barrier-Barrier"]$edgecolor <- "#dcdcdc"
# weightedcomboedge[weightedcomboedge$edgetype=="Concern-Concern"]$edgecolor <- "#dcdcdc"
# weightedcomboedge[weightedcomboedge$edgetype=="Policy-Concern"]$edgecolor <- "#808080"
# weightedcomboedge[weightedcomboedge$edgetype=="Policy-Barrier"]$edgecolor <- "#808080"
# weightedcomboedge[weightedcomboedge$edgetype=="Barrier-Concern"]$edgecolor <- "#808080"
#   
# 
# 
# ####create network-----------------------------------------------------------
# set.seed(2)
# combinednet=igraph::graph_from_data_frame(weightedcomboedge, directed=FALSE)
# combode=igraph::degree(combinednet)
# combode
# igraph::betweenness(combinednet)
# igraph::closeness(combinednet)
# igraph::edge_density(combinednet)
# 
# ####create vertex attributes------------------------------------
# combinednet <- igraph::set.vertex.attribute(combinednet, "Name", index=attrib$Vector, value=attrib$Vector)
# combinednet <- igraph::set.vertex.attribute(combinednet, "Type", index=attrib$Vector, attrib$Type)
# combinednet <- igraph::set.vertex.attribute(combinednet, "Color", index=attrib$Vector, value=attrib$Color)
# 
# 
# igraph::list.vertex.attributes(combinednet)
# 
# ####plot with vertex color using attributes-----------------------------------
# plot(combinednet, layout=layout_with_graphopt, vertex.size=combode/4, edge.width=weightedcomboedge$weight/10, vertex.color=V(combinednet)$Color, vertex.label.cex=0.5, edge.color=weightedcomboedge$edgecolor)
# 
# plot(combinednet, layout=layout_with_fr, vertex.size=combode/4, edge.width=weightedcomboedge$weight/10, vertex.color=V(combinednet)$Color, vertex.label.cex=0.5, edge.color=weightedcomboedge$edgecolor)
# 
# plot(combinednet, layout=layout_with_drl, vertex.size=combode/4, edge.width=weightedcomboedge$weight/20, vertex.color=V(combinednet)$Color, vertex.label.cex=0.5, edge.color=weightedcomboedge$edgecolor)
# 
# 
# ####try and remove edges below certain weight----------------------------
# 
# tenedgethreshold <- combinednet
# tenedgethreshold<- delete.edges(tenedgethreshold, which(E(tenedgethreshold)$weight <10)-1)
# tende=igraph::degree(tenedgethreshold)
# tenedgethreshold <- delete.vertices(tenedgethreshold, V(tenedgethreshold)[degree(tenedgethreshold)==0])
# plot(tenedgethreshold, layout=layout_with_graphopt, vertex.size=tende/4, edge.width=E(tenedgethreshold)$weight/15, vertex.color=V(tenedgethreshold)$Color, vertex.label.cex=0.5, edge.color=E(tenedgethreshold)$edgecolor)
# 
# ####try to do community detection--------------------------------------
# #louvain method
# louvain_communities <- igraph::cluster_louvain(combinednet, weights=E(combinednet)$weight)
# combinednet$community <- louvain_communities$membership
# unique(combinednet$community)
# 
# plot(combinednet, layout=layout_with_fr, vertex.color=rainbow(3, alpha=0.6)[louvain_communities$membership], vertex.label.cex=0.25)
# 
# louvain_communities$membership
# 
# louvain_communities
# 
# groups(louvain_communities)
# 
# #Newman's Modularity
# simple_combinednet = simplify(combinednet)
# fastgreedycommunity <- cluster_fast_greedy(simple_combinednet, weights=E(combinednet)$weight)
# 
# fastgreedycommunity
# sizes(fastgreedycommunity)
# membership(fastgreedycommunity)
# groups(fastgreedycommunity)
# 
# plot(fastgreedycommunity, simple_combinednet)
# 
# plot(combinednet, layout=layout_with_fr, vertex.color=rainbow(5, alpha=0.6)[fastgreedycommunity$membership], vertex.label.cex=0.25)
# 
# #Edge Betweenness
# cluster_edge_betweenness(combinednet, weights=E(combinednet)$weight, directed=FALSE)
# #only gives one group with modularity 0 so not worth using this method
# 
# #Info Map
# 
# infomapcommunity <- cluster_infomap(combinednet, e.weights=E(combinednet)$weight)
# infomapcommunity
# membership(infomapcommunity)
# groups(infomapcommunity)
# plot(infomapcommunity, simple_combinednet)
# 
# plot(combinednet, layout=layout_with_fr, vertex.color=rainbow(14, alpha=0.6)[infomapcommunity$membership], vertex.label.cex=0.25)
# 
# #walktrap method
# ?walktrap.community
# walktrapcommunity <- cluster_walktrap(combinednet, weights=E(combinednet)$weight)
# combinednet
# walktrapcommunity
# membership(walktrapcommunity)
# groups(walktrapcommunity)
# plot(walktrapcommunity, combinednet)
# 
# plot(combinednet, layout=layout_with_fr, vertex.color=rainbow(16, alpha=0.6)[walktrapcommunity$membership], vertex.label.cex=0.25)

####still havent tried latent space analysis----------------------

