
#put in all the setup code from Tyler's R markdown-------------------

packs =c('tidyverse','purrr','data.table','statnet','latentnet','bipartite','lvm4net',
         'ggthemes','here','ggnetwork','gridExtra','ggrepel','corrplot')
library(tidyverse)
library(purrr)
library(data.table)
library(statnet)
library(latentnet)
install.packages("bipartite")
library(bipartite)
install.packages("lvm4net")
library(lvm4net)
install.packages("ggthemes")
library(ggthemes)
install.packages("here")
install.packages("ggnetwork")
install.packages("gridExtra")
install.packages("ggrepel")
install.packages("corrplot")
library(here)
library(ggnetwork)
library(gridExtra)
library(ggrepel)
library(corrplot)

library(readxl)
orig = readxl::read_excel('input/SLRSurvey_Full.xlsx')
orig$Q4[is.na(orig$Q4)]<-'Other'
#recode anything with fewer than 10 respondents as other
# see what wed recode
#as.data.table(table(orig$Q4))[order(-N),][N<10,]
orig$Q4[orig$Q4 %in% as.data.table(table(orig$Q4))[order(-N),][N<10,]$V1] <- 'Other'
orig$Q4[grepl('Other',orig$Q4)]<-'Other'
incidence_dt = fread("CurrentFiles/Data/AdjMatrix_MinOtherRecode.csv")

incidence_mat = as.matrix(incidence_dt[,-c('ResponseId','DK')])
rownames(incidence_mat)<-incidence_dt$ResponseId
#drop isolates
incidence_mat = incidence_mat[rowSums(incidence_mat)!=0,]
incidence_mat = incidence_mat[,!grepl('Other',colnames(incidence_mat))]

#create network object
bip_net = as.network(incidence_mat,matrix.type = 'incidence',bipartite = T,directed = F,loops = F)
#code actor types
bip_net %v% 'Actor_Type' <- orig$Q4[match(network.vertex.names(bip_net),orig$ResponseId)]
#code concept types
concept_types = fread('input/Combined_VectorTypes_NoNewOther.csv')
bip_net %v% 'Concept_Type' <- concept_types$Type[match(network.vertex.names(bip_net), concept_types$Vector)]
set.vertex.attribute(bip_net,'Concept_Type',value = 'Person',v = which(is.na(bip_net %v% 'Concept_Type')))

bip_net %v% 'id' <- network.vertex.names(bip_net)
bip_net %v% 'id_concept' <- ifelse({bip_net %v% 'Concept_Type'} == 'Person',network.vertex.names(bip_net),bip_net %v% 'Concept_Type')

bip_net %v% 'b1_dummy_b2_names' <- ifelse({bip_net %v% 'Concept_Type'} == 'Person','Person',bip_net %v% 'vertex.names')
#convert to incidence matrix
Y = as.sociomatrix(bip_net)
Y = Y[,!grepl('Other',colnames(Y))]
Y<-Y[,sort(colnames(Y))]

combos = data.table(melt(crossprod(Y)))[Var1!=Var2,]
combos = combos[as.numeric(combos$Var1) <= as.numeric(combos$Var2),]
combos$Var1 <- paste0(combos$Var1,' (',concept_types$Type[match(combos$Var1,concept_types$Vector)],')')
combos$Var2 <- paste0(combos$Var2,' (',concept_types$Type[match(combos$Var2,concept_types$Vector)],')')

htmlTable::htmlTable(combos[order(-value),][1:10,],caption ='top 10 combos')


require(htmlTable)
concept_unipartite = (crossprod(Y))

uni_net = as.network(concept_unipartite,matrix.type = 'adjacency',directed = F,ignore.eval = F,names.eval = 'cooccurence')

uni_net %v% 'Frequency' <- diag(concept_unipartite)

uni_net %v% 'Type' <- concept_types$Type[match(network.vertex.names(uni_net),concept_types$Vector)]
glist = lapply(c('Policy','Barrier','Concern'),function(x) get.inducedSubgraph(uni_net,v = which(uni_net %v% 'Type' == x)) %>% ggnetwork() %>% 
                 ggplot(.,aes(x = x,xend = xend,y = y,yend = yend,label = vertex.names))+ 
                 geom_edges(aes(size = cooccurence/nrow(Y),alpha = cooccurence)) +
                 geom_point(aes(size = Frequency/nrow(Y))) + 
                 ggtitle(paste(x,'co-occurence')) + 
                 geom_nodelabel_repel(size = 2) + theme_map() + guides(alpha = F,size = F) + 
                 scale_size_continuous(range = c(0.1,4))+
                 theme(legend.position = c(0.8,0.2))
)

grid.arrange(glist[[1]],glist[[2]],glist[[3]],ncol = 2)



soc = as.sociomatrix(uni_net,attrname = 'cooccurence')
gov_types = c('Regional Authority','Existing Agency','Collab')
htmlTable(do.call(cbind,lapply(gov_types,function(x) 
  as.data.table(soc[x,],keep.rownames = T)[order(-V2),][1:10,][,V2:=paste0(V1,' (',round(V2/sum(Y[,x]),2),')')][,'V2'])),
  header = gov_types,caption  = 'Top 10 choices by respondents who choose each governance preference',
  tfoot = '# = proportion of respondents by governance strategy who also choose item')

htmlTable(cbind(as.data.table(soc['Regional Authority',],keep.rownames = T)[order(-V2),][1:10,1],
                as.data.table(soc['Existing Agency',],keep.rownames = T)[order(-V2),][1:10,1],
                as.data.table(soc['Collab',],keep.rownames = T)[order(-V2),][1:10,1]))


#start of Tyler's lta/lca code------------------------------

#### note -- mlta takes vectors instead of single D and G values -- but pblapply is used to parallelize #####
opts = data.table(expand.grid(D = 1,G = 2,fix = c(F,T)))
opts = opts[D>0|!fix,]
#### this part takes a while, so I commented out and upload an RDS at the end ###
require(doParallel)
cluster = makeCluster(4)
registerDoParallel(cluster)
clusterExport(cl = cluster,varlist = list('opts','Y'))
clusterEvalQ(cl = cluster,require(lvm4net))
mlta_tests = foreach(i = 1:nrow(opts)) %dopar% {mlta(X = Y, D = opts$D[i], G = opts$G[i],wfix = opts$fix[i],nstarts = 5,maxiter = 1e3)}
saveRDS(mlta_tests,'scratch/mlta_results2.rds')

mlta_tests = readRDS('scratch/mlta_results2.rds')
mlta_results = data.table(opts,BIC = sapply(mlta_tests,function(x) x$BIC))
mlta_results$G_fix = paste0('G = ',mlta_results$G,' (fixed slope = ',mlta_results$fix,')')
mlta_results$BIC <- round(mlta_results$BIC)
mlta_cast = dcast(mlta_results[order(BIC)],G_fix ~ D,value.var = 'BIC')


htmlTable(mlta_cast[,-1],caption = 'BIC scores by MLTA specification',rnames = mlta_cast$G_fix,header = paste0('D = ',1),
          tfoot = '*fixed slope refers to whether slope is constant across all groups are group-specific')

#looking at just D=1, G=2 from Tyler's Code

index = which(opts$D==1&opts$G==2&opts$fix==T)
betas = mlta_tests[[index]]$b
wus = mlta_tests[[index]]$w
group_probs = mlta_tests[[index]]$z
colnames(betas) <- colnames(wus) <- colnames(Y)

ggplot() + geom_histogram(aes(x =group_probs[,1]),binwidth = 0.02) + theme_bw() + ggtitle('p(G = g) by respondent, D = 1, G = 2, w = fixed') + 
  ylab('# respondents') + xlab('p(G=1)')

#code for intercept terms by group graphic------------------------------------

beta_dt = data.table(t(betas),keep.rownames = T)
ggplot(beta_dt,aes(x = `Group 1`,y = `Group 2`,label = rn)) + geom_abline(lty = 2,col = 'grey40') +
  geom_point(pch = 21,col = 'grey50') + geom_text_repel(size = 2.5,max.overlaps = 100,min.segment.length = 0.2) +
  xlab('intercept, group 1') + ylab('intercept, group 2') + theme_bw() + ggtitle('Intercept estimates for item-response by group','p = 1 / exp(-beta)')

#create list of group probabilities for respondents------------------------
#looks like group_probs is the probabilities
#need to figure out how to match that to the respondent IDs and then the survey
#row names of Y has the respondent IDs so could match group probs to Y (both 1:698 rows) and then from there match to respondent ID from Y to orig
#match that back to survey results--------------------------------------


#redo from corplot.Rmd to replace mtla_results.rds with original one again (accidentally replaced it when I ran my code copy to only have D=1 and G=2but now mine is mtla_results2.rds)----------------------

opts = data.table(expand.grid(D = 0:4,G = 1:5,fix = c(F,T)))
opts = opts[D>0|!fix,]
require(doParallel)
cluster = makeCluster(4)
registerDoParallel(cluster)
clusterExport(cl = cluster,varlist = list('opts','Y'))
clusterEvalQ(cl = cluster,require(lvm4net))
mlta_tests = foreach(i = 1:nrow(opts)) %dopar% {mlta(X = Y, D = opts$D[i], G = opts$G[i],wfix = opts$fix[i],nstarts = 5,maxiter = 1e3)}
saveRDS(mlta_tests,'scratch/mlta_results.rds')

mlta_tests = readRDS('scratch/mlta_results.rds')
mlta_results = data.table(opts,BIC = sapply(mlta_tests,function(x) x$BIC))
mlta_results$G_fix = paste0('G = ',mlta_results$G,' (fixed slope = ',mlta_results$fix,')')
mlta_results$BIC <- round(mlta_results$BIC)
mlta_cast = dcast(mlta_results[order(BIC)],G_fix ~ D,value.var = 'BIC')


htmlTable(mlta_cast[,-1],caption = 'BIC scores by MLTA specification',rnames = mlta_cast$G_fix,header = paste0('D = ',0:4),
          tfoot = '*fixed slope refers to whether slope is constant across all groups are group-specific')