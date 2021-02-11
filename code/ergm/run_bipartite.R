
packs =c('tidyverse','purrr','data.table','statnet','latentnet','bipartite','lvm4net','ggthemes')
need = packs[!sapply(packs,function(x) suppressMessages(require(x,character.only=T)))]
sapply(need,function(x) suppressMessages(install.packages(x,,type= 'source')))
sapply(packs[need],function(x) suppressMessages(require(x,character.only=T)))

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


diff_level = which(!sapply(str_split(str_remove(names(summary(bip_net ~b1twostar(b1attr = 'b1_dummy_b2_names'))),'^.*names\\.'),'\\.'),function(x) x[[1]]==x[[2]]))
regional_authority = grep('Regional Authority',names(summary(bip_net ~b1twostar(b1attr = 'b1_dummy_b2_names'))))
collab = grep('Collab',names(summary(bip_net ~b1twostar(b1attr = 'b1_dummy_b2_names'))))
existing_agency = grep('Existing Agency',names(summary(bip_net ~b1twostar(b1attr = 'b1_dummy_b2_names'))))

mod = ergm(bip_net ~ edges + gwb1degree(1,fixed = T) + 
             b2factor( 'b1_dummy_b2_names') + 
             b1twostar(b1attr = 'b1_dummy_b2_names',
                       levels2 = intersect(diff_level,union(union(regional_authority,collab),existing_agency))) +
             offset(b1degree(0)) + 
       offset(b1starmix(4, 'Concept_Type', base=NULL, diff=F)),offset.coef = c(-Inf,-Inf),eval.loglik = F,verbose = F,
       control = control.ergm(parallel = 4,MCMC.samplesize = 5e3))


saveRDS(mod,'scratch/start_mod.rds')


mod2 = ergm(bip_net ~ edges + 
             b2factor( 'b1_dummy_b2_names') + 
             b1twostar(b1attr = 'b1_dummy_b2_names',
                       levels2 = intersect(diff_level,union(union(regional_authority,collab),existing_agency))) +
             offset(b1starmix(4, 'Concept_Type', base=NULL, diff=F)),offset.coef = -Inf,eval.loglik = F,verbose = F,
           control = control.ergm(parallel = 4,init = coef(mod)))

mod3 = ergm(bip_net ~ edges + 
              b2factor( 'b1_dummy_b2_names') + 
              b1twostar(b1attr = 'b1_dummy_b2_names',
                        levels2 = intersect(diff_level,union(union(regional_authority,collab),existing_agency))) +
              offset(b1starmix(4, 'Concept_Type', base=NULL, diff=F)),offset.coef = -Inf,eval.loglik = F,verbose = F,
            control = control.ergm(parallel = 4,init = coef(mod2),MCMC.samplesize = 20e3))

control.ergm



summary(mod)



summary(bip_net ~ b1starmix(3, 'id_concept', base=NULL, diff=F))
summary(bip_net ~ )
mod
summary(mod)

summary(bip_net ~ b2factor( 'b1_dummy_b2_names'))


nodemix( 'b1_dummy_b2_names', base=NULL, b1levels=NULL, b2levels=NULL, levels=NULL, levels2=-c(same_level))

mod = ergm(bip_net ~ edges + b2factor('vertex.names') + b2nodematch('vertex.names'))
             summary(mod)
             offset(b1starmix(4, 'id_concept', base=NULL, diff=F)),offset.coef = rep(-Inf,bip_net$gal$bipartite))


test = simulate(mod,nsim = 1000)

tt = sapply(test,function(x) sum(summary(x ~ b1starmix(4, 'id_concept', base=NULL, diff=F))))
table(tt)


bip_net
table(summary(bip_net ~ b1starmix(4, 'id_concept', base=NULL, diff=T)))

test = melt(incidence_mat)
test$type = concept_types$Type[match(test$Var2,concept_types$Vector)]
test = test[test$value>0,]


incidence_mat[rowSums(incidence_mat)>9,colnames(incidence_mat) %in% concept_types$Vector[concept_types$Type=='Concern']]
data.table(test)[,.N,by=.(Var1,type)][order(-N)][order(-N),]
concept_types$Type
incidence_mat[rowSums(incidence_mat)>9,]
degree(bip_net,gmode = 'graph')

summary(bip_net ~ b1starmix(4, 'id_concept', base=NULL, diff=F))

summary(bip_net ~ b1nodemix('id_concept'))



test[test$Var1=='R_1rrjWdbjhKjDYkV',]

summary(bip_net ~ b1starmix(3, 'Concept_Type', base=NULL, diff=T))

summary(bip_net ~ b2nodematch('Concept_Type',diff = T,byb1attr = 'id'))

summary()
summary(bip_net ~  b1nodematch('Concept_Type',diff=F, keep=NULL, alpha=1, beta=1, byb2attr='Concept_Type', levels=NULL))

m = ergm(bip_net ~ edges + b1mindegree(1) + ,eval.loglik = F)



warnings()



