#set seed for replicable results
seed = 24

### load packages (not all of htese are needed at this point, so shoudl be cleaned up a bit)
packs =c('tidyverse','purrr','data.table','statnet','latentnet','bipartite','lvm4net','mirt','pbapply','Hmisc','htmlTable',
         'ggthemes','here','ggnetwork','gridExtra','ggrepel','corrplot','htmlTable','readxl','nFactors','ggrepel','plotly','ggalluvial')
need = packs[!packs %in% names(installed.packages()[,2])]
invisible(sapply(need,function(x) suppressMessages(install.packages(x,type= 'source'))))
invisible(sapply(packs,function(x) suppressMessages(library(x,character.only = T))))

#above code didn't work for Kyra so below commented out code is just to reload packages manually each time Kyra runs code
#library(tidyverse)
#library(purrr)
#library(data.table)
#library(statnet)
#library(latentnet)
#library(bipartite)
#library(lvm4net)
#library(mirt)
#library(pbapply)
#library(Hmisc)
#library(htmlTable)
#library(ggthemes)
#library(here)
#library(ggnetwork)
#library(gridExtra)
#library(ggrepel)
#library(corrplot)
#library(readxl)
#library(nFactors)
#library(plotly)
#library(ggalluvial)


### make bw theme default ###

### read in data
old = theme_set(theme_bw())
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

#DROP BARRIERS#
if(DROP_BARRIERS){
  bip_net = get.inducedSubgraph(x = bip_net,v = which(bip_net %v% 'Concept_Type' != 'Barrier'))
}
#convert to incidence matrix
Y = as.sociomatrix(bip_net)
Y = Y[,!grepl('Other',colnames(Y))]
Y<-Y[,sort(colnames(Y))]

#make predictor dt

#fix barriers so can use in predictor dt (switch NA to 0)
orig$Q20_HumRes[is.na(orig$Q20_HumRes)]=0
orig$Q20_FinRes[is.na(orig$Q20_FinRes)]=0
orig$Q20_ExpCollab[is.na(orig$Q20_ExpCollab)]=0
orig$Q20_StakeOpp[is.na(orig$Q20_StakeOpp)]=0
orig$Q20_PolLead[is.na(orig$Q20_PolLead)]=0
orig$Q20_SLRUncertain[is.na(orig$Q20_SLRUncertain)]=0
orig$Q20_SciInfo[is.na(orig$Q20_SciInfo)]=0
orig$Q20_OrgLead[is.na(orig$Q20_OrgLead)]=0
orig$Q20_OverallPlan[is.na(orig$Q20_OverallPlan)]=0
orig$Q20_Permits[is.na(orig$Q20_Permits)]=0
orig$Q20_PubSupport[is.na(orig$Q20_PubSupport)]=0
orig$Q20_CBORelation[is.na(orig$Q20_CBORelation)]=0


#make predictor dt
predictor_dt = data.table(id = rownames(Y))
predictor_dt$Q1_Focus <- orig$Q1_Num[match(predictor_dt$id,orig$ResponseId)]
predictor_dt$When_SLR <- orig$WhenSLR[match(predictor_dt$id,orig$ResponseId)]
predictor_dt$Q16_Risk_Agree <- orig$RiskAgree[match(predictor_dt$id,orig$ResponseId)]
predictor_dt$Q17_Action_Agree <- orig$ActionAgree[match(predictor_dt$id,orig$ResponseId)]
predictor_dt$Q4_Fed <- orig$Q4_Fed[match(predictor_dt$id,orig$ResponseId)]
predictor_dt$Q4_State <- orig$Q4_State[match(predictor_dt$id,orig$ResponseId)]
predictor_dt$Q4_RegGov <- orig$Q4_RegGov[match(predictor_dt$id,orig$ResponseId)]
predictor_dt$Q4_LocalGov <- orig$Q4_LocalGov[match(predictor_dt$id,orig$ResponseId)]
predictor_dt$Q4_WaterSD <- orig$Q4_WaterSD[match(predictor_dt$id,orig$ResponseId)]
predictor_dt$Q4_EnviroSD <- orig$Q4_EnviroSD[match(predictor_dt$id,orig$ResponseId)]
predictor_dt$Q4_Enviro <- orig$Q4_Enviro[match(predictor_dt$id,orig$ResponseId)]
predictor_dt$Q4_Trade <- orig$Q4_Trade[match(predictor_dt$id,orig$ResponseId)]
predictor_dt$Q4_Ed <- orig$Q4_Ed[match(predictor_dt$id,orig$ResponseId)]
predictor_dt$Q4_Multistake <- orig$Q4_Multistake[match(predictor_dt$id,orig$ResponseId)]
predictor_dt$Q4_Multijuris <- orig$Q4_Multijuris[match(predictor_dt$id,orig$ResponseId)]
predictor_dt$Q4_Political <- orig$Q4_Political[match(predictor_dt$id,orig$ResponseId)]
predictor_dt$Q4_CBO <- orig$Q4_CBO[match(predictor_dt$id,orig$ResponseId)]
predictor_dt$Q4_NGO <- orig$Q4_NGO[match(predictor_dt$id,orig$ResponseId)]
predictor_dt$Q8_Sum <- orig$Q8_Sum[match(predictor_dt$id,orig$ResponseId)]
predictor_dt$Q32_Sum <- orig$Q32_Sum[match(predictor_dt$id,orig$ResponseId)]
predictor_dt$Q11_STAware <- orig$Q11_STAware[match(predictor_dt$id,orig$ResponseId)]
predictor_dt$Q11_LTAware <- orig$Q11_LTAware[match(predictor_dt$id,orig$ResponseId)]
predictor_dt$Q12_STConcern <- orig$Q12_STConcern[match(predictor_dt$id,orig$ResponseId)]
predictor_dt$Q12_LTConcern <- orig$Q12_LTConcern[match(predictor_dt$id,orig$ResponseId)]
predictor_dt$Q20_HumRes <- orig$Q20_HumRes[match(predictor_dt$id,orig$ResponseId)]
predictor_dt$Q20_FinRes <- orig$Q20_FinRes[match(predictor_dt$id,orig$ResponseId)]
predictor_dt$Q20_ExpCollab <- orig$Q20_ExpCollab[match(predictor_dt$id,orig$ResponseId)]
predictor_dt$Q20_StakeOpp <- orig$Q20_StakeOpp[match(predictor_dt$id,orig$ResponseId)]
predictor_dt$Q20_PolLead <- orig$Q20_PolLead[match(predictor_dt$id,orig$ResponseId)]
predictor_dt$Q20_SLRUncertain <- orig$Q20_SLRUncertain[match(predictor_dt$id,orig$ResponseId)]
predictor_dt$Q20_SciInfo <- orig$Q20_SciInfo[match(predictor_dt$id,orig$ResponseId)]
predictor_dt$Q20_OrgLead <- orig$Q20_OrgLead[match(predictor_dt$id,orig$ResponseId)]
predictor_dt$Q20_OverallPlan <- orig$Q20_OverallPlan[match(predictor_dt$id,orig$ResponseId)]
predictor_dt$Q20_Permits <- orig$Q20_Permits[match(predictor_dt$id,orig$ResponseId)]
predictor_dt$Q20_PubSupport <- orig$Q20_PubSupport[match(predictor_dt$id,orig$ResponseId)]
predictor_dt$Q20_CBORelation <- orig$Q20_CBORelation[match(predictor_dt$id,orig$ResponseId)]
predictor_dt$Q20_Sum <- orig$Q20_Sum[match(predictor_dt$id,orig$ResponseId)]


predictor_dt$Q1_Focus[is.na(predictor_dt$Q1_Focus)]<-round(mean(predictor_dt$Q1_Focus,na.rm = T))
predictor_dt$When_SLR[is.na(predictor_dt$When_SLR)]<-round(mean(predictor_dt$When_SLR,na.rm = T))
predictor_dt$Q16_Risk_Agree[is.na(predictor_dt$Q16_Risk_Agree)]<-round(mean(predictor_dt$Q16_Risk_Agree,na.rm = T))
predictor_dt$Q17_Action_Agree[is.na(predictor_dt$Q17_Action_Agree)]<-round(mean(predictor_dt$Q17_Action_Agree,na.rm = T))
predictor_dt$Q4_Fed[is.na(predictor_dt$Q4_Fed)]<-round(mean(predictor_dt$Q4_Fed,na.rm = T))
predictor_dt$Q4_State[is.na(predictor_dt$Q4_State)]<-round(mean(predictor_dt$Q4_State,na.rm = T))
predictor_dt$Q4_RegGov[is.na(predictor_dt$Q4_RegGov)]<-round(mean(predictor_dt$Q4_RegGov,na.rm = T))
predictor_dt$Q4_LocalGov[is.na(predictor_dt$Q4_LocalGov)]<-round(mean(predictor_dt$Q4_LocalGov,na.rm = T))
predictor_dt$Q4_WaterSD[is.na(predictor_dt$Q4_WaterSD)]<-round(mean(predictor_dt$Q4_WaterSD,na.rm = T))
predictor_dt$Q4_EnviroSD[is.na(predictor_dt$Q4_EnviroSD)]<-round(mean(predictor_dt$Q4_EnviroSD,na.rm = T))
predictor_dt$Q4_Enviro[is.na(predictor_dt$Q4_Enviro)]<-round(mean(predictor_dt$Q4_Enviro,na.rm = T))
predictor_dt$Q4_Trade[is.na(predictor_dt$Q4_Trade)]<-round(mean(predictor_dt$Q4_Trade,na.rm = T))
predictor_dt$Q4_Ed[is.na(predictor_dt$Q4_Ed)]<-round(mean(predictor_dt$Q4_Ed,na.rm = T))
predictor_dt$Q4_Multistake[is.na(predictor_dt$Q4_Multistake)]<-round(mean(predictor_dt$Q4_Multistake,na.rm = T))
predictor_dt$Q4_Multijuris[is.na(predictor_dt$Q4_Multijuris)]<-round(mean(predictor_dt$Q4_Multijuris,na.rm = T))
predictor_dt$Q4_Political[is.na(predictor_dt$Q4_Political)]<-round(mean(predictor_dt$Q4_Political,na.rm = T))
predictor_dt$Q4_CBO[is.na(predictor_dt$Q4_CBO)]<-round(mean(predictor_dt$Q4_CBO,na.rm = T))
predictor_dt$Q4_NGO[is.na(predictor_dt$Q4_NGO)]<-round(mean(predictor_dt$Q4_NGO,na.rm = T))

predictor_dt$Q8_Sum[is.na(predictor_dt$Q8_Sum)]<-round(mean(predictor_dt$Q8_Sum,na.rm = T))
predictor_dt$Q32_Sum[is.na(predictor_dt$Q32_Sum)]<-round(mean(predictor_dt$Q32_Sum,na.rm = T))
predictor_dt$Q11_STAware[is.na(predictor_dt$Q11_STAware)]<-round(mean(predictor_dt$Q11_STAware,na.rm = T))
predictor_dt$Q11_LTAware[is.na(predictor_dt$Q11_LTAware)]<-round(mean(predictor_dt$Q11_LTAware,na.rm = T))
predictor_dt$Q12_STConcern[is.na(predictor_dt$Q12_STConcern)]<-round(mean(predictor_dt$Q12_STConcern,na.rm = T))
predictor_dt$Q12_LTConcern[is.na(predictor_dt$Q12_LTConcern)]<-round(mean(predictor_dt$Q12_LTConcern,na.rm = T))
predictor_dt$Q20_HumRes[is.na(predictor_dt$Q20_HumRes)]<-round(mean(predictor_dt$Q20_HumRes,na.rm = T))
predictor_dt$Q20_FinRes[is.na(predictor_dt$Q20_FinRes)]<-round(mean(predictor_dt$Q20_FinRes,na.rm = T))
predictor_dt$Q20_ExpCollab[is.na(predictor_dt$Q20_ExpCollab)]<-round(mean(predictor_dt$Q20_ExpCollab,na.rm = T))
predictor_dt$Q20_StakeOpp[is.na(predictor_dt$Q20_StakeOpp)]<-round(mean(predictor_dt$Q20_StakeOpp,na.rm = T))
predictor_dt$Q20_PolLead[is.na(predictor_dt$Q20_PolLead)]<-round(mean(predictor_dt$Q20_PolLead,na.rm = T))
predictor_dt$Q20_SLRUncertain[is.na(predictor_dt$Q20_SLRUncertain)]<-round(mean(predictor_dt$Q20_SLRUncertain,na.rm = T))
predictor_dt$Q20_SciInfo[is.na(predictor_dt$Q20_SciInfo)]<-round(mean(predictor_dt$Q20_SciInfo,na.rm = T))
predictor_dt$Q20_OrgLead[is.na(predictor_dt$Q20_OrgLead)]<-round(mean(predictor_dt$Q20_OrgLead,na.rm = T))
predictor_dt$Q20_OverallPlan[is.na(predictor_dt$Q20_OverallPlan)]<-round(mean(predictor_dt$Q20_OverallPlan,na.rm = T))
predictor_dt$Q20_Permits[is.na(predictor_dt$Q20_Permits)]<-round(mean(predictor_dt$Q20_Permits,na.rm = T))
predictor_dt$Q20_PubSupport[is.na(predictor_dt$Q20_PubSupport)]<-round(mean(predictor_dt$Q20_PubSupport,na.rm = T))
predictor_dt$Q20_CBORelation[is.na(predictor_dt$Q20_CBORelation)]<-round(mean(predictor_dt$Q20_CBORelation,na.rm = T))
predictor_dt$Q20_Sum[is.na(predictor_dt$Q20_Sum)]<-round(mean(predictor_dt$Q20_Sum,na.rm = T))


mY = melt(Y)
mY$cat = {bip_net %v% 'Concept_Type'}[match(mY$Var2,bip_net %v% 'vertex.names')]
mY<-data.table(mY)
mY = mY[,mean(value),by=.(Var2,cat)][order(-V1)]
mY$fact = fct_reorder(mY$Var2,mY$V1,mean)


#plot raw response frequencies
#ideally this would be a fact_wrap call but getting factors to drop out takes some effort
plist = list(ggplot(mY[cat =='Concern',]) + geom_point(aes(x = fact,y = 100 * V1))+
               geom_text(aes(x = fact,y = 100 * V1 +5,label = fact),hjust = 0) + scale_y_continuous(limits = c(0,100)) +facet_wrap(~cat) + 
               theme_bw() + theme(axis.title = element_blank(),axis.text.y = element_blank(),axis.ticks.y= element_blank())+coord_flip(),
             ggplot(mY[cat =='Policy',]) + geom_point(aes(x = fact,y = 100 * V1))+
               geom_text(aes(x = fact,y = 100 * V1 +5,label = fact),hjust = 0) + scale_y_continuous(limits = c(0,100)) +facet_wrap(~cat) + 
               theme_bw() + theme(axis.title = element_blank(),axis.text.y = element_blank(),axis.ticks.y= element_blank())+coord_flip(),
             if(!DROP_BARRIERS){
               ggplot(mY[cat =='Barrier',]) + geom_point(aes(x = fact,y = 100 * V1))+
                 geom_text(aes(x = fact,y = 100 * V1 +5,label = fact), hjust = 0) + scale_y_continuous(limits = c(0,100)) +facet_wrap(~cat) + theme_bw() + theme(axis.title = element_blank(),axis.text.y = element_blank(),axis.ticks.y= element_blank())+coord_flip()}else{NULL})

if(DROP_BARRIERS){grid.arrange(plist[[1]],plist[[2]],bottom= 'Respondent selection #',ncol=2)}
if(!DROP_BARRIERS){grid.arrange(plist[[1]],plist[[2]],plist[[3]],bottom= 'Respondent selection #',ncol=3)}

#####  FIT MIRT MODELS ###3
#### WARNING d = 3 TAKES A LITTLE BIT. AND DOESN"T CONVERGE AT 500 CYCLES #####
#### need to set dims = 1:3 to also run the 3 dim model ####
dims = 1:2
cycles = 500
mods = mclapply(dims,function(d){
  # note that formula allows fitting predictors, but this only seems to work with dominance models like 2PL, which I guess makes sense since in unfolding model it's about relative location
  mirt(Y,model = d,itemtype = 'ideal',method = 'EM',technical = list(NCYCLES = cycles))#,formula = ~Q1_Focus + When_SLR + Q16_Risk_Agree + Q17_Action_Agree,covdata = predictor_dt)
},mc.cores = min(length(dims),4),mc.cleanup = T,mc.preschedule = T)


###Code to Run on Window####
#mods = lapply(dims,function(d){
  # note that formula allows fitting predictors, but this only seems to work with dominance models like 2PL, which I guess makes sense since in unfolding model it's about relative location
#mirt(Y,model = d,itemtype = 'ideal',method = 'EM',technical = list(NCYCLES = cycles))#,formula = ~Q1_Focus + When_SLR + Q16_Risk_Agree + Q17_Action_Agree,covdata = predictor_dt)})

#make a data.table of item factor scores for d = 1
rotated.factors = data.table(mirt::summary(mods[[1]])$rotF,item = colnames(Y))
# plot d = 1 factor scores
ggplot(rotated.factors[order(-F1)]) + 
  geom_point(aes(y = fct_reorder(item,.x = F1),x = F1)) + 
  theme_bw() + 
  scale_y_discrete(name = 'item') + 
  scale_x_continuous(name = 'Factor loading' ) + 
  ggtitle('Unidimensional model item discrimination')

#make a data.table of item factor scores for d = 2
rotated.factors = data.table(mirt::summary(mods[[2]],'oblimin')$rotF,item = colnames(Y))
#add item type variable
rotated.factors$type = concept_types$Type[match(rotated.factors$item,concept_types$Vector)]
#add item response proportion variable
rotated.factors = merge(rotated.factors,data.table(item = colnames(Y),colMeans(Y)))

# plot d = 2 factor scores
ggplot(rotated.factors)+
  geom_vline(xintercept = 0,lty = 2,col = 'grey50') + 
  geom_hline(yintercept= 0,lty = 2,col = 'grey50')+
  geom_point(aes(y =F2,x = F1,shape = type,colour = type,size = V2)) + 
  geom_text_repel(aes(y =F2,x = F1,label = item),max.overlaps = 30) + 
  theme_bw() + 
  scale_color_tableau(type = 'regular',name = 'item type')+
  scale_shape_discrete(name = 'item type')+
  scale_size_continuous(name = 'response %',breaks = c(0.2,0.4,0.6),labels = c('20%','40%','60%'))+
  theme(legend.position = c(0.25,0.25))+
  scale_y_continuous(name = 'Factor loading, dimension 2' ) + 
  scale_x_continuous(name = 'Factor loading, dimension 1' ) + 
  ggtitle('Bi-dimensional model') 

#plot item and respondent locations for d = 2 model
ggplot() + 
  geom_point(aes(x = F1,y = F2,col = 'red'),data = rotated.factors,pch = 17,size = 2)+
  geom_label_repel(aes(y =F2,x = F1,label = item),max.overlaps = 20,data = rotated.factors) + 
  geom_point(aes(x = F1,y = F2,col = 'red'),data = rotated.factors,pch = 17,size = 2)+
  geom_point(aes(x = F1,y = F2,col = 'black'),data = data.table(fscores(mods[[2]],rotate = 'oblimin')),pch = 21) + 
  xlab('Factor 1 score') + ylab("Factor 2 score") + 
  ggtitle('Estimated ideal points for survey respondents') + 
  scale_color_manual(name = '',labels=c('respondent','item'),values = c('black','red')) + 
  theme(legend.position = 'bottom',legend.direction = 'horizontal',
        legend.backgroun = element_rect(fill = alpha('white',0)))


# compute correlations between factors scores and select varibles
# note still need ot find a good way to test correlation significance without having to run cor.test a bunch of times

facts =  data.table(fscores(mods[[2]],rotate = 'oblimin'),predictor_dt)
htmlTable(
round(rbind(cor(facts[,.(F1,Q1_Focus,When_SLR,Q16_Risk_Agree,Q17_Action_Agree,Q4_Fed, Q4_State, Q4_RegGov, Q4_LocalGov, Q4_WaterSD, Q4_EnviroSD, Q4_Enviro, Q4_Trade, Q4_Ed, Q4_Multistake, Q4_Multijuris, Q4_Political, Q4_CBO, Q4_NGO, Q8_Sum, Q32_Sum, Q11_STAware, Q11_LTAware, Q12_STConcern, Q12_LTConcern, Q20_HumRes, Q20_FinRes, Q20_ExpCollab, Q20_StakeOpp, Q20_PolLead, Q20_SLRUncertain, Q20_SciInfo, Q20_OrgLead, Q20_OverallPlan, Q20_Permits, Q20_PubSupport, Q20_CBORelation, Q20_Sum)])[1,-1],
cor(facts[,.(F2,Q1_Focus,When_SLR,Q16_Risk_Agree,Q17_Action_Agree,Q4_Fed, Q4_State, Q4_RegGov, Q4_LocalGov, Q4_WaterSD, Q4_EnviroSD, Q4_Enviro, Q4_Trade, Q4_Ed, Q4_Multistake, Q4_Multijuris, Q4_Political, Q4_CBO, Q4_NGO, Q8_Sum, Q32_Sum, Q11_STAware, Q11_LTAware, Q12_STConcern, Q12_LTConcern, Q20_HumRes, Q20_FinRes, Q20_ExpCollab, Q20_StakeOpp, Q20_PolLead, Q20_SLRUncertain, Q20_SciInfo, Q20_OrgLead, Q20_OverallPlan, Q20_Permits, Q20_PubSupport, Q20_CBORelation, Q20_Sum)],)[1,-1]),3)
)


htmlTable(rbind(cor.test(facts$F1, facts$Q1_Focus)$p.value, cor.test(facts$F1, facts$When_SLR)$p.value, cor.test(facts$F1, facts$Q16_Risk_Agree)$p.value, cor.test(facts$F1, facts$Q17_Action_Agree)$p.value, cor.test(facts$F1, facts$Q4_Fed)$p.value, cor.test(facts$F1, facts$Q4_State)$p.value, cor.test(facts$F1, facts$Q4_RegGov)$p.value, cor.test(facts$F1, facts$Q4_LocalGov)$p.value, cor.test(facts$F1, facts$Q4_WaterSD)$p.value, cor.test(facts$F1, facts$Q4_EnviroSD)$p.value, cor.test(facts$F1, facts$Q4_Enviro)$p.value, cor.test(facts$F1, facts$Q4_Trade)$p.value, cor.test(facts$F1, facts$Q4_Ed)$p.value, cor.test(facts$F1, facts$Q4_Multistake)$p.value, cor.test(facts$F1, facts$Q4_Multijuris)$p.value, cor.test(facts$F1, facts$Q4_Political)$p.value, cor.test(facts$F1, facts$Q4_CBO)$p.value, cor.test(facts$F1, facts$Q4_NGO)$p.value, cor.test(facts$F1, facts$Q8_Sum)$p.value, cor.test(facts$F1, facts$Q32_Sum)$p.value, cor.test(facts$F1, facts$Q11_STAware)$p.value, cor.test(facts$F1, facts$Q11_LTAware)$p.value, cor.test(facts$F1, facts$Q12_STConcern)$p.value, cor.test(facts$F1, facts$Q12_LTConcern)$p.value, cor.test(facts$F1, facts$Q20_HumRes)$p.value, cor.test(facts$F1, facts$Q20_FinRes)$p.value, cor.test(facts$F1, facts$Q20_ExpCollab)$p.value, cor.test(facts$F1, facts$Q20_StakeOpp)$p.value, cor.test(facts$F1, facts$Q20_PolLead)$p.value, cor.test(facts$F1, facts$Q20_SLRUncertain)$p.value, cor.test(facts$F1, facts$Q20_SciInfo)$p.value, cor.test(facts$F1, facts$Q20_OrgLead)$p.value, cor.test(facts$F1, facts$Q20_OverallPlan)$p.value, cor.test(facts$F1, facts$Q20_Permits)$p.value, cor.test(facts$F1, facts$Q20_PubSupport)$p.value, cor.test(facts$F1, facts$Q20_CBORelation)$p.value, cor.test(facts$F1, facts$Q20_Sum)$p.value))


htmlTable(rbind(cor.test(facts$F2, facts$Q1_Focus)$p.value, cor.test(facts$F2, facts$When_SLR)$p.value, cor.test(facts$F2, facts$Q16_Risk_Agree)$p.value, cor.test(facts$F2, facts$Q17_Action_Agree)$p.value, cor.test(facts$F2, facts$Q4_Fed)$p.value, cor.test(facts$F2, facts$Q4_State)$p.value, cor.test(facts$F2, facts$Q4_RegGov)$p.value, cor.test(facts$F2, facts$Q4_LocalGov)$p.value, cor.test(facts$F2, facts$Q4_WaterSD)$p.value, cor.test(facts$F2, facts$Q4_EnviroSD)$p.value, cor.test(facts$F2, facts$Q4_Enviro)$p.value, cor.test(facts$F2, facts$Q4_Trade)$p.value, cor.test(facts$F2, facts$Q4_Ed)$p.value, cor.test(facts$F2, facts$Q4_Multistake)$p.value, cor.test(facts$F2, facts$Q4_Multijuris)$p.value, cor.test(facts$F2, facts$Q4_Political)$p.value, cor.test(facts$F2, facts$Q4_CBO)$p.value, cor.test(facts$F2, facts$Q4_NGO)$p.value, cor.test(facts$F2, facts$Q8_Sum)$p.value, cor.test(facts$F2, facts$Q32_Sum)$p.value, cor.test(facts$F2, facts$Q11_STAware)$p.value, cor.test(facts$F2, facts$Q11_LTAware)$p.value, cor.test(facts$F2, facts$Q12_STConcern)$p.value, cor.test(facts$F2, facts$Q12_LTConcern)$p.value, cor.test(facts$F2, facts$Q20_HumRes)$p.value, cor.test(facts$F2, facts$Q20_FinRes)$p.value, cor.test(facts$F2, facts$Q20_ExpCollab)$p.value, cor.test(facts$F2, facts$Q20_StakeOpp)$p.value, cor.test(facts$F2, facts$Q20_PolLead)$p.value, cor.test(facts$F2, facts$Q20_SLRUncertain)$p.value, cor.test(facts$F2, facts$Q20_SciInfo)$p.value, cor.test(facts$F2, facts$Q20_OrgLead)$p.value, cor.test(facts$F2, facts$Q20_OverallPlan)$p.value, cor.test(facts$F2, facts$Q20_Permits)$p.value, cor.test(facts$F2, facts$Q20_PubSupport)$p.value, cor.test(facts$F2, facts$Q20_CBORelation)$p.value, cor.test(facts$F2, facts$Q20_Sum)$p.value))

#Corr Plot

corrsdata <- subset(facts,select=c("F1", "F2", "Q1_Focus", "When_SLR", "Q16_Risk_Agree", "Q17_Action_Agree", "Q4_Fed","Q4_State", "Q4_RegGov", "Q4_LocalGov", "Q4_WaterSD", "Q4_EnviroSD", "Q4_Enviro", "Q4_Trade", "Q4_Ed", "Q4_Multistake", "Q4_Multijuris", "Q4_Political", "Q4_CBO", "Q4_NGO", "Q8_Sum", "Q32_Sum", "Q11_STAware", "Q11_LTAware", "Q12_STConcern", "Q12_LTConcern", "Q20_HumRes", "Q20_FinRes", "Q20_ExpCollab", "Q20_StakeOpp", "Q20_PolLead", "Q20_SLRUncertain", "Q20_SciInfo", "Q20_OrgLead", "Q20_OverallPlan", "Q20_Permits","Q20_PubSupport", "Q20_CBORelation", "Q20_Sum"))

library(data.table)
setnames(corrsdata, old = c("F1", "F2", "Q1_Focus", "When_SLR", "Q16_Risk_Agree", "Q17_Action_Agree", "Q4_Fed","Q4_State", "Q4_RegGov", "Q4_LocalGov", "Q4_WaterSD", "Q4_EnviroSD", "Q4_Enviro", "Q4_Trade", "Q4_Ed", "Q4_Multistake", "Q4_Multijuris", "Q4_Political", "Q4_CBO", "Q4_NGO", "Q8_Sum", "Q32_Sum", "Q11_STAware", "Q11_LTAware", "Q12_STConcern", "Q12_LTConcern", "Q20_HumRes", "Q20_FinRes", "Q20_ExpCollab", "Q20_StakeOpp", "Q20_PolLead", "Q20_SLRUncertain", "Q20_SciInfo", "Q20_OrgLead", "Q20_OverallPlan", "Q20_Permits","Q20_PubSupport", "Q20_CBORelation", "Q20_Sum"), new = c("F1", "F2", "Focus", "When_SLR", "Risk_Agree", "Action_Agree", "Fed","State", "RegGov", "LocalGov", "WaterSD", "EnviroSD", "Enviro", "Trade", "Ed", "Multistake", "Multijuris", "Political", "CBO", "NGO", "Tasks", "InfoTypes", "STAware", "LTAware", "STConcern", "LTConcern", "HumRes", "FinRes", "CollabExp", "PublicOppose", "PoliticalLead", "SLRUncertain", "SciInfo", "OrgLead", "OverallPlan", "Permits","PubSupport", "CBORelations", "No.Barriers"))

corrs <- cor(corrsdata)
corrplot(corrs, method='color')
corrplot(corrs, method='circle')



# plot distributions of items and people on factors 1 and 2 in 2 factor model
grid.arrange(
  ggplot() + 
    geom_density(data = rotated.factors,aes(x = F1,fill = 'item'),alpha = 0.7) +  
    geom_density(data = data.table(fscores(mods[[2]],rotate = 'oblimin')),aes(x = F1,fill = 'respondent'),alpha = 0.7) +
    scale_fill_manual(values = c('light blue','dark blue'),name = 'estimated location') + 
    xlab('factor 1'),
  ggplot() + 
    geom_density(data = rotated.factors,aes(x = F2,fill = 'item'),alpha = 0.7) +  
    geom_density(data = data.table(fscores(mods[[2]],rotate = 'oblimin')),aes(x = F2,fill = 'respondent'),alpha = 0.7) +
    scale_fill_manual(values = c('light blue','dark blue'),name = 'estimated location') + 
    xlab('factor 2'))



if(any(dims==3)){
# same stuff but for d = 3
rotated.factors = data.table(mirt::summary(mods[[3]],'oblimin')$rotF,item = colnames(Y))
rotated.factors$type = concept_types$Type[match(rotated.factors$item,concept_types$Vector)]
rotated.factors = merge(rotated.factors,data.table(item = colnames(Y),colMeans(Y)))
f1f2 = ggplot() + 
  geom_point(aes(x = F1,y = F2,col = 'red'),data = rotated.factors,pch = 17,size = 2)+
  geom_label_repel(aes(y =F2,x = F1,label = item),max.overlaps = 20,data = rotated.factors) + 
  geom_point(aes(x = F1,y = F2,col = 'red'),data = rotated.factors,pch = 17,size = 2)+
  geom_point(aes(x = F1,y = F2,col = 'black'),data = data.table(fscores(mods[[3]],rotate = 'oblimin')),pch = 21) + 
  xlab('Factor 1 score') + ylab("Factor 2 score") + 
  scale_color_manual(name = '',labels=c('respondent','item'),values = c('black','red')) + 
  theme(legend.position = c(0.9,0.1),legend.direction = 'horizontal',
        legend.backgroun = element_rect(fill = alpha('white',0)))

f1f3 = ggplot() + 
  geom_point(aes(x = F1,y = F3,col = 'red'),data = rotated.factors,pch = 17,size = 2)+
  geom_label_repel(aes(y =F3,x = F1,label = item),max.overlaps = 20,data = rotated.factors) + 
  geom_point(aes(x = F1,y = F3,col = 'red'),data = rotated.factors,pch = 17,size = 2)+
  geom_point(aes(x = F1,y = F3,col = 'black'),data = data.table(fscores(mods[[3]],rotate = 'oblimin')),pch = 21) + 
  xlab('Factor 1 score') + ylab("Factor 3 score") + 
  scale_color_manual(name = '',labels=c('respondent','item'),values = c('black','red')) + 
  theme(legend.position = c(0.9,0.1),legend.direction = 'horizontal',
        legend.backgroun = element_rect(fill = alpha('white',0)))


f2f3 = ggplot() + 
  geom_point(aes(x = F2,y = F3,col = 'red'),data = rotated.factors,pch = 17,size = 2)+
  geom_label_repel(aes(y =F3,x = F2,label = item),max.overlaps = 20,data = rotated.factors) + 
  geom_point(aes(x = F2,y = F3,col = 'red'),data = rotated.factors,pch = 17,size = 2)+
  geom_point(aes(x = F2,y = F3,col = 'black'),data = data.table(fscores(mods[[3]],rotate = 'oblimin')),pch = 21) + 
  xlab('Factor 2 score') + ylab("Factor 3 score") + 
  scale_color_manual(name = '',labels=c('respondent','item'),values = c('black','red')) + 
  theme(legend.position = c(0.9,0.1),legend.direction = 'horizontal',
        legend.backgroun = element_rect(fill = alpha('white',0)))


grid.arrange(f1f2,f1f3,f2f3,ncol = 1)
}else{print("you didn't run a 3 dimensional model; set dims = 1:3 if you want the 3d model to be fit")}






