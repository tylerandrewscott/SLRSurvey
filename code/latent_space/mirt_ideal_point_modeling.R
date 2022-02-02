#set seed for replicable results
seed = 24

#DropBarriers
DROP_BARRIERS=T

### load packages (not all of htese are needed at this point, so shoudl be cleaned up a bit)
packs =c('reshape2','Hmisc','tidyverse','purrr','data.table','statnet','latentnet','bipartite','lvm4net','mirt','pbapply','Hmisc',
         'htmlTable','parallel','parallel','semTable',
         'ggthemes','here','ggnetwork','gridExtra','ggrepel','corrplot','htmlTable','readxl','nFactors','ggrepel','plotly','ggalluvial')
need = packs[!packs %in% names(installed.packages()[,2])]
invisible(sapply(need,function(x) suppressMessages(install.packages(x,type= 'source'))))
invisible(sapply(packs,function(x) suppressMessages(library(x,character.only = T))))


packs =c('Hmisc','tidyverse','purrr','data.table','statnet','latentnet','bipartite','lvm4net','mirt','pbapply','Hmisc','htmlTable','parallel','parallel',
         'ggthemes','here','ggnetwork','gridExtra','ggrepel','corrplot','htmlTable','readxl','nFactors','ggrepel','plotly','ggalluvial')
need = packs[!packs %in% names(installed.packages()[,2])]
invisible(sapply(need,function(x) suppressMessages(install.packages(x,type= 'library'))))
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
predictor_dt$Q8_JobTasks <- orig$Q8_Sum[match(predictor_dt$id,orig$ResponseId)]
predictor_dt$Q32_InfoTypes <- orig$Q32_Sum[match(predictor_dt$id,orig$ResponseId)]
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
predictor_dt$Q19_Sum <- orig$Q19_SUM[match(predictor_dt$id, orig$ResponseId)]

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

predictor_dt$Q8_JobTasks[is.na(predictor_dt$Q8_JobTasks)]<-round(mean(predictor_dt$Q8_JobTasks,na.rm = T))
predictor_dt$Q32_InfoTypes[is.na(predictor_dt$Q32_InfoTypes)]<-round(mean(predictor_dt$Q32_InfoTypes,na.rm = T))
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
predictor_dt$Q19_Sum[is.na(predictor_dt$Q19_Sum)]<-round(mean(predictor_dt$Q20_Sum, na.rm=T))

mY = reshape2::melt(Y)
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
#mods = lapply(dims,function(d){mirt(Y,model = d,itemtype = 'ideal',method = 'EM',technical = list(NCYCLES = cycles))}) #,formula = ~Q1_Focus + When_SLR + Q16_Risk_Agree + Q17_Action_Agree,covdata = predictor_dt)

#make a data.table of item factor scores for d = 1
rotated.factors = data.table(mirt::summary(mods[[1]])$rotF,item = colnames(Y))
# plot d = 1 factor scores
gg1 = ggplot(rotated.factors[order(-F1)]) + 
  geom_point(aes(y = fct_reorder(item,.x = F1),x = F1)) + 
  theme_bw() + 
  scale_y_discrete(name = 'item') + 
  scale_x_continuous(name = 'Factor loading' ) + 
  ggtitle('Single dimension item discrimination')

ggsave(filename = 'output/figures/figure_1d_discrimination.png',plot = gg1,width = 4.5,height = 4.5, units = 'in',dpi = 350)

#make a data.table of item factor scores for d = 2
rotated.factors = data.table(mirt::summary(mods[[2]],'oblimin')$rotF,item = colnames(Y))
#add item type variable
rotated.factors$type = concept_types$Type[match(rotated.factors$item,concept_types$Vector)]
#add item response proportion variable
rotated.factors = merge(rotated.factors,data.table(item = colnames(Y),colMeans(Y)))

# plot d = 2 factor scores
gg2 = ggplot(rotated.factors)+
  geom_vline(xintercept = 0,lty = 2,col = 'grey50') + 
  geom_hline(yintercept= 0,lty = 2,col = 'grey50')+
  geom_point(aes(y =F2,x = F1,shape = type,colour = type,size = V2)) + 
  geom_text_repel(aes(y =F2,x = F1,label = item),max.overlaps = 30) + 
  theme_bw() + 
  scale_color_tableau(type = 'regular',name = 'item type',labels = c('problem','solution'))+
  scale_shape_discrete(name = 'item type',labels = c('problem','solution'))+
  scale_size_continuous(name = '% chosen',breaks = c(0.1,0.3,0.5,0.7),labels = c('10%','30%','50%','70%'))+
  theme(legend.position = c(0.25,0.25))+
  scale_y_continuous(name = 'Factor loading, dimension 2' ) + 
  scale_x_continuous(name = 'Factor loading, dimension 1' ) + 
  ggtitle('Bi-dimensional model') 

ggsave(filename = 'output/figures/figure_factor_loadings.png',plot = gg2,width = 7,height = 5.5, units = 'in',dpi = 350)
#plot item and respondent locations for d = 2 model
gg3<-ggplot() + 
  geom_point(aes(x = F1,y = F2,col = 'red'),data = rotated.factors,pch = 17,size = 2)+
  geom_label_repel(aes(y =F2,x = F1,label = item),max.overlaps = 20,data = rotated.factors) + 
  geom_point(aes(x = F1,y = F2,col = 'red'),data = rotated.factors,pch = 17,size = 2)+
  geom_point(aes(x = F1,y = F2,col = 'black'),data = data.table(fscores(mods[[2]],rotate = 'oblimin')),pch = 21) + 
  xlab('Factor 1 score') + ylab("Factor 2 score") + 
  ggtitle('Estimated ideal points for survey respondents') + 
  scale_color_manual(name = '',labels=c('respondent','item'),values = c('black','red')) + 
  theme(legend.position = 'bottom',legend.direction = 'horizontal',
        legend.backgroun = element_rect(fill = alpha('white',0)))


ggsave(filename = 'output/figures/figure_respondent_and_item_locations.png',plot = gg3,width = 7,height = 5.5, units = 'in',dpi = 350)
#
# compute correlations between factors scores and select variables
# note still need to find a good way to test correlation significance without having to run cor.test a bunch of times


facts =  data.table(fscores(mods[[2]],rotate = 'oblimin'),predictor_dt)

varlist = grep('^Q',names(facts),value = T)


f1_cor_tests = lapply(varlist,function(x) cor.test(facts$F1,facts[[x]],method = 'kendall'))
est1 = formatC(round(sapply(f1_cor_tests,function(x) x$estimate),3),flag = '0',digits = 3)
p1 = sapply(f1_cor_tests,function(x) x$p.value)


f2_cor_tests = lapply(varlist,function(x) cor.test(facts$F2,facts[[x]],method = 'kendall'))
est2 = formatC(round(sapply(f2_cor_tests,function(x) x$estimate),3),flag = '0',digits = 3)
p2 = sapply(f2_cor_tests,function(x) x$p.value)


stargazer::stargazer(data.table(item = varlist,
           F1 = paste0(est1,ifelse(p1<0.05,'*',ifelse(p1<0.01,'**',ifelse(p1<0.001,'***','')))),
           F2 = paste0(est2,ifelse(p2<0.05,'*',ifelse(p2<0.01,'**',ifelse(p2<0.001,'***',''))))),summary = F,out = 'output/tables/correlation_tests.html')



#Corr Plot

corrsdata <- subset(facts,select=c("F1", "F2", "Q1_Focus", "When_SLR", "Q16_Risk_Agree", "Q17_Action_Agree", "Q4_Fed","Q4_State", "Q4_RegGov", "Q4_LocalGov", "Q4_WaterSD", "Q4_EnviroSD", "Q4_Enviro", "Q4_Trade", "Q4_Ed", "Q4_Multistake", "Q4_Multijuris", "Q4_Political", "Q4_CBO", "Q4_NGO", "Q8_JobTasks", "Q32_InfoTypes", "Q11_STAware", "Q11_LTAware", "Q12_STConcern", "Q12_LTConcern", "Q20_HumRes", "Q20_FinRes", "Q20_ExpCollab", "Q20_StakeOpp", "Q20_PolLead", "Q20_SLRUncertain", "Q20_SciInfo", "Q20_OrgLead", "Q20_OverallPlan", "Q20_Permits","Q20_PubSupport", "Q20_CBORelation", "Q20_Sum"))

library(data.table)
setnames(corrsdata, old = c("F1", "F2", "Q1_Focus", "When_SLR", "Q16_Risk_Agree", "Q17_Action_Agree", "Q4_Fed","Q4_State", "Q4_RegGov", "Q4_LocalGov", "Q4_WaterSD", "Q4_EnviroSD", "Q4_Enviro", "Q4_Trade", "Q4_Ed", "Q4_Multistake", "Q4_Multijuris", "Q4_Political", "Q4_CBO", "Q4_NGO", "Q8_JobTasks", "Q32_InfoTypes", "Q11_STAware", "Q11_LTAware", "Q12_STConcern", "Q12_LTConcern", "Q20_HumRes", "Q20_FinRes", "Q20_ExpCollab", "Q20_StakeOpp", "Q20_PolLead", "Q20_SLRUncertain", "Q20_SciInfo", "Q20_OrgLead", "Q20_OverallPlan", "Q20_Permits","Q20_PubSupport", "Q20_CBORelation", "Q20_Sum"), new = c("F1", "F2", "Focus", "When_SLR", "Risk_Agree", "Action_Agree", "Fed","State", "RegGov", "LocalGov", "WaterSD", "EnviroSD", "Enviro", "Trade", "Ed", "Multistake", "Multijuris", "Political", "CBO", "NGO", "Tasks", "InfoTypes", "STAware", "LTAware", "STConcern", "LTConcern", "HumRes", "FinRes", "CollabExp", "PublicOppose", "PoliticalLead", "SLRUncertain", "SciInfo", "OrgLead", "OverallPlan", "Permits","PubSupport", "CBORelations", "No.Barriers"))

corrs <- cor(corrsdata)
png(filename='output/figures/figure_corrplot.png')
corrplot(corrs, method='color')
dev.off()
corrplot(corrs, method='circle')


# 
# # plot distributions of items and people on factors 1 and 2 in 2 factor model
# grid.arrange(
#   ggplot() + 
#     geom_density(data = rotated.factors,aes(x = F1,fill = 'item'),alpha = 0.7) +  
#     geom_density(data = data.table(fscores(mods[[2]],rotate = 'oblimin')),aes(x = F1,fill = 'respondent'),alpha = 0.7) +
#     scale_fill_manual(values = c('light blue','dark blue'),name = 'estimated location') + 
#     xlab('factor 1'),
#   ggplot() + 
#     geom_density(data = rotated.factors,aes(x = F2,fill = 'item'),alpha = 0.7) +  
#     geom_density(data = data.table(fscores(mods[[2]],rotate = 'oblimin')),aes(x = F2,fill = 'respondent'),alpha = 0.7) +
#     scale_fill_manual(values = c('light blue','dark blue'),name = 'estimated location') + 
#     xlab('factor 2'))



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

####Start of Regression Modeling-----------------

#Reformatting Data for Inclusion in Regression
#create average scores for short and long term awareness/concern
facts$aware <- ((facts$Q11_STAware+facts$Q11_LTAware)/2)
facts$concern <- ((facts$Q12_STConcern+ facts$Q12_LTConcern)/2)
#Combine CBO and NGO Actor Types
facts$nonprofit <- ifelse(facts$Q4_CBO=="1"|facts$Q4_NGO=="1", 1, 0)
#Combine federal, state, and regional gov
facts$govagency <- ifelse(facts$Q4_Fed=="1"|facts$Q4_State=="1"|facts$Q4_RegGov=="1", 1, 0)
#Combine local gov with special districts
facts$localagency <- ifelse(facts$Q4_LocalGov=="1"|facts$Q4_EnviroSD=="1"|facts$Q4_WaterSD=="1", 1, 0)
#Keep Enviro Groups Separate for first version
facts$enviro <- ifelse(facts$Q4_Enviro=="1", 1, 0)
#create category of all the actors not in these categories
facts$noactorcat <- ifelse(facts$Q4_CBO=="0"& facts$Q4_NGO=="0"& facts$Q4_Fed=="0"& facts$Q4_State=="0"& facts$Q4_RegGov=="0"& facts$Q4_LocalGov=="0"& facts$Q4_EnviroSD=="0"& facts$Q4_WaterSD=="0"& facts$Q4_Ed=="0"& facts$Q4_Multijuris=="0"& facts$Q4_Multistake=="0"& facts$Q4_Political=="0"& facts$Q4_Trade=="0"& facts$Q4_Enviro=="0", 1, 0)
#Create other for all other actors without enviro
facts$othergroup <- ifelse(facts$Q4_Ed=="1"|facts$Q4_Multijuris=="1"|facts$Q4_Multistake=="1"|facts$Q4_Political=="1"|facts$Q4_Trade=="1"|facts$noactorcat=="1", 1, 0)
#create second other option with enviro included
facts$othergroup_wEnviro <- ifelse(facts$Q4_Ed=="1"|facts$Q4_Multijuris=="1"|facts$Q4_Multistake=="1"|facts$Q4_Political=="1"|facts$Q4_Trade=="1"|facts$Q4_Enviro=="1"|facts$noactorcat=="1", 1, 0)
#create new nonprofit with enviro
facts$noprofit <- ifelse(facts$Q4_CBO=="1"|facts$Q4_NGO=="1"|facts$Q4_Enviro=="1", 1, 0)
#rename the no category actor
facts$OtherActorType <- ifelse(facts$noactorcat=="1", 1, 0)

#create a percent of possible collaborative activities variable
facts$Q19_percent <- facts$Q19_Sum/15

require(lavaan)
require(tidySEM)

facts$Q32_InfoTypes
facts$When_SLR = facts$When_SLR * -1


cfa_form <- '
Engagement  =~  Q32_InfoTypes + Q1_Focus + Q11_STAware + Q11_LTAware + Q19_Sum\
Concern =~ Q12_STConcern + Q12_LTConcern + When_SLR
'
#cfa_fit = cfa(cfa_form,data = facts,ordered = c('Q1_Focus','When_SLR','Q11_STAware','Q11_LTAware','Q12_LTConcern','Q12_STConcern'))
cfa_fit = cfa(cfa_form,data = facts)


#### THIS IS A GRAPH OF THE CFA FOR ENGAGEMENT

start= get_layout('Q32_InfoTypes','Q1_Focus','Q11_STAware','Q11_LTAware','Q19_Sum',NA,
NA,NA,'Engagement','Concern',NA,NA,
NA,NA,'Q12_STConcern','Q12_LTConcern','When_SLR',NA,rows = 3)

cfa_graph = prepare_graph(cfa_fit,
                          layout =start)
cfa_graph = hide_var(cfa_graph)

(gg_cfa = plot(cfa_graph) + 
  ggtitle('Confirmatory factor analysis: indicators of policy engagement') +
  coord_flip())
ggsave(plot = gg_cfa,filename = 'output/figures/cfa_plot.png',width = 6,height = 4.5,dpi = 300, units = 'in')

fitmeasures(cfa_fit)

###Graph of CFA for engagement with Q19 percent instead of sum
cfa_form2 <- '
Engagement  =~  Q32_InfoTypes + Q1_Focus + Q11_STAware + Q11_LTAware + Q19_percent\
Concern =~ Q12_STConcern + Q12_LTConcern + When_SLR
'
cfa_fit2 = cfa(cfa_form2,data = facts)
start2= get_layout('Q32_InfoTypes','Q1_Focus','Q11_STAware','Q11_LTAware','Q19_percent',NA,
                  NA,NA,'Engagement','Concern',NA,NA,
                  NA,NA,'Q12_STConcern','Q12_LTConcern','When_SLR',NA,rows = 3)

cfa_graph2 = prepare_graph(cfa_fit2,
                          layout =start2)
cfa_graph2 = hide_var(cfa_graph2)

(gg_cfa2 = plot(cfa_graph2) + 
    ggtitle('Confirmatory factor analysis: indicators of policy engagement') +
    coord_flip())
ggsave(plot = gg_cfa2,filename = 'output/figures/cfa_plot_V2.png',width = 6,height = 4.5,dpi = 300, units = 'in')
####Final SEM Model Redo (all actor types and two latent variables)---------------------
sem_formfin <- '
#2 latent variables for engagement
Engagement  =~  Q32_InfoTypes + Q1_Focus + Q11_STAware + Q11_LTAware + Q19_Sum

Concern =~ Q12_STConcern + Q12_LTConcern + When_SLR

#regression with all actor types (local gov baseline)
F1 ~ Engagement + Concern + Q4_CBO + Q4_NGO + Q4_Fed + Q4_State + Q4_RegGov + Q4_EnviroSD + Q4_WaterSD + Q4_Ed + Q4_Multijuris + Q4_Multistake + Q4_Political + Q4_Trade + Q4_Enviro + OtherActorType
F2 ~ Engagement + Concern + Q4_CBO + Q4_NGO + Q4_Fed + Q4_State + Q4_RegGov + Q4_EnviroSD + Q4_WaterSD + Q4_Ed + Q4_Multijuris + Q4_Multistake + Q4_Political + Q4_Trade + Q4_Enviro + OtherActorType
#resid corrs
F1~~F2
'
sem_fitfin = sem(sem_formfin,data =  facts)

##### there are basically 4 layers, each 9 wide
start= get_layout(
  # row 1
  'Q4_CBO','Q4_NGO','Q4_State','Q4_RegGov','Q4_EnviroSD',
  'Q4_WaterSD','Q4_Ed','Q4_Trade','OtherActorType',
  # row 2
  NA,NA,NA,'F1',NA,NA,'F2',NA,NA,
  # row 3
  NA,'Q4_Enviro','Q4_Multijuris',NA,'Engagement','Concern',NA,'Q4_Multistake','Q4_Political',
  # row 4
  NA,'Q32_InfoTypes','Q1_Focus','Q11_STAware','Q11_LTAware',
                  'Q19_Sum','Q12_STConcern','Q12_LTConcern','When_SLR',
                  rows = 4)

sem_graph = prepare_graph(sem_fitfin,
                          layout =start)
sem_graph = hide_var(sem_graph)

(gg_sem = plot(sem_graph) + 
    ggtitle('SEM: indicators of policy engagement') + coord_flip())
  


start= get_layout(
  # row 2
  NA,'F1',NA,'Q4_CBO',NA,'F2',NA,
  # row 3
  'Q32_InfoTypes',NA,'Engagement',NA,'Concern',NA,'When_SLR',
  # row 4
  'Q1_Focus','Q11_STAware','Q11_LTAware',
  'Q19_Sum',NA,'Q12_STConcern','Q12_LTConcern',
  rows = 3)

sem_graph = prepare_graph(sem_fitfin,
                          layout =start)
sem_graph = hide_var(sem_graph)

sem_graph$edges[sem_graph$edges$from=='Q4_CBO'&sem_graph$edges$to=='F2',]$label <- '---'
sem_graph$edges[sem_graph$edges$from=='Q4_CBO'&sem_graph$edges$to=='F1',]$label <- '---'

sem_graph$nodes$label[sem_graph$nodes$label=='Q4_CBO']<-''

(gg_sem = plot(sem_graph) + 
    #ggplot() + 
    geom_rect(data = sem_graph$nodes[sem_graph$nodes$label=='',],fill = 'white',
              linetype = 3,colour = 'black',
              aes(xmin = node_xmin-0.1,xmax = node_xmax+.1,ymax = node_ymax+0.1,ymin = node_ymin-0.1))+
    geom_text(data = sem_graph$nodes[sem_graph$nodes$label=='',],
    aes(label = 'Org. type*',x = x,y = y)) + coord_flip() +
  annotate('text',x = 1,y =6.5,label = '*represents 13 org.\ntype indicators',col = 'black') +
    theme(text = element_text(family = 'Times',size = 12)) +
    ggtitle('SEM: Engagement and org. type as predictors of factor location')
  ) 

ggsave(plot = gg_sem,filename = 'output/figures/sem_plot_stylizedOrgType.png',width = 7,height = 5,dpi = 600, units = 'in')

library(stargazer)
summary(cfa_fit)
summary(sem_fitfin)
sem_fitfin@Fit@est
cfa_fit@Fit@est

sem_form <- '
#latent variable for engagement
Engagement  =~  Q32_InfoTypes + Q1_Focus + When_SLR + Q11_STAware + Q11_LTAware + Q12_LTConcern + Q12_STConcern
#regression 
F1 ~ Engagement + nonprofit + govagency + othergroup_wEnviro
F2 ~ Engagement + nonprofit + govagency + othergroup_wEnviro
#resid corrs
F1~~F2
'
sem_fit = sem(sem_form,data =  facts)

summary(sem_fit,fit.measures = T)



sem_html <- semTable(sem_fit, type="html")
#install.packages("readr")
library(readr)
readr::write_file(sem_html, "output/tables/sem_model_V1_10-7.html")

####Try SEM with environmental groups separated
sem_form2 <- '
#latent variable for engagement
Engagement  =~  Q32_InfoTypes + Q1_Focus + When_SLR + Q11_STAware + Q11_LTAware + Q12_LTConcern + Q12_STConcern
#regression 
F1 ~ Engagement + nonprofit + govagency + enviro + othergroup
F2 ~ Engagement + nonprofit + govagency + enviro + othergroup
#resid corrs
F1~~F2
'
sem_fit2 = sem(sem_form2,data =  facts)

summary(sem_fit2,fit.measures = T)

sem_html2 <- semTable(sem_fit2, type="html")
readr::write_file(sem_html2, "output/tables/sem_model_V2_10-7.html")

####Try SEM with enviro groups added to nonprofit
sem_form3 <- '
#latent variable for engagement
Engagement  =~  Q32_InfoTypes + Q1_Focus + When_SLR + Q11_STAware + Q11_LTAware + Q12_LTConcern + Q12_STConcern
#regression 
F1 ~ Engagement + noprofit + govagency + othergroup
F2 ~ Engagement + noprofit + govagency + othergroup
#resid corrs
F1~~F2
'
sem_fit3 = sem(sem_form3,data =  facts)

summary(sem_fit3,fit.measures = T)

test = reshape2::melt(facts[,c(grep('Q4',names(facts),value = T),'F1','F2'),with = F],
               id.vars = c('F1','F2'))
test = data.table(test)

test_mean = test[value ==1,list(mean(F1),mean(F2)),by=.(variable)]

head(test_mean)
ggplot(test_mean,aes(x = V1,y = V2,label= variable)) + 
  geom_point() + scale_x_continuous(limits = c(-1,1))+
  scale_y_continuous(limits = c(-1,1))+
  geom_label_repel()



sem_html3 <- semTable(sem_fit3, type="html")
readr::write_file(sem_html3, "output/tables/sem_model_V3_10-7.html")
####Sem Paths Plot Diagram of SEM Model------------
#path plot for first sem
ly = get_layout(sem_fit, 
                     layout_algorithm = 'layout_with_kk')
sem_plot = tidySEM::prepare_graph(sem_fit,layout = ly)
sem_plot <- tidySEM::hide_var(sem_plot)

gg_sem = plot(sem_plot) + ggtitle('F1 and F1 regressed on engagement + covariates')

ggsave(filename = 'output/figures/SEM_model_diagram.png',plot = gg_sem,dpi = 300,width = 7,height = 5,units = 'in')

#second sem model
ly2 = get_layout(sem_fit2, 
                layout_algorithm = 'layout_with_kk')
sem_plot2 = tidySEM::prepare_graph(sem_fit2,layout = ly2)
sem_plot2 <- tidySEM::hide_var(sem_plot2)

gg_sem2 = plot(sem_plot2) + ggtitle('F1 and F1 regressed on engagement + covariates')

ggsave(filename = 'output/figures/SEM_model_diagram2.png',plot = gg_sem2,dpi = 300,width = 7,height = 5,units = 'in')

#third sem model 
ly3 = get_layout(sem_fit3, 
                 layout_algorithm = 'layout_with_kk')
sem_plot3 = tidySEM::prepare_graph(sem_fit3,layout = ly3)
sem_plot3 <- tidySEM::hide_var(sem_plot3)

gg_sem3 = plot(sem_plot3) + ggtitle('F1 and F1 regressed on engagement + covariates')

ggsave(filename = 'output/figures/SEM_model_diagram3.png',plot = gg_sem3,dpi = 300,width = 7,height = 5,units = 'in')

####Final SEM Model Redo (all actor types and two latent variables)---------------------
sem_formfin <- '
#2 latent variables for engagement
Engagement  =~  Q32_InfoTypes + Q1_Focus + Q11_STAware + Q11_LTAware + Q19_Sum

Concern =~ Q12_STConcern + Q12_LTConcern + When_SLR

#regression with all actor types (local gov baseline)
F1 ~ Engagement + Concern + Q4_CBO + Q4_NGO + Q4_Fed + Q4_State + Q4_RegGov + Q4_EnviroSD + Q4_WaterSD + Q4_Ed + Q4_Multijuris + Q4_Multistake + Q4_Political + Q4_Trade + Q4_Enviro + OtherActorType
F2 ~ Engagement + Concern + Q4_CBO + Q4_NGO + Q4_Fed + Q4_State + Q4_RegGov + Q4_EnviroSD + Q4_WaterSD + Q4_Ed + Q4_Multijuris + Q4_Multistake + Q4_Political + Q4_Trade + Q4_Enviro + OtherActorType
#resid corrs
F1~~F2
'


sem_fitfin = sem(sem_formfin,data =  facts)

summary(sem_fitfin,fit.measures = T)

sem_htmlfin <- semTable(sem_fitfin, type="html")
#install.packages("readr")
library(readr)
readr::write_file(sem_htmlfin, "output/tables/sem_model_10-18.html")

####Redo Final Model but combine CBO and NGO- final final I think 

sem_formfin2 <- '
#2 latent variables for engagement
Engagement  =~  Q32_InfoTypes + Q1_Focus + Q11_STAware + Q11_LTAware + Q19_percent

Concern =~ Q12_STConcern + Q12_LTConcern + When_SLR

#regression with all actor types (local gov baseline)
F1 ~ Engagement + Concern + nonprofit + Q4_Fed + Q4_State + Q4_RegGov + Q4_EnviroSD + Q4_WaterSD + Q4_Ed + Q4_Multijuris + Q4_Multistake + Q4_Political + Q4_Trade + Q4_Enviro + OtherActorType
F2 ~ Engagement + Concern + nonprofit + Q4_Fed + Q4_State + Q4_RegGov + Q4_EnviroSD + Q4_WaterSD + Q4_Ed + Q4_Multijuris + Q4_Multistake + Q4_Political + Q4_Trade + Q4_Enviro + OtherActorType
#resid corrs
F1~~F2
'

sem_fitfin2 = sem(sem_formfin2,data =  facts)

summary(sem_fitfin2,fit.measures = T)
library(semTable)
sem_htmlfin2 <- semTable(sem_fitfin2, type="html")
#install.packages("readr")
library(readr)
readr::write_file(sem_htmlfin2, "output/tables/sem_model_11-4.html")


#path plot for final sem
library(tidySEM)
lyfin = get_layout(sem_fitfin, 
                layout_algorithm = 'layout_with_kk')
sem_plotfin = tidySEM::prepare_graph(sem_fitfin,layout = lyfin)
sem_plotfin <- tidySEM::hide_var(sem_plotfin)

gg_semfin = plot(sem_plotfin) + ggtitle('F1 and F1 regressed on engagement + concern + covariates')

ggsave(filename = 'output/figures/SEM_final_diagram_11-4.png',plot = gg_sem,dpi = 300,width = 7,height = 5,units = 'in')


lyfin2 = get_layout(sem_fitfin2, 
                   layout_algorithm = 'layout_with_kk')
sem_plotfin2 = tidySEM::prepare_graph(sem_fitfin2,layout = lyfin2)
sem_plotfin2 <- tidySEM::hide_var(sem_plotfin2)

gg_semfin2 = plot(sem_plotfin2) + ggtitle('F1 and F1 regressed on engagement + concern + covariates')

ggsave(filename = 'output/figures/SEM_final_diagram_11-2.png',plot = gg_sem,dpi = 300,width = 7,height = 5,units = 'in')


#Regular OLS
ols1 <- lm(F1~Q1_Focus+Q32_InfoTypes+When_SLR+aware+concern+nonprofit+govagency+Q4_RegGov+Q4_LocalGov, data=facts)
ols2 <- lm(F2~Q1_Focus+Q32_InfoTypes+When_SLR+aware+concern+nonprofit+govagency+Q4_RegGov+Q4_LocalGov, data=facts)
summary(ols1)
summary(ols2)

library(stargazer)
stargazer(ols1, ols2)



#SUR
#install.packages("systemfit")
#initial regression
library(systemfit)
r1 <- F1~Q1_Focus+Q32_InfoTypes+When_SLR+aware+concern+nonprofit+govagency+Q4_RegGov+Q4_LocalGov
r2 <- F2~Q1_Focus+Q32_InfoTypes+When_SLR+aware+concern+nonprofit+govagency+Q4_RegGov+Q4_LocalGov
fitsur <-systemfit(list(f1reg=r1, f2reg=r2), data=facts)
summary(fitsur)

#regression without awareness
r1.2 <- F1~Q1_Focus+Q32_InfoTypes+When_SLR+concern+nonprofit+govagency+Q4_RegGov+Q4_LocalGov
r2.2 <- F2~Q1_Focus+Q32_InfoTypes+When_SLR+concern+nonprofit+govagency+Q4_RegGov+Q4_LocalGov
fitsur2 <-systemfit(list(f1reg=r1.2, f2reg=r2.2), data=facts)
summary(fitsur2)

#regression with awareness and info types interaction
r1.3 <- F1~Q1_Focus+Q32_InfoTypes*aware+When_SLR+concern+nonprofit+govagency+Q4_RegGov+Q4_LocalGov
r2.3 <- F2~Q1_Focus+Q32_InfoTypes*aware+When_SLR+concern+nonprofit+govagency+Q4_RegGov+Q4_LocalGov
fitsur3 <-systemfit(list(f1reg=r1.3, f2reg=r2.3), data=facts)
summary(fitsur3)

#regression with awareness and concern interaction
r1.4 <- F1~Q1_Focus+Q32_InfoTypes+aware*concern+When_SLR+nonprofit+govagency+Q4_RegGov+Q4_LocalGov
r2.4 <- F2~Q1_Focus+Q32_InfoTypes+aware*concern+When_SLR+nonprofit+govagency+Q4_RegGov+Q4_LocalGov
fitsur4 <- systemfit(list(f1reg=r1.4, f2reg=r2.4), data=facts)
summary(fitsur4)

#install.packages("texreg")
library(texreg)
texreg(list(fitsur))
texreg(list(fitsur2))
texreg(list(fitsur3))
texreg(list(fitsur4))

htmlreg(list(fitsur), file='output/tables/table_SURresults.html')

#Correlations with new variables
cor(facts$F1, facts$govagency)

cor.test(facts$F1, facts$govagency)
cor.test(facts$F1, facts$aware)
cor.test(facts$F1, facts$concern)
cor.test(facts$F1, facts$nonprofit)

cor.test(facts$F2, facts$govagency)
cor.test(facts$F2, facts$aware)
cor.test(facts$F2, facts$concern)
cor.test(facts$F2, facts$nonprofit)

corrsdata2 <- subset(facts,select=c("F1", "F2", "Q1_Focus", "When_SLR", "Q16_Risk_Agree", "Q17_Action_Agree", "govagency", "Q4_RegGov", "Q4_LocalGov", "Q4_WaterSD", "Q4_EnviroSD", "Q4_Enviro", "Q4_Trade", "Q4_Ed", "Q4_Multistake", "Q4_Multijuris", "Q4_Political", "nonprofit", "Q8_JobTasks", "Q32_InfoTypes", "aware", "concern"))


setnames(corrsdata2, old = c("F1", "F2", "Q1_Focus", "When_SLR", "Q16_Risk_Agree", "Q17_Action_Agree", "govagency", "Q4_RegGov", "Q4_LocalGov", "Q4_WaterSD", "Q4_EnviroSD", "Q4_Enviro", "Q4_Trade", "Q4_Ed", "Q4_Multistake", "Q4_Multijuris", "Q4_Political", "nonprofit", "Q8_JobTasks", "Q32_InfoTypes", "aware", "concern"), new = c("F1", "F2", "Focus", "When_SLR", "Risk_Agree", "Action_Agree", "Fed State Gov", "RegGov", "LocalGov", "WaterSD", "EnviroSD", "Enviro", "Trade", "Ed", "Multistake", "Multijuris", "Political", "Nonprofit", "Tasks", "InfoTypes", "Awareness", "Concern"))

corrs2 <- cor(corrsdata2)
corrplot(corrs2, method='color')
corrplot(corrs2, method='circle')

####Create Basic Descriptive Figures for Paper--------------------
#concerns
allconcerns <-incidence_dt[,c(17:29)]

allconcerns %>%
  summarise_all(sum)

concernsums <- c(434, 137, 393, 41, 227, 128, 24, 315, 51, 36, 88, 81, 21)
concernnames <- c("Transpo", "Water", "Stormwater Wastewater", "Energy", "Ecosystem", "Erosion", "Commercial", "DACs", "Econ Growth", "Property Value", "Housing", "Public Health", "Other")  
concernmatrix <- data.frame(name=concernnames, value=concernsums)

ggsave(filename='output/figures/figure_concerns_bar_plot.png', width = 4.5,height = 4.5, dpi = 350)
ggplot(concernmatrix, aes(x=name, y=value))+
  geom_bar(stat="identity")+
  coord_flip()+
  geom_text(aes(label = value), vjust = 0.4, hjust=1, colour="white")+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank())+
  ggtitle("Concerns for SLR Impacts")+
  theme(plot.title = element_text(hjust = 0.5))
dev.off()

#policies
allpolicies <-incidence_dt[,c(2:16)]

allpolicies %>%
  summarise_all(sum)

policysums <- c(261, 167, 75, 141, 162, 120, 121, 200, 82, 100, 42, 89, 149, 243, 34)
policynames <- c("SLR Plan", "Vulnerability Assessment", "Local Tax", "Fund Lobbying", "Streamline Permits", "Info Platform", "DACs Focus", "Green Infrastructure", "Visioning", "Innovative Design", "Local Response", "Regional Authority", "Existing Agency", "Collaboration", "Other")  
policymatrix <- data.frame(value=policysums, name=policynames)

ggsave(filename='output/figures/figure_policies_bar_plot.png', width = 4.5,height = 4.5, dpi = 350)
ggplot(policymatrix, aes(x=name, y=value))+
  geom_bar(stat="identity")+
  coord_flip()+
  geom_text(aes(label = value), vjust = 0.4, hjust=1, colour="white")+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank())+
  ggtitle("Policy Preferences")+
  theme(plot.title = element_text(hjust = 0.5))
dev.off()

########Kyra Edits to Figures for Paper Update 12-29-21-----------
#Figure 1 Concerns Bar Plot
#Fixes- Remove Other, Reorder by Frequency
concernsums_v2 <- c(434, 393, 315, 227, 137, 128, 88, 81, 51, 41, 36, 24)
concernnames_v2 <- c("Transpo", "Stormwater Wastewater", "DACs", "Ecosystem", "Water", "Erosion", "Housing", "Public Health", "Econ Growth", "Energy", "Property Value", "Commercial")  
concernmatrix_v2 <- data.frame(name=concernnames_v2, value=concernsums_v2)

ggsave(filename='output/figures/figure_concerns_bar_plot_final.png', width = 4.5,height = 4.5, dpi = 350)
ggplot(concernmatrix_v2, aes(x=reorder(name, value), y=value))+
  geom_bar(stat="identity")+
  coord_flip()+
  geom_text(aes(label = value), vjust = 0.4, hjust=1, colour="white")+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank())+
  ggtitle("Policy Problems- Concerns for SLR Impacts")+
  theme(plot.title = element_text(hjust = 0.5))


#Figure 1 Policies Bar Plot
#Fixes- Remove Other, Reorder by Frequency
policysums_v2 <- c(261, 167, 75, 141, 162, 120, 121, 200, 82, 100, 42, 89, 149, 243)
policynames_v2 <- c("SLR Plan", "Vulnerability Assessment", "Local Tax", "Fund Lobbying", "Streamline Permits", "Info Platform", "DACs Focus", "Green Infrastructure", "Visioning", "Innovative Design", "Local Response", "Regional Authority", "Existing Agency", "Collaboration")  
policymatrix_v2 <- data.frame(value=policysums_v2, name=policynames_v2)

ggsave(filename='output/figures/figure_policies_bar_plot_final.png', width = 4.5,height = 4.5, dpi = 350)
ggplot(policymatrix_v2, aes(x=reorder(name, value), y=value))+
  geom_bar(stat="identity")+
  coord_flip()+
  geom_text(aes(label = value), vjust = 0.4, hjust=1, colour="white")+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank())+
  ggtitle("Preferred Policy Solutions")+
  theme(plot.title = element_text(hjust = 0.5))

#Figure 4 CFA Plot
#Fixes- Retitle and Change Labels

facts$ShortTermAware <- facts$Q11_STAware
facts$LongTermAware <- facts$Q11_LTAware
facts$CollabActions <- facts$Q19_Sum
facts$ShortTermConcern <- facts$Q12_STConcern
facts$LongTermConcern <- facts$Q12_LTConcern
facts$InfoTypes <- facts$Q32_InfoTypes
facts$Focus <- facts$Q1_Focus

cfa_form <- '
Engagement  =~  InfoTypes + Focus + ShortTermAware + LongTermAware + CollabActions\
Concern =~ ShortTermConcern + LongTermConcern + When_SLR
'
#cfa_fit = cfa(cfa_form,data = facts,ordered = c('Focus','When_SLR','ShortTermAware','LongTermAware','LongTermConcern','ShortTermConcern'))
cfa_fit = cfa(cfa_form,data = facts)


#### THIS IS A GRAPH OF THE CFA FOR ENGAGEMENT

start= get_layout('InfoTypes','Focus','ShortTermAware','LongTermAware','CollabActions',NA,
                  NA,NA,'Engagement','Concern',NA,NA,
                  NA,NA,'ShortTermConcern','LongTermConcern','When_SLR',NA,rows = 3)

cfa_graph = prepare_graph(cfa_fit,
                          layout =start)
cfa_graph = hide_var(cfa_graph)

(gg_cfa = plot(cfa_graph) + 
    ggtitle('Confirmatory factor analysis of SLR involvement constructs') +
    coord_flip())
ggsave(plot = gg_cfa,filename = 'output/figures/cfa_plot_final.png',width = 6,height = 4.5,dpi = 300, units = 'in')


#Figure 5 SEM Plot
#Fixes- Add p value key, change * note symbol for org type

sem_formfin <- '
#2 latent variables for engagement
Engagement  =~  InfoTypes + Focus + ShortTermAware + LongTermAware + CollabActions

Concern =~ ShortTermConcern + LongTermConcern + When_SLR

#regression with all actor types (local gov baseline)
F1 ~ Engagement + Concern + Q4_CBO + Q4_NGO + Q4_Fed + Q4_State + Q4_RegGov + Q4_EnviroSD + Q4_WaterSD + Q4_Ed + Q4_Multijuris + Q4_Multistake + Q4_Political + Q4_Trade + Q4_Enviro + OtherActorType
F2 ~ Engagement + Concern + Q4_CBO + Q4_NGO + Q4_Fed + Q4_State + Q4_RegGov + Q4_EnviroSD + Q4_WaterSD + Q4_Ed + Q4_Multijuris + Q4_Multistake + Q4_Political + Q4_Trade + Q4_Enviro + OtherActorType
#resid corrs
F1~~F2
'
sem_fitfin = sem(sem_formfin,data =  facts)

##### there are basically 4 layers, each 9 wide
start= get_layout(
  # row 1
  'Q4_CBO','Q4_NGO','Q4_State','Q4_RegGov','Q4_EnviroSD',
  'Q4_WaterSD','Q4_Ed','Q4_Trade','OtherActorType',
  # row 2
  NA,NA,NA,'F1',NA,NA,'F2',NA,NA,
  # row 3
  NA,'Q4_Enviro','Q4_Multijuris',NA,'Engagement','Concern',NA,'Q4_Multistake','Q4_Political',
  # row 4
  NA,'InfoTypes','Focus','ShortTermAware','LongTermAware',
  'CollabActions','ShortTermConcern','LongTermConcern','When_SLR',
  rows = 4)

sem_graph = prepare_graph(sem_fitfin,
                          layout =start)
sem_graph = hide_var(sem_graph)

(gg_sem = plot(sem_graph) + 
    ggtitle('SEM: indicators of policy engagement') + coord_flip())



start= get_layout(
  # row 2
  NA,'F1',NA,'Q4_CBO',NA,'F2',NA,
  # row 3
  'InfoTypes',NA,'Engagement',NA,'Concern',NA,'When_SLR',
  # row 4
  'Focus','ShortTermAware','LongTermAware',
  'CollabActions',NA,'ShortTermConcern','LongTermConcern',
  rows = 3)

sem_graph = prepare_graph(sem_fitfin,
                          layout =start)
sem_graph = hide_var(sem_graph)

sem_graph$edges[sem_graph$edges$from=='Q4_CBO'&sem_graph$edges$to=='F2',]$label <- '---'
sem_graph$edges[sem_graph$edges$from=='Q4_CBO'&sem_graph$edges$to=='F1',]$label <- '---'

sem_graph$nodes$label[sem_graph$nodes$label=='Q4_CBO']<-''

(gg_sem = plot(sem_graph) + 
    #ggplot() + 
    geom_rect(data = sem_graph$nodes[sem_graph$nodes$label=='',],fill = 'white',
              linetype = 3,colour = 'black',
              aes(xmin = node_xmin-0.1,xmax = node_xmax+.1,ymax = node_ymax+0.1,ymin = node_ymin-0.1))+
    geom_text(data = sem_graph$nodes[sem_graph$nodes$label=='',],
              aes(label = 'Org. type^',x = x,y = y)) + coord_flip() +
    annotate('text',x = 2.25,y =6,label = '^ represents 13 org.\ntype indicators',col = 'black') +
    annotate('text', x=1.25, y=6, label='p-values *<0.05, **<0.01, ***<0.001', col='black')+
    theme(text = element_text(family = 'Times',size = 12)) +
    ggtitle('SEM: Engagement and org. type as predictors of factor location')
) 

ggsave(plot = gg_sem,filename = 'output/figures/sem_plot_stylizedOrgType_edited.png',width = 7,height = 5,dpi = 600, units = 'in')

#Attempt Figure 6- Average Location by Org Type

#Average F1 and F2 for Fed
facts %>%
  group_by(Q4_Fed) %>%
  dplyr::summarize(Mean = mean(F1, na.rm=TRUE))

facts %>%
  group_by(Q4_Fed) %>%
  dplyr::summarize(Mean = mean(F2, na.rm=TRUE))

#Average F1 and F2 for Local Gov
facts %>%
  group_by(Q4_LocalGov) %>%
  dplyr::summarize(Mean = mean(F1, na.rm=TRUE))

facts %>%
  group_by(Q4_LocalGov) %>%
  dplyr::summarize(Mean = mean(F2, na.rm=TRUE))

#Average F1 and F2 for Enviro SD
facts %>%
  group_by(Q4_EnviroSD) %>%
  dplyr::summarize(Mean = mean(F1, na.rm=TRUE))

facts %>%
  group_by(Q4_EnviroSD) %>%
  dplyr::summarize(Mean = mean(F2, na.rm=TRUE))

#Average F1 and F2 for Nonprofit
facts %>%
  group_by(nonprofit) %>%
  dplyr::summarize(Mean = mean(F1, na.rm=TRUE))

facts %>%
  group_by(nonprofit) %>%
  dplyr::summarize(Mean = mean(F2, na.rm=TRUE))

SigOrgType <- c("Local Gov", "Nonprofit", "Fed Gov", "Enviro SD")
AvgF1_OrgType <- c(0.0548, -0.479, -0.0613, 0.242)
AvgF2_OrgType <- c(0.0804, -0.209, -0.516, -0.476)

AvgFactors_OrgType <- cbind(SigOrgType, AvgF1_OrgType, AvgF2_OrgType)
AvgFactors_OrgType <- as.data.frame(AvgFactors_OrgType)


#Working on Getting Labels for Org Type but for some reason won't work
(AvgFact_OrgTypeFig<-ggplot()+ 
  geom_point(data = rotated.factors,col = 'grey50',aes(y =F2,x = F1,shape = type)) + 
    geom_text_repel(col = 'grey50',data = rotated.factors,aes(y =F2,x = F1,label = item),max.overlaps = 30) + 
  geom_point(data = AvgFactors_OrgType, shape = 15,aes(x=as.numeric(AvgF1_OrgType), y=as.numeric(AvgF2_OrgType)))+
  ggrepel::geom_label_repel(data=AvgFactors_OrgType, aes(label=SigOrgType,x=as.numeric(AvgF1_OrgType), y=as.numeric(AvgF2_OrgType)),min.segment.length = 0.2)+
  xlab('Factor 1 score') + ylab("Factor 2 score") + 
    theme(legend.position = c(0.2,0.2))+
    scale_shape_discrete(name = 'item type',labels = c('problem','solution'),solid = F)+
  ggtitle('Average ideal points for survey respondents by organization type','overlaid on item factor scores'))

ggsave(plot = AvgFact_OrgTypeFig,filename = 'output/figures/avgFscores_orgtype.png',width = 7,height = 7,dpi = 600, units = 'in')

library(data.table)
G = 1:5
opts = data.table(expand.grid(D = 2,G = G))

### this part takes a while, so I commented out and upload an RDS at the end ###
require(doParallel)
cluster = makeCluster(3)
registerDoParallel(cluster)
clusterExport(cl = cluster,varlist = list('opts','Y'))
clusterEvalQ(cl = cluster,require(lvm4net))
mlta_tests = foreach(i = 1:nrow(opts)) %dopar% {mlta(X = Y, D = opts$D[i], G = opts$G[i],nstarts = 5,maxiter = 1e3)}

mlta_results = data.table(opts,BIC = sapply(mlta_tests,function(x) x$BIC))
mlta_results$G_fix = paste0('G = ',mlta_results$G,mlta_results$fix)
mlta_results$BIC <- round(mlta_results$BIC)
mlta_cast = dcast(mlta_results[order(BIC)],G_fix ~ D,value.var = 'BIC')
names(mlta_cast)<-c('# latent groups','BIC score')
htmlTable(mlta_cast)





