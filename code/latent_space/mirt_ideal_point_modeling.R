#set seed for replicable results
seed = 24

### load packages (not all of htese are needed at this point, so shoudl be cleaned up a bit)
packs =c('tidyverse','purrr','data.table','statnet','latentnet','bipartite','lvm4net','mirt','pbapply','Hmisc','htmlTable',
         'ggthemes','here','ggnetwork','gridExtra','ggrepel','corrplot','htmlTable','readxl','nFactors','ggrepel','plotly','ggalluvial')
need = packs[!packs %in% names(installed.packages()[,2])]
invisible(sapply(need,function(x) suppressMessages(install.packages(x,type= 'source'))))
invisible(sapply(packs,function(x) suppressMessages(library(x,character.only = T))))

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
predictor_dt = data.table(id = rownames(Y))
predictor_dt$Q1_Focus <- orig$Q1_Num[match(predictor_dt$id,orig$ResponseId)]
predictor_dt$When_SLR <- orig$WhenSLR[match(predictor_dt$id,orig$ResponseId)]
predictor_dt$Q16_Risk_Agree <- orig$RiskAgree[match(predictor_dt$id,orig$ResponseId)]
predictor_dt$Q17_Action_Agree <- orig$ActionAgree[match(predictor_dt$id,orig$ResponseId)]

predictor_dt$Q1_Focus[is.na(predictor_dt$Q1_Focus)]<-round(mean(predictor_dt$Q1_Focus,na.rm = T))
predictor_dt$When_SLR[is.na(predictor_dt$When_SLR)]<-round(mean(predictor_dt$When_SLR,na.rm = T))
predictor_dt$Q16_Risk_Agree[is.na(predictor_dt$Q16_Risk_Agree)]<-round(mean(predictor_dt$Q16_Risk_Agree,na.rm = T))
predictor_dt$Q17_Action_Agree[is.na(predictor_dt$Q17_Action_Agree)]<-round(mean(predictor_dt$Q17_Action_Agree,na.rm = T))

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
round(rbind(cor(facts[,.(F1,Q1_Focus,When_SLR,Q16_Risk_Agree,Q17_Action_Agree)])[1,-1],
cor(facts[,.(F2,Q1_Focus,When_SLR,Q16_Risk_Agree,Q17_Action_Agree)],)[1,-1]),3)
)


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






