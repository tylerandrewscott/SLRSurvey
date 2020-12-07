setwd("C:/Users/kyras/OneDrive/Desktop/SLRSurvey/SLRSurvey/CurrentFiles/Data/Regressions")
library(tidyverse)
library(MASS)
#install.packages("AER")
library(AER)
#install.packages("pscl")
library(pscl)
#install.packages("VGAM")
library(VGAM)
library(dplyr)
library(nlme)

#load data
survey <- read_csv("Survey_Regressions.csv")

#START OF LEVEL OF INVOLVEMENT REGRESSIONS------------------

#OLS regression on LOI------------------

#lm(formula= survey_LOIcomplete$LOI_Numeric~VARIABLES, data=survey_LOIcomplete)

ols_LOI <- lm(formula=survey$Q1_Num~survey$Q2_Personal+survey$Q2_MultiOrg+survey$Q5_F+survey$Q32_Sum+survey$Q11_LTAware+survey$Q11_STAware+survey$Q12_LTConcern+survey$Q12_STConcern+survey$WhenSLR+survey$ActionAgree+survey$RiskAgree+survey$Q20_Sum, data=survey)

sumols_LOI <- summary(ols_LOI)

sumols_LOI

#Ordinal Logistic Regression on LOI-------------------
survey$Q1_numcheck <- survey$Q1_Num
survey$Q1_fact <- as.factor(survey$Q1_Num)
levels(survey$Q1_fact)

polr_LOI <- polr(formula=survey$Q1_fact~survey$Q2_Personal+survey$Q2_MultiOrg+survey$Q5_F+survey$Q32_Sum+survey$Q11_LTAware+survey$Q11_STAware+survey$Q12_LTConcern+survey$Q12_STConcern+survey$WhenSLR+survey$ActionAgree+survey$RiskAgree+survey$Q20_Sum, Hess=TRUE, data=survey, method="logistic")
sumpolr_LOI <- summary(polr_LOI)
sumpolr_LOI
coeftest(polr_LOI)
pR2(polr_LOI)

#Generalized Ordinal Logistic Regression on LOI --------------
#relaxes the proportional odds assumption 
gologit_LOI <- vglm(formula=survey$Q1_Num ~ survey$Q2_Personal+survey$Q2_MultiOrg+survey$Q32_Sum+survey$Q11_LTAware+survey$Q11_STAware+survey$Q12_LTConcern+survey$Q12_STConcern+survey$WhenSLR+survey$ActionAgree+survey$RiskAgree+survey$Q20_Sum, data=survey, Hess=TRUE, parallel=FALSE, family=cumulative)
warnings()
sumgolog_LOI <- summary(gologit_LOI)
sumgolog_LOI

#Generalized Least Squares Regression on LOI----------
GLS_LOI <- gls(Q1_Num~ Q2_Personal+ Q2_MultiOrg+ Q5_F+ Q32_Sum+ Q11_LTAware+ Q11_STAware+ Q12_LTConcern+ Q12_STConcern+ WhenSLR+ ActionAgree+ RiskAgree+ Q20_Sum, data=survey)
sumgls_LOI <- summary(GLS_LOI)
sumgls_LOI

#Ordered Probit on LOI---------------

ordprobit_LOI <- polr(formula=survey$Q1_fact~survey$Q2_Personal+survey$Q2_MultiOrg+survey$Q5_F+survey$Q32_Sum+survey$Q11_LTAware+survey$Q11_STAware+survey$Q12_LTConcern+survey$Q12_STConcern+survey$WhenSLR+survey$ActionAgree+survey$RiskAgree+survey$Q20_Sum, Hess=TRUE, method="probit", data=survey)
sumordprob_LOI <- summary(ordprobit_LOI)
sumordprob_LOI
coeftest(ordprobit_LOI)
pR2(ordprobit_LOI)

#START OF COLLABORATIVE ACTIVITIES REGRESSIONS------------

#Create simple binary variable for collaborative activities engaged in last year
survey$collab_binary <- ifelse(survey$Q19_SUM>0, 1, 0)

#OLS Regression on Number of Collaborative Activities Engaged In-------------

ols_collab <- lm(formula=survey$Q19_SUM~survey$Q2_Personal+survey$Q2_MultiOrg+survey$Q5_F+survey$Q32_Sum+survey$Q11_LTAware+survey$Q11_STAware+survey$Q12_LTConcern+survey$Q12_STConcern+survey$WhenSLR+survey$ActionAgree+survey$RiskAgree+survey$Q20_Sum, data=survey)
sumols_collab <- summary(ols_collab)
sumols_collab

#GLS on Number of Collaborative Activities Engaged In----------------
GLS_collab <- gls(Q19_SUM~ Q2_Personal+ Q2_MultiOrg+ Q5_F+ Q32_Sum+ Q11_LTAware+ Q11_STAware+ Q12_LTConcern+ Q12_STConcern+ WhenSLR+ ActionAgree+ RiskAgree+ Q20_Sum, data=survey)
sumgls_collab <- summary(GLS_collab)
sumgls_collab

#Probit for Binary Collab Activities Variable-----------------

probit_collab <- glm(collab_binary~Q2_Personal+ Q2_MultiOrg+ Q5_F+ Q32_Sum+ Q11_LTAware+ Q11_STAware+ Q12_LTConcern+ Q12_STConcern+ WhenSLR+ ActionAgree+ RiskAgree+ Q20_Sum, family=binomial(link="probit"), data=survey)
sumprobit_collab <- summary(probit_collab)
sumprobit_collab
confint(probit_collab)

####Redo the above models with additional variables added---------------

#OLS regression on LOI------------------

#lm(formula= survey_LOIcomplete$LOI_Numeric~VARIABLES, data=survey_LOIcomplete)

ols_LOI <- lm(formula=survey$Q1_Num~survey$Q2_Personal+survey$Q2_MultiOrg+survey$Q5_F+survey$Q32_Sum+survey$Q11_LTAware+survey$Q11_STAware+survey$Q12_LTConcern+survey$Q12_STConcern+survey$WhenSLR+survey$ActionAgree+survey$RiskAgree+survey$Q20_Sum + Q4_Fed + Q4_State + Q4_RegGov + Q4_LocalGov + Q4_WaterSD + Q4_EnviroSD + Q4_Enviro + Q4_Trade+ Q8_Exec + Q8_Policy + Q8_Planning + Q8_Science + Q8_Gov + Q8_PM + Q8_Outreach, data=survey)

sumols_LOI <- summary(ols_LOI)

sumols_LOI

#Ordinal Logistic Regression on LOI-------------------
survey$Q1_numcheck <- survey$Q1_Num
survey$Q1_fact <- as.factor(survey$Q1_Num)
levels(survey$Q1_fact)

polr_LOI <- polr(data=survey, formula=Q1_fact~Q2_Personal+Q2_MultiOrg+ Q5_F+ Q32_Sum+ Q11_LTAware+ Q11_STAware+ Q12_LTConcern+ Q12_STConcern+ WhenSLR+ ActionAgree+ RiskAgree+ Q20_Sum  + Q4_Fed + Q4_State + Q4_RegGov + Q4_LocalGov + Q4_WaterSD + Q4_EnviroSD + Q4_Enviro + Q4_Trade+ Q8_Exec + Q8_Policy + Q8_Planning + Q8_Science + Q8_Gov + Q8_PM + Q8_Outreach, Hess=TRUE, method="logistic")
sumpolr_LOI <- summary(polr_LOI)
sumpolr_LOI
coeftest(polr_LOI)
pR2(polr_LOI)

#Generalized Ordinal Logistic Regression on LOI --------------
#relaxes the proportional odds assumption 
gologit_LOI <- vglm(data=survey, formula= Q1_Num ~  Q2_Personal+ Q2_MultiOrg+ Q32_Sum+ Q11_LTAware+ Q11_STAware+ Q12_LTConcern+ Q12_STConcern+ WhenSLR+ ActionAgree+ RiskAgree+ Q20_Sum + Q4_Fed + Q4_State + Q4_RegGov + Q4_LocalGov + Q4_WaterSD + Q4_EnviroSD + Q4_Enviro + Q4_Trade+ Q8_Exec + Q8_Policy + Q8_Planning + Q8_Science + Q8_Gov + Q8_PM + Q8_Outreach, Hess=TRUE, parallel=FALSE, family=cumulative)
warnings()
sumgolog_LOI <- summary(gologit_LOI)
sumgolog_LOI

#Generalized Least Squares Regression on LOI----------
GLS_LOI <- gls(Q1_Num~ Q2_Personal+ Q2_MultiOrg+ Q5_F+ Q32_Sum+ Q11_LTAware+ Q11_STAware+ Q12_LTConcern+ Q12_STConcern+ WhenSLR+ ActionAgree+ RiskAgree+ Q20_Sum + Q4_Fed + Q4_State + Q4_RegGov + Q4_LocalGov + Q4_WaterSD + Q4_EnviroSD + Q4_Enviro + Q4_Trade+ Q8_Exec + Q8_Policy + Q8_Planning + Q8_Science + Q8_Gov + Q8_PM + Q8_Outreach, data=survey)
sumgls_LOI <- summary(GLS_LOI)
sumgls_LOI

#Ordered Probit on LOI---------------

ordprobit_LOI <- polr(formula= Q1_fact~ Q2_Personal+ Q2_MultiOrg+ Q5_F+ Q32_Sum+ Q11_LTAware+ Q11_STAware+ Q12_LTConcern+ Q12_STConcern+ WhenSLR+ ActionAgree+ RiskAgree+ Q20_Sum + Q4_Fed + Q4_State + Q4_RegGov + Q4_LocalGov + Q4_WaterSD + Q4_EnviroSD + Q4_Enviro + Q4_Trade+ Q8_Exec + Q8_Policy + Q8_Planning + Q8_Science + Q8_Gov + Q8_PM + Q8_Outreach, Hess=TRUE, method="probit", data=survey)
sumordprob_LOI <- summary(ordprobit_LOI)
sumordprob_LOI
coeftest(ordprobit_LOI)
pR2(ordprobit_LOI)

#START OF COLLABORATIVE ACTIVITIES REGRESSIONS------------

#Create simple binary variable for collaborative activities engaged in last year
survey$collab_binary <- ifelse(survey$Q19_SUM>0, 1, 0)

#OLS Regression on Number of Collaborative Activities Engaged In-------------

ols_collab <- lm(formula= Q19_SUM~ Q2_Personal+ Q2_MultiOrg+ Q5_F+ Q32_Sum+ Q11_LTAware+ Q11_STAware+ Q12_LTConcern+ Q12_STConcern+ WhenSLR+ ActionAgree+ RiskAgree+ Q20_Sum +  + Q4_Fed + Q4_State + Q4_RegGov + Q4_LocalGov + Q4_WaterSD + Q4_EnviroSD + Q4_Enviro + Q4_Trade+ Q8_Exec + Q8_Policy + Q8_Planning + Q8_Science + Q8_Gov + Q8_PM + Q8_Outreach, data=survey)
sumols_collab <- summary(ols_collab)
sumols_collab

#GLS on Number of Collaborative Activities Engaged In----------------
GLS_collab <- gls(Q19_SUM~ Q2_Personal+ Q2_MultiOrg+ Q5_F+ Q32_Sum+ Q11_LTAware+ Q11_STAware+ Q12_LTConcern+ Q12_STConcern+ WhenSLR+ ActionAgree+ RiskAgree+ Q20_Sum + Q4_Fed + Q4_State + Q4_RegGov + Q4_LocalGov + Q4_WaterSD + Q4_EnviroSD + Q4_Enviro + Q4_Trade+ Q8_Exec + Q8_Policy + Q8_Planning + Q8_Science + Q8_Gov + Q8_PM + Q8_Outreach, data=survey)
sumgls_collab <- summary(GLS_collab)
sumgls_collab

#Probit for Binary Collab Activities Variable-----------------

probit_collab <- glm(collab_binary~Q2_Personal+ Q2_MultiOrg+ Q5_F+ Q32_Sum+ Q11_LTAware+ Q11_STAware+ Q12_LTConcern+ Q12_STConcern+ WhenSLR+ ActionAgree+ RiskAgree+ Q20_Sum + Q4_Fed + Q4_State + Q4_RegGov + Q4_LocalGov + Q4_WaterSD + Q4_EnviroSD + Q4_Enviro + Q4_Trade+ Q8_Exec + Q8_Policy + Q8_Planning + Q8_Science + Q8_Gov + Q8_PM + Q8_Outreach, family=binomial(link="probit"), data=survey)
sumprobit_collab <- summary(probit_collab)
sumprobit_collab
confint(probit_collab)




