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

#load data
survey <- read_csv("Survey_Regressions.csv")

#OLS regression on level of involvement------------------

#lm(formula= survey_LOIcomplete$LOI_Numeric~VARIABLES, data=survey_LOIcomplete)

ols_LOI <- lm(formula=survey$Q1_Num~survey$Q2_Personal+survey$Q2_MultiOrg+survey$Q5_F+survey$Q32_Sum+survey$Q11_LTAware+survey$Q11_STAware+survey$Q12_LTConcern+survey$Q12_STConcern+survey$WhenSLR+survey$ActionAgree+survey$RiskAgree+survey$Q20_Sum, data=survey)

sumols_LOI <- summary(ols_LOI)

sumols_LOI

#Ordinal Logistic Regression -------------------
survey$Q1_numcheck <- survey$Q1_Num
survey$Q1_fact <- as.factor(survey$Q1_Num2)
levels(survey$Q1_fact)

polr_LOI <- polr(formula=survey$Q1_fact~survey$Q2_Personal+survey$Q2_MultiOrg+survey$Q5_F+survey$Q32_Sum+survey$Q11_LTAware+survey$Q11_STAware+survey$Q12_LTConcern+survey$Q12_STConcern+survey$WhenSLR+survey$ActionAgree+survey$RiskAgree+survey$Q20_Sum, Hess=TRUE, data=survey)
sumpolr_LOI <- summary(polr_LOI)
sumpolr_LOI
coeftest(polr_LOI)
pR2(polr_LOI)

#Generalized Ordinal Logistic Regression --------------
#relaxes the proportional odds assumption 
survey = filter(survey, WhenSLR != "#NULL!")
survey = filter(survey, RiskAgree != "#NULL!")
survey = filter(survey, ActionAgree != "#NULL!")
survey = filter(survey, Q11_STAware != "#NULL!")
survey = filter(survey, Q11_LTAware != "#NULL!")
survey = filter(survey, Q12_STConcern != "#NULL!")
survey = filter(survey, Q12_LTConcern != "#NULL!")

survey$WhenSLR <- as.numeric(survey$WhenSLR)
survey$ActionAgree <- as.numeric(survey$ActionAgree)
survey$RiskAgree <- as.numeric(survey$RiskAgree)
survey$Q11_STAware <- as.numeric(survey$Q11_STAware)
survey$Q11_LTAware <- as.numeric(survey$Q11_LTAware)
survey$Q12_STConcern <- as.numeric(survey$Q12_STConcern)
survey$Q12_LTConcern <- as.numeric(survey$Q12_LTConcern)


gologit_LOI <- vglm(formula=survey$Q1_Num ~ survey$Q2_Personal+survey$Q2_MultiOrg+survey$Q5_F+survey$Q32_Sum+survey$Q11_LTAware+survey$Q11_STAware+survey$Q12_LTConcern+survey$Q12_STConcern+survey$WhenSLR+survey$ActionAgree+survey$RiskAgree+survey$Q20_Sum, data=survey, Hess=TRUE, parallel=FALSE, family=cumulative)

