setwd("C:/Users/kyras/OneDrive/Desktop/SLRSurvey/SLRSurvey/Current Files/Data")
library(tidyverse)
library(MASS)

#load data
survey <- read_csv("Survey_Regressions.csv")

#OLS regression on level of involvement------------------

unique(survey$Q1_Num)

#lm(formula= survey_LOIcomplete$LOI_Numeric~VARIABLES, data=survey_LOIcomplete)

ols_LOI <- lm(formula=survey$Q1_Num~survey$Q2_Personal+survey$Q2_MultiOrg+survey$Q5_F+survey$Q32_Sum+survey$Q11_LTAware+survey$Q11_STAware+survey$Q12_LTConcern+survey$Q12_STConcern+survey$WhenSLR+survey$ActionAgree+survey$RiskAgree+survey$Q20_Sum, data=survey)

sumols_LOI <- summary(ols_LOI)

sumols_LOI

#Ordinal Logistic Regression -------------------
survey$Q1_fact <- as.factor(survey$Q1_Num)
polr_LOI <- polr(formula=survey$Q1_fact~survey$Q2_Personal+survey$Q2_MultiOrg+survey$Q5_F+survey$Q32_Sum+survey$Q11_LTAware+survey$Q11_STAware+survey$Q12_LTConcern+survey$Q12_STConcern+survey$WhenSLR+survey$ActionAgree+survey$RiskAgree+survey$Q20_Sum, Hess=TRUE, data=survey)
sumpolr_LOI <- summary(polr_LOI)
sumpolr_LOI

