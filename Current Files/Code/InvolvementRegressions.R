setwd("C:/Users/kyras/OneDrive/Desktop/SLRSurvey/SLRSurvey/Current Files/Data")
library(tidyverse)
library(MASS)

#load data
survey <- read_csv("Survey_Regressions.csv")

#OLS regression on level of involvement------------------

unique(survey$Q1_Num)

#lm(formula= survey_LOIcomplete$LOI_Numeric~VARIABLES, data=survey_LOIcomplete)

lm(formula=survey$Q1_Num~survey$Q2_Personal+survey$Q2_MultiOrg+survey$Q5_F, data=survey)
