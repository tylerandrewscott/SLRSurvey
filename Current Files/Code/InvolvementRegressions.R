library(tidyverse)
library(MASS)

#load data
survey <- read_csv("Data/SLRSurvey_Full.csv")

#Remove NAs from level of involvement ordinal data---------------------------------------
summary(survey$Q1_Num)
survey$LOI_Numeric <- as.numeric(survey$Q1_Num)

survey_LOIcomplete <- survey %>% 
  filter(!is.na(survey$LOI_Numeric))

#OLS regression on level of involvement------------------

#lm(formula= survey_LOIcomplete$LOI_Numeric~VARIABLES, data=survey_LOIcomplete)