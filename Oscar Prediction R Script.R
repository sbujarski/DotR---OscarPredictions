#Data on the Rocks
#Planned for Monday March 26th 2018
#Oscar Prediction

#REQUIRED PACKAGES
library(SpPack)
library(xlsx) #package to import xls files directly
library(plyr) #data wrangling
library(dplyr) #data wrangling 2
library(tidyr) #data wrangling 3
library(lubridate) #data manipulation with date
library(Deducer) #for rocplot 
library(pscl) # for mcfadden R2 in logistic regression
library(caret) #for crossvalidation methods
library(ROCR) #For crossvalidation AUC curve
library(scales) #for percent axis

#IMPORT DATA
OscarData <- read.xlsx("Oscar Winner Data.xlsx", sheetName = "BestPicture")
SpDesc(OscarData)


#Planning multiple approaches
#1 logistic regression
#2 random forest classification
#3 Bayesian analysis

#Approach 1 - Logistic Regression----

#Function to normalize probabilities to a year of nominations
NormtoYear <- function(rawprobs){
  normprobs <- rawprobs/sum(rawprobs)
  return(normprobs)
}
#test function--works fine
NormtoYear(c(.1,.2,.4))

#creating binary variables of nomination, and win. 
OscarData <- OscarData %>% mutate(
  Globes.Drama.N = ifelse(Globes.Drama==0,0,1),
  Globes.Drama.W = ifelse(Globes.Drama==2,1,0),
  Globes.Comedy.N = ifelse(Globes.Comedy==0,0,1),
  Globes.Comedy.W = ifelse(Globes.Comedy==2,1,0),
  
  CCA.N = ifelse(CCA==0,0,1),
  CCA.W = ifelse(CCA==2,1,0),
  
  SAG.N = ifelse(SAG==0,0,1),
  SAG.W = ifelse(SAG==2,1,0),
  
  BAFTA.N = ifelse(BAFTA==0,0,1),
  BAFTA.W = ifelse(BAFTA==2,1,0),
  
  PGA.N = ifelse(PGA==0,0,1),
  PGA.W = ifelse(PGA==2,1,0),
  
  DGA.N = ifelse(DGA==0,0,1),
  DGA.W = ifelse(DGA==2,1,0),
  
  WGA.Original.N = ifelse(WGA.Original==0,0,1),
  WGA.Original.W = ifelse(WGA.Original==2,1,0),
  
  WGA.Adapted.N = ifelse(WGA.Adapted==0,0,1),
  WGA.Adapted.W = ifelse(WGA.Adapted==2,1,0)
)

SpDesc(OscarData)


#Test logistic model with just other awards noms and wins
#No cross-validation (for now)
TestModel <- glm(BPWin ~ Globes.Drama.N + Globes.Drama.W + Globes.Comedy.N + Globes.Comedy.W +
                   CCA.N + CCA.W + SAG.N + SAG.W + BAFTA.N + BAFTA.W + PGA.N + PGA.W + DGA.N + DGA.W +
                   WGA.Original.N + WGA.Original.W + WGA.Adapted.N + WGA.Adapted.W,
                 data=OscarData, family=binomial())
summary(TestModel)
anova(TestModel, test="Chisq")
pR2(TestModel)
rocplot(TestModel)


#leave one year out crossvalidation
for(yr in 1997:2017)



