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
library(glmnet) #for glmnet function with lasso and ridge regularization

#IMPORT DATA
OscarData <- read.xlsx("Oscar Winner Data.xlsx", sheetName = "BestPicture")
SpDesc(OscarData)


#Planning multiple approaches
#1 logistic regression
#2 MLM logistic model with movies nested in years (other wins are by definition anticorrelated. )
#3 random forest classification
#4 Bayesian analysis

#Approach 1 - Logistic Regression----

ProbNorm <- function(rawprobs){
  normprobs <- rawprobs/sum(rawprobs)
  return(normprobs)
}
#test function--works fine
ProbNorm(c(.1,.2,.4))

#creating binary variables of nomination, and win. 
OscarData <- OscarData %>% mutate(
  Globes.Drama.N = ifelse(Globes.Drama==1,1,0),
  Globes.Drama.W = ifelse(Globes.Drama==2,1,0),
  Globes.Comedy.N = ifelse(Globes.Comedy==1,1,0),
  Globes.Comedy.W = ifelse(Globes.Comedy==2,1,0),
  
  CCA.N = ifelse(CCA==1,1,0),
  CCA.W = ifelse(CCA==2,1,0),
  
  SAG.N = ifelse(SAG==1,1,0),
  SAG.W = ifelse(SAG==2,1,0),
  
  BAFTA.N = ifelse(BAFTA==1,1,0),
  BAFTA.W = ifelse(BAFTA==2,1,0),
  
  PGA.N = ifelse(PGA==1,1,0),
  PGA.W = ifelse(PGA==2,1,0),
  
  DGA.N = ifelse(DGA==1,1,0),
  DGA.W = ifelse(DGA==2,1,0),
  
  WGA.Original.N = ifelse(WGA.Original==1,1,0),
  WGA.Original.W = ifelse(WGA.Original==2,1,0),
  
  WGA.Adapted.N = ifelse(WGA.Adapted==1,1,0),
  WGA.Adapted.W = ifelse(WGA.Adapted==2,1,0)
)

SpDesc(OscarData)
table(OscarData$Globes.Drama.N, OscarData$Globes.Drama.W)

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
L1outPreds <- data.frame(Year=NA,Name=NA, BPWin=NA, Prob=NA)

for(yr in 1997:2016){
  #split data based on leave-one(year)-out
  TrainData <- na.exclude(subset(OscarData, Year!=yr))
  TestData <- na.exclude(subset(OscarData, Year==yr))
  
  #run model with lasso regularization
  Model <- glmnet(x=as.matrix(TrainData[c("Globes.Drama.N", "Globes.Drama.W", "Globes.Comedy.N", "Globes.Comedy.W",
                     "CCA.N", "CCA.W", "SAG.N", "SAG.W", "BAFTA.N", "BAFTA.W", "PGA.N", "PGA.W", "DGA.N", "DGA.W")]),
                  y=as.factor(TrainData$BPWin), family="binomial", alpha=0)
  Model
  coef(Model)[,Model$dim[2]]
  
  predictions <- predict(Model, newx=as.matrix(TestData[c("Globes.Drama.N", "Globes.Drama.W", "Globes.Comedy.N", "Globes.Comedy.W",
                                  "CCA.N", "CCA.W", "SAG.N", "SAG.W", "BAFTA.N", "BAFTA.W", "PGA.N", "PGA.W", "DGA.N", "DGA.W")]))
  logits <- predictions[,dim(predictions)[2]]
  
  #convert to probabilities
  odds <- exp(logits)
  probs <- odds/(1+odds)
  probsnorm <- ProbNorm(probs)
  
  #return data from year left out
  L1outPreds <- rbind(L1outPreds,data.frame(Year=yr,Name=TestData$Name, BPWin=TestData$BPWin, Prob=probsnorm))
}

L1outPreds <- na.exclude(L1outPreds)

#examining predictions
#seems to be okay
for(yr in 1997:2016){
  print(paste("Year: ", yr))
  print(subset(L1outPreds, Year==yr))
  print("")
}

#What percentage would have been called correctly
calls <- c()
callsper <- c()
for(yr in 1997:2016){
  #was the movie called that year? 
  callsnew <-  ifelse(subset(L1outPreds, Year==yr)$Prob==max(subset(L1outPreds, Year==yr)$Prob),1,0)
  calls <-c(calls, callsnew)

}

L1outPreds$calls <- calls
table(L1outPreds$BPWin, L1outPreds$calls)

SpHist(subset(L1outPreds, BPWin==1)$Prob)







