#Data on the Rocks
#Planned for Monday March 26th 2018
#Oscar Prediction

#REQUIRED PACKAGES
library(SpPack)
library(xlsx) #package to import xls files directly
library(dplyr) #data wrangling 2
library(tidyr) #data wrangling 3
library(lubridate) #data manipulation with date
library(Deducer) #for rocplot 
library(pscl) # for mcfadden R2 in logistic regression
library(caret) #for crossvalidation methods
library(ROCR) #For crossvalidation AUC curve
library(scales) #for percent axis
library(glmnet) #for glmnet function with lasso and ridge regularization
library(lme4) # for glmer logistic multilevel modeling
library(randomForest)

#IMPORT DATA
OscarData <- read.xlsx("Oscar Winner Data.xlsx", sheetName = "BestPicture")
SpDesc(OscarData)


#Planning multiple approaches
#1 logistic regression
#2 MLM logistic model with movies nested in years (other wins are by definition anticorrelated)
#3 random forest classification
#4 Bayesian analysis

#General Functions----
#ProbNorm - Function to normalize probabilities to 100% for use within a year
ProbNorm <- function(rawprobs){
  normprobs <- rawprobs/sum(rawprobs)
  return(normprobs)
}
#test function--works fine
ProbNorm(c(.1,.2,.4))

#Data management----
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
table(OscarData$WGA.Original.N, OscarData$WGA.Original.W)

#Approach #1 - Logistic Modeling with regularization
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

#Now test with regularization via glmnet
#leave one year out crossvalidation
L1outPreds <- data.frame(Year=NA,Name=NA, BPWin=NA, Prob=NA)

for(yr in 1997:2016){
  #split data based on leave-one(year)-out
  TrainData <- na.exclude(subset(OscarData, Year!=yr))
  TestData <- na.exclude(subset(OscarData, Year==yr))
  
  #run model with lasso regularization
  Model <- glmnet(x=as.matrix(TrainData[c("Globes.Drama.N", "Globes.Drama.W", "Globes.Comedy.N", "Globes.Comedy.W",
                     "CCA.N", "CCA.W", "SAG.N", "SAG.W", "BAFTA.N", "BAFTA.W", "PGA.N", "PGA.W", "DGA.N", "DGA.W", 
                     "RT.All",	"RT.Top",	"AA.Actor",	"AA.ActorSup",	"AA.Actress",	"AA.ActressSup",	"AA.Director",
                     "AA.Adapt",	"AA.Original")]),
                  y=as.factor(TrainData$BPWin), family="binomial", alpha=0)
  Model
  coef(Model)[,Model$dim[2]]
  
  predictions <- predict(Model, newx=as.matrix(TestData[c("Globes.Drama.N", "Globes.Drama.W", "Globes.Comedy.N", "Globes.Comedy.W",
                                                           "CCA.N", "CCA.W", "SAG.N", "SAG.W", "BAFTA.N", "BAFTA.W", "PGA.N", "PGA.W", "DGA.N", "DGA.W", 
                                                           "RT.All",	"RT.Top",	"AA.Actor",	"AA.ActorSup",	"AA.Actress",	"AA.ActressSup",	"AA.Director",
                                                           "AA.Adapt",	"AA.Original")]))
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

#Accuracy testing via root mean squared error
L1outPreds <- left_join(L1outPreds, OscarData)
#calculating RMSE
sqrt(sum((L1outPreds$Prob - L1outPreds$BPWin)^2)/sum(!is.na(L1outPreds$BPWin)))
#0.2992295
#misses on average by 30%
#can I do better than that?

#What does this predict for 2018
TrainData <- subset(OscarData, Year!=2017)
TestData <- subset(OscarData, Year==2017)

Model <- glmnet(x=as.matrix(TrainData[c("Globes.Drama.N", "Globes.Drama.W", "Globes.Comedy.N", "Globes.Comedy.W",
                                        "CCA.N", "CCA.W", "SAG.N", "SAG.W", "BAFTA.N", "BAFTA.W", "PGA.N", "PGA.W", "DGA.N", "DGA.W", 
                                        "RT.All",	"RT.Top",	"AA.Actor",	"AA.ActorSup",	"AA.Actress",	"AA.ActressSup",	"AA.Director",
                                        "AA.Adapt",	"AA.Original")]),
                y=as.factor(TrainData$BPWin), family="binomial", alpha=0)
Model
coef(Model)[,Model$dim[2]]

#regularized logistic regression coefficient plots
RLR.Coef <- data.frame(coef=coef(Model)[,Model$dim[2]])
RLR.Coef$variable <- row.names(RLR.Coef)
#remove intercept
RLR.Coef <- subset(RLR.Coef, variable != "(Intercept)")
  
#use ggplot to plot coefficients
RLR.CoefPlot <- ggplot(data=RLR.Coef, aes(x = coef, y = reorder(variable, coef))) +
  geom_point(size=3) +
  scale_x_continuous("Logistic Regression Coefficient") +
  ggtitle("Regularized Logistic Regression Coefficients") +
  DotRTheme() +
  theme(axis.title.y = element_blank())
ggsave(RLR.CoefPlot, filename="RLR.CoefPlot.png", height=8, width = 8, dpi=300)


predictions <- predict(Model, newx=as.matrix(TestData[c("Globes.Drama.N", "Globes.Drama.W", "Globes.Comedy.N", "Globes.Comedy.W",
                                                        "CCA.N", "CCA.W", "SAG.N", "SAG.W", "BAFTA.N", "BAFTA.W", "PGA.N", "PGA.W", "DGA.N", "DGA.W", 
                                                        "RT.All",	"RT.Top",	"AA.Actor",	"AA.ActorSup",	"AA.Actress",	"AA.ActressSup",	"AA.Director",
                                                        "AA.Adapt",	"AA.Original")]))
logits <- predictions[,dim(predictions)[2]]

#convert to probabilities
odds <- exp(logits)
probs <- odds/(1+odds)
probsnorm <- ProbNorm(probs)

#return data from year left out
data.frame(Year=yr,Name=TestData$Name, BPWin=TestData$BPWin, Prob=probsnorm)
#   Year                                      Name BPWin        Prob
# 1 2016                      Call Me by Your Name    NA 0.003289117
# 2 2016                              Darkest Hour    NA 0.001800000
# 3 2016                                   Dunkirk    NA 0.010466688
# 4 2016                                   Get Out    NA 0.002938352
# 5 2016                                 Lady Bird    NA 0.035453765
# 6 2016                            Phantom Thread    NA 0.010451622
# 7 2016                                  The Post    NA 0.003493502
# 8 2016                        The Shape of Water    NA 0.790435670  (80% Heavy Favorite)
# 9 2016 Three Billboards Outside Ebbing, Missouri    NA 0.141671285


#Approach #2 ----
#Multilevel Logistic models to account for nesting of movies within years
#Models don't actually run because of intense multicolinearity
test.glmer <- glmer(BPWin ~ Globes.Drama.N + Globes.Drama.W + Globes.Comedy.N + Globes.Comedy.W +
                      CCA.N + CCA.W + SAG.N + SAG.W + BAFTA.N + BAFTA.W + PGA.N + PGA.W + DGA.N + DGA.W +
                      WGA.Original.N + WGA.Original.W + WGA.Adapted.N + WGA.Adapted.W +
                      RT.All + RT.Top + AA.Actor + AA.ActorSup + AA.Actress + AA.ActressSup + 
                      AA.Director + AA.Adapt + AA.Original + (1 | Year),
                      data=OscarData,
                      family=binomial,
                      control = glmerControl(optimizer = "bobyqa"))
summary(test.glmer)
#colinearity is a huge issue for this model
# Warning messages:
# 1: In vcov.merMod(object, use.hessian = use.hessian) :
#   variance-covariance matrix computed from finite-difference Hessian is
# not positive definite or contains NA values: falling back to var-cov estimated from RX
# 2: In vcov.merMod(object, correlation = correlation, sigm = sig) :
#   variance-covariance matrix computed from finite-difference Hessian is
# not positive definite or contains NA values: falling back to var-cov estimated from RX 


#Approach #3 ----
#Random Forest Classification

# Create the forest.
test.RF <- randomForest(as.factor(BPWin) ~ Globes.Drama.N + Globes.Drama.W + Globes.Comedy.N + Globes.Comedy.W +
                        CCA.N + CCA.W + SAG.N + SAG.W + BAFTA.N + BAFTA.W + PGA.N + PGA.W + DGA.N + DGA.W +
                        WGA.Original.N + WGA.Original.W + WGA.Adapted.N + WGA.Adapted.W +
                        RT.All + RT.Top + AA.Actor + AA.ActorSup + AA.Actress + AA.ActressSup + 
                        AA.Director + AA.Adapt + AA.Original, na.action=na.exclude, importance = T,
                        ntree = 5000, data = OscarData)
print(test.RF) 

# Importance of each predictor.
print(importance(test.RF,type = 2))

#plot Accuracy and Gini index
varImpPlot(test.RF)


#Does random forest need dummy coding
#No doesn't seem to care which makes sense conceptually based on what it's doing
test.RF <- randomForest(as.factor(BPWin) ~ Globes.Drama + Globes.Comedy +
                          CCA + SAG + BAFTA + PGA + DGA +
                          WGA.Original + WGA.Adapted +
                          RT.All + RT.Top + AA.Actor + AA.ActorSup + AA.Actress + AA.ActressSup + 
                          AA.Director + AA.Adapt + AA.Original, na.action=na.exclude, importance = T,
                        ntree = 5000, data = OscarData)
print(test.RF) 
varImpPlot(test.RF)

#Leave 1 out cross-validation
L1outPreds.RF <- data.frame(Year=NA,Name=NA, BPWin=NA, Prob=NA)

for(yr in 1997:2016){
  #split data based on leave-one(year)-out
  TrainData <- na.exclude(subset(OscarData, Year!=yr))
  TestData <- na.exclude(subset(OscarData, Year==yr))
  
  #run model
  Model <- randomForest(as.factor(BPWin) ~ Globes.Drama + Globes.Comedy +
                          CCA + SAG + BAFTA + PGA + DGA +
                          WGA.Original + WGA.Adapted +
                          RT.All + RT.Top + AA.Actor + AA.ActorSup + AA.Actress + AA.ActressSup + 
                          AA.Director + AA.Adapt + AA.Original, na.action=na.exclude, importance = T,
                        ntree = 5000, data = TrainData)
  
  predictions <- predict(Model, TestData, type="prob")[,2]
  
  #normalize probabilities to year
  probsnorm <- ProbNorm(predictions)
  
  #return data from year left out
  L1outPreds.RF <- rbind(L1outPreds.RF,data.frame(Year=yr,Name=TestData$Name, BPWin=TestData$BPWin, Prob=probsnorm))
}

L1outPreds.RF <- na.exclude(L1outPreds.RF)

#examining predictions
#seems to be okay
for(yr in 1997:2016){
  print(paste("Year: ", yr))
  print(subset(L1outPreds.RF, Year==yr))
  print("")
}

#What percentage would have been called correctly
calls <- c()
callsper <- c()
for(yr in 1997:2016){
  #was the movie called that year? 
  callsnew <-  ifelse(subset(L1outPreds.RF, Year==yr)$Prob==max(subset(L1outPreds.RF, Year==yr)$Prob),1,0)
  calls <-c(calls, callsnew)
  
}

L1outPreds.RF$calls <- calls
table(L1outPreds.RF$BPWin, L1outPreds.RF$calls)

SpHist(subset(L1outPreds.RF, BPWin==1)$Prob)

#Accuracy testing via root mean squared error
L1outPreds.RF <- left_join(L1outPreds.RF, OscarData)
#calculating RMSE
sqrt(sum((L1outPreds.RF$Prob - L1outPreds.RF$BPWin)^2)/sum(!is.na(L1outPreds.RF$BPWin)))
#0.2727928
#misses on average by 27%, but that is considering the outcome is binary
#Pretty similar to the logistic regression with regularization

#What does this predict for 2018
TrainData <- subset(OscarData, Year!=2017)
TestData <- subset(OscarData, Year==2017)

Model <- randomForest(as.factor(BPWin) ~ Globes.Drama + Globes.Comedy +
                        CCA + SAG + BAFTA + PGA + DGA +
                        WGA.Original + WGA.Adapted +
                        RT.All + RT.Top + AA.Actor + AA.ActorSup + AA.Actress + AA.ActressSup + 
                        AA.Director + AA.Adapt + AA.Original, na.action=na.exclude, importance = T,
                      ntree = 5000, data = TrainData)
print(Model)
RF.Imp <- data.frame(varImpPlot(Model))
RF.Imp$variable <- row.names(RF.Imp)
#use ggplot
RF.ImpPlot <- ggplot(data=RF.Imp, aes(x = MeanDecreaseGini, y = reorder(variable, MeanDecreaseGini))) +
  geom_point(size=3) +
  scale_x_continuous("Gini Index of Variable Importance", breaks = seq(0,6,2)) +
  ggtitle("Random Forest Variable Importance") +
  DotRTheme() +
  theme(axis.title.y = element_blank())
ggsave(RF.ImpPlot, filename="RF.ImpPlot.png", height=6, width = 8, dpi=300)



predictions <- predict(Model, TestData, type="prob")[,2]

probsnorm <- ProbNorm(predictions)

data.frame(Year=yr,Name=TestData$Name, BPWin=TestData$BPWin, Prob=probsnorm)
#   Year                                      Name BPWin         Prob
# 1 2016                      Call Me by Your Name    NA 0.0036460865
# 2 2016                              Darkest Hour    NA 0.0000000000
# 3 2016                                   Dunkirk    NA 0.0007292173
# 4 2016                                   Get Out    NA 0.0554205153
# 5 2016                                 Lady Bird    NA 0.1076810890
# 6 2016                            Phantom Thread    NA 0.0097228974
# 7 2016                                  The Post    NA 0.0043753038
# 8 2016                        The Shape of Water    NA 0.6033057851  #shape of water still favorite, but less so. 
# 9 2016 Three Billboards Outside Ebbing, Missouri    NA 0.2151191055


#Approach #4----
#Random Forest in combination with bootstrapping
#Advantage is that it gives a range of predicted likelihoods

#How many years of data do I have
dim(table(OscarData$Year))
#21 years of data (20 not counting the 2017 released movies)

#write a function to resample first from years, and then from moview within years
MLboot <- function(dataset){
  #sample from available years
  bootyears <- sample(unique(dataset$Year), size=length(unique(dataset$Year)), replace=T)
  
  #initialize boot data with all variables from OscarData
  bootdata <- dataset[1,]
  bootdata[1,] <- NA
  
  #first bootstrap years 
  for(b in 1:length(unique(dataset$Year))){
    #bootstrap movies within years
    #first subset data to single year
    yeardata <- subset(dataset, Year==bootyears[b])
    
    #second, bootstrap movies from within yeardata
    for (k in 1:dim(yeardata)[1]){
      bootdata <- rbind(bootdata, yeardata[sample(1:dim(yeardata)[1], 1),])
    }
  }
  
  #delete first NA row
  bootdata <- na.exclude(bootdata)
  return(bootdata)
}

#test bootstrapping function
#seems to work great
MLboot(subset(OscarData, Year <= 2000))


#Leave 1 out cross-validation with bootstrapping (adds in confidence bands of prediction)
Nboots = 1000
L1outPreds.RFboot <- data.frame(Year=NA,Name=NA, BPWin=NA, Prob=NA, LL=NA, UL=NA)

for(yr in 1997:2016){
  #split data based on leave-one(year)-out
  TrainData <- na.exclude(subset(OscarData, Year!=yr))
  TestData <- na.exclude(subset(OscarData, Year==yr))
  
  bootpreds <- setNames(data.frame(matrix(ncol = length(TestData$Name), nrow = 1)), TestData$Name)
  
  #loop of bootstrapping predictions
  for(bs in 1:Nboots){
    
    print(noquote(paste("Test Year: ", yr, "  Boot Number: ", bs, " of ", Nboots)))
    #run RF model
    Model <- randomForest(as.factor(BPWin) ~ Globes.Drama + Globes.Comedy +
                            CCA + SAG + BAFTA + PGA + DGA +
                            WGA.Original + WGA.Adapted +
                            RT.All + RT.Top + AA.Actor + AA.ActorSup + AA.Actress + AA.ActressSup + 
                            AA.Director + AA.Adapt + AA.Original, na.action=na.exclude, importance = T,
                          ntree = 500, data = MLboot(TrainData)) #bumping ntree down to 500 because we are going to bootstrap
    
    predictions <- predict(Model, TestData, type="prob")[,2]
    
    #normalize probabilities to year
    probsnorm <- ProbNorm(predictions)
    names(probsnorm) <- TestData$Name
    
    bootpreds <- rbind(bootpreds, probsnorm)
  }
  
  bootpreds <- na.exclude(bootpreds)
  boot.meanCI <- t(sapply(bootpreds, function(x) {
    c(M = mean(x), quantile(x, c(0.10, 0.90)))
    }))
  
  #return data from year left out
  L1outPreds.RFboot <- rbind(L1outPreds.RFboot, data.frame(Year=yr,Name=TestData$Name, BPWin=TestData$BPWin, 
                                                      Prob=boot.meanCI[,1], LL=boot.meanCI[,2], UL=boot.meanCI[,3]))
}

L1outPreds.RFboot <- na.exclude(L1outPreds.RFboot)
View(L1outPreds.RFboot)
write.csv(L1outPreds.RFboot, "L1outPreds.RFboot.csv")


#What percentage would have been called correctly
calls <- c()
callsper <- c()
for(yr in 1997:2016){
  #was the movie called that year? 
  callsnew <-  ifelse(subset(L1outPreds.RF, Year==yr)$Prob==max(subset(L1outPreds.RF, Year==yr)$Prob),1,0)
  calls <-c(calls, callsnew)
}

L1outPreds.RFboot$calls <- calls
table(L1outPreds.RFboot$BPWin, L1outPreds.RFboot$calls)

SpHist(subset(L1outPreds.RFboot, BPWin==1)$Prob)

#Accuracy testing via root mean squared error
L1outPreds.RFboot <- left_join(L1outPreds.RFboot, OscarData)
#calculating RMSE
sqrt(sum((L1outPreds.RFboot$Prob - L1outPreds.RFboot$BPWin)^2)/sum(!is.na(L1outPreds.RFboot$BPWin)))
#0.2718278
#misses on average by 27%, but that is considering the outcome is binary
#All processes are almost identical
#going with RF w/ bootstrapping though because it gives confidence bands

#plotting historical leave 1(year) out cross-validation with bootstrapping
for(yr in 1997:2016){
  plot <- ggplot(subset(L1outPreds.RFboot, Year==yr), aes(x=Prob, y=reorder(Name, Prob), colour=as.factor(BPWin))) +
    geom_point(size=5) +
    geom_errorbarh(aes(xmax=UL, xmin=LL), height = 0, size=3, alpha=.5) +
    scale_x_continuous("Predicted Win Probability", labels=percent, limits=c(0,1.05), breaks=seq(0,1,.2)) +
    scale_colour_manual(values=c("grey", "gold")) + 
    ggtitle(yr) + 
    DotRTheme() + theme(axis.title.y=element_blank())
  plot
  ggsave(plot, filename=paste(yr, "plot.png"), width=8, height=5, dpi=300)
}

#Plot all historical data together
#Shorten titles
L1outPreds.RFboot$Name.Sh <- ifelse(L1outPreds.RFboot$Name=="Crouching Tiger, Hidden Dragon", "Crouching Tiger",
                                    ifelse(L1outPreds.RFboot$Name=="The Lord of the Rings: The Fellowship of the Ring", "LotR: Fellowship",
                                           ifelse(L1outPreds.RFboot$Name=="The Lord of the Rings: The Two Towers", "LotR: Two Towers",
                                                  ifelse(L1outPreds.RFboot$Name=="The Lord of the Rings: The Return of the King", "LotR: Return of the King",
                                                         ifelse(L1outPreds.RFboot$Name=="Master and Commander: The Far Side of the World", "Master and Commander",
                                                                ifelse(L1outPreds.RFboot$Name=="The Curious Case of Benjamin Button", "Benjamin Button",
                                                                       ifelse(L1outPreds.RFboot$Name=="Precious: Based on the Novel Push by Sapphire", "Precious",
                                                                              ifelse(L1outPreds.RFboot$Name=="Les MisÃ©rables", "Les Miserables",
                                                                                     ifelse(L1outPreds.RFboot$Name=="Birdman or (The Unexpected Virtue of Ignorance)", "Birdman",L1outPreds.RFboot$Name)))))))))



Historicalplot <- ggplot(L1outPreds.RFboot, aes(x=Prob, y=reorder(Name.Sh, Prob), colour=as.factor(BPWin))) +
  geom_point(size=5) +
  geom_errorbarh(aes(xmax=UL, xmin=LL), height = 0, size=3, alpha=.5) +
  facet_wrap( ~ Year, nrow = 5, scales = 'free_y') + 
  scale_x_continuous("Predicted Win Probability", labels=percent, limits=c(0,1.05), breaks=seq(0,1,.2)) +
  scale_colour_manual(values=c("grey", "gold")) + 
  ggtitle("Historical Predictions") + 
  DotRTheme() + 
  theme(axis.text.y = element_text(size=10), axis.text.x = element_text(size=10), 
        axis.title.y=element_blank(), strip.text = element_text(size=12, face="bold"))
Historicalplot
ggsave(Historicalplot, filename="HistoricalplotAll.png", width=20, height = 12, dpi=300)

#plotting historical leave 1(year) out cross-validation with bootstrapping
#redo with shortenned titles
for(yr in 1997:2016){
  plot <- ggplot(subset(L1outPreds.RFboot, Year==yr), aes(x=Prob, y=reorder(Name.Sh, Prob), colour=as.factor(BPWin))) +
    geom_point(size=5) +
    geom_errorbarh(aes(xmax=UL, xmin=LL), height = 0, size=3, alpha=.5) +
    scale_x_continuous("Predicted Win Probability", labels=percent, limits=c(0,1.05), breaks=seq(0,1,.2)) +
    scale_colour_manual(values=c("grey", "gold")) + 
    ggtitle(yr) + 
    DotRTheme() + theme(axis.title.y=element_blank())
  plot
  ggsave(plot, filename=paste(yr, "plot.png"), width=8, height=5, dpi=300)
}

#2017 predictions
#split data based on leave-one(year)-out
Nboots=1000
yr <- 2017
TrainData <- na.exclude(subset(OscarData, Year!=yr))
TestData <- subset(OscarData, Year==yr)

bootpreds <- setNames(data.frame(matrix(ncol = length(TestData$Name), nrow = 1)), TestData$Name)

#loop of bootstrapping predictions
for(bs in 1:Nboots){
  
  print(noquote(paste("Test Year: ", yr, "  Boot Number: ", bs, " of ", Nboots)))
  #run RF model
  Model <- randomForest(as.factor(BPWin) ~ Globes.Drama + Globes.Comedy +
                          CCA + SAG + BAFTA + PGA + DGA +
                          WGA.Original + WGA.Adapted +
                          RT.All + RT.Top + AA.Actor + AA.ActorSup + AA.Actress + AA.ActressSup + 
                          AA.Director + AA.Adapt + AA.Original, na.action=na.exclude, importance = T,
                        ntree = 500, data = MLboot(TrainData)) #bumping ntree down to 500 because we are going to bootstrap
  
  predictions <- predict(Model, TestData, type="prob")[,2]
  
  #normalize probabilities to year
  probsnorm <- ProbNorm(predictions)
  names(probsnorm) <- TestData$Name
  
  bootpreds <- rbind(bootpreds, probsnorm)
}

bootpreds <- na.exclude(bootpreds)

#computing mean prediction and 80% CI
boot.meanCI <- t(sapply(bootpreds, function(x) {
  c(M = mean(x), quantile(x, c(0.10, 0.90)))
}))
Predictions2018 <- data.frame(Year=yr, Name=TestData$Name, Prob=boot.meanCI[,1], LL=boot.meanCI[,2], UL=boot.meanCI[,3])

#rename 3 billboards
levels(Predictions2018$Name) <- c(levels(Predictions2018$Name), "Three Billboards") 
Predictions2018$Name[9] <- "Three Billboards"

OscarPred2018.plot <- ggplot(Predictions2018, aes(x=Prob, y=reorder(Name, Prob))) +
  geom_point(size=5, colour="#929292") +
  geom_errorbarh(aes(xmax=UL, xmin=LL), height = 0, size=3, alpha=.5, colour="#929292") +
  scale_x_continuous("Predicted Win Probability", labels=percent, limits=c(0,1.05), breaks=seq(0,1,.2)) +
  ggtitle("Best Picture 2018 Predictions") + 
  DotRTheme() + theme(axis.title.y=element_blank())
OscarPred2018.plot
ggsave(OscarPred2018.plot, filename="OscarPred2018.plot.png", width=8, height = 7, dpi=500)



#Thinking of plotting probabilities over the awards season
Timeline <- read.xlsx("Awards Season Timeline.xlsx", sheetName = "Sheet1")
SpDesc(Timeline)
Timeline <- Timeline[-1,] #start with RT scores as default

TimeData <- function(dataset, timeline, date){
  #split data into Nominations and Award
  Nominations <- subset(timeline, Nom==1)
  Awards <- subset(timeline, Nom==0)
  
  #Loop through dates on Nominations timeline to get varaibles to pass to randomForest
  variables=c("RT.All", "RT.Top") #start with RT scores only
  for(n in 1:length(Nominations$Date)){
    if(date>=Nominations$Date[n]){ #add in variables based on whether date has past
      variables <- c(variables, Nominations[n,4:10][!is.na(Nominations[n,4:10])])
    }
  }
  
  #make matrix of data from variables list
  newdataset <- dataset[,variables]

  #Rescore 2s to 1s if only nominations and no winners
  for(v in 3:length(variables)){
    if(date < subset(Awards, substr(Var1,1,2)==substr(variables[v],1,2))$Date){
      newdataset[,variables[v]] <- ifelse(newdataset[,variables[v]]==2,1,newdataset[,variables[v]])
    }
  }
  
  #return new dataset with only nominations/awards that have already happened
  return(list(variables=variables, newdataset=newdataset))
}

#test TimeData -- works great!
TimeData(TestData, Timeline, date="2018-01-09")
TimeData(TestData, Timeline, date="2018-02-01")


#Make full timeline
yr <- 2017
Nboots <- 1000
TrainData <- na.exclude(subset(OscarData, Year!=yr))
TestData <- subset(OscarData, Year==yr)

Predictions2018Timeline <- data.frame(Year=yr, date=date("2017-12-01"),Name=TestData$Name, Prob=1/9, LL=1/9, UL=1/9) #start with even odds

for(t in 1:(length(Timeline$Date)-1)){
  #process Test data for time
  TimeDataRes <- TimeData(TestData, Timeline, date=Timeline$Date[t])
  newTestData <- TimeDataRes$newdataset
  variables <- TimeDataRes$variables
  
  bootpreds <- setNames(data.frame(matrix(ncol = length(TestData$Name), nrow = 1)), TestData$Name)
  
  #loop of bootstrapping predictions with new testdata
  for(bs in 1:Nboots){
    
    print(noquote(paste("Date: ", Timeline$Date[t], "  Boot Number: ", bs, " of ", Nboots)))
    
    #Need to make dataframe to use MLboot
    bootedTrainData <- MLboot(TrainData[c("Name", "Year", "BPWin", variables)])
    
    #run RF model
    Model <- randomForest(y=as.factor(bootedTrainData$BPWin), x=bootedTrainData[,variables], na.action=na.exclude, importance = T,
                          ntree = 500)
    
    predictions <- predict(Model, newTestData, type="prob")[,2]
    
    #normalize probabilities to year
    probsnorm <- ProbNorm(predictions)
    names(probsnorm) <- TestData$Name
    
    bootpreds <- rbind(bootpreds, probsnorm)
  }
  
  bootpreds <- na.exclude(bootpreds)
  
  #computing mean prediction and 80% CI
  boot.meanCI <- t(sapply(bootpreds, function(x) {
    c(M = mean(x), quantile(x, c(0.10, 0.90)))
  }))
  
  Predictions2018Timeline <- rbind(Predictions2018Timeline, data.frame(Year=yr, date=Timeline$Date[t], Name=TestData$Name, Prob=boot.meanCI[,1], LL=boot.meanCI[,2], UL=boot.meanCI[,3]))
}
#rename 3 billboards
levels(Predictions2018Timeline$Name) <- c(levels(Predictions2018Timeline$Name), "Three Billboards") 
for(i in 1:length(Predictions2018Timeline$Name)){
  if(Predictions2018Timeline$Name[i] == "Three Billboards Outside Ebbing, Missouri"){
    Predictions2018Timeline$Name[i] <- "Three Billboards"
  }
}

#plot timeline of prediction
#too ugly to have the all on top of eachother
OscarPred2018Timeline.plot <- ggplot(subset(Predictions2018Timeline, Predictions2018Timeline$Name %in% c("The Shape of Water", "Three Billboards", "Lady Bird", "Get Out")),
                                     aes(x=date, y=Prob, colour=Name)) +
  geom_line(size=2) +
  geom_ribbon(aes(ymin=LL, ymax=UL, fill=Name), alpha=0.2, colour=NA) + 
  scale_x_date(date_labels = "%b %d") +
  scale_y_continuous("Win Probability", labels=percent) + 
  ggtitle("Best Picture 2018 Predictions") + 
  DotRTheme(legend.position = "right")
OscarPred2018Timeline.plot
ggsave(OscarPred2018Timeline.plot, filename="OscarPred2018Timeline.plot.png", width=8, height = 7, dpi=500)



#try animating
Dates <- unique(Predictions2018Timeline$date)
Dates[17] <- "2018-02-25" #Adding final prediction date
Predictions2018$date <- date("2018-02-25")
Predictions2018Timeline <- rbind(Predictions2018Timeline, Predictions2018)
Predictions2018Timeline$Name <- factor(Predictions2018Timeline$Name, levels = rev(c("The Shape of Water", "Three Billboards", "Lady Bird", "Get Out",
                                                                                "Phantom Thread", "Call Me by Your Name", "The Post", "Dunkirk",
                                                                                "Darkest Hour")))
#Add annotations of what happened
Events <- c("","Critics Choice\nNominations", "Golden Globes\nNominations", "Screen Actors Guild\nNominations", 
            "Writers Guild\nNominations", "Producers Guild\nNominations", "Golden Globes\nAwards", "BAFTA\nNominations",
            "Directors Guild\nNominations", "Critics Choice\nAwards", "Producers Guild\nAwards", "Screen Actors Guild\nAwards",
            "Oscars\nNominations", "Directors Guild\nAwards", "Writers Guild\nAwards", "BAFTA\nAwards", "Final\nPrediction")

for(d in 1:length(Dates)){
  plot <- ggplot(subset(Predictions2018Timeline, date==Dates[d]), aes(x=Prob, y=Name)) +
    geom_point(size=5, colour="#929292") +
    geom_errorbarh(aes(xmax=UL, xmin=LL), height = 0, size=3, alpha=.5, colour="#929292") +
    scale_x_continuous("Predicted Win Probability", labels=percent, limits=c(0,1.05), breaks=seq(0,1,.2)) +
    ggtitle(paste("Best Picture 2018 Predictions")) + 
    annotate("text", label=format(Dates[d], format="%b %d %Y"), x=.73, y=1.8, colour="black", size=6, hjust=.5, vjust=0.5) +
    annotate("text", label=Events[d], x=.73, y=3.0, colour="black", size=7, fontface="bold", hjust=.5, vjust=0.5) +
    DotRTheme() + theme(axis.title.y=element_blank())
  print(plot)
  ggsave(plot, filename=paste(Dates[d], "Prediction Plot.png"), width=8, height = 7, dpi=200)
}


# Predictions2018$Name <- factor(Predictions2018$Name, levels = rev(c("The Shape of Water", "Three Billboards", "Lady Bird", "Get Out",
#                                                                                     "Phantom Thread", "Call Me by Your Name", "The Post", "Dunkirk",
#                                                                                     "Darkest Hour")))
# plot <- ggplot(Predictions2018, aes(x=Prob, y=Name)) +
#   geom_point(size=5, colour="#929292") +
#   geom_errorbarh(aes(xmax=UL, xmin=LL), height = 0, size=3, alpha=.5, colour="#929292") +
#   scale_x_continuous("Predicted Win Probability", labels=percent, limits=c(0,1.05), breaks=seq(0,1,.2)) +
#   ggtitle(paste("Best Picture 2018 Predictions")) + 
#   annotate("text", label="Final Prediction", x=.8, y=3.5, colour="black", size=6, fontface="bold", hjust=.5, vjust=0.5) +
#   DotRTheme() + theme(axis.title.y=element_blank())
# plot
# ggsave(plot, filename="Final Prediction Plot.png", width=8, height = 7, dpi=200)
