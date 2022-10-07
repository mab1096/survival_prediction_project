rm(list=ls())
library(tidyverse)
library(modelr)
library(gmodels)
library(skimr)
train <- read.csv("train.csv",stringsAsFactors=F)
test <- read.csv("test.csv",stringsAsFactors=F)



train$Title <- substring(train$Name,regexpr(",",train$Name)+2,regexpr("\\.",train$Name)-1)
train$Title <- substring(train$Name, regexpr(",", train$Name)+2, regexpr("\\.", train$Name)-1)
train$Title[train$Title %in% c("Capt","Don","Major","Col","Rev","Dr","Sir","Mr","Jonkheer")] <- "man"
train$Title[train$Title %in% c("Dona","the Countess","Mme","Mlle","Ms","Miss","Lady","Mrs")] <- "woman"
train$Title[train$Title %in% c("Master")] <- "boy"
train$Surname <- substring(train$Name,0,regexpr(",",train$Name)-1)
train$Surname[train$Title=='man'] <- 'noGroup'
train$SurnameFreq <- ave(1:891,train$Surname,FUN=length)
train$Surname[train$SurnameFreq<=1] <- 'noGroup'
train$SurnameSurvival <- ave(train$Survived,train$Surname)



library(ggplot2)
train$AdjustedSurvival <- (train$SurnameSurvival * train$SurnameFreq - train$Survived) / (train$SurnameFreq-1)
train$predict <- 0
train$predict[train$Title=='woman'] <- 1
train$predict[train$Title=='boy' & train$AdjustedSurvival==1] <- 1
train$predict[train$Title=='woman' & train$AdjustedSurvival==0] <- 0
trials = 25; sum = 0
for (j in 1:trials){
  x = sample(1:890); s = 0
  for (i in 0:9){
    # Engineer "woman-child-groups" from training subset
    train$Surname <- substring(train$Name,0,regexpr(",",train$Name)-1)
    train$Surname[train$Title=='man'] <- 'noGroup'
    train$SurnameFreq <- ave(1:891,train$Surname,FUN=length)
    train$Surname[train$SurnameFreq<=1] <- 'noGroup'
    train$SurnameSurvival <- NA
    # calculate training subset's surname survival rate
    train$SurnameSurvival[-x[1:89+i*89]] <- ave(train$Survived[-x[1:89+i*89]],train$Surname[-x[1:89+i*89]])
    # calculate testing subset's surname survival rate from training set's rate
    for (k in x[1:89+i*89]) 
      train$SurnameSurvival[k] <- train$SurnameSurvival[which(!is.na(train$SurnameSurvival) & train$Surname==train$Surname[k])[1]]
    # apply gender model plus new predictor
    train$predict <- 0
    train$predict[train$Title=='woman'] <- 1
    train$predict[train$Title=='boy' & train$SurnameSurvival==1] <- 1
    train$predict[train$Title=='woman' & train$SurnameSurvival==0] <- 0
    
    c = sum(abs(train$predict[x[1:89+i*89]] - train$Survived[x[1:89+i*89]]))
    s = s + c}
  cat( sprintf("Trial %d has 10-fold CV accuracy = %f\n",j,1-s/890))
  sum = sum + 1-s/890}
cat(sprintf("Average 10-fold CV accuracy from %d trials = %f\n",trials,sum/trials))




test$Title <- substring(test$Name,regexpr(",",test$Name)+2,regexpr("\\.",test$Name)-1)
test$Title[test$Title %in% c("Capt","Don","Major","Col","Rev","Dr","Sir","Mr","Jonkheer")] <- "man"
test$Title[test$Title %in% c("Dona","the Countess","Mme","Mlle","Ms","Miss","Lady","Mrs")] <- "woman"
test$Title[test$Title %in% c("Master")] <- "boy"
test$Survived <- NA; test$predict <- NA; train$AdjustedSurvival <- NULL
train$Surname <- NULL; train$SurnameFreq <- NULL; train$SurnameSurvival <- NULL
allData <- rbind(train,test)
allData$Surname <- substring(allData$Name,0,regexpr(",",allData$Name)-1)
allData$Surname[allData$Title=='man'] <- 'noGroup'
allData$SurnameFreq <- ave(1:1309,allData$Surname,FUN=length)
allData$Surname[allData$SurnameFreq<=1] <- 'noGroup'
for(i in which(allData$Title!='man' & allData$Surname=='noGroup'))
  allData$Surname[i] = allData$Surname[allData$Ticket==allData$Ticket[i]][1]
allData$Surname[is.na(allData$Surname)] <- 'noGroup'
allData$SurnameSurvival <- NA
allData$SurnameSurvival[1:891] <- ave(allData$Survived[1:891],allData$Surname[1:891])
for (i in 892:1309) allData$SurnameSurvival[i] <- allData$SurnameSurvival[which(allData$Surname==allData$Surname[i])[1]]
allData$predict <- 0
allData$predict[allData$Title=='woman'] <- 1
allData$predict[allData$Title=='boy' & allData$SurnameSurvival==1] <- 1
allData$predict[allData$Title=='woman' & allData$SurnameSurvival==0] <- 0




m1_submission <- data.frame(PassengerId = allData$PassengerId[892:1309], Survived = allData$predict[892:1309])
write.csv(m1_submission,"finalsubmission.csv",row.names=F)
