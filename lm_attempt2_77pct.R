rm(list=ls())
library(tidyverse)
library(modelr)
#install.packages(gmodels) (hashtag/note b/c I already have)
library(gmodels)
library(skimr)



gs<-read_csv("gender_submission.csv")
summary(gs)

#read in data
train<-read_csv("train.csv")
test<-read_csv("test.csv")

skim(train)
colSums(is.na(train))
colSums(is.na(test))

table(train$Embarked)
train$Embarked[is.na(train$Embarked)]<-"S"
train$Age[is.na(train$Age)]<-mean(train$Age, na.rm = T)

test$Age[is.na(test$Age)]<-mean(test$Age)

#insert na removal code here

#Pclass1 <- train %>% select(Pclass)
#Pclass1<- train %>% filter(Pclass == 1)
#Train3<-mutate(train, ifelse(Pclass < 1, "Pclass1", TRUE))
#penis <- ifelse(train$Pclass==1,1,0))


train<- mutate(train, firstclass = ifelse(train$Pclass==1,1,0))
train<- mutate(train, secondclass = ifelse(train$Pclass==2,1,0))
train<- mutate(train, thirdclass = ifelse(train$Pclass==3,1,0))
train<- mutate(train, femalefirstclass = ifelse(train$Pclass==1 & train$Sex=="female",1,0))
train<- mutate(train, femalesecondclass = ifelse(train$Pclass==2 & train$Sex=="female",1,0))
train<- mutate(train, femalethirdclass = ifelse(train$Pclass==3 & train$Sex=="female",1,0))
train<- mutate(train, c_embark = ifelse(train$Embarked=="C",1,0))
train<- mutate(train, onesibsp = ifelse(train$SibSp==1,1,0))
train<- mutate(train, oneparch = ifelse(train$Parch==1,1,0))
train<- mutate(train, twoparch = ifelse(train$Parch==2,1,0))
train<- mutate(train, midfare = ifelse(train$Fare>=16 & train$Fare<=162,1,0))
train<- mutate(train, female = ifelse(train$Sex=="female",1,0))



test<- mutate(test, firstclass = ifelse(test$Pclass==1,1,0))
test<- mutate(test, secondclass = ifelse(test$Pclass==2,1,0))
test<- mutate(test, thirdclass = ifelse(test$Pclass==3,1,0))
test<- mutate(test, femalefirstclass = ifelse(test$Pclass==1 & test$Sex=="female",1,0))
test<- mutate(test, femalesecondclass = ifelse(test$Pclass==2 & test$Sex=="female",1,0))
test<- mutate(test, femalethirdclass = ifelse(test$Pclass==3 & test$Sex=="female",1,0))
test<- mutate(test, c_embark = ifelse(test$Embarked=="C",1,0))
test<- mutate(test, onesibsp = ifelse(test$SibSp==1,1,0))
test<- mutate(test, oneparch = ifelse(test$Parch==1,1,0))
test<- mutate(test, twoparch = ifelse(test$Parch==2,1,0))
test<- mutate(test, midfare = ifelse(test$Fare>=16 & test$Fare<=162,1,0))
test<- mutate(test, female = ifelse(test$Sex=="female",1,0))




m1 <-lm(Survived ~ femalefirstclass + femalesecondclass + femalethirdclass + onesibsp + 
          oneparch + twoparch , data = train)
summary(m1)


summary(m1)

#add predictions and convert probabililities into 1,0 from logical T, F
train<-train %>% 
  add_predictions(m1) 


#need predictions in binary (1,0) format, not probabilities
train <- train %>%
  mutate(pred = as.numeric(pred > 0.5))

#evaluate in sample performance
CrossTable(x = train$Survived, y = train$pred,
           prop.chisq = F)

#if we like this model...

m1_submission<- test %>%
  add_predictions(m1, var = "pred") %>%
  mutate(pred = as.numeric(pred > 0.5)) %>%
  select(PassengerId,
         Survived = pred)

#will need to do the same na removals for this to submit
colSums(is.na(m1_submission))



write_csv(m1_submission, "lm_attempt2.csv")
lm_attempt2<-read.csv("lm_attempt2.csv")

