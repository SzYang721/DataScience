################################ EDA

################################ Package loading

library(dplyr)
##install.packages("mice")
library(mice)
library(leaps)
library(MASS)
library (boot)
library(class)
library(reshape2)
library(GGally)
install.packages("tree")
library(tree)

################################
getwd()
setwd("E:/ANU Sem 2/STAT3040STAT7040 - Statistical Learning/Lab and tute/Week 10")
train <-
  read.csv(file="train.csv",header=TRUE,stringsAsFactors=T,na.strings = "")
test<-
  read.csv(file="test.csv",header=TRUE,stringsAsFactors=T,na.strings = "")

################################

summary(train)
n<-nrow(train)

################################

##Missing data map
miss_map<-function(data){
  image(is.na(data), axes = FALSE, col = gray(0:1))
  axis(2, at = 1:ncol(data)/ncol(data), labels = colnames(data))
  axis(1, at = 1:nrow(data)/nrow(data), labels = row.names(data),
       las = 2,col = "white") 
}
par(mfrow = c(1,1))
miss_map(train)

t_missing <- unlist(lapply(train, function(x) sum(is.na(x))))/nrow(train)
sort(t_missing[t_missing > 0], decreasing = TRUE)
## All below the 25% threhold, ready to impute
cor(train, use = "pairwise.complete.obs")

################################

train$Age[is.na(train$Age)==1]<-29.70
train$Fare[is.na(train$Fare)]<-32.20 
train$Embarked[is.na(train$Embarked)]<-"S"
miss_map(train)
summary(train)

################################

test$Age[is.na(test$Age)]<-mean(test$Age,na.rm = TRUE)
test$Fare[is.na(test$Fare)]<-mean(test$Fare,na.rm = TRUE)
test$Embarked[is.na(test$Embarked)]<-"S"


################################

install.packages("randomForest")
library(randomForest)
set.seed(3)
bag.mod<-randomForest(as.factor(Survived) ~ Pclass + Sex + 
                        Age + SibSp + Parch+ Fare+Embarked,
                      data = train, subset = sam, mtry = 7,
                      ntree = 2000)

## the mtry is the main difference between the bagging and randomforest

################################

y.hat<-predict(bag.mod, newdata = test)


################################

varImpPlot(bag.mod)

################################





