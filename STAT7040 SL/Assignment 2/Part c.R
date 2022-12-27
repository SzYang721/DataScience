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

################################
getwd()
setwd("E:/ANU Sem 2/STAT3040STAT7040 - Statistical Learning/Assignment 2")
train <-
  read.csv(file="train.csv",header=TRUE,stringsAsFactors=F,na.strings = "")
test.raw<-
  read.csv(file="test.csv",header=TRUE,stringsAsFactors=F,na.strings = "")


################################  Create side variable for train

Side<-rep(NA, nrow(train))
Port<-grep("P",train$Cabin)
Starb<-grep("S",train$Cabin)
Side[Port]<-"P"
Side[Starb]<-"S"
Side<-as.factor(Side)
Transported<-train$Transported
train<-train[,1:13]
train<-data.frame(Transported,Side,train)

################################  Create side variable for test

Side<-rep(NA, nrow(test.raw))
Port<-grep("P",test.raw$Cabin)
Starb<-grep("S",test.raw$Cabin)
Side[Port]<-"P"
Side[Starb]<-"S"
Side<-as.factor(Side)
test.raw<-test.raw[,1:13]
test<-data.frame(Side,test.raw)

################################  Create Passenger Group variable train

train$PassengerGroup <- 
  sapply(train$PassengerId,function(x) strsplit(x,'_')[[1]][1])

################################  Create Passenger Group variable test

test$PassengerGroup <- 
  sapply(test$PassengerId,function(x) strsplit(x,'_')[[1]][1])

train <- train %>% dplyr::select(-PassengerId,-Cabin,-Name)

##(c)
summary(train)


require(reshape2)
melt.train <- melt(train)
ggplot(data = melt.train, aes(x = value)) + 
  stat_density() + 
  facet_wrap(~variable, scales = "free")

## apply transformation in the character variable

train$Transported<-as.factor(train$Transported)
train$Side<-as.factor(train$Side)
train$HomePlanet<-as.factor(train$HomePlanet)
train$CryoSleep<-as.factor(train$CryoSleep)
train$Destination<-as.factor(train$Destination)
train$VIP<-as.factor(train$VIP)
train$PassengerGroup<-as.factor(train$PassengerGroup)

##EDA discover the pattern of missing data

##suggest a transformation of the the consumption variable

## apply transformation in the Consumption variable

set.seed(1)
train$RoomService<-log(train$RoomService+1)+rnorm(nrow(train),1,0.01)
train$FoodCourt<-log(train$FoodCourt+1)+rnorm(nrow(train),1,0.01)
train$ShoppingMall<-log(train$ShoppingMall+1)+rnorm(nrow(train),1,0.0001)
train$Spa<-log(train$Spa+1)+rnorm(nrow(train),1,0.01)
train$VRDeck<-log(train$VRDeck+1)+rnorm(nrow(train),1,0.01)
summary(train)

melt.train <- melt(train)
ggplot(data = melt.train, aes(x = value)) + 
  stat_density() + 
  facet_wrap(~variable, scales = "free")

##scatter plot matrix
pairs(train, col = "blue")
ggpairs(train[,1:12]) 
#the CryoSleep and the consumption are correlated
#the Side and other qualitative var and the consumption are not correlated
## VIP may expense more on the boat

par(mfrow = c(2,3))
plot(train$Side,train$Age)
plot(train$CryoSleep,train$RoomService)
plot(train$CryoSleep,train$FoodCourt)
plot(train$CryoSleep,train$ShoppingMall)
plot(train$CryoSleep,train$Spa)
plot(train$CryoSleep,train$VRDeck)

