##Assignment 2
################################ Package loading

library(dplyr)
##install.packages("mice")
library(mice)
library(leaps)
library(MASS)
library (boot)
library(class)

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



################################  Create Passenger Group variable train

train$PassengerGroup <- 
  sapply(train$PassengerId,function(x) strsplit(x,'_')[[1]][1])

## apply transformation in the Consumption variable

set.seed(1)
train$RoomService<-log(train$RoomService+1)+rnorm(nrow(train),1,0.01)
train$FoodCourt<-log(train$FoodCourt+1)+rnorm(nrow(train),1,0.01)
train$ShoppingMall<-log(train$ShoppingMall+1)+rnorm(nrow(train),1,0.0001)
train$Spa<-log(train$Spa+1)+rnorm(nrow(train),1,0.01)
train$VRDeck<-log(train$VRDeck+1)+rnorm(nrow(train),1,0.01)
summary(train)

##manage variable
train <- train %>% dplyr::select(-PassengerId,-Cabin,-Name)

train$Transported<-as.factor(train$Transported)
train$Side<-as.factor(train$Side)
train$HomePlanet<-as.factor(train$HomePlanet)
train$CryoSleep<-as.factor(train$CryoSleep)
train$Destination<-as.factor(train$Destination)
train$VIP<-as.factor(train$VIP)
train$PassengerGroup<-as.factor(train$PassengerGroup)

################################  Detect the pattern of missing data train

##Missing data map
miss_map<-function(data){
  image(is.na(data), axes = FALSE, col = gray(0:1))
  axis(2, at = 1:ncol(data)/ncol(data), labels = colnames(data))
  axis(1, at = 1:nrow(data)/nrow(data), labels = row.names(data),
       las = 2,col = "white") 
}
par(mfrow = c(1,1))
miss_map(train)
##people travel together tends to miss the similar data

t_missing <- unlist(lapply(train, function(x) sum(is.na(x))))/nrow(train)
sort(t_missing[t_missing > 0], decreasing = TRUE)
## All below the 25% threhold, ready to impute
cor(train[,8:12], use = "pairwise.complete.obs")
##pairs(train[,c(1,4,8:12)],col = "blue")
##no nigh proportional missing value
##no high correlated numeric value


################################  get train and valid data set

valid<-train[c(1:1693),]
train<-train[-c(1:1693),]

##exclude the passager group variable

imp <- mice(train, maxit=0)
predM <- imp$predictorMatrix
meth <- imp$method
predM[, c("PassengerGroup")] <- 0

# Specify a separate imputation model for variables of interest 

# Ordered categorical variables 
poly <- c("CryoSleep", "VIP","Transported")

# Dichotomous variable
log <- c("Side")

# Unordered categorical variable 
poly2 <- c("HomePlanet","Destination","PassengerGroup")

meth[poly] <- "polr"
meth[log] <- "logreg"
meth[poly2] <- "polyreg"

train <- mice(train, maxit = 5, 
             predictorMatrix = predM, 
             method = meth, print =  FALSE)

train.comp <- mice::complete(train, 1)
train.comp$PassengerGroup<-as.factor(train.comp$PassengerGroup)
write.csv(train.comp,file="train_comp.csv",row.names=FALSE)
train.comp <-
  read.csv(file="train_comp.csv",header=TRUE,stringsAsFactors=T)
################################ Missing data check
par(mfrow = c(1,1))
miss_map(train.comp)



















################################  Detect the pattern of missing data valid

##Missing data map
miss_map<-function(data){
  image(is.na(data), axes = FALSE, col = gray(0:1))
  axis(2, at = 1:ncol(data)/ncol(data), labels = colnames(data))
  axis(1, at = 1:nrow(data)/nrow(data), labels = row.names(data),
       las = 2,col = "white") 
}
par(mfrow = c(1,1))
miss_map(valid)
##people travel together tends to miss the similar data

v_missing <- unlist(lapply(valid, function(x) sum(is.na(x))))/nrow(valid)
sort(v_missing[v_missing > 0], decreasing = TRUE)
## All below the 25% threhold, ready to impute
cor(valid[,8:12], use = "pairwise.complete.obs")
##pairs(train[,c(1,4,8:12)],col = "blue")
##no nigh proportional missing value
##no high correlated numeric value


##exclude the passager group variable
train <- train %>% dplyr::select(-PassengerId,-Cabin,-Name)

imp <- mice(valid, maxit=0)
predM <- imp$predictorMatrix
meth <- imp$method
predM[, c("PassengerGroup")] <- 0

# Specify a separate imputation model for variables of interest 


# Ordered categorical variables 
poly <- c("CryoSleep", "VIP","Transported")

# Dichotomous variable
log <- c("Side")

# Unordered categorical variable 
poly2 <- c("HomePlanet","Destination","PassengerGroup")

meth[poly] <- "polr"
meth[log] <- "logreg"
meth[poly2] <- "polyreg"

valid <- mice(valid, maxit = 5, 
              predictorMatrix = predM, 
              method = meth, print =  FALSE)

valid<- mice::complete(valid, 1)
valid$PassengerGroup<-as.factor(valid$PassengerGroup)
write.csv(valid,file="valid_comp.csv",row.names=FALSE)
valid <-
  read.csv(file="valid_comp.csv",header=TRUE,stringsAsFactors=T)
################################ Missing data check
par(mfrow = c(1,1))
miss_map(valid)














################################  Create side variable for test

Side<-rep(NA, nrow(test.raw))
Port<-grep("P",test.raw$Cabin)
Starb<-grep("S",test.raw$Cabin)
Side[Port]<-"P"
Side[Starb]<-"S"
Side<-as.factor(Side)
test.raw<-test.raw[,1:13]
test.raw<-data.frame(Side,test.raw)

################################  Create Passenger Group variable test

test.raw$PassengerGroup <- 
  sapply(test.raw$PassengerId,function(x) strsplit(x,'_')[[1]][1])


## apply transformation in the Consumption variable

set.seed(1)
test.raw$RoomService<-log(test.raw$RoomService+1)+rnorm(nrow(test.raw),1,0.01)
test.raw$FoodCourt<-log(test.raw$FoodCourt+1)+rnorm(nrow(test.raw),1,0.01)
test.raw$ShoppingMall<-log(test.raw$ShoppingMall+1)+rnorm(nrow(test.raw),1,0.0001)
test.raw$Spa<-log(test.raw$Spa+1)+rnorm(nrow(test.raw),1,0.01)
test.raw$VRDeck<-log(test.raw$VRDeck+1)+rnorm(nrow(test.raw),1,0.01)
summary(test.raw)

##manage variable
test.raw <- test.raw %>% dplyr::select(-PassengerId,-Cabin,-Name)

test.raw$Transported<-as.factor(test$Transported)
test.raw$Side<-as.factor(test.raw$Side)
test.raw$HomePlanet<-as.factor(test.raw$HomePlanet)
test.raw$CryoSleep<-as.factor(test.raw$CryoSleep)
test.raw$Destination<-as.factor(test.raw$Destination)
test.raw$VIP<-as.factor(test.raw$VIP)
test.raw$PassengerGroup<-as.factor(test.raw$PassengerGroup)



################################  Detect the pattern of missing data test

miss_map(test.raw)
test_missing <- unlist(lapply(test.raw, function(x) sum(is.na(x))))/nrow(test)
sort(test_missing[test_missing > 0], decreasing = TRUE)
test_numeric<-test.raw[,c(5,7:11)]
cor(test_numeric, use = "pairwise.complete.obs")
pairs(test_numeric,col = "blue")


test.raw <- test.raw %>% dplyr::select(-PassengerId,-Cabin,-Name)

test.raw$Side<-as.factor(test.raw$Side)
test.raw$HomePlanet<-as.factor(test.raw$HomePlanet)
test.raw$CryoSleep<-as.factor(test.raw$CryoSleep)
test.raw$Destination<-as.factor(test.raw$Destination)
test.raw$VIP<-as.factor(test.raw$VIP)


imp <- mice(test.raw, maxit=0)
predM <- imp$predictorMatrix
meth <- imp$method
predM[, c("PassengerGroup")] <- 0

# Ordered categorical variables 
poly <- c("CryoSleep", "VIP")

# Dichotomous variable
log <- c("Side")

# Unordered categorical variable 
poly2 <- c("HomePlanet","Destination","PassengerGroup")

meth[poly] <- "polr"
meth[log] <- "logreg"
meth[poly2] <- "polyreg"

test <- mice(test.raw, maxit = 5, 
              predictorMatrix = predM, 
              method = meth, print =  FALSE)

test <- mice::complete(test, 1)
test$PassengerGroup<-as.factor(test$PassengerGroup)
write.csv(test,file="test_comp.csv",row.names=FALSE)
test <-
  read.csv(file="test_comp.csv",header=TRUE,stringsAsFactors=T)


################################ Missing data check test
par(mfrow = c(1,1))
miss_map(test)

