################################ KNN

################################ Package loading

library(dplyr)
##install.packages("mice")
library(mice)
library(leaps)
library(MASS)
library (boot)
library(class)
#install.packages("mlr3")
library(mlr)
library(mlr3)
#install.packages("mlr3viz")
library(mlr3viz)
#install.packages("GGally")
library(GGally)
#install.packages("kknn")
library(kknn)
#install.packages("mlr3verse")
library(mlr3verse)
#install.packages("FSelector")
library(FSelector)
library(mlr3fselect)



################################ data loading

setwd("E:/ANU Sem 2/STAT3040STAT7040 - Statistical Learning/Assignment 2")
train.comp <-
  read.csv(file="train_comp.csv",header=TRUE,stringsAsFactors=T)
valid<-
  read.csv(file="valid_comp.csv",header=TRUE,stringsAsFactors=T)
test<-
  read.csv(file="test_comp.csv",header=TRUE,stringsAsFactors=T)


##(g)

################################ i

train.comp$Side<-as.numeric(train.comp$Side)+rnorm(nrow(train.comp),1,0.01)
train.comp$HomePlanet<-as.numeric(train.comp$HomePlanet)+rnorm(nrow(train.comp),1,0.01)
train.comp$CryoSleep<-as.numeric(train.comp$CryoSleep)+rnorm(nrow(train.comp),1,0.01)
train.comp$Destination<-as.numeric(train.comp$Destination)+rnorm(nrow(train.comp),1,0.01)
train.comp$VIP<-as.numeric(train.comp$VIP)+rnorm(nrow(train.comp),1,0.01)
train.comp$PassengerGroup<-as.numeric(train.comp$PassengerGroup)+rnorm(nrow(train.comp),1,0.01)

valid$Side<-as.numeric(valid$Side)+rnorm(nrow(valid),1,0.01)
valid$HomePlanet<-as.numeric(valid$HomePlanet)+rnorm(nrow(valid),1,0.01)
valid$CryoSleep<-as.numeric(valid$CryoSleep)+rnorm(nrow(valid),1,0.01)
valid$Destination<-as.numeric(valid$Destination)+rnorm(nrow(valid),1,0.01)
valid$VIP<-as.numeric(valid$VIP)+rnorm(nrow(valid),1,0.01)
valid$PassengerGroup<-as.numeric(valid$PassengerGroup)+rnorm(nrow(valid),1,0.01)

test$Side<-as.numeric(test$Side)+rnorm(nrow(test),1,0.01)
test$HomePlanet<-as.numeric(test$HomePlanet)+rnorm(nrow(test),1,0.01)
test$CryoSleep<-as.numeric(test$CryoSleep)+rnorm(nrow(test),1,0.01)
test$Destination<-as.numeric(test$Destination)+rnorm(nrow(test),1,0.01)
test$VIP<-as.numeric(test$VIP)+rnorm(nrow(test),1,0.01)
test$PassengerGroup<-as.numeric(test$PassengerGroup)+rnorm(nrow(test),1,0.01)

shrink.data<-train.comp[,c(1,6,8:13)]

cv.out<-method.fwd("knn",train.comp,10)
cv.out[[2]]

plot(cv.out[[1]][,1],type = "p",lwd = 2,cex = 1.2,
     ylab = "Cross Validation (Steps)",
     xlab = "Steps",
     main = "Forward Selection based on CV",
     ylim = c(0.21,0.46)
)
points(which.min(cv.out[[1]][,1]),min(cv.out[[1]][,1]),
       pch = 18,col = "red",cex = 1.5)
points(1:(ncol(train.comp)-1),cv.out[[1]][,1]+cv.out[[1]][,2],
       pch = 25,col = "red",cex = 1)
points(1:(ncol(train.comp)-1),cv.out[[1]][,1]-cv.out[[1]][,2],
       pch = 24,col = "red",cex = 1)
mod.lowcv<-cv.out[[2]]
mod.final<-as.formula(Transported ~  CryoSleep + FoodCourt 
                      + Age + Spa + ShoppingMall + VRDeck + 
                        RoomService)

################################ Forward Selection machine learning

task = as_task_classif(train.comp, target = "Transported", positive = "True")
knn_learner <- lrn("classif.kknn")
resampling = rsmp("cv", folds = 10)
measure = msr("classif.ce")
resampling$instantiate(task)

terminator = trm("stagnation", iters = 50)
instance = FSelectInstanceSingleCrit$new(
  task = task,
  learner = knn_learner,
  resampling = resampling,
  measure = measure,
  terminator = terminator)

fselector = fs("sequential")
fselector$optimize(instance)
fselector$optimization_path(instance)
as.data.table(instance$archive, exclude_columns = c("runtime_learners", "timestamp", "batch_nr", "resample_result", "uhash"))
##Only the Destination is FALSE var and is not significant in our case



################################ ii

X.train <- model.matrix(mod.final, data=shrink.data)
X.test <- model.matrix(mod.final, data=valid)
Y.train <- as.factor(shrink.data$Transported)

mod <- knn(X.train, X.test, Y.train , k = 20)
mean(valid$Transported != mod)
table (mod,valid$Transported)

mod <- knn(X.train, X.test, Y.train , k = 5)
mean(valid$Transported != mod)
table (mod,valid$Transported)

mod <- knn(X.train, X.test, Y.train , k = 80)
mean(valid$Transported != mod)
table (mod,valid$Transported)

################################ v

boot.pred.valid<-as.data.frame(boot.fn("knn",mod.final,100,shrink.data,valid,5))
boot.pred.valid<-cbind(boot.pred.valid,boot.pred.valid[,1]+1.96*boot.pred.valid[,2],boot.pred.valid[,1]-1.96*boot.pred.valid[,2])
names(boot.pred.valid)<-c("Est.prob","Std of prob","Upper 95% CI","Lower 95% CI")
boot.pred.valid

boot.pred.test<-as.data.frame(boot.fn("knn",mod.final,100,shrink.data,test,5))
boot.pred.test<-cbind(boot.pred.test,boot.pred.test[,1]+1.96*boot.pred.test[,2],boot.pred.test[,1]-1.96*boot.pred.test[,2])
names(boot.pred.test)<-c("Est.prob","Std of prob","Upper 95% CI","Lower 95% CI")
boot.pred.test



################################ iv
X.train <- model.matrix(mod.final, data=train.comp)
X.test <- model.matrix(mod.final, data=valid)
Y.train <- as.factor(train.comp$Transported)
err.knn<-rep(0,100)
for(i in 1:100){
  mod <- knn(X.train, X.test, Y.train , k = i)
  err.knn[i]<-mean(valid$Transported != mod)
  }
plot(err.knn)
points(which.min(err.knn),min(err.knn),
       pch = 18,col = "red",cex = 1.5)
which.min(err.knn)

predictors<-as.formula(paste("~",all.vars(mod.final)[-1], collapse = "+"))
X.test <- model.matrix(predictors, data=test)
pred.test <- knn(X.train, X.test, Y.train , k = 20)
submiss.g<-data.frame(test.raw$PassengerId,pred.test)
names(submiss.g)<-c("PassengerId","Transported")
write.csv(submiss.g,file="submission g.csv",row.names=FALSE)
## Score: 0.78255


