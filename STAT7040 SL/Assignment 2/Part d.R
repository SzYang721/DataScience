################################ GLM

################################ Package loading

library(dplyr)
library(mice)
library(leaps)
library(MASS)
library (boot)
library(class)

################################ data loading

setwd("E:/ANU Sem 2/STAT3040STAT7040 - Statistical Learning/Assignment 2")
train.comp <-
  read.csv(file="train_comp.csv",header=TRUE,stringsAsFactors=T)
valid<-
  read.csv(file="valid_comp.csv",header=TRUE,stringsAsFactors=T)
test<-
  read.csv(file="test_comp.csv",header=TRUE,stringsAsFactors=T)



##(d)

################################ i

full.mod<-as.formula(Transported~.)
null.mod<-as.formula(Transported~1)

glm.full<-glm(full.mod,data = train.comp,
              family = binomial)
glm.null <- glm(null.mod,
                data = train.comp,family = binomial
)

################################ CV

cv.err<-function(method,formula,data,K,knnfold){
  n<-nrow(data)
  set.seed(888)
  fold<-sample(rep(1:K,each = n/K))
  mse.out <- rep(0,K)
  
  ##
  for(k in 1:K){
    data.train <- data[fold!=k,]
    data.test <- data[fold==k,]
    
    if(method=="glm"){
      mod.train <- glm(formula, data = data.train,family = "binomial")
      pred.test <- predict(mod.train, newdata = data.test,type = "response")
      glm.pred<-rep("False",nrow(data.test))
      glm.pred[pred.test>0.5] = "True"
      mse.out[k] <- mean(data.test$Transported != glm.pred)
      }
    else if(method == "lda"){
      mod.train <- lda(formula, data = data.train)
      lda.class <- predict(mod.train, newdata = data.test)$class
      mse.out[k] <- mean(data.test$Transported != lda.class)
      }
    else if(method == "qda"){
      mod.train <- qda(formula, data = data.train)
      qda.class <- predict(mod.train, newdata = data.test)$class
      mse.out[k] <- mean(data.test$Transported != qda.class)
    }
    else if(method == "knn"){
      X.train <- model.matrix(formula, data =data.train)
      X.test <- model.matrix(formula, data = data.test)
      mod.out <- knn(X.train, X.test, data.train$Transported , k = knnfold)
      mse.out[k] <- mean(data.test$Transported != mod.out)
    }
  }
  mse.est <- mean(mse.out)
  
  sd.mse.est <- sd(mse.out)/sqrt(K)
  
  return(c(mse.est,sd.mse.est))
}

cv.err("glm",glm.null,train.comp,10)

################################ forward selection

method.fwd<-function(method,data,kfold){
  variable<-names(data[2:ncol(data)])
  max.steps<-length(variable)
  mod<-"Transported ~ "
  a<-1
  cv.out<-matrix(0,nrow = max.steps,ncol = 2)
  while(a<=max.steps){
    cv.list<-matrix(0,nrow = length(variable),ncol = 2)
    for(i in 1:length(variable)){
      if(a == 1){formula<-as.formula(paste(mod,variable[i]))}
      else if(a>1){formula<-as.formula(paste(mod,"+",variable[i]))}
      if(method=="glm")
        re<-cv.err("glm",formula,data,10)
      else if(method=="lda")
        re<-cv.err("lda",formula,data,10)
      else if(method == "qda")
        re<-cv.err("qda",formula,data,10)
      else if(method == "knn")
        re<-cv.err("knn",formula,data,10,knnfold = kfold)
      cv.list[i,1]<-re[1]
      cv.list[i,2]<-re[2]
    } 
    selected.var<-variable[which.min(cv.list[,1])]
    cv.out[a,1]<-min(cv.list[,1])
    cv.out[a,2]<-cv.list[which.min(cv.list[,1]),2]
    variable<-variable[-which.min(cv.list[,1])]
    mod<-paste(mod, "+",selected.var)
    a <-a +1
  }
  return(list(cv.out,mod))
}

cv.out<-method.fwd("glm",train.comp)
cv.out[[2]]
plot(cv.out[[1]][,1],type = "p",lwd = 2,cex = 1.2,
     ylab = "Cross Validation (Steps)",
     xlab = "Steps",
     main = "Forward Selection based on CV",
     ylim = c(0.21,0.28))
points(which.min(cv.out[[1]][,1]),min(cv.out[[1]][,1]),
       pch = 18,col = "red",cex = 1.5)
points(1:(ncol(train.comp)-1),cv.out[[1]][,1]+cv.out[[1]][,2],
       pch = 25,col = "red",cex = 1)
points(1:(ncol(train.comp)-1),cv.out[[1]][,1]-cv.out[[1]][,2],
       pch = 24,col = "red",cex = 1)
mod.last<-cv.out[[2]]
mod.final<-as.formula(Transported ~CryoSleep + Side + 
                        FoodCourt 
                      + VRDeck + RoomService + Spa 
                      + HomePlanet + ShoppingMall)

################################ ii

mod.final<-as.formula(Transported ~CryoSleep + Side + 
                        FoodCourt + VRDeck + RoomService + 
                        Spa + HomePlanet + ShoppingMall)
best.mod<-glm(mod.final,data = train.comp,family = "binomial")

pred.valid <- predict(best.mod, newdata = valid,type = "response")
glm.pred<-rep("False",nrow(valid))
glm.pred[pred.valid>0.5] = "True"
c.table<-table(glm.pred,valid$Transported)
c.table
glm.pred<-c(mean(valid$Transported != glm.pred),
            c.table[2,1]/(c.table[1,1]+c.table[2,1]),
            c.table[1,2]/(c.table[1,2]+c.table[2,2]))

pred.valid <- predict(best.mod, newdata = valid,type = "response")
glm.pred.0.8<-rep("False",nrow(valid))
glm.pred.0.8[pred.valid>0.8] = "True"
c.table<-table(glm.pred.0.8,valid$Transported)
c.table
glm.pred.0.8<-c(mean(valid$Transported != glm.pred.0.8),
                c.table[2,1]/(c.table[1,1]+c.table[2,1]),
                c.table[1,2]/(c.table[1,2]+c.table[2,2]))

pred.valid <- predict(best.mod, newdata = valid,type = "response")
glm.pred.0.2<-rep("False",nrow(valid))
glm.pred.0.2[pred.valid>0.2] = "True"
c.table<-table(glm.pred.0.2,valid$Transported)
c.table
glm.pred.0.2<-c(mean(valid$Transported != glm.pred.0.2),
                c.table[2,1]/(c.table[1,1]+c.table[2,1]),
                c.table[1,2]/(c.table[1,2]+c.table[2,2]))
con.out<-rbind(glm.pred,glm.pred.0.8,glm.pred.0.2)
colnames(con.out) <-  c("Mean Error Rate","False postitve rate",
                        "False negative rate")
con.out


################################ iii

summary(best.mod)
summary (best.mod)$coef
summary (best.mod)$coef[,1]+1.96*summary (best.mod)$coef[,2]
summary (best.mod)$coef[,1]-1.96*summary (best.mod)$coef[,2]

################################ iv

boots<-function(best.mod,data, K){
  nvar<-length(best.mod$coefficients)
  boot.cof<-matrix(0,nvar,K)
  boot.se<-matrix(0,nvar,K)
  for(i in 1:K){
    set.seed(i+i^2+i*2022)
    resample <- data[sample(nrow(data), nrow(data),replace = TRUE), ]
    new.mod<-glm(best.mod$formula,data = resample,family = "binomial")
    boot.cof[,i]<-summary (new.mod)$coef[,1]
    boot.se[,i]<-summary (new.mod)$coef[,2]
  }
  boot.out<-matrix(0,nvar,2)
  for(i in 1:nvar){
    boot.out[i,1]<-mean(boot.cof[i,])
    boot.out[i,2]<-sd(boot.cof[i,])
  }
  return(boot.out)
}
boot.out<-as.data.frame(boots(best.mod,train.comp,2000))
boot.out<-cbind(boot.out,boot.out[,1]+1.96*boot.out[,2],boot.out[,1]-1.96*boot.out[,2])
names(boot.out)<-c("Coefficient","Standard Error","Upper 95% CI","Lower 95% CI")
boot.out
summary(best.mod)
#larger variance and the coefficient is closed

################################ v

boot.fn <- function (method, mod, K, data, test, index){
  boot.pred<-matrix(0,K,index)
  for(i in 1:K){
    set.seed(i+i^2+i*2022)
    resample <- data[sample(nrow(data), nrow(data),replace = TRUE), ]
    if(method == "glm"){
      new.mod<-glm(mod$formula,data = resample,family = "binomial")
      pred.test<-predict(new.mod, newdata = test[1:index,],type = "response")
    }
    else if(method == "lda"){
      new.mod<-lda(mod,data = resample)
      pred.test <- predict(new.mod, newdata = test[1:index,])$posterior[,2]
    }
    else if(method == "qda"){
      new.mod<-qda(mod,data = resample)
      pred.test <- predict(new.mod, newdata = test[1:index,])$posterior[,2]
    }
    else if(method=="knn"){
      X.train <- model.matrix(mod, data=resample)
      all.vars(mod)
      predictors<-as.formula(paste("~",all.vars(mod)[-1], collapse = "+"))
      X.test <- model.matrix(predictors, data=test[1:index,])
      Y.train <- as.factor(resample$Transported)
      pred.test <- knn(X.train, X.test, Y.train , k = 20)
      pred.test<-ifelse(pred.test=="True",1,0)
    }
    for(a in 1:index){
        boot.pred[i,a]<-pred.test[a]
    }
  }
  out<-matrix(0,index,2)
  out[,1]<-apply(boot.pred,2,mean)
  out[,2]<-apply(boot.pred,2,sd)
  return(out)
}

boot.pred.valid<-as.data.frame(boot.fn("glm",best.mod,100,train.comp,valid,5))
boot.pred.valid<-cbind(boot.pred.valid,boot.pred.valid[,1]+
                         1.96*boot.pred.valid[,2],boot.pred.valid[,1]-1.96*boot.pred.valid[,2])
names(boot.pred.valid)<-c("Est.prob","Std of prob","Upper 95% CI","Lower 95% CI")
boot.pred.valid

boot.pred.test<-as.data.frame(boot.fn("glm",best.mod,100,train.comp,test,5))
boot.pred.test<-cbind(boot.pred.test,boot.pred.test[,1]+1.96*boot.pred.test[,2],boot.pred.test[,1]-1.96*boot.pred.test[,2])
names(boot.pred.test)<-c("Est.prob","Std of prob","Upper 95% CI","Lower 95% CI")
boot.pred.test

################################ vi

pred.test <- predict(best.mod, newdata = test,type = "response")
glm.pred<-rep("False",nrow(test))
glm.pred[pred.test>0.5] = "True"
submiss.d<-data.frame(test.raw$PassengerId,glm.pred)
names(submiss.d)<-c("PassengerId","Transported")
write.csv(submiss.d,file="submission d.csv",row.names=FALSE)
## Ranking: 844 Songze Yang
## Score: 0.78653
