################################ QDA

################################ Package loading

library(dplyr)
##install.packages("mice")
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


##(f)

################################ i

full.mod<-as.formula(Transported~.)
null.mod<-as.formula(Transported~1)

cv.out<-method.fwd("qda",train.comp)

plot(cv.out[[1]][,1],type = "p",lwd = 2,cex = 1.2,
     ylab = "Cross Validation (Steps)",
     xlab = "Steps",
     main = "Forward Selection based on CV",
     ylim = c(0.21,0.30)
)
points(which.min(cv.out[[1]][,1]),min(cv.out[[1]][,1]),
       pch = 18,col = "red",cex = 1.5)
points(1:(ncol(train.comp)-1),cv.out[[1]][,1]+cv.out[[1]][,2],
       pch = 25,col = "red",cex = 1)
points(1:(ncol(train.comp)-1),cv.out[[1]][,1]-cv.out[[1]][,2],
       pch = 24,col = "red",cex = 1)
mod.lowcv<-cv.out[[2]]
mod.final<-as.formula(Transported ~ CryoSleep + Side + Age
                      + RoomService + VRDeck + Spa)

################################ ii

best.mod<-qda(mod.final,data = train.comp)
pred.valid <- predict(best.mod, newdata = valid)
qda.class<-pred.valid$class
mean(valid$Transported != qda.class)
table (qda.class,valid$Transported)
pred.valid$posterior[1:20, 1]
sum (pred.valid$posterior[, 2] > .9)


qda.pred<-rep("False",nrow(valid))
qda.pred[pred.valid$posterior[, 2] > .9] = "True"
mean(valid$Transported != qda.pred)
table(qda.pred,valid$Transported)


qda.pred<-rep("False",nrow(valid))
qda.pred[pred.valid$posterior[, 2] > .1] = "True"
mean(valid$Transported != qda.pred)
table(qda.pred,valid$Transported)

################################ v

boot.pred.valid<-as.data.frame(boot.fn("qda",mod.final,100,train.comp,valid,5))
boot.pred.valid<-cbind(boot.pred.valid,boot.pred.valid[,1]+1.96*boot.pred.valid[,2],boot.pred.valid[,1]-1.96*boot.pred.valid[,2])
names(boot.pred.valid)<-c("Est.prob","Std of prob","Upper 95% CI","Lower 95% CI")
View(boot.pred.valid)

boot.pred.test<-as.data.frame(boot.fn("qda",mod.final,100,train.comp,test,5))
boot.pred.test<-cbind(boot.pred.test,boot.pred.test[,1]+1.96*boot.pred.test[,2],boot.pred.test[,1]-1.96*boot.pred.test[,2])
names(boot.pred.test)<-c("Est.prob","Std of prob","Upper 95% CI","Lower 95% CI")
View(boot.pred.test)


################################ iv

pred.test <- predict(best.mod, newdata = test)$class
submiss.f<-data.frame(test.raw$PassengerId,pred.test)
names(submiss.f)<-c("PassengerId","Transported")
write.csv(submiss.f,file="submission f.csv",row.names=FALSE)
## Score: 0.77577




