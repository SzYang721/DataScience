################################ LDA

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



##(e)

################################ i

full.mod<-as.formula(Transported~.)
null.mod<-as.formula(Transported~1)

cv.out<-method.fwd("lda",train.comp)

plot(cv.out[[1]][,1],type = "p",lwd = 2,cex = 1.2,
     ylab = "Cross Validation (Steps)",
     xlab = "Steps",
     main = "Forward Selection based on CV",
     ylim = c(0.21,0.28)
     )
points(which.min(cv.out[[1]][,1]),min(cv.out[[1]][,1]),
       pch = 18,col = "red",cex = 1.5)
points(1:(ncol(train.comp)-1),cv.out[[1]][,1]+cv.out[[1]][,2],
       pch = 25,col = "red",cex = 1)
points(1:(ncol(train.comp)-1),cv.out[[1]][,1]-cv.out[[1]][,2],
       pch = 24,col = "red",cex = 1)
mod.lowcv<-cv.out[[2]]
mod.final<-as.formula(Transported ~  CryoSleep + Side + 
                        FoodCourt + VRDeck + RoomService + 
                        Spa + HomePlanet + ShoppingMall)

################################ ii

best.mod<-lda(mod.final,data = train.comp)
pred.valid <- predict(best.mod, newdata = valid)
lda.class<-pred.valid$class
mean(valid$Transported != lda.class)
c.table<-table (lda.class,valid$Transported)
c.table
lda.pred.default<-c(mean(valid$Transported != lda.class),
            c.table[2,1]/(c.table[1,1]+c.table[2,1]),
            c.table[1,2]/(c.table[1,2]+c.table[2,2]))

lda.pred<-rep("False",nrow(valid))
lda.pred[pred.valid$posterior[, 2] > .8] = "True"
mean(valid$Transported != lda.pred)
c.table<-table(lda.pred,valid$Transported)
c.table
lda.pred.0.8<-c(mean(valid$Transported != lda.pred),
            c.table[2,1]/(c.table[1,1]+c.table[2,1]),
            c.table[1,2]/(c.table[1,2]+c.table[2,2]))

lda.pred<-rep("False",nrow(valid))
lda.pred[pred.valid$posterior[, 2] > .2] = "True"
mean(valid$Transported != lda.pred)
table(lda.pred,valid$Transported)
c.table
lda.pred.0.8<-c(mean(valid$Transported != lda.pred),
                c.table[2,1]/(c.table[1,1]+c.table[2,1]),
                c.table[1,2]/(c.table[1,2]+c.table[2,2]))
con.out<-rbind(lda.pred.default,lda.pred.0.8,lda.pred.0.8)
colnames(con.out) <-  c("Mean Error Rate","False postitve rate",
                        "False negative rate")
con.out

################################ v

boot.pred.valid<-as.data.frame(boot.fn("lda",mod.final,100,train.comp,valid,5))
boot.pred.valid<-cbind(boot.pred.valid,boot.pred.valid[,1]+1.96*boot.pred.valid[,2],boot.pred.valid[,1]-1.96*boot.pred.valid[,2])
names(boot.pred.valid)<-c("Est.prob","Std of prob","Upper 95% CI","Lower 95% CI")
View(boot.pred.valid)

boot.pred.test<-as.data.frame(boot.fn("lda",mod.final,100,train.comp,test,5))
boot.pred.test<-cbind(boot.pred.test,boot.pred.test[,1]+1.96*boot.pred.test[,2],boot.pred.test[,1]-1.96*boot.pred.test[,2])
names(boot.pred.test)<-c("Est.prob","Std of prob","Upper 95% CI","Lower 95% CI")
View(boot.pred.test)


################################ iv

pred.test <- predict(best.mod, newdata = test)$class
submiss.e<-data.frame(test.raw$PassengerId,pred.test)
names(submiss.e)<-c("PassengerId","Transported")
write.csv(submiss.e,file="submission e.csv",row.names=FALSE)
## Score: 0.78653


