################################ Package loading

library (glmnet)

################################ Data Loading

setwd("E:/ANU Sem 3/STAT3050STAT4050STAT7050 - Advanced Statistical Learning - Sem 2 2022/A1")
data <-
  read.table(file="prostate.data.txt",header=TRUE)

################################ EDA

str(data)
train<-data[data$train==TRUE,]
test<-data[data$train==FALSE,]
train<-train[,1:9]
test<-test[,1:9]
nrow(train)
nrow(test)
nrow(data)
pairs(train)
dim(train)

################################ OLS Estimation

y<-train$lpsa
OLS<-lm(y~.,data = train)
summary(OLS)
OLS.pred<-predict(OLS,newdata = test)
mse.OLS<-mean((test$lpsa-OLS.pred)^2)
mse.OLS

################################ Ridge Estimation

x <- model.matrix (lpsa ~., data = train)

ridge <- glmnet (x, y, alpha = 0)

set.seed (1)
cv.out <- cv.glmnet (x, y, alpha = 0,type.measure = "mse")
plot (cv.out)
bestlam <- cv.out$lambda.min
bestlam

x.test <- model.matrix (lpsa~.,test)
ridge.pred <- predict (ridge , s = bestlam ,
                       newx = x.test)
mse.ridge<-mean((test$lpsa-ridge.pred)^2)
mse.ridge

coef(ridge, s = bestlam)

################################ Lasso Estimation

lasso <- glmnet (x, y, alpha = 1)
plot (lasso)

set.seed (1)
cv.out <- cv.glmnet (x, y, alpha = 1,type.measure = "mse")
plot (cv.out)
which.min(cv.out$cvm)
which(cv.out$lambda==bestlam)
bestlam <- cv.out$lambda.min
bestlam

lasso.pred <- predict (lasso , s = bestlam ,
                       newx = x.test)
mse.lasso<-mean((test$lpsa-lasso.pred)^2)
mse.lasso

coef(lasso, s = bestlam)

################################  naive elastic net
grid.search<-seq(0,1,by = 0.01)
set.seed(11)
foldid <- sample(1:10, size = length(y), replace = TRUE)
cv.result<-matrix(rep(0,length(grid.search)*2),length(grid.search),3)##matrix to store cv and lambda value

for(i in 1:length(grid.search)){
  cv.out <- cv.glmnet (x, y,foldid = foldid,alpha = grid.search[i])
  cv.result[i,1]<-cv.out$lambda.min
  cv.result[i,2]<-cv.out$cvm[which(cv.out$lambda==cv.out$lambda.min)]
  cv.result[i,3]<-grid.search[i]
  }

best.lam<-cv.result[,1][which.min(cv.result[,2])]
best.alpha<-cv.result[,3][which.min(cv.result[,2])]

elasticnet.pred <- predict (elasticnet , s = best.lam ,
                            newx = x.test)
mse.elasticnet<-mean((test$lpsa-elasticnet.pred)^2)
mse.elasticnet

coef(elasticnet, s = bestlam)

################################ elastic net


foldid <- sample(1:10, size = length(y), replace = TRUE)
cv1 <- cv.glmnet (x, y,foldid = foldid,alpha = 1)
cv.5 <- cv.glmnet (x, y,foldid = foldid,alpha = 0.5)
cv0<- cv.glmnet (x, y,foldid = foldid,alpha = 0)

cv1$lambda.min
cv.5$lambda.min
cv0$lambda.min
min(cv0$cvm)
min(cv1$cvm)
min(cv.5$cvm)
par(mfrow = c(2,2))
plot(cv1)
plot(cv.5)
plot(cv0)
plot(log(cv1$lambda)   , cv1$cvm , pch = 19, col = "red",
     xlab = "log(Lambda)", ylab = cv1$name)
points(log(cv.5$lambda), cv.5$cvm, pch = 19, col = "grey")
points(log(cv0$lambda) , cv0$cvm , pch = 19, col = "blue")
legend("topleft", legend = c("alpha= 1", "alpha= .5", "alpha 0"),
       pch = 19, col = c("red","grey","blue"))
