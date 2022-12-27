################################ Package loading

library(PerformanceAnalytics)
library(mice)
library(corrplot)
library(faraway)
library(MASS)
library(leaps)
library(reshape2)
library(tidyverse)
library(ggplot2)
library(GGally)
library(ISLR)
library(corrplot)
library(Hmisc)
library(dplyr)
library(VIM)
library(lsr)
library (glmnet)
library (pls)
library(tree)
library(randomForest)
library (gbm)
library (BART)
library(keras)
library(tensorflow)
library(xgboost)
library(Matrix)
library(caret)
library(class)

################################

setwd("E:/ANU Sem 2/STAT3040STAT7040 - Statistical Learning/A Final Project")
train <-
  read.csv(file="train.csv",header=TRUE)
test<-read.csv(file="test.csv",header=TRUE)

################################ EDA

str(train)
colSums(is.na(train))
sum(duplicated(train))## no duplicated data
## summary statistics
summary(train)
n<-nrow(train)
ggpairs(train,columns = c(2:3,5:6,12:ncol(train)))
ggpairs(train,columns = c(2:4,7:11))

## boxplot showing year and price
ggplot(data = train)+
  geom_boxplot(mapping = aes(x = year, y = price))
ggplot(train, aes(x=built, y=price)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4)
ggplot(train, aes(x=year)) +
  geom_histogram(color="darkblue",position="identity",alpha=0.5,fill="white")


## the map
train$sent<-as.factor(train$sent)
ggplot(train,aes(x=lat, y=price,color=sent)) + 
  geom_point()
sum(ifelse(na.omit(train)$lat<(-2),1,0))
na.omit(train)
nrow(na.omit(train))


## distribution matrix

require(reshape2)
melt.train <- melt(train[,c(2,4,9:11)])
ggplot(data = melt.train, aes(x = value)) + 
  stat_density() + 
  facet_wrap(~variable, scales = "free")


mice_plot <- aggr(train, col=c('orange','yellow'),
                              numbers=TRUE, sortVars=TRUE,
                              labels=names(train), cex.axis=0.6,
                                gap=3, ylab=c("Missing data","Pattern"))

t_missing <- unlist(lapply(train, function(x) sum(is.na(x))))/nrow(train)
sort(t_missing[t_missing > 0], decreasing = TRUE)
## All below the 25% threhold, ready to impute
x<-cor(train[,-1], use = "pairwise.complete.obs")

symnum(abs(x),cutpoints = c(0, 0.5, 0.7, 0.95, 0.99,1),
       symbols = c(" ", ",", "+", "*", "B"),
       abbr.colnames = TRUE)
##chart.Correlation(train[,c(1:11,13:15)], histogram=TRUE, pch=19) too slow
res <- cor(train[,-1],use = "pairwise.complete.obs")
corrplot::corrplot(res, type="upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)


################################ Multiple Imputation
################################## impute the train
imp <- mice(train,m=1,maxit=0)
predM <- imp$predictorMatrix
meth <- imp$method

################################## impute the lon first

predM[, c(1,2,3)] <- 0

imp <- mice(train,m=1 ,maxit=100,predictorMatrix = predM, 
            method = meth, print =  TRUE)

train.imp<-mice::complete(imp,1)
write.csv(train.imp,file="train.imp.csv",row.names=FALSE)
train.imp <-
  read.csv(file="train.imp.csv",header=TRUE,stringsAsFactors=T)

## adjust variables
train.imp$reno<-as.factor(train.imp$reno)

################################## impute the test

test$reno<-as.factor(test$reno)

imp.t <- mice(test, maxit=0)
predM.t <- imp.t$predictorMatrix
meth.t <- imp.t$method

##################################

predM.t[, 1] <- 0

imp.t<- mice(test, m=1,maxit = 100, 
            predictorMatrix = predM.t, 
            method = meth.t, print =  FALSE)
test.comp<-mice::complete(imp.t,1)
write.csv(test.comp,file="test.imp.csv",row.names=FALSE)
test.imp <-
  read.csv(file="test.imp.csv",header=TRUE,stringsAsFactors=T)
test.imp$reno<-as.factor(test.imp$reno)
##################################
## outlier

par(mfrow=c(2,8))
for(i in 1:ncol(train.imp)-1){
  plot(train.imp[,i+1],ylab = colnames(train.imp)[i+1])
}
## detect the outliers
## reno is imbalanced
par(mfrow=c(1,1))

plot(train.imp$lat,train.imp$lon)
plot(train.imp$lat,train.imp$lon,ylim=c(-4.05326,-4.05366),xlim=c(-3.1656,-3.166))
plot(train.imp$lat,train.imp$lon,ylim=c(0,1),xlim=c(-3.16,-3.18))
identify(train.imp$lat, train.imp$lon, labels=row.names(train.imp))
sum(ifelse(train.imp$lon<(-2),1,0))
sum(ifelse(train.imp$lat<(-2.5),1,0))
##12084
################################################################
train.imp$lat[12084]<-mean(train.imp$lat)
################################################################
plot(train.imp$bedrooms,train.imp$bathrooms)
identify(train.imp$bedrooms, train.imp$bathrooms, labels=row.names(train.imp))
##15189 15786 22011
plot(train.imp$bedrooms,train.imp$sq.m.h)
plot(train.imp$bathrooms,train.imp$sq.m.h)

plot(test.imp$lat,test.imp$lon)

## Feature Scaling
train.imp$sq.m.h<-log(train.imp$sq.m.h)
train.imp$sq.m.block<-log(train.imp$sq.m.block)
train.imp$sq.m.pool<-log(train.imp$sq.m.pool+0.1)

train.imp$sq.m.h<-(train.imp$sq.m.h-mean(train.imp$sq.m.h))/sd(train.imp$sq.m.h)
train.imp$sq.m.block<-(train.imp$sq.m.block-mean(train.imp$sq.m.block))/sd(train.imp$sq.m.block)
train.imp$sq.m.pool<-(train.imp$sq.m.pool-mean(train.imp$sq.m.pool))/sd(train.imp$sq.m.pool)

plot(train.imp$sq.m.h)
abline(mean(train.imp$sq.m.h)+2.56*sd(train.imp$sq.m.h),0,col="red")
abline(mean(train.imp$sq.m.h)-2.56*sd(train.imp$sq.m.h),0,col="red")
plot(train.imp$sq.m.block)
abline(mean(train.imp$sq.m.block)+2.56*sd(train.imp$sq.m.block),0,col="red")
abline(mean(train.imp$sq.m.block)-2.56*sd(train.imp$sq.m.block),0,col="red")
plot(train.imp$sq.m.pool)
abline(mean(train.imp$sq.m.pool)+2.56*sd(train.imp$sq.m.pool),0,col="red")
abline(mean(train.imp$sq.m.pool)-2.56*sd(train.imp$sq.m.pool),0,col="red")
plot(train.imp$price)

test.imp$sq.m.h<-log(test.imp$sq.m.h)
test.imp$sq.m.block<-log(test.imp$sq.m.block)
test.imp$sq.m.pool<-log(test.imp$sq.m.pool+0.1)

test.imp$sq.m.h<-(test.imp$sq.m.h-mean(test.imp$sq.m.h))/sd(test.imp$sq.m.h)
test.imp$sq.m.block<-(test.imp$sq.m.block-mean(test.imp$sq.m.block))/sd(test.imp$sq.m.block)
test.imp$sq.m.pool<-(test.imp$sq.m.pool-mean(test.imp$sq.m.pool))/sd(test.imp$sq.m.pool)

write.csv(train.imp,file="train.imp.csv",row.names=FALSE)
write.csv(test.imp,file="test.imp.csv",row.names=FALSE)
train.imp <-
  read.csv(file="train.imp.csv",header=TRUE,stringsAsFactors=T)
train.imp$reno<-as.factor(train.imp$reno)
test.imp <-
  read.csv(file="test.imp.csv",header=TRUE,stringsAsFactors=T)
test.imp$reno<-as.factor(test.imp$reno)
# Model 1
# Regression 

################################## prediction for price
################################## linear model
## Validation set method
set.seed (1)
train.1 <- sample (c(TRUE , FALSE), nrow (train.imp),
                   replace = TRUE)
test.1 <- (!train.1)

regfit.best <- regsubsets (price ~.,
                           data = train.imp[train.1,c(-1,-3)], nvmax = ncol(train.imp)-2)
summary (regfit.best)

test.mat <- model.matrix (price ~., data = train.imp[test.1, c(-1,-3)])

predict.regsubsets <- function (object , newdata , id, ...) {
  form <- as.formula (object$call[[2]])
  mat <- model.matrix (form , newdata)
  coefi <- coef (object , id = id)
  xvars <- names (coefi)
  mat[, xvars] %*% coefi
}

val.errors <- rep (NA, 12)
for (i in 1:12) {
  coefi <- coef (regfit.best , id = i)
  pred <- test.mat[, names (coefi)] %*% coefi
  val.errors[i] <- mean ((train.imp$price[test.1] - pred)^2)
}
i<-which.min (val.errors)
plot(val.errors)
coef (regfit.best , i)

## k-fold validation
k <- 10
n <- nrow (train.imp)
set.seed (1)
folds <- sample (rep (1:k, length = n))
cv.errors <- matrix(NA, k, (ncol(train.imp)-2),
                       dimnames = list (NULL , paste (1:(ncol(train.imp)-2))))

for (j in 1:k) {
  best.fit <- regsubsets (price ~ .,
                        data = train.imp[folds != j, c(-1,-3)],
                        nvmax = (ncol(train.imp)-2))
  for(i in 1:(ncol(train.imp)-3)) {
    pred <- predict (best.fit , train.imp[folds == j, c(-1,-3)], id = i)
    cv.errors[j, i] <-mean ((train.imp$price[folds == j] - pred)^2)
    }
  }
mean.cv.errors <- apply (cv.errors , 2, mean)
mean.cv.errors
which.min(mean.cv.errors[1:12])
par (mfrow = c(1, 1))
plot (mean.cv.errors , type = "b")

##our best is i=5
best.fit <- regsubsets (price ~ .,
                        data = train.imp[,c(-1,-3)],nvmax = ncol(train.imp)-3)
coef (best.fit , id = 8)
test.mat <- model.matrix (price ~., data = test.imp)
pred<-predict.regsubsets(best.fit,test.imp,id = 9)
sub.lm<-data.frame(test$id,pred)
names(sub.lm)<-c("id","price")
write.csv(sub.lm,file="submission lm.csv",row.names=FALSE)

################################## shrinkage method
### Ridge regression

x <- model.matrix (price ~., train.imp)[, -c(1:3)]
y<-train.imp$price

ridge.mod <- glmnet (x, y, alpha = 0)

set.seed (1)
cv.out <- cv.glmnet (x, y, alpha = 0)
plot (cv.out)
bestlam <- cv.out$lambda.min

x.test <- model.matrix (id~.,test.imp)[,-1]
ridge.pred <- predict (ridge.mod , s = bestlam ,
                       newx = x.test)
## Lasso

lasso.mod <- glmnet (x, y, alpha = 1)
plot (lasso.mod)

set.seed (1)
cv.out <- cv.glmnet (x, y, alpha = 1)
plot (cv.out)
bestlam <- cv.out$lambda.min
lasso.pred <- predict (lasso.mod , s = bestlam ,
                       newx = )

## PCR
set.seed (2)
pcr.fit <- pcr(price~., data = train.imp[,c(-1,-3)], scale = TRUE,
                validation = "CV")
summary (pcr.fit)
validationplot (pcr.fit, val.type ="MSEP")
pcr.pred <- predict (pcr.fit , test.imp, ncomp = 12)

## PLS
set.seed(1)
pls.fit <- plsr (price~., data = train.imp[,c(-1,-3)] ,
                 scale = TRUE , validation = "CV")
summary (pls.fit)
validationplot (pls.fit , val.type = "MSEP")
pls.pred <- predict (pls.fit , test.imp, ncomp = 4)

################################## tree model
### trees

tree.reg<-tree(price~.,data = train.imp[,c(-1,-3)])
plot(tree.reg)
text(tree.reg,pretty = 33)
cv.regtree<-cv.tree(tree.reg,K=10)
plot(cv.regtree$size,cv.regtree$dev,type = "b")
prune.regtree<-prune.tree(tree.reg,best=8)
plot(prune.regtree)
text(prune.regtree,pretty = 0)

## bagging
set.seed(1)
bag.regtree<-randomForest(price~.,data=train.imp[,c(-1,-3)]
                          ,mtry=12,importance = TRUE,ntree =50)
plot(bag.regtree$mse)
x<-c(1:50)
bag.result<-cbind(x,bag.regtree$mse)
write.csv(bag.result,file="bagtree result.csv",row.names=FALSE)
bag.result<-read.csv(file="bagtree result.csv",header=TRUE)
plot(bag.result)

bag.500trees <- predict (bag.regtree, newdata = test.imp)

bag.regtree$mse[100]
bag.regtree<-randomForest(price~.,data=train.imp[,c(-1,-3)]
                          ,mtry=12,importance = TRUE,ntree =100)
bag.100trees <- predict(bag.regtree, newdata = test.imp)

sub.bagtree<-data.frame(test$id,bag.100trees)
names(sub.bagtree)<-c("id","price")
write.csv(sub.bagtree,file="submission bagtree.csv",row.names=FALSE)

## random forest 
set.seed(1)
rf.4 <- randomForest (price~., data = train.imp[,c(-1,-3)],
                          mtry = 4, importance = TRUE)
plot(rf.4,xlim = c(250,300))
pred.rf.4 <- predict(rf.4, newdata = test.imp)

sub.rf.4<-data.frame(test$id,pred.rf.4)
names(sub.rf.4)<-c("id","price")
write.csv(sub.rf.4,file="submission rf 4.csv",row.names=FALSE)


rf.4.50 <- randomForest (price~., data = train.imp[,c(-1,-3)],
                           mtry = 4, importance = TRUE,ntree = 50)
x<-c(1:50)
rf.result<-cbind(x,rf.4.50$mse)
write.csv(rf.result,file="rf result.csv",row.names=FALSE)
rf.result<-read.csv(file="rf result.csv",header=TRUE)
plot(rf.result)
pred.rf.4.50 <- predict(rf.4.50, newdata = test.imp)
sub.rf.4.50<-data.frame(test$id,pred.rf.4.50)
names(sub.rf.4.50)<-c("id","price")
write.csv(sub.rf.4.50,file="submission rf 4 50.csv",row.names=FALSE)


## boosting 

set.seed (1)
boost<- gbm (price~., data = train.imp[,c(-1,-3)],
             distribution = "gaussian",n.trees = 100,
             interaction.depth = 4,cv.folds = 10)
plot(boost$cv.error[50])
x<-c(1:100)
boost.result<-cbind(x,boost$train.error)
write.csv(boost.result,file="boost result price.csv",row.names=FALSE)
boost.result<-read.csv(file="boost result price.csv",header=TRUE)
plot(boost.result)

yhat.boost <- predict (boost,
                       newdata = test.imp, n.trees = 240)
sub.boost<-data.frame(test$id,yhat.boost)
names(sub.boost)<-c("id","price")
write.csv(sub.boost,file="submission boost.csv",row.names=FALSE)



set.seed(1)
boost.adj <- gbm (price~., data = train.imp[,c(-1,-3)],
                     distribution = "gaussian", n.trees = 5000,
                     interaction.depth = 4, shrinkage = 0.2, verbose = F)
yhat.boost.adj <- predict (boost.adj ,
                       newdata = test.imp, n.trees = 5000)
sub.boost.adj<-data.frame(test$id,yhat.boost.adj)
names(sub.boost.adj)<-c("id","price")
write.csv(sub.boost.adj,file="submission boost adj.csv",row.names=FALSE)
## this do not make improvement


## Bayesian Additive Regression Trees

x <- train.imp[, 4:15]
y <- train.imp[, "price"]
set.seed (1)
bartfit <- gbart (x , y , x.test = test.imp[,-1])
yhat.bart <- bartfit$yhat.test.mean
sub.bart<-data.frame(test$id,yhat.bart)
names(sub.bart)<-c("id","price")
write.csv(sub.bart,file="submission BART.csv",row.names=FALSE)


################################## deep learning

n<-nrow(train.imp)
set.seed (13)
ntest <- trunc(n/3)
testid <- sample (1:n, ntest)

x <- model.matrix (price ~.-1, data = train.imp[,c(-1,-3)])
y <- train.imp$price

modnn <- keras_model_sequential () %>%
  layer_dense (units = 50, activation = "relu",
               input_shape = ncol (x)) %>%
  layer_dropout (rate = 0.2) %>%
  #layer_dense (units = 60, activation = "relu") %>%
  #layer_dropout (rate = 0.4) %>%
  #layer_dense (units = 12, activation = "relu") %>%
  #layer_dropout (rate = 0.4) %>%
  layer_dense (units = 1)

modnn %>% compile (loss = "mse",
                   optimizer = "adam",
                   metrics = list ("mean_squared_error")
                  )

history <- modnn %>% fit(
  x[-testid , ], y[-testid], epochs = 500, batch_size = 50,
  validation_data = list (x[testid , ], y[testid])
)
 ## 50 without scale
price<-rep(0,nrow(test.imp))
test.withp<-cbind(price,test.imp)
x.test<-model.matrix(price~.-1,data = test.imp[,-1])%>%scale()
npred <- predict (modnn , x.test)
sub.nn.single<-data.frame(test$id,npred)
names(sub.nn.single)<-c("id","price")
write.csv(sub.nn.single,file="submission NN single.csv",row.names=FALSE)


plot (history)
x.test<-model.matrix(price~.-1,data = test.imp[,-1])%>%scale()
npred <- predict (modnn , x.test)
sub.nn.2<-data.frame(test$id,npred)
names(sub.nn.2)<-c("id","price")
write.csv(sub.nn.2,file="submission NN 2.csv",row.names=FALSE)



################################## XGboost method
## Xgboost first try with cv

traindata1 <- data.matrix(train[,c(4:ncol(train))]) 
traindata2 <- Matrix(traindata1,sparse=T)
traindata3 <- data.matrix(train[,2])
traindata4 <- list(data=traindata2,label=traindata3)
dtrain <- xgb.DMatrix(data = traindata4$data, label = traindata4$label)

testdata1 <- data.matrix(test[,c(2:ncol(test))])
dtest <- xgb.DMatrix(data = testdata1)

set.seed(1200)
xgb_param<-list(objective="reg:squarederror")
xgb_cv<-xgb.cv(params=xgb_param,data=dtrain,nrounds=3000,nfold=10,showsd=T,stratified=T,print_every_n=40,
               early_stopping_rounds = 50,maximize=F)
xgb_model<-xgb.train(data=dtrain, params=xgb_param,nrounds =131)
pred.missing<-predict(xgb_model,dtest)
sub.XG.missing<-data.frame(test$id,pred.missing)
names(sub.XG.missing)<-c("id","price")
write.csv(sub.XG.2,file="submission XGboost missing.csv",row.names=FALSE)


train_control<-trainControl(method = "cv",number = 5, search = "grid")

set.seed(50)
gbmGrid<-expand.grid(eta = 0.3,
                     max_depth=c(3,5,7),
                     nrounds = (1:10)*50,
                     #default value
                     gamma = 0,
                     subsample = 1,
                     min_chlid_weight=1,
                     colsample_bytree = 0.6)

model<-train(price~., data = train.imp[,c(-1,-3)],method = "xgbTree",
             trControl = train_control,)
print(model)

set.seed(1200)
xgb_param<-list(objective="reg:squarederror",
                eta=0.3,
                subsample=1,
                min_child_weight=1,
                colsample_bytree=0.8,
                eval_m
                
                )
xgb_cv<-xgb.cv(params=xgb_param,data=dtrain,nrounds=3000,nfold=10,showsd=T,stratified=T,print_every_n=40,
               early_stopping_rounds = 10,maximize=F)
xgb_model<-xgb.train(data=dtrain, params=xgb_param,nrounds =89)
pred.tuned<-predict(xgb_model,dtest)
sub.XG.m.tuned<-data.frame(test$id,pred.tuned)
names(sub.XG.m.tuned)<-c("id","price")
write.csv(sub.XG.m.tuned,file="submission XGboost missing tuned.csv",row.names=FALSE)

######################################################### prediction for sent
################################################# GLM LDA QDA
## Validation set method
train.imp$sent<-as.factor(train.imp$sent)
train.imp$reno<-as.factor(train.imp$reno)
full.mod<-as.formula(sent~.)
null.mod<-as.formula(sent~1)

glm.full<-glm(full.mod,data = train.imp,
              family = binomial)
glm.null <- glm(null.mod,
                data = train.imp,family = binomial
)
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
      glm.pred<-rep(0,nrow(data.test))
      glm.pred[pred.test>0.5] = 1
      mse.out[k] <- mean(data.test$sent != glm.pred)
    }
    else if(method == "lda"){
      mod.train <- lda(formula, data = data.train)
      lda.class <- predict(mod.train, newdata = data.test)$class
      mse.out[k] <- mean(data.test$sent != lda.class)
    }
    else if(method == "qda"){
      mod.train <- qda(formula, data = data.train)
      qda.class <- predict(mod.train, newdata = data.test)$class
      mse.out[k] <- mean(data.test$sent != qda.class)
    }
    else if(method == "knn"){
      X.train <- model.matrix(formula, data =data.train)
      X.test <- model.matrix(formula, data = data.test)
      mod.out <- knn(X.train, X.test, data.train$sent , k = knnfold)
      mse.out[k] <- mean(data.test$sent != mod.out)
    }
  }
  mse.est <- mean(mse.out)
  
  sd.mse.est <- sd(mse.out)/sqrt(K)
  
  return(c(mse.est,sd.mse.est))
}

cv.err("knn",train.imp,10,20)

method.fwd<-function(method,data,kfold){
  variable<-names(data[4:ncol(data)])
  max.steps<-length(variable)
  mod<-"sent ~ "
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

cv.out<-method.fwd("glm",train.imp,10)
cv.out[[2]]
plot(cv.out[[1]][,1],type = "p",lwd = 2,cex = 1.2,
     ylab = "Cross Validation (Steps)",
     xlab = "Steps",
     main = "Forward Selection based on CV",
     ylim = c(0.2,0.23))
points(which.min(cv.out[[1]][,1]),min(cv.out[[1]][,1]),
       pch = 18,col = "red",cex = 1.5)
points(1:(ncol(train.imp)-3),cv.out[[1]][,1]+cv.out[[1]][,2],
       pch = 25,col = "red",cex = 1)
points(1:(ncol(train.imp)-3),cv.out[[1]][,1]-cv.out[[1]][,2],
       pch = 24,col = "red",cex = 1)
mod.last<-cv.out[[2]]
glm.final<-glm(sent~sq.m.h + built + lon + bedrooms,data = train.imp,
               family = binomial)

################################## shrinkage method
### LDA
train.imp <-
  read.csv(file="train.imp.csv",header=TRUE,stringsAsFactors=T)
test.imp <-
  read.csv(file="test.imp.csv",header=TRUE,stringsAsFactors=T)
train.scale<-as.data.frame(scale(train.imp))
cv.out<-method.fwd("lda",train.scale,10)
cv.out[[2]]
plot(cv.out[[1]][,1],type = "p",lwd = 2,cex = 1.2,
     ylab = "Cross Validation (Steps)",
     xlab = "Steps",
     main = "Forward Selection based on CV",
     ylim = c(0.2,0.23))
points(which.min(cv.out[[1]][,1]),min(cv.out[[1]][,1]),
       pch = 18,col = "red",cex = 1.5)
points(1:(ncol(train.imp)-3),cv.out[[1]][,1]+cv.out[[1]][,2],
       pch = 25,col = "red",cex = 1)
points(1:(ncol(train.imp)-3),cv.out[[1]][,1]-cv.out[[1]][,2],
       pch = 24,col = "red",cex = 1)
mod.last<-cv.out[[2]]
mod.train <- lda(formula, data = data.train)
lda.class <- predict(mod.train, newdata = data.test)$class
mse.out[k] <- mean(data.test$sent != lda.class)
mod.final<-as.formula(sent ~  + sq.m.h + built + bedrooms + sq.m.pool)
best.mod<-lda(mod.final,data = train.imp)
plot(best.mod, type = "both")
pred<- predict(best.mod, newdata = train.imp)
lda.class<-pred$class
group0<-train.imp[lda.class==1,]$sq.m.h
group1<-train.imp[lda.class==0,]$sq.m.h

hgA <- hist(group0, plot = FALSE) # Save first histogram data
hgB <- hist(group1, plot = FALSE) # Save 2nd histogram data

plot(hgA,col=rgb(0,0,1,1/4)) # Plot 1st histogram using a transparent color
plot(hgB, col=rgb(1,0,0,1/4),add = TRUE) # Add 2nd histogram using different color
abline(v = mean(best.mod$means[,1]),col = "orange")


hist(group1$sq.m.h,probability = TRUE)
lines(group1$sq.m.h)
hist(pos$price)
a<-mean(best.mod$means[1,])
abline(v = a, )
abline()
summary(best.mod)


## QDA
cv.out<-method.fwd("qda",train.imp,10)
cv.out[[2]]
plot(cv.out[[1]][,1],type = "p",lwd = 2,cex = 1.2,
     ylab = "Cross Validation (Steps)",
     xlab = "Steps",
     main = "Forward Selection based on CV",
     ylim = c(0.2,0.23))
points(which.min(cv.out[[1]][,1]),min(cv.out[[1]][,1]),
       pch = 18,col = "red",cex = 1.5)
points(1:(ncol(train.imp)-3),cv.out[[1]][,1]+cv.out[[1]][,2],
       pch = 25,col = "red",cex = 1)
points(1:(ncol(train.imp)-3),cv.out[[1]][,1]-cv.out[[1]][,2],
       pch = 24,col = "red",cex = 1)
mod.last<-cv.out[[2]]
mod.final<-as.formula(Transported ~CryoSleep + Side + 
                        FoodCourt 
                      + VRDeck + RoomService + Spa 
                      + HomePlanet + ShoppingMall)

## boost tree
set.seed (1)
boost<- gbm (sent~., data = train.imp[,c(-1,-2)],
             distribution = "bernoulli",n.trees = 500,
             interaction.depth = 4,verbose = 1,cv.folds = 10)
plot(boost$train.error[500])
yhat.boost <- predict (boost,
                       newdata = test.imp, n.trees = 5000,type = "response")
boost.pred<-rep(0,nrow(test))
boost.pred[yhat.boost>0.5] = 1
sub.boost.sent<-data.frame(test$id,boost.pred)
names(sub.boost.sent)<-c("id","sent")
write.csv(sub.boost.sent,file="submission boost sent.csv",row.names=FALSE)

x<-c(1:500)
boost.result.sent<-cbind(x,boost$cv.error)
write.csv(boost.result.sent,file="boost result sent.csv",row.names=FALSE)
boost.result.sent<-read.csv(file="boost result sent.csv",header=TRUE)
plot(boost.result.sent)

## Xgboost

traindata1 <- data.matrix(train.imp[,c(4:ncol(train.imp))])
traindata2 <- Matrix(traindata1,sparse=T) 
traindata3 <- data.matrix(train.imp[,3]) 
traindata4 <- list(data=traindata2,label=traindata3) 
dtrain <- xgb.DMatrix(data = traindata4$data, label = traindata4$label) 

testdata1 <- data.matrix(test.imp[,c(2:ncol(test.imp))]) 
dtest <- xgb.DMatrix(data = testdata1)

set.seed(1200)
xgb_param<-list(objective="binary:logistic")
xgb_cv<-xgb.cv(params=xgb_param,data=dtrain,nrounds=3000,nfold=10,showsd=T,stratified=T,print_every_n=40,
               early_stopping_rounds = 10,maximize=F)
xgb_model<-xgb.train(data=dtrain, params=xgb_param,nrounds =195)
pred<-predict(xgb_model,dtest)
boost.pred<-rep(0,nrow(test))
boost.pred[pred<0.5] = 1
sub.XG.sent<-data.frame(test$id,boost.pred)
names(sub.XG.sent)<-c("id","sent")
write.csv(sub.XG.sent,file="sent XGboost.csv",row.names=FALSE)
