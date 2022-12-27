##Question 6.1
#(a)
#the forward selection models with k predictors has the smallest training 
#RSS

#(b)
#We dont know

#(c)
#i.True
#ii.True
#iii.FALSE
#iv.FALSE
#v.FALSE

##Question 6.4
#(a)#iv.
#(b)#iii.
#(c)#ii
#(d)#i
#(e)#ii


##Question 6.8
#(a)
set.seed(100)
X<-rnorm(100,2,4)
e<-rnorm(100,0,0.5)




#(b)
Y<-2+5*X+2*X^2+3*X^3+e



#(c)
data<-data.frame(Y)
for (i in 1:10){
  K<-X^i
  data<-data.frame(data,K)
  
}
names(data)<-c("Y","X","X2","X3","X4","X5","X6","X7","X8","X9","X10")

library(leaps)
regfit.full<-regsubsets(Y~.,data)
reg.summary<-summary(regfit.full)
r<-which.max(reg.summary$adjr2)
c<-which.min(reg.summary$cp)
b<-which.min(reg.summary$bic)
par(mfrow = c(1,3))
plot(reg.summary$adjr2,xlab = "number of variables",
     ylab = "Adjusted R squared", type = "l")
points(r,reg.summary$adjr2[r],col = "red",cex = 2,
      pch = 20)
plot(reg.summary$cp,xlab = "number of variables",
     ylab = "CP",type = "l")
points(c,reg.summary$cp[c],col = "red",cex = 2,
       pch = 20)
plot(reg.summary$bic,xlab = "number of variables",
     ylab = "BIC", type = "l")
points(b,reg.summary$bic[b],col = "red",cex = 2,
       pch = 20)

par(mfrow = c(1,4))
plot(regfit.full,scale = "r2")
plot(regfit.full,scale = "adjr2")
plot(regfit.full,scale = "Cp")
plot(regfit.full,scale = "bic")




#(d)
regfit.fwd<-regsubsets(Y~.,data,method = "forward")
reg.summary<-summary(regfit.fwd)
r<-which.max(reg.summary$adjr2)
c<-which.min(reg.summary$cp)
b<-which.min(reg.summary$bic)
par(mfrow = c(1,3))
plot(reg.summary$adjr2,xlab = "number of variables",
     ylab = "Adjusted R squared", type = "l")
points(r,reg.summary$adjr2[r],col = "red",cex = 2,
       pch = 20)
plot(reg.summary$cp,xlab = "number of variables",
     ylab = "CP",type = "l")
points(c,reg.summary$cp[c],col = "red",cex = 2,
       pch = 20)
plot(reg.summary$bic,xlab = "number of variables",
     ylab = "BIC", type = "l")
points(b,reg.summary$bic[b],col = "red",cex = 2,
       pch = 20)




regfit.bwd<-regsubsets(Y~.,data,method = "backward")
reg.summary<-summary(regfit.bwd)
r<-which.max(reg.summary$adjr2)
c<-which.min(reg.summary$cp)
b<-which.min(reg.summary$bic)
par(mfrow = c(1,3))
plot(reg.summary$adjr2,xlab = "number of variables",
     ylab = "Adjusted R squared", type = "l")
points(r,reg.summary$adjr2[r],col = "red",cex = 2,
       pch = 20)
plot(reg.summary$cp,xlab = "number of variables",
     ylab = "CP",type = "l")
points(c,reg.summary$cp[c],col = "red",cex = 2,
       pch = 20)
plot(reg.summary$bic,xlab = "number of variables",
     ylab = "BIC", type = "l")
points(b,reg.summary$bic[b],col = "red",cex = 2,
       pch = 20)


#(e)
library(glmnet)
x<-model.matrix(Y~.,data)[,-1]
y<-data$Y
lasso.mod<-glmnet(x,y,alpha = 1)
par(mfrow = c(1,1))
plot(lasso.mod,xvar = "lambda",label = TRUE)


train<-sample(1:nrow(x),nrow(x)/2)
test<-(-train)
y.test<-y[test]
set.seed(1)

cv.out<-cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)
bestlam<-cv.out$lambda.min
out<-glmnet(x,y,alpha = 1,lambda = bestlam)
coef(out)






#(f)
set.seed(100)
Y<-2+3*X^7+e
data<-data.frame(Y)
for (i in 1:10){
  K<-X^i
  data<-data.frame(data,K)
  
}
names(data)<-c("Y","X","X2","X3","X4","X5","X6","X7","X8","X9","X10")

#best subset selection
regfit.full<-regsubsets(Y~.,data)
reg.summary<-summary(regfit.full)
r<-which.max(reg.summary$adjr2)
c<-which.min(reg.summary$cp)
b<-which.min(reg.summary$bic)
par(mfrow = c(1,3))
plot(reg.summary$adjr2,xlab = "number of variables",
     ylab = "Adjusted R squared", type = "l")
points(r,reg.summary$adjr2[r],col = "red",cex = 2,
       pch = 20)
plot(reg.summary$cp,xlab = "number of variables",
     ylab = "CP",type = "l")
points(c,reg.summary$cp[c],col = "red",cex = 2,
       pch = 20)
plot(reg.summary$bic,xlab = "number of variables",
     ylab = "BIC", type = "l")
points(b,reg.summary$bic[b],col = "red",cex = 2,
       pch = 20)

##lasso
x<-model.matrix(Y~.,data)[,-1]
y<-data$Y
lasso.mod<-glmnet(x,y,alpha = 1)
par(mfrow = c(1,1))
plot(lasso.mod)


train<-sample(1:nrow(x),nrow(x)/2)
test<-(-train)
y.test<-y[test]
set.seed(1)

cv.out<-cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)
bestlam<-cv.out$lambda.min
out<-glmnet(x,y,alpha = 1,lambda = bestlam)
coef(out)