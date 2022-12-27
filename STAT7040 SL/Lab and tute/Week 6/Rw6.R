#5.2
try<-c(5,100,10000)
p<-rep(1:3)
for(c in 1:3){
  p[c]<-1-(1-1/try[c])^try[c]
}
p

p1<-rep(1:100000)
for(x in 1:100000){
  p1[x]<-1-(1-1/x)^x
}
xaxis<-c(1:100000)
plot(xaxis,p1,ylim = c(0.6,0.7))


store <- rep (NA, 10000)
for (i in 1:10000){
  store[i] <- sum ( sample (1:100, rep =TRUE) == 4) > 0
}
mean (store)
#the result is very closed

#5.5

library(ISLR)
View(Default)
#a)
glm.fit <- glm(default ~ income
               + balance, data=Default, family=binomial)
summary(glm.fit)

#b)
nrow<-nrow(Default)
set.seed(1)
sam<-sample(1:10000,5000,replace = FALSE)
train<-Default[sam,]
test<-Default[-sam,]
glm.fit2 <- glm(default ~ income
                + balance, data=train, family=binomial)
summary(glm.fit2)
glm.probs <- predict (glm.fit2, test, type= "response")
glm.pred <- rep(0, 5000)
glm.pred<-ifelse(glm.probs > 0.5,1,0)
test.y<-ifelse(as.factor(test$default)==1,1,0)
table(glm.pred, test.y)
mean(glm.pred!= test.y)


#c)
glm.test<-function(n){
  set.seed(n)
  sam<-sample(1:10000,5000,replace = FALSE)
  train<-Default[sam,]
  test<-Default[-sam,]
  glm.fit2 <- glm(default ~ income
                  + balance, data=train, family=binomial)
  summary(glm.fit2)
  glm.probs <- predict (glm.fit2, test, type= "response")
  glm.pred <- rep(0, 5000)
  glm.pred<-ifelse(glm.probs > 0.5,1,0)
  test.y<-ifelse(as.factor(test$default)==1,1,0)
  table(glm.pred, test.y)
  mean(glm.pred!= test.y)
}
k<-c(1:3)
k[1]<-glm.test(888)
k[2]<-glm.test(777)
k[3]<-glm.test(666)
mean(k)

#d)
glm.test2<-function(n){
  set.seed(n)
  sam<-sample(1:10000,5000,replace = FALSE)
  train<-Default[sam,]
  test<-Default[-sam,]
  glm.fit2 <- glm(default ~ income
                  + balance+student, data=train, family=binomial)
  summary(glm.fit2)
  glm.probs <- predict (glm.fit2, test, type= "response")
  glm.pred <- rep(0, 5000)
  glm.pred<-ifelse(glm.probs > 0.5,1,0)
  test.y<-ifelse(as.factor(test$default)==1,1,0)
  table(glm.pred, test.y)
  mean(glm.pred!= test.y)
}
y<-c(1:3)
y[1]<-glm.test2(888)
y[2]<-glm.test2(777)
y[3]<-glm.test2(666)
mean(y)
# adding student yield a similar result, thus student is not a significant 
# variable

## 5.6
#a)
summary(glm.fit)
#the standard errors are 4.985e-06 and 2.274e-04 for income and balance.

#b)

boot.fn<-function(Default, Index){
  glm.fit<-glm(default~income+balance, data = Default, family = binomial, subset = Index)
  out<-coef(glm.fit)[2:3]
  return(out)
}
n<-nrow(Default)
Index<-sample(1:n,replace = TRUE)

boot.fn(Default,Index)

#c)
library(boot)
boot(Default,boot.fn,100)

#d)
## Using the bootstrap to estimate the coefficients give us asympototic
## normality results.

