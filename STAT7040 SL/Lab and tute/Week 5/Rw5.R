install.packages("ISLR")
library(ISLR)
data(Auto)
med.mpg <- median(Auto$mpg)
med.mpg
n<-nrow(Auto)
mpg01 <- rep(0, n)
mpg01[Auto$mpg>med.mpg] <- 1
mpg01 <- mpg01
summary(as.factor(mpg01))
Auto <- data.frame(mpg01, Auto)
head(Auto)


#b
library(ggplot2)
c <- ggplot(Auto, aes(y=mpg , x=weight, colour=factor(cylinders)))
c + geom_point()

p1 <- ggplot(Auto, aes(y=mpg , x=displacement,
                       colour=factor(cylinders))) + geom_point()
p1

p2 <- ggplot(Auto, aes(y=mpg , x=acceleration,
                       colour=factor(cylinders))) + geom_point()
p2

p3 <- ggplot(Auto, aes(y=mpg , x=year,
                       colour=factor(cylinders))) + geom_point()
p3

p4 <- ggplot(Auto, aes(factor(origin), mpg)) +
  geom_boxplot(aes(fill = factor(origin)))
p4 + scale_fill_discrete(labels=c("American", "European", "Japanese"))

boxplot(myda)
cor(Auto[, 4:9])

#c
set.seed(10)
n <- nrow(Auto)
sam <- sample(1:n, 100)
train <- Auto[-sam,]
test <- Auto[sam, ]
test.y <- test[,1]
test.X <- test[,-c(1:2)]

#d
cor(train[, c(1,4:9)])[,1]

library(MASS)
lda.fit <- lda(mpg01 ~ displacement + horsepower + weight, data=train)
lda.fit

lda.pred <- predict(lda.fit, test)
lda.class <- lda.pred$class
table(lda.class, test.y)

mean(lda.class!= test.y)
#e
qda.fit <- qda(mpg01 ~ displacement + horsepower + weight, data=train)
qda.fit

qda.pred <- predict(qda.fit, test)
qda.class <- qda.pred$class
table(qda.class, test.y)
mean(qda.class!= test.y)

#f
glm.fit <- glm(mpg01 ~ cylinders + displacement + horsepower
               + weight, data=train, family=binomial)
summary(glm.fit)
glm.probs <- predict (glm.fit, test, type= "response")
glm.pred <- rep(0, 100)
glm.pred[glm.probs > 0.5] <- 1
table(glm.pred, test.y)
mean(glm.pred!= test.y)

glm.fit2 <- glm(mpg01 ~ horsepower
               + weight, data=train, family=binomial)
summary(glm.fit2)
glm.probs2 <- predict (glm.fit2, test, type= "response")
glm.pred2 <- rep(0, 100)
glm.pred2[glm.probs2 > 0.5] <- 1
table(glm.pred2, test.y)
mean(glm.pred2!= test.y)

#g
library (class)
set.seed(100)
train.X <- train[,4:7]
train.y <- train[,1]
test.X <- test[,4:7]
test.y <- test[,1]
##
k.use <- 3:25
test.er <- rep(0, length(k.use))
for(c in 1:length(k.use)){
  knn.pred <- knn(train.X, test.X, train.y, k=k.use[c])#3 to 25 nearest neighbor
  test.er[c] <- mean(knn.pred!=test.y)
}
##
plot(k.use, test.er, type="l", lwd=3)
# the quadratic method is the best