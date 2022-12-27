rm(list = ls())

setwd("E:/ANU Sem 2/STAT3040STAT7040 - Statistical Learning/Assignment 1")
Data <-
  read.table("Data.txt",header=TRUE,stringsAsFactors=TRUE
  )

train.data = Data[1:300,]
test.data = Data[301:length(Data$Y),]
Data$Y<-log(Data$Y)
library(tidyverse)
ggplot(data = train.data)

#a)
plot(train.data$x1)
identify(train.data$x1)






mod<-lm(Y~x1+x2+x3+x4+x5+x6+x7,data = train.data)
par(mfrow = c(2,2))
plot(mod,which = c(1:4))
#the residual plot suggests the non-constant variance problem

library(MASS)
boxcox(Y~x1+x2+x3+x4+x5+x6+x7,data = train.data,plotit = T)
#box cox indicates log transformation

train.data$Y<-log(train.data$Y)
pairs(train.data[,1:8], col = "red")
#no curvature is manifested in the dataset


#b)
par(mfrow = c(1,1))
mod2<-lm(Y~x3,data = train.data)
mod21<-lm(Y~x3+I(x3^2),data = train.data)
summary(mod21)
mod22<-lm(Y~x3+I(x3^2)+I(x3^3),data = train.data)
summary(mod22)
mod23<-lm(Y~x3+I(x3^2)+I(x3^3)+I(x3^4),data = train.data)
summary(mod23)


sort.x3 <- sort(train.data$x3)
fit.lm25 <- mod25$coef[1] + mod25$coef[2]*sort.x3 +
  mod25$coef[3]*sort.x3^2+mod25$coef[3]*sort.x3^3+
  mod25$coef[3]*sort.x3^4
lines(sort.x3, fit.lm25, type="l", lwd=3, col="blue")

mod24<-lm(log(Y)~x3,data = train.data)
summary(mod24)
plot(train.data$x3,log(train.data$Y),data = train.data)
abline(mod24$coefficients,col = "red")
plot(train.data$x3,train.data$Y,data = train.data)

plot(train.data$x3,train.data$Y, pch=16, col="red", cex.lab=1.5)
mod25<-lm(Y~x3+I(x3^2)+I(x3^3)+I(x3^4)+I(x3^5),data = train.data)
summary(mod25)
sort.x3 <- sort(train.data$x3)
fit.lm25 <- mod25$coef[1] + mod25$coef[2]*sort.x3 +
  mod25$coef[3]*sort.x3^2+mod25$coef[3]*sort.x3^3+
  mod25$coef[3]*sort.x3^4+mod25$coef[3]*sort.x3^5
lines(sort.x3, fit.lm25, type="l", lwd=3, col="blue")



mod26<-lm(Y~x3,data = train.data)
summary(mod26)
plot(train.data$x3,log(train.data$Y), pch=16, col="red", cex.lab=1.5)
abline(mod26$coefficients,col = "blue")



plot(x3, Y,data = train.data)
lines(mod21$coefficients)

#c)

#d)


