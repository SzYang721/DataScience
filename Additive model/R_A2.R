##### Package Loading

library(dplyr)
library(tidyverse)
library(mgcv)
library(npreg)
library(np)
library(data.table)

ksmooth.gcv <- function(x, y){
  nobs <- length(y)
  xs <- sort(x, index.return = TRUE)
  x <- xs$x
  y <- y[xs$ix]
  xdif <- outer(x, x, FUN = "-")
  tune.ksmooth <- function(h){
    xden <- dnorm(xdif / h)
    xden <- xden / rowSums(xden)
    df <- sum(diag(xden))
    fit <- xden %*% y
    mean((fit - y)^2) / (1 - df/nobs)^2
  }
  xrng <- diff(range(x))
  oh <- optimize(tune.ksmooth, interval = c(xrng/nobs, xrng))$minimum
  if(any(oh == c(xrng/nobs, xrng)))
    warning("Minimum found on boundary of search range.\nYou should retune model with expanded range.")
  xden <- dnorm(xdif / oh)
  xden <- xden / rowSums(xden)
  df <- sum(diag(xden))
  fit <- xden %*% y
  list(x = x, y = fit, df = df, h = oh)
}

##### Data Loading

getwd()
setwd("E:/ANU Sem 3/STAT3050STAT4050STAT7050 - Advanced Statistical Learning - Sem 2 2022/A2")
HE<-read_csv(file = "HE.csv")
GHE<-read_csv(file = "GHE.csv")
GDP<-read_csv(file = "GDP.csv")
DR_young<-read_csv(file = "DR_young.csv")
DR_old<-read_csv(file = "DR_old.csv")

##### Question 1 

country<-t(HE[,1])
countrylist<-list()
model_ss<-list()
model_kernel<-list()
MSE_train_ks<-NULL
MSE_test_ks<-NULL
MSE_train_ss<-NULL
MSE_test_ss<-NULL
fit_value_ks<-NULL
fit_value_ss<-NULL
for (i in 1:length(country)){
  ### Get data of each country
  country_data <-as.data.frame(t(rbind(HE[i,-1],GHE[i,-1],GDP[i,-1],DR_young[i,-1],DR_old[i,-1])))
  colnames(country_data)<-c("HE","GHE","GDP","DR_young","DR_old")
  countrylist[[i]]<-country_data
  ###
  
  ### apply the dimension reduction method
  PCA_train <- as.data.frame(prcomp(country_data[1:30,2:5], scale. = TRUE)$x)
  PCA_test <- as.data.frame(prcomp(country_data[-c(1:30),2:5], scale. = TRUE)$x)
  y_train<-scale(country_data$HE)[1:30]
  y_test<-scale(country_data$HE)[-c(1:30)]
  mod_data<-cbind(y_train,PCA_train)
  ###
  
  ### Kernel smoothing method
  ## ksmooth.gcv, ksmooth() is used but cannot make prediction out of it. npreg() provides this functionality.
  # Just for prediction
  bw <- npregbw(y_train~PC1,data = mod_data)
  mod_kernel<-npreg(bw,data = mod_data)
  model_kernel[[i]]<-mod_kernel
  ###
  
  ### Smoothing spline method
  mod_ss<-ss(PCA_train$PC1,y_train, all.knots = TRUE, method = "GCV")
  model_ss[[i]]<-mod_ss
  ###
  
  ### Calculate the train and test MSE for kernel smoothing method
  MSE_train_ks<-rbind(MSE_train_ks,mod_kernel$MSE)
  pred<-predict(mod_kernel,newdata = PCA_test)
  fit_value_ks<-rbind(fit_value_ks,cbind(as.data.frame(t(fitted(mod_kernel))),as.data.frame(t(pred))))
  MSE_test_ks<-rbind(MSE_test_ks,mean((pred- y_test)^2))
  ###
  
  ### Calculate the train and test MSE for Smoothing spline method
  MSE_train_ss<-rbind(MSE_train_ss,mean((mod_ss$y - y_train)^2))
  pred<-predict.ss(mod_ss,x = PCA_test$PC1)
  fit_value_ss<-rbind(fit_value_ss,cbind(as.data.frame(t(mod_ss$y)),t(pred$y)))
  MSE_test_ss<-rbind(MSE_test_ss,mean((pred$y - y_test)^2))
  ###
  
}
colnames(fit_value_ks)<-colnames(HE[,-1])
rownames(fit_value_ks)<-country

colnames(fit_value_ss)<-colnames(HE[,-1])
rownames(fit_value_ss)<-country

df_MSE<-as.data.frame(cbind(MSE_train_ks,MSE_test_ks,MSE_train_ss,MSE_test_ss))
colnames(df_MSE)<-c("Train MSE Kernel smoothing","Test MSE Kernel smoothing","Train MSE Smoothing spline","Test MSE Smoothing spline")
rownames(df_MSE)<-country

##### Question 2

country<-t(HE[,1])

pool_train<-data.frame(matrix(ncol = 5, nrow = 0))
pool_test<-data.frame(matrix(ncol = 5, nrow = 0))
for (i in 1:length(country)){
  data <-data.table::transpose(rbind(country[i],HE[i,-1],GHE[i,-1],GDP[i,-1],DR_young[i,-1],DR_old[i,-1]))
  colnames(data)<-c("Country","HE","GHE","GDP","DR_young","DR_old")
  
  pool_train<-rbind(pool_train,data[1:30,])
  pool_test<-rbind(pool_test,data[-c(1:30),])
}

for (i in 2:6){
  pool_train[,i]<-as.numeric(pool_train[,i])
  pool_test[,i]<-as.numeric(pool_test[,i])
}

PCA_training<-as.data.frame(prcomp(pool_train[,3:6], scale. = TRUE)$x)
PCA_testing<-as.data.frame(prcomp(pool_test[,3:6], scale. = TRUE)$x)
y_train2<-scale(pool_train$HE)
y_test2<-scale(pool_test$HE)
mod_data2<-cbind(y_train2,PCA_training)
mod_ss2<-ss(PCA_training$PC1,y_train2, all.knots = TRUE, method = "GCV")

MSE_train_ss2<-mean((mod_ss2$y - y_train2)^2)
pred2<-predict.ss(mod_ss2,x = PCA_testing$PC1)
MSE_test_ss2<-mean((pred2$y - y_test2)^2)

##### Question 3

mean(MSE_test_ss)
MSE_test_ss2

##### Question 4

scale_pool_tr<-as.data.frame(scale(pool_train[,-1]))
scale_pool_test<-as.data.frame(scale(pool_test[,-1]))
mod_gam_pool=gam(HE~s(GHE)+s(GDP)+s(DR_old)+s(DR_young),data = scale_pool_tr)
summary(mod_gam_pool)
pred_additive4<-predict.gam(mod_gam_pool,newdata = scale_pool_test)
mean((pred_additive4 - scale_pool_test$HE)^2)

##### Question 5
mod_gam_pool5 <- gam(HE~s(GDP,DR_young)+s(DR_old)+s(GHE),data = scale_pool_tr)
summary(mod_gam_pool5)
pred_additive5<-predict.gam(mod_gam_pool5,newdata = scale_pool_test)
mean((pred_additive5 - scale_pool_test$HE)^2)






