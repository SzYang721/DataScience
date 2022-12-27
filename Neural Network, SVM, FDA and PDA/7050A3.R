rm(list = ls())

library(readr)
library(tensorflow)
library(tidyverse)
library(keras)

SPAM <- read_csv("E:/ANU Sem 3/STAT3050STAT4050STAT7050 - Advanced Statistical Learning - Sem 2 2022/A3/SPAM.csv")
str(SPAM)

train<-SPAM[which(SPAM$testid == 0),]
test<-SPAM[which(SPAM$testid == 1),]


n<-nrow(train)
set.seed (13)

x_train <- model.matrix (spam ~.-1, data = train[,-2])
y_train <- train$spam
y_train <- to_categorical(y_train, 2)

x_test <- model.matrix (spam ~.-1, data = test[,-2])
y_test <- test$spam
y_test <- to_categorical(y_test, 2)

model <- keras_model_sequential() %>% 
  layer_dense(units = 256, activation = "relu", input_shape = ncol(x_train)) %>% 
  layer_dropout(rate = 0.4) %>% 
  layer_dense(units = 128, activation = "relu") %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 2, activation = "softmax")

model %>% compile(
  loss = "binary_crossentropy",
  optimizer = "adam",
  metrics = c("accuracy")
)


model %>% fit(x_train, y_train, epochs = 200)

model %>% evaluate(x_test,  y_test, verbose = 2)



modnn <- keras_model_sequential () %>%
  layer_dense(units = 256, activation = "relu", input_shape = ncol (x_train)) %>%
  layer_dropout (rate = 0.3) %>%
  layer_dense (units = 128, activation = "relu") %>%
  layer_dropout (rate = 0.3) %>%
  layer_dense (units = 256, activation = "relu") %>%
  layer_dropout (rate = 0.3) %>%
  layer_dense(units = 2, activation = "softmax")

summary(modnn)

modnn %>% compile(
  loss = "binary_crossentropy",
  optimizer = "adam",
  metrics = c("accuracy")
)

history <- modnn %>% fit(
  x_train, y_train, epochs = 100
)

modnn %>% evaluate(x_test,  y_test, verbose = 2)

predictions <- predict(model, x_test)
predictions


