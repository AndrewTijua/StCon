library(fastDummies)
library(data.table)
library(plyr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(qqplotr)
library(broom)
library(xtable)
library(lindia)
library(e1071)
library(parallel)
library(caret)
library(Hmisc)
library(doFuture)
registerDoFuture()
plan(multiprocess, workers = availableCores() - 1)

set.seed(1)

passenger_data_raw <- fread(file = "data/titanicMod.csv")
passenger_data_cleaned <-
  select(passenger_data_raw,
         c(pclass, survived, sex, age, sibsp, parch, fare))

lr_fit <-
  glm(data = passenger_data_cleaned,
      formula = survived ~ .,
      family = binomial(link = "logit"))
par(mfrow = c(2, 2))
plot(lr_fit)
par(mfrow = c(1, 1))

predicted <-
  round(predict(lr_fit, newdata = passenger_data_cleaned, type = "response"))

1 - sum(abs(predicted - passenger_data_cleaned$survived)) / nrow(passenger_data_cleaned) #21% wrongly predicted

passenger_data_cleaned$survived <-
  factor(passenger_data_cleaned$survived)

predicted <- as.factor(predicted)

confusionMatrix(predicted, passenger_data_cleaned$survived)

###svm

passenger_data_cleaned$survived <-
  factor(passenger_data_cleaned$survived)

t_ctrl <-
  trainControl(
    method = "repeatedcv",
    number = 10,
    repeats = 3,
    allowParallel = TRUE
  )

svm_lin <-
  train(
    survived ~ .,
    data = passenger_data_cleaned,
    method = "svmLinear",
    trControl = t_ctrl,
    preprocess = c("centre", "scale"),
    tuneLength = 10
  )

print(svm_lin)

predicted_svm <- predict(svm_lin, newdata = passenger_data_cleaned)

confusionMatrix(predicted_svm, passenger_data_cleaned$survived)

svm_laplace <-
  ksvm(
    survived ~ .,
    data = passenger_data_cleaned,
    kernel = "laplacedot",
    C = 10,
    cross = 3,
    kpars = list(sigma = 0.15)
  )

svm_laplace

predicted_svm_laplace <-
  predict(svm_laplace, newdata = passenger_data_cleaned)

confusionMatrix(factor(predicted_svm_laplace), factor(passenger_data_cleaned$survived))

psvm <- passenger_data_cleaned
psvm$predicted <- predict(svm_laplace, newdata = passenger_data_cleaned)
psvm$correct <- psvm$predicted == psvm$survived
###logic regression
