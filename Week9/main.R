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
summary(lr_fit)

predicted <-
  round(predict(lr_fit, newdata = passenger_data_cleaned, type = "response"))

1 - sum(abs(predicted - passenger_data_cleaned$survived)) / nrow(passenger_data_cleaned) #21% wrongly predicted

confusionMatrix(factor(predicted), factor(passenger_data_cleaned$survived))
