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
library(aod)
library(viridis)
registerDoFuture()
plan(multiprocess, workers = availableCores() - 1)

scale_fill_discrete <- scale_fill_viridis_d
scale_colour_discrete <- scale_colour_viridis_d


set.seed(1)

passenger_data_raw <- fread(file = "data/titanicMod.csv")
passenger_data_cleaned <-
  select(passenger_data_raw,
         c(pclass, survived, sex, age, sibsp))
passenger_data_cleaned$pclass <-
  factor(passenger_data_cleaned$pclass)

lr_fit <-
  glm(data = passenger_data_cleaned,
      formula = survived ~ .,
      family = binomial(link = "logit"))
par(mfrow = c(2, 2))
plot(lr_fit)
par(mfrow = c(1, 1))
summary(lr_fit)
confint(lr_fit)

predicted <-
  round(predict(lr_fit, newdata = passenger_data_cleaned, type = "response"))

1 - sum(abs(predicted - passenger_data_cleaned$survived)) / nrow(passenger_data_cleaned) #21% wrongly predicted

confusionMatrix(factor(predicted), factor(passenger_data_cleaned$survived))



age_data_m <-
  with(
    passenger_data_cleaned,
    data.frame(
      pclass = factor(rep(1:3, each = 81)),
      age = rep(seq(0:80), 3)-1,
      sex = "male",
      sibsp = mean(sibsp)
    )
  )
age_data_pm <-
  cbind(age_data_m,
        predict(
          lr_fit,
          newdata = age_data_m,
          type = "link",
          se = TRUE
        ))
age_data_pm <- within(age_data_pm, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})

m_plot_ageclass <- ggplot(age_data_pm, aes(x = age, y = PredictedProb)) + 
  geom_line(aes(color = pclass), size = 1.25) + 
  geom_line(aes(color = pclass, y = LL), size = 0.75) + 
  geom_line(aes(color = pclass, y = UL), size = 0.75)  + 
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = pclass), alpha = 0.5) + 
  lims(x = c(0, 80), y = c(0, 1))

age_data_f <-
  with(
    passenger_data_cleaned,
    data.frame(
      pclass = factor(rep(1:3, each = 81)),
      age = rep(seq(0:80), 3)-1,
      sex = "female",
      sibsp = mean(sibsp)
    )
  )
age_data_pf <-
  cbind(age_data_f,
        predict(
          lr_fit,
          newdata = age_data_f,
          type = "link",
          se = TRUE
        ))
age_data_pf <- within(age_data_pf, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})

w_plot_ageclass <- ggplot(age_data_pf, aes(x = age, y = PredictedProb)) + 
  geom_line(aes(color = pclass), size = 1.25) + 
  geom_line(aes(color = pclass, y = LL), size = 0.75) + 
  geom_line(aes(color = pclass, y = UL), size = 0.75)  + 
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = pclass), alpha = 0.5) + 
  lims(x = c(0, 80), y = c(0, 1))

ggarrange(m_plot_ageclass, w_plot_ageclass)
