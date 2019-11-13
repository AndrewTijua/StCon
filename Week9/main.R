library(fastDummies)
library(usdm)
library(data.table)
library(plyr)
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
library(ggmosaic)
library(vcd)
library(corrplot)
library(dplyr)
registerDoFuture()
plan(multiprocess, workers = availableCores() - 1)

options(datatable.fread.datatable=FALSE)

scale_fill_discrete <- scale_fill_viridis_d
scale_colour_discrete <- scale_colour_viridis_d


set.seed(1)

df.chisq.test <- function(data, sim = FALSE) {
  require(plyr)
  combos <- combn(ncol(data), 2)
  
  adply(combos, 2, function(x) {
    test <-
      chisq.test(data[, x[1]], data[, x[2]], simulate.p.value = sim)
    
    out <- data.frame(
      "Variable 1" = colnames(data)[x[1]]
      ,
      "Variable 2" = colnames(data[x[2]])
      ,
      "Chi.Square" = round(test$statistic, 3)
      ,
      "df" = test$parameter
      ,
      "p.value" = round(test$p.value, 3)
    )
    return(out)
    
  })[,-1]
}

passenger_data_raw <- fread(file = "data/titanicMod.csv")

passenger_data_cleaned <-
  select(passenger_data_raw,
         c(pclass, survived, sex, age, sibsp, embarked, cabin))

passenger_data_cleaned$pclass <-
  factor(passenger_data_cleaned$pclass)

passenger_data_cleaned$embarked <-
  factor(passenger_data_cleaned$embarked)

passenger_data_cleaned$sex <- factor(passenger_data_cleaned$sex)

passenger_data_cleaned$cabin[!is.na(passenger_data_cleaned$cabin)] <- 'Y'
passenger_data_cleaned$cabin[is.na(passenger_data_cleaned$cabin)] <- 'N'
passenger_data_cleaned$cabin <- factor(passenger_data_cleaned$cabin)

chisq.test(passenger_data_raw$parch, passenger_data_raw$sibsp, simulate.p.value = TRUE)
chisq.test(passenger_data_cleaned$cabin, passenger_data_cleaned$pclass, simulate.p.value = TRUE)
chisq.test(passenger_data_cleaned$age, passenger_data_cleaned$embarked, simulate.p.value = TRUE)

df.chisq.test(passenger_data_cleaned, sim = TRUE)

lr_fit <-
  glm(data = passenger_data_cleaned,
      formula = survived ~ .,
      family = binomial(link = "logit"))
par(mfrow = c(2, 2))
plot(lr_fit)
par(mfrow = c(1, 1))
summary(lr_fit)
confint(lr_fit)

anova(lr_fit, test = "Chisq")

predicted <-
  round(predict(lr_fit, newdata = passenger_data_cleaned, type = "response"))

1 - sum(abs(predicted - passenger_data_cleaned$survived)) / nrow(passenger_data_cleaned) #21% wrongly predicted

confusionMatrix(factor(predicted), factor(passenger_data_cleaned$survived))

plot_fit <-
  glm(data = passenger_data_cleaned,
      formula = survived ~ age + pclass + sex,
      family = binomial(link = "logit"))

age_data_m <-
  with(
    passenger_data_cleaned,
    data.frame(
      pclass = factor(rep(1:3, each = 81)),
      age = rep(seq(0:80), 3)-1,
      sex = "male"
    )
  )
age_data_pm <-
  cbind(age_data_m,
        predict(
          plot_fit,
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
  lims(x = c(0, 80), y = c(0, 1)) + 
  labs(title = "Predicted Male Survival by Class")

age_data_f <-
  with(
    passenger_data_cleaned,
    data.frame(
      pclass = factor(rep(1:3, each = 81)),
      age = rep(seq(0:80), 3)-1,
      sex = "female"
    )
  )
age_data_pf <-
  cbind(age_data_f,
        predict(
          plot_fit,
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
  lims(x = c(0, 80), y = c(0, 1)) +
  labs(title = "Predicted Female Survival by Class")

ggarrange(m_plot_ageclass, w_plot_ageclass, common.legend = TRUE)