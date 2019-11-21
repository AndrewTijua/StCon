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
library(broom)
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
      formula = survived ~ . + age*sex + age*pclass,
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

size_bfl <- 0.95
size_bou <- 0.55
alp <- 0.45

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
  geom_line(aes(color = pclass), size = size_bfl) + 
  geom_line(aes(color = pclass, y = LL), size = size_bou) + 
  geom_line(aes(color = pclass, y = UL), size = size_bou)  + 
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = pclass), alpha = alp) + 
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
  geom_line(aes(color = pclass), size = size_bfl) + 
  geom_line(aes(color = pclass, y = LL), size = size_bou) + 
  geom_line(aes(color = pclass, y = UL), size = size_bou)  + 
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = pclass), alpha = alp) + 
  lims(x = c(0, 80), y = c(0, 1)) +
  labs(title = "Predicted Female Survival by Class")

ggarrange(m_plot_ageclass, w_plot_ageclass, common.legend = TRUE)

ggQQ = function(lm, probs = c(0.05,0.95)) {
  # extract standardized residuals from the fit
  d <- data.frame(std.resid = rstandard(lm))
  # calculate 1Q/4Q line
  y <- quantile(d$std.resid[!is.na(d$std.resid)], probs)
  x <- qnorm(probs)
  slope <- diff(y)/diff(x)
  int <- y[1L] - slope * x[1L]
  
  p <- ggplot(data=d, aes(sample=std.resid)) +
    stat_qq(shape=1, size=3) +           # open circles
    labs(title="Normal Q-Q",             # plot title
         x="Theoretical Quantiles",      # x-axis label
         y="Standardized Residuals") +   # y-axis label
    geom_abline(slope = slope, intercept = int, linetype="dashed")  # dashed reference line
  return(p)
}

ggplot_lm <- function(model,
                      probs = c(0.05, 0.95),
                      di = "norm",
                      de = TRUE) {
  data <- model$model
  p1 <- ggplot(mapping = aes(x = .fitted,
                             y = .resid)) +
    geom_point(data = model) +
    xlab("Fitted Values") +
    ylab("Residuals") +
    ggtitle("Residuals vs Fitted")
  
  p1<-ggplot(model, aes(.fitted, .resid))+geom_point()
  p1<-p1+stat_smooth(method="loess")+geom_hline(yintercept=0, col="red", linetype="dashed")
  p1<-p1+xlab("Fitted values")+ylab("Residuals")
  p1<-p1+ggtitle("Residual vs Fitted Plot")
  
  res.df <- data.frame(s.res = rstandard(model))
  #res.df$y = quantile(res.df$s.res[!is.na(res.df$s.res)], probs)
  
  p2 <-
    ggplot(data = res.df, aes(sample = s.res)) +
    stat_qq_point(distribution = di,
                  detrend = de,
                  qprobs = probs) +
    stat_qq_line(distribution = di,
                 detrend = de,
                 qprobs = probs) +
    stat_qq_band(distribution = di,
                 detrend = de,
                 qprobs = probs) +
    labs(x = "Theoretical", y = "Standardised \nResiduals", title = "Q-Q Plot for response")
  
  #p2 <- ggQQ(model, probs)
  
  p3<-ggplot(model, aes(.hat, .stdresid))+geom_point(aes(size=.cooksd), na.rm=TRUE)
  p3<-p3+stat_smooth(method="loess", na.rm=TRUE)
  p3<-p3+xlab("Leverage")+ylab("Standardized Residuals")
  p3<-p3+ggtitle("Residual vs Leverage Plot")
  p3<-p3+scale_size_continuous("Cook's Distance", range=c(1,5))
  #p4 <- gg_scalelocation(model)
  p4<-ggplot(model, aes(seq_along(.cooksd), .cooksd))+geom_bar(stat="identity", position="identity")
  p4<-p4+xlab("Obs. Number")+ylab("Cook's distance")
  p4<-p4+ggtitle("Cook's distance")
  return(ggarrange(p1, p2, p3, p4))
}

ggplot_lm(model = lr_fit, de = TRUE)

ill_passenger_data <- fread(file = "data/newPassengers.csv")

ill_passenger_data$pclass <-
  factor(ill_passenger_data$pclass)

ill_passenger_data$embarked <-
  factor(ill_passenger_data$embarked)

ill_passenger_data$sex <- factor(ill_passenger_data$sex)

ill_passenger_data$p_surv <- (predict(lr_fit, newdata = ill_passenger_data, type = "response"))
