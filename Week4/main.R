library(tidyverse)
library(ggplot2)
library(ggpubr)
library(xtable)
library(e1071)
library(viridis)

opts <- options()  # save old options

scale_fill_discrete <- scale_fill_viridis_d

OT2016 <- read.table("Olympictrack2016.txt", header = TRUE)
OT2016$Time.100 <- 100 / OT2016$Time.100
OT2016$Time.200 <- 200 / OT2016$Time.200
OT2016$Time.400 <- 400 / OT2016$Time.400

OT2016_men <- OT2016 %>% filter(Sex == "Men")
OT2016_women <- OT2016 %>% filter(Sex == "Women")


par(mfrow = c(2, 2))
ohmd <- lm(data = OT2016, Time.100 ~ Year + Sex)
thmd <- lm(data = OT2016, Time.200 ~ Year + Sex)
fhmd <- lm(data = OT2016, Time.400 ~ Year + Sex)
summary(ohmd)
summary(thmd)
summary(fhmd)

plot(ohmd)

year_start <- 1900
year_end <- 2020
preddata <- data.frame(Year = rep(seq(year_start, year_end, 1), 2),
                       Sex = factor(rep(
                         c('Men', 'Women'), each = (year_end - year_start) / 1 + 1
                       )))


preddata$Time.100 <- predict(ohmd, preddata)
preddata$Time.100.pred <-
  predict(ohmd, preddata, interval = "prediction", level = 0.95)[, 3]
preddata$Time.100.pred <- preddata$Time.100.pred - preddata$Time.100
preddata$Time.200 <- predict(thmd, preddata)
preddata$Time.200.pred <-
  predict(thmd, preddata, interval = "prediction", level = 0.95)[, 3]
preddata$Time.200.pred <- preddata$Time.200.pred - preddata$Time.200
preddata$Time.400 <- predict(fhmd, preddata)
preddata$Time.400.pred <-
  predict(fhmd, preddata, interval = "prediction", level = 0.95)[, 3]
preddata$Time.400.pred <- preddata$Time.400.pred - preddata$Time.400

p <- ggplot(preddata, aes(x = Year, color = Sex, fill = Sex))
p.100 <-
  p + geom_line(aes(y = Time.100), size = 1) +
  geom_ribbon(aes(
    ymin = Time.100 - Time.100.pred,
    ymax = Time.100 + Time.100.pred
  ),
  alpha = 0.1) + ylab("100m Dash Speed (m/s)") + ggtitle("Plot of regressed \n100m dash speeds against year")
p.200 <- p + geom_line(aes(y = Time.200), size = 1) +
  geom_ribbon(aes(
    ymin = Time.200 - Time.200.pred,
    ymax = Time.200 + Time.200.pred
  ),
  alpha = 0.1) + ylab("200m Dash Speed (m/s)") + ggtitle("Plot of regressed \n200m dash speeds against year")
p.400 <- p + geom_line(aes(y = Time.400), size = 1) +
  geom_ribbon(aes(
    ymin = Time.400 - Time.400.pred,
    ymax = Time.400 + Time.400.pred
  ),
  alpha = 0.1) + ylab("400m Dash Speed (m/s)") + ggtitle("Plot of regressed \n400m dash speeds against year")

pr.100 <-
  ggplot(ohmd, aes(
    x = .fitted,
    y = .resid,
    color = Sex,
    fill = Sex
  )) + geom_point() + xlab("Fitted Values (m/s)") + ylab("Residuals (m/s)") + ggtitle("Plot of residuals against \nfitted values for the 100m dash")
pr.200 <-
  ggplot(thmd, aes(
    x = .fitted,
    y = .resid,
    color = Sex,
    fill = Sex
  )) + geom_point() + xlab("Fitted Values (m/s)") + ylab("Residuals (m/s)") + ggtitle("Plot of residuals against \nfitted values for the 200m dash")
pr.400 <-
  ggplot(fhmd, aes(
    x = .fitted,
    y = .resid,
    color = Sex,
    fill = Sex
  )) + geom_point() + xlab("Fitted Values (m/s)") + ylab("Residuals (m/s)") + ggtitle("Plot of residuals against \nfitted values for the 400m dash")

fig1 <- ggarrange(p.100,
          pr.100,
          p.200,
          pr.200,
          p.400,
          pr.400,
          ncol = 2,
          nrow = 3)

annotate_figure(fig1, top = text_grob("Plots for sex as factor (parallel regressions)", face = "bold", size = 20))


ohmd_m <- lm(data = OT2016_men, Time.100 ~ Year)
thmd_m <- lm(data = OT2016_men, Time.200 ~ Year)
fhmd_m <- lm(data = OT2016_men, Time.400 ~ Year)
summary(ohmd_m)
summary(thmd_m)
summary(fhmd_m)

ohmd_w <- lm(data = OT2016_women, Time.100 ~ Year)
thmd_w <- lm(data = OT2016_women, Time.200 ~ Year)
fhmd_w <- lm(data = OT2016_women, Time.400 ~ Year)
summary(ohmd_w)
summary(thmd_w)
summary(fhmd_w)

preddata_m <- data.frame(Year = rep(seq(year_start, year_end, 1), 1))
preddata_w <- data.frame(Year = rep(seq(year_start, year_end, 1), 1))

preddata_m$Time.100 <- predict(ohmd_m, preddata_m)
preddata_m$Time.100.pred <-
  predict(ohmd_m, preddata_m, interval = "prediction", level = 0.95)[, 3]
preddata_m$Time.100.pred <- preddata_m$Time.100.pred - preddata_m$Time.100
preddata_m$Time.200 <- predict(thmd_m, preddata_m)
preddata_m$Time.200.pred <-
  predict(thmd_m, preddata_m, interval = "prediction", level = 0.95)[, 3]
preddata_m$Time.200.pred <- preddata_m$Time.200.pred - preddata_m$Time.200
preddata_m$Time.400 <- predict(fhmd_m, preddata_m)
preddata_m$Time.400.pred <-
  predict(fhmd_m, preddata_m, interval = "prediction", level = 0.95)[, 3]
preddata_m$Time.400.pred <- preddata_m$Time.400.pred - preddata_m$Time.400

preddata_w$Time.100 <- predict(ohmd_w, preddata_w)
preddata_w$Time.100.pred <-
  predict(ohmd_w, preddata_w, interval = "prediction", level = 0.95)[, 3]
preddata_w$Time.100.pred <- preddata_w$Time.100.pred - preddata_w$Time.100
preddata_w$Time.200 <- predict(thmd_w, preddata_w)
preddata_w$Time.200.pred <-
  predict(thmd_w, preddata_w, interval = "prediction", level = 0.95)[, 3]
preddata_w$Time.200.pred <- preddata_w$Time.200.pred - preddata_w$Time.200
preddata_w$Time.400 <- predict(fhmd_w, preddata_w)
preddata_w$Time.400.pred <-
  predict(fhmd_w, preddata_w, interval = "prediction", level = 0.95)[, 3]
preddata_w$Time.400.pred <- preddata_w$Time.400.pred - preddata_w$Time.400

preddata_nonpar <- bind_rows(preddata_m, preddata_w, .id = "Sex")
preddata_nonpar$Sex <- factor(preddata_nonpar$Sex)
levels(preddata_nonpar$Sex) <- c("Men", "Women")

ohmd_m <- fortify(ohmd_m)
ohmd_m$Sex <- as.factor("Men")
ohmd_w <- fortify(ohmd_w)
ohmd_w$Sex <- as.factor("Women")
thmd_m <- fortify(thmd_m)
thmd_m$Sex <- as.factor("Men")
thmd_w <- fortify(thmd_w)
thmd_w$Sex <- as.factor("Women")
fhmd_m <- fortify(fhmd_m)
fhmd_m$Sex <- as.factor("Men")
fhmd_w <- fortify(fhmd_w)
fhmd_w$Sex <- as.factor("Women")


p <- ggplot(preddata_nonpar, aes(x = Year, color = Sex, fill = Sex))
p.100 <-
  p + geom_line(aes(y = Time.100), size = 1) +
  geom_ribbon(aes(
    ymin = Time.100 - Time.100.pred,
    ymax = Time.100 + Time.100.pred
  ),
  alpha = 0.1) + ylab("100m Dash Speed (m/s)") + ggtitle("Plot of regressed \n100m dash speeds against year")
p.200 <- p + geom_line(aes(y = Time.200), size = 1) +
  geom_ribbon(aes(
    ymin = Time.200 - Time.200.pred,
    ymax = Time.200 + Time.200.pred
  ),
  alpha = 0.1) + ylab("200m Dash Speed (m/s)") + ggtitle("Plot of regressed \n200m dash speeds against year")
p.400 <- p + geom_line(aes(y = Time.400), size = 1) +
  geom_ribbon(aes(
    ymin = Time.400 - Time.400.pred,
    ymax = Time.400 + Time.400.pred
  ),
  alpha = 0.1) + ylab("400m Dash Speed (m/s)") + ggtitle("Plot of regressed \n400m dash speeds against year")

pr.100 <-
  ggplot(mapping = aes(
    x = .fitted,
    y = .resid,
    color = Sex,
    fill = Sex
  )) + geom_point(data = ohmd_m) + geom_point(data = ohmd_w)  + xlab("Fitted Values (m/s)") + ylab("Residuals (m/s)") + ggtitle("Plot of residuals against \nfitted values for the 100m dash")
pr.200 <-
  ggplot(mapping = aes(
    x = .fitted,
    y = .resid,
    color = Sex,
    fill = Sex
  )) + geom_point(data = thmd_m) + geom_point(data = thmd_w) + xlab("Fitted Values (m/s)") + ylab("Residuals (m/s)") + ggtitle("Plot of residuals against \nfitted values for the 200m dash")
pr.400 <-
  ggplot(mapping = aes(
    x = .fitted,
    y = .resid,
    color = Sex,
    fill = Sex
  )) + geom_point(data = fhmd_m) + geom_point(data = fhmd_w) + xlab("Fitted Values (m/s)") + ylab("Residuals (m/s)") + ggtitle("Plot of residuals against \nfitted values for the 400m dash")

fig1 <- ggarrange(p.100,
                  pr.100,
                  p.200,
                  pr.200,
                  p.400,
                  pr.400,
                  ncol = 2,
                  nrow = 3)

annotate_figure(fig1, top = text_grob("Plots for sex as factor (separate regressions)", face = "bold", size = 20))


