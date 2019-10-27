library(caret)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(xtable)
library(e1071)
library(viridis)
library(mice)
library(VIM)
library(car)
library(qqplotr)

scale_fill_discrete <- scale_fill_viridis_d
scale_colour_discrete <- scale_colour_viridis_d

NEED_ic <- read.table("data/NEED.NE.txt", header = TRUE)
NEED_ic$Age.band <- as.factor(NEED_ic$Age.band)
NEED_ic$Floor.area <- as.factor(NEED_ic$Floor.area)
NEED_dna <- drop_na(NEED_ic)
#cleansing
md.pattern(NEED_ic)
aggr(NEED_ic)

NEED <- complete(mice(NEED_ic, m = 5, printFlag = FALSE), 1)

NEED_mod <- lm(data = NEED, (Gas.cons) ~ .)
NEED_modsqrt <- lm(data = NEED, sqrt(Gas.cons) ~ .)
NEED_dna_mod <- lm(data = NEED_dna, Gas.cons ~ .)

AOV_NEED_mod <- aov(data = NEED, (Gas.cons) ~ .)
AOV_NEED_modsqrt <- aov(data = NEED, sqrt(Gas.cons) ~ .)
AOV_NEED_dna_mod <- aov(data = NEED_dna, Gas.cons ~ .)

TukeyHSD(AOV_NEED_mod)

summary(NEED_modsqrt)
summary(NEED_dna_mod)

par(mfrow = c(2, 2))
plot(NEED_modsqrt)
par(mfrow = c(2, 1))
qqPlot(NEED_mod)
qqPlot(NEED_modsqrt)
par(mfrow = c(1, 1))

de = TRUE
di <- "norm"
probs <- c(0.05, 0.95)
sq_qq <-
  ggplot(NEED, aes(sample = sqrt(Gas.cons))) +
  stat_qq_point(distribution = di,
                detrend = de,
                qprobs = probs) +
  stat_qq_line(distribution = di,
               detrend = de,
               qprobs = probs) +
  stat_qq_band(distribution = di,
               detrend = de,
               qprobs = probs) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles", title = "Detrended Q-Q Plot for transformed response")
l_qq <-
  ggplot(NEED, aes(sample = (Gas.cons))) +
  stat_qq_point(distribution = di,
                detrend = de,
                qprobs = probs) +
  stat_qq_line(distribution = di,
               detrend = de,
               qprobs = probs) +
  stat_qq_band(distribution = di,
               detrend = de,
               qprobs = probs) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles", title = "Detrended Q-Q Plot for response")

fr.sqrt <-
  ggplot(mapping = aes(x = .fitted,
                       y = .resid)) +
  geom_point(data = NEED_modsqrt) +
  xlab("Fitted Values") +
  ylab("Residuals") +
  ggtitle("Plot of residuals against \nfitted values for the transformed response")

fr.lin <-
  ggplot(mapping = aes(x = .fitted,
                       y = .resid)) +
  geom_point(data = NEED_mod) +
  xlab("Fitted Values") +
  ylab("Residuals") +
  ggtitle("Plot of residuals against \nfitted values for the response")

ggarrange(sq_qq, l_qq, fr.sqrt, fr.lin)
