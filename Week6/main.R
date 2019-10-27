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

NEED_ic <- read.table("data/NEED.NE.txt", header = TRUE)
NEED_dna <- drop_na(NEED_ic)
#cleansing
md.pattern(NEED_ic)
aggr(NEED_ic)

NEED <- complete(mice(NEED_ic, m=5,printFlag = FALSE),1)

NEED_mod <- lm(data = NEED, Gas.cons ~.)
NEED_dna_mod <- lm(data = NEED_dna, Gas.cons ~.)

summary(NEED_mod)
summary(NEED_dna_mod)

par(mfrow = c(2,2))
plot(NEED_mod)
par(mfrow = c(1,1))
