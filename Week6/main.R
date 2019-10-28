library(plyr)
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
library(extrafont)
library(Hmisc)
library(corrplot)
library(polycor)
library(lsr)
library(rcompanion)

#loadfonts(device = "win")

scale_fill_discrete <- scale_fill_viridis_d
scale_colour_discrete <- scale_colour_viridis_d

NEED_ic <- read.table("data/NEED.NE.txt", header = TRUE)
NEED_ic$Age.band <- as.factor(NEED_ic$Age.band)
NEED_ic$Floor.area <- as.factor(NEED_ic$Floor.area)
NEED_dna <- drop_na(NEED_ic)
#cleansing
md.pattern(NEED_ic)
aggr(NEED_ic)

NEED_mice <- mice(NEED_ic, m = 5, printFlag = FALSE)
NEED <- complete(NEED_mice, 1)
NEED_mod <- lm(data = NEED, (Gas.cons) ~ .)
NEED_modsqrt <- lm(data = NEED, sqrt(Gas.cons) ~ .)
NEED_dna_mod <- lm(data = NEED_dna, Gas.cons ~ .)

AOV_NEED_mod <- aov(data = NEED, (Gas.cons) ~ .)
AOV_NEED_modsqrt <- aov(data = NEED, sqrt(Gas.cons) ~ .)
AOV_NEED_dna_mod <- aov(data = NEED_dna, Gas.cons ~ .)

TukeyHSD(AOV_NEED_mod)

densityplot(NEED_mice)
densityplot(~ Gas.cons | New.boiler, data = NEED)
stripplot(NEED_mice, pch = 20, cex = 1.2)

summary(NEED_modsqrt)
summary(NEED_dna_mod)
par(family = "sans")
par(mfrow = c(2, 2))
plot(NEED_modsqrt)
par(mfrow = c(2, 1))
qqPlot(NEED_mod)
qqPlot(NEED_modsqrt)
par(mfrow = c(1, 1))

dp1 <- ggplot(data = NEED) + geom_density(aes(x = Gas.cons, fill = New.boiler), alpha = 0.3) + geom_point(aes(x = Gas.cons, y = 0))# + facet_grid(~New.boiler)
dp2 <- ggplot(data = NEED) + geom_density(aes(x = Gas.cons, fill = Type), alpha = 0.2) + geom_point(aes(x = Gas.cons, y = 0))# + facet_grid(~Type)
dp3 <- ggplot(data = NEED) + geom_density(aes(x = Gas.cons, fill = Loft.depth), alpha = 0.2) + geom_point(aes(x = Gas.cons, y = 0))# + facet_grid(~Loft.depth)
dp4 <- ggplot(data = NEED) + geom_density(aes(x = Gas.cons, fill = Floor.area), alpha = 0.3) + geom_point(aes(x = Gas.cons, y = 0))# + facet_grid(~Floor.area)
ggarrange(dp1, dp2, dp3, dp4)

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

p1 <- ggplot(data = NEED, aes(x = Age.band, y=Gas.cons, fill = Age.band)) + geom_violin(draw_quantiles = c(0.05,0.25,0.5,0.75,0.95), size = 0.5, alpha = 0.5) + labs(x = "Age Band", y = "Gas Consumption") + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
p2 <- ggplot(data = NEED, aes(x = Type, y=Gas.cons, fill = Type)) + geom_violin(draw_quantiles = c(0.05,0.25,0.5,0.75,0.95), size = 0.5, alpha = 0.5) + labs(x = "Type of Property", y = "Gas Consumption") + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
p3 <- ggplot(data = NEED, aes(x = Floor.area, y=Gas.cons, fill = Floor.area)) + geom_violin(draw_quantiles = c(0.05,0.25,0.5,0.75,0.95), size = 0.5, alpha = 0.5) + labs(x = "Floor Area Band", y = "Gas Consumption") + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
p4 <- ggplot(data = NEED, aes(x = New.boiler, y=Gas.cons, fill = New.boiler)) + geom_violin(draw_quantiles = c(0.05,0.25,0.5,0.75,0.95), size = 0.5, alpha = 0.5) + labs(x = "New Boiler", y = "Gas Consumption") + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
p1t4 <- ggarrange(p1,p2,p3,p4)

annotate_figure(p1t4, top = text_grob("Violin plots of Gas Consumption by Factor", size = 14, face = 'bold'))

chisq.test(NEED$Type, NEED$Floor.area)
chisq.test(NEED$Type, NEED$New.boiler)

df.chisq.test <- function(data, sim = FALSE) {
  require(plyr)
  combos <- combn(ncol(data), 2)
  
  adply(combos, 2, function(x) {
    test <- chisq.test(data[, x[1]], data[, x[2]], simulate.p.value = sim)
    
    out <- data.frame(
      "Row" = colnames(data)[x[1]]
      ,
      "Column" = colnames(data[x[2]])
      ,
      "Chi.Square" = round(test$statistic, 3)
      ,
      "df" = test$parameter
      ,
      "p.value" = round(test$p.value, 3)
    )
    return(out)
    
  })
}

df.chisq.test(NEED)
