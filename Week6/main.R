library(plyr)
library(dplyr)
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
library(klaR)
library(vctrs)
library(lindia)

set.seed(1)

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

tHSD <- TukeyHSD(AOV_NEED_modsqrt)
tHSD

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

j_w = 1e2
j_h = 1e-6
dp1 <- ggplot(data = NEED) + geom_density(aes(x = Gas.cons, fill = New.boiler), alpha = 0.3) + geom_jitter(aes(x = Gas.cons, y = 0), width = j_w, height = j_h)# + facet_grid(~New.boiler)
dp2 <- ggplot(data = NEED) + geom_density(aes(x = Gas.cons, fill = Type), alpha = 0.2) + geom_jitter(aes(x = Gas.cons, y = 0), width = j_w, height = j_h)# + facet_grid(~Type)
dp3 <- ggplot(data = NEED) + geom_density(aes(x = Gas.cons, fill = Loft.depth), alpha = 0.2) + geom_jitter(aes(x = Gas.cons, y = 0), width = j_w, height = j_h)# + facet_grid(~Loft.depth)
dp4 <- ggplot(data = NEED) + geom_density(aes(x = Gas.cons, fill = Floor.area), alpha = 0.3) + geom_jitter(aes(x = Gas.cons, y = 0), width = j_w, height = j_h)# + facet_grid(~Floor.area)
dp5 <- ggplot(data = NEED) + geom_density(aes(x = Gas.cons, fill = Cavity.wall), alpha = 0.3) + geom_jitter(aes(x = Gas.cons, y = 0), width = j_w, height = j_h)# + facet_grid(~Loft.depth)
dp6 <- ggplot(data = NEED) + geom_density(aes(x = Gas.cons, fill = Age.band), alpha = 0.1) + geom_jitter(aes(x = Gas.cons, y = 0), width = j_w, height = j_h)# + facet_grid(~Floor.area)
ggarrange(dp1, dp2, dp3, dp4, dp5, dp6)

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
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles", title = "Detrended Q-Q Plot for \ntransformed response")
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
p5 <- ggplot(data = NEED, aes(x = Cavity.wall, y=Gas.cons, fill = Cavity.wall)) + geom_violin(draw_quantiles = c(0.05,0.25,0.5,0.75,0.95), size = 0.5, alpha = 0.5) + labs(x = "Cavity Wall", y = "Gas Consumption") + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
p6 <- ggplot(data = NEED, aes(x = Loft.depth, y=Gas.cons, fill = Loft.depth)) + geom_violin(draw_quantiles = c(0.05,0.25,0.5,0.75,0.95), size = 0.5, alpha = 0.5) + labs(x = "Loft Insulation Depth", y = "Gas Consumption") + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
p1t4 <- ggarrange(p1,p2,p3,p4,p5,p6)

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
    
  })[,-1]
}

csq <- df.chisq.test(NEED)
csq

trainx <- NEED[,c(-1, -3)]
trainy <- NEED[,3]
NEED_nb <- train(trainx, trainy, 'nb', trControl = trainControl(method = 'repeatedcv', number = 10, repeats=5))
NEED_nb
table(predict(NEED_nb$finalModel,trainx)$class, trainy)

NEED_model_initial <- lm(data = NEED, Gas.cons ~ Age.band + Type + Floor.area)
NEED_model_initial_sr <- lm(data = NEED, sqrt(Gas.cons) ~ Age.band + Type + Floor.area)
AOV_model_initial_sr <- aov(data = NEED, sqrt(Gas.cons) ~ Age.band + Type + Floor.area)

summary(NEED_model_initial)
summary(NEED_model_initial_sr)
par(mfrow = c(2, 2))
plot(NEED_model_initial)
plot(NEED_model_initial_sr) #residuals are linearised


NEED_model_modifications_sr <- lm(data = NEED, sqrt(Gas.cons) ~ Loft.depth + Cavity.wall + New.boiler) # extremely poor fit
AOV_model_modifications_sr <- aov(data = NEED, sqrt(Gas.cons) ~ Loft.depth + Cavity.wall + New.boiler)
summary(NEED_model_modifications_sr)
plot(NEED_model_modifications_sr)
par(mfrow = c(1, 1))



#densityplot(~ Gas.cons | Type, data = NEED)

ggplot_lm <- function(model, probs, di = "norm", de = TRUE){
  data <- model$model
  
  p1 <- ggplot(mapping = aes(x = .fitted,
                             y = .resid)) +
    geom_point(data = model) +
    xlab("Fitted Values") +
    ylab("Residuals") +
    ggtitle("Residuals vs Fitted")
  p2 <- ggplot(data, aes(sample = pull(data, 1))) + 
    stat_qq_point(distribution = di,
                  detrend = de,
                  qprobs = probs) +
    stat_qq_line(distribution = di,
                 detrend = de,
                 qprobs = probs) +
    stat_qq_band(distribution = di,
                 detrend = de,
                 qprobs = probs) +
    labs(x = "Predicted", y = "Residual", title = "Detrended Q-Q Plot for response")
  p3 <- gg_resleverage(model)
  p4 <- gg_scalelocation(model)
  return(ggarrange(p1,p2,p4,p3))
}

gg <- ggplot_lm(NEED_modsqrt, probs = probs)
gg

gg1 <- ggplot_lm(NEED_model_initial_sr, probs = probs)
annotate_figure(gg1, top = text_grob("Diagnostic for base\nproperty model", size = 14, face = 'bold'))
gg2 <- ggplot_lm(NEED_model_modifications_sr, probs = probs)
annotate_figure(gg2, top = text_grob("Diagnostic for \nmodification model", size = 14, face = 'bold'))

anova(AOV_NEED_modsqrt, AOV_model_initial_sr)
anova(AOV_NEED_modsqrt, AOV_model_modifications_sr)
