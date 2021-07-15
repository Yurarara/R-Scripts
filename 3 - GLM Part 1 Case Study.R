library(checkpoint)
book_directory = "F:/R directories/book directory"
checkpoint_directory = "D:/Documents/R/win-library/4.1"
checkpoint("2021-06-24", R.version = "4.1.0", 
           project = book_directory, 
           checkpointLocation = checkpoint_directory, 
           scanForPackages = FALSE, 
           scan.rnw.with.knitr = TRUE, 
           use.knitr = TRUE)
library(knitr)
library(data.table)
library(cowplot)
library(ggplot2)
library(visreg)
library(ez)
library(emmeans)
library(rms)
library(ipw)
library(JWileymisc)
library(RcppEigen)
library(texreg)
setwd("F:/R directories")
options(
  width = 70,
  stringsAsFactors = FALSE,
  datatable.print.nrows = 20,
  datatable.print.topn = 3, 
  digits = 2
)

# This is a pseudo case study based on a real-world research experiment.
# The experiment studied the relationship of adolescents' sleep status and their
# negative emotions. The orirginal experiment sample contained 150 teenagers,
# while the following parameters were recorded.
# MOOD: a general index indicating depression to anxiety
# DBAS: Dysfunctional Beliefs and Attitudes about Sleep
# DAS: Dysfunctional Attitude Scale
# Stress: a general index indicating stress level
# SSQ: Subjective Sleep Quality
# Meanwhile, the sleeping quality is measured via SOLacti: Sleep Onset Latency,
# i.e. the time (in minutes) before falling asleep.
# In total, the higher these parameters are, the worse the sleep quality is.


# Case Study Part 1 - Standardised Z-score
# The data used in this case study is simulated based on some given standardised
# regression coefficient. This ensures the simulated data behaves as close to
# the actual data as possible.
# The source is as followed:
# B. Bei, J. F. Wiley, N. B. Allen, and J. Trinder. A cognitive vulnerability 
# model of sleep and mood in adolescents under restricted and extended sleep
# opportunities. Sleep, 38(3):453–461, 2015

set.seed(12345)
adosleep <- data.table(
  SOLacti = rnorm(150, 4.4, 1.3)^2,
  DBAS = rnorm(150, 72, 26),
  DAS = rnorm(150, 125, 32), 
  Female = rbinom(150, 1, .53),
  Stress = rnorm(150, 32, 11)
)
adosleep[, SSQ := rnorm(150,
            (.36 * 3 / 12.5) * SOLacti +
            (.16 * 3 / 26) * DBAS +
            (.18 * 3 / .5) * Female +
            (.20 * 3 / 11) * Stress, 2.6)]
adosleep[, MOOD := rnorm(150, 
            (-.07 / 12.5) * SOLacti +
            (.29 / 3) * SSQ +
            (.14 / 26) * DBAS +
            (.21 / 32) * DAS +
            (.12 / 32) * SSQ * (DAS-50) +
            (.44 / .5) * Female +
            (.28 / 11) * Stress, 2)]
adosleep[, Female := factor(Female, levels = 0:1, 
                            labels = c("Males", "Females"))]
# The experiment was based on the assumption that the SSQ is related to negative
# emotions. However, this relationship is adjusted by DAS.
# In another word, DAS reflects the fragile nature of adolescences. Adolescences
# with higher DAS tend to be influenced by negative emotions more easily.
# On contrast, for those with lower DAS, we expect to see weaker relationship
# between SSQ and negative emotions.

# To begin with, check outliers for major variables via density plots.
plot_grid(
  plot(testDistribution(adosleep$MOOD, extremevalues = "theoretical", 
                        plot = FALSE, verlab = "MOOD"))$Density, 
  plot(testDistribution(adosleep$SSQ, extremevalues = "theoretical", 
                        plot = FALSE, verlab = "SSQ"))$Density, 
  plot(testDistribution(adosleep$SOLacti, extremevalues = "theoretical", 
                        plot = FALSE, verlab = "SOLacti"))$Density, 
  plot(testDistribution(adosleep$DAS, extremevalues = "theoretical", 
                        plot = FALSE, verlab = "DAS"))$Density,
  ncol = 2
)
# Next, check duo-variable (continuous) correlation.
plot(SEMSummary(
  ~ MOOD + SOLacti + DBAS + DAS + Stress + SSQ,
  data = adosleep), plot = "cor") +
  theme(axis.text.x = element_text(
    angle = 45, hjust = 1, vjust = 1
  ))
# Then, to make data clearer, we produce a table illustrating the mean and SD of
# continuous variables, as well as N value for discrete variables.
# The egltable() in JWileymisc package can well do that.
egltable(c("SOLacti", "SSQ", "MOOD", "Stress", "DBAS", "DAS", "Female"),
         data = as.data.frame(adosleep))
# Calculate z-score for the sake of it.
adosleep[, zMOOD := as.vector(scale(MOOD))]
adosleep[, zDBAS := as.vector(scale(DBAS))]
adosleep[, zDAS := as.vector(scale(DAS))]
adosleep[, zSSQ := as.vector(scale(SSQ))]
adosleep[, zSOLacti := as.vector(scale(SOLacti))]
adosleep[, zStress := as.vector(scale(Stress))]
# We investigate 3 models: 
# 1. a model with all covariables
# 2. a model with extra variables without cross effect
# 3. a model with extra variables and hypothesised cross effect, namely between
# SSQ and DAS.
m.adosleep1 <- ols(zMOOD ~ zSOLacti + zDBAS + Female + zStress, data = adosleep)
m.adosleep2 <- update(m.adosleep1, . ~ . + zSSQ + zDAS)
m.adosleep3 <- update(m.adosleep2, . ~ . + zSSQ:zDAS)
screenreg(list(m.adosleep1, m.adosleep2, m.adosleep3))
# In our assumption, the higher SSQ, the worse sleep quality. In model 2, SSQ
# has positive coefficient and high significance towards the MOOD index.
# Meanwhile, in model 3, there is significant positive correlation between SSQ
# and DAS. To further interpret, higher DAS means higher SSQ, and thus higher
# MOOD index. This conclusion agrees to the initial assumption.
# Then, check variance inflation factor and the residual distribution. Here we
# only check model 3: more complex the model, more possible there's colinearity.
# VIF close to 1 - little or no colinearity.
# QQPlot indicates general normality.
vif(m.adosleep3)
plot(testDistribution(resid(m.adosleep3), plot = FALSE, verlab = "Residuals"))$QQPlot


# Case Study Part 2 - Raw Data
# For investigating cross effects, plots are recommended to give a clear evidence.
# To generate plots, it's better to use original scale if the data were reasonably
# measured. Therefore, we construct again the model with raw data.
m.adosleep.raw <- ols(MOOD ~ SOLacti + DBAS + Female + Stress + SSQ * DAS, 
                      data = adosleep)
# The plot for analysing cross effects requires predictions and expected values
# of variables.
adosleep.newdat <- as.data.table(with(adosleep, expand.grid(
  SOLacti = mean(SOLacti),
  DBAS = mean(DBAS), 
  Female = factor("Females", levels(Female)),
  Stress = mean(Stress),
  SSQ = seq(from = min(SSQ), to = max(SSQ), length.out = 100),
  DAS = mean(DAS) + c(1, -1) * sd(DAS)
)))
adosleep.newdat$MOOD <- predict(m.adosleep.raw, newdata = adosleep.newdat,
                                se.fit = FALSE)
adosleep.newdat[, DAS := factor(round(DAS), 
                                levels = c(100, 161),
                                labels = c("M - 1 SD", "M + 1 SD"))]
ggplot(adosleep.newdat, aes(SSQ, MOOD, linetype = DAS)) + 
  geom_line(size = 2) +
  scale_x_continuous("Subjective Sleep Quality") +
  ylab("Negative Mood") +
  theme_cowplot() +
  theme(
    legend.position = c(.85, .15),
    legend.key.width = unit(2, "cm")
  )
