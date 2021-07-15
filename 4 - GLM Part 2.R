library(checkpoint)
book_directory = "F:/R directories/book directory"
checkpoint_directory = "D:/Documents/R/win-library/4.1"
checkpoint("2021-06-25", R.version = "4.1.0", 
           project = book_directory, 
           checkpointLocation = checkpoint_directory, 
           scanForPackages = FALSE, 
           scan.rnw.with.knitr = TRUE, 
           use.knitr = TRUE)
library(knitr)
library(data.table)
library(cowplot)
library(ggplot2)
library(ggthemes)
library(scales)
library(viridis)
library(VGAM)
library(ipw)
library(JWileymisc)
library(xtable)
library(texreg)
setwd("F:/R directories")
options(
  width = 70,
  stringAsFactors = FALSE,
  datable.print.nrows = 20,
  datable.print.topn = 3,
  digits = 2
)
# GLM Part 2 - (1): Basic Logistic Regression
# In this part, we're investigating the other GLM models including Logistic 
# regression, Poisson regression and negative binomial regression.
# First, the Logistic regression.
# To apply the binomial Logistic regression model, we need first define a
# binomial response variable: here, we're interested in whether the individual
# is a smoker. 
acl <- readRDS("advancedr_acl_data.RDS")
acl$CurSmoke <- as.integer(acl$Smoke_W1 == "(1) Cur Smok")
# The vglm() function can generate the Logistic regression models. In this case,
# we set the 'family' argument to 'binomialff()" and determine the link function.
m.lr <- vglm(CurSmoke ~ Sex, family = binomialff(link = "logitlink"),
             data = acl, model = TRUE)
summary(m.lr)
# In fact, this is a special case of Logistic regression where both the response
# and explanatory variable are binomial. This makes it easy to calculate the 
# probability ratio, and we do it as followed for cross-checking with the result
# from the model.
or.tab <- xtabs(~ Sex + CurSmoke, data = acl)
or.tab.res <- (or.tab[1,1]/or.tab[2,1])/(or.tab[1,2]/or.tab[2,2])
xtable(or.tab, caption = "Observed Frequency Table", label = "tglm2-obsfreq")
# To interpret the result, we can compare the probability of smoking by gender.
# In order to restore the scale, use "response" in the 'type' argument.
preddat <- data.table(Sex = levels(acl$Sex))
preddat$ yhat <- predict(m.lr, newdata = preddat, type = "response")
ggplot(preddat, aes(Sex, yhat)) +
  geom_bar(stat = "identity") +
  scale_y_continuous("Smoking Probability", labels = percent) +
  theme_tufte()
# xtable() can well produce a LaTex command for regression result.
xtable(coef(summary(m.lr)), digits = 2, caption = paste(
  "Summary of logistic regression model",
  "including coefficients, standard errors",
  "and p-values."), label = "tglm2-orsimple"
)
# Then, we further introduce other variables (namely self efficacy). To analyse
# cross effects, ipw is applied here. Confounding factors include gender, race
# and age.
# m0: model without weight
m0.lr <- vglm(CurSmoke ~ SelfEfficacy_W1, 
              family = binomialff(link = "logitlink"),
              data = acl, model = TRUE)
w <- ipwpoint(
  exposure = SelfEfficacy_W1,
  family = "gaussian",
  numerator = ~ 1,
  denominator = ~ 1 + Sex + RaceEthnicity + AGE_W1,
  data = acl
)
# m1: model with weight: sex, race, age
m1.lr <- vglm(CurSmoke ~ SelfEfficacy_W1,
              family = binomialff(link = "logitlink"),
              data = acl, model = TRUE,
              weights = winsorizor(w$ipw.weights, .01))
# The coefficients can be interpreted as following:
# As the self efficacy increases by 1 unit, the probability of smoking shrinks
# by 0.92 times.
xtable(rbind(
  data.table(Type = "Raw", coef(summary(m0.lr))),
  data.table(Type = "Adj", coef(summary(m1.lr)))), 
  digits = 2,
  caption = "Comparison of uadjusted(raw) and adjusted regression models",
  label = "tglm2-lrcompare")
# Again, we use predict() to predict probability (based on fitted model) and 
# plot the result.
preddat2 <- data.table(SelfEfficacy_W1 = 
                         seq(from = min(acl$SelfEfficacy_W1, na.rm = TRUE),
                             to = max(acl$SelfEfficacy_W1, na.rm = TRUE),
                             length.out = 1000))
preddat2$yhat <- predict(m1.lr, newdata = preddat2, type = "response")
ggplot(preddat2, aes(SelfEfficacy_W1, yhat)) +
  geom_line() +
  scale_x_continuous("Self Efficacy") +
  scale_y_continuous("Smoking Probability", label = percent) +
  theme_tufte() + coord_cartesian(ylim = c(.25, .40))


# GLM Part 2 (2): Ordered Logistic Regression and Multinomial Logistic Regression
# Ordered logistic regression can be applied to discrete response variables,
# or to naturally ordered categorical variables. In our data, the Physics
# Activity Category is such variable.
acl$PhysActCat_W2 <- factor(acl$PhysActCat_W2, ordered = TRUE)
m0.or <- vglm(PhysActCat_W2 ~ SelfEfficacy_W1,
              family = propodds(),
              data = acl)
# Estimate and apply IPWs
w <- ipwpoint(exposure = SelfEfficacy_W1,
              family = "gaussian",
              numerator = ~ 1,
              denominator = ~ 1 + Sex + RaceEthnicity + AGE_W1,
              data = acl)
m1.or <- vglm(PhysActCat_W2 ~ SelfEfficacy_W1,
              family = propodds(),
              data = acl, model = TRUE,
              weights = winsorizor(w$ipw.weights, .01))
# In an ordered logistic regression model, the number of fitted intercepts 
# should be 1 fewer than the number of classes of response variable: in this
# case, there should be 4 intercepts and 1 coefficient, as we assume the effect
# of self efficacy is identical throughout all 5 classes.
screenreg(list(m0.or, m1.or))
# Again, we use predict() to produce the probability prediction plot.
# As the response variable has multiple categories, we use melt() to integrate
# them into a larger data frame.
preddat3 <- data.table(SelfEfficacy_W1 = 
                         seq(from = min(acl$SelfEfficacy_W1, na.rm = TRUE),
                             to = max(acl$SelfEfficacy_W1, na.rm = TRUE),
                             length.out = 1000))
preddat3 <- cbind(preddat3, 
                  predict(m1.or, newdata = preddat3, type = "response"))
preddat3 <- melt(preddat3, id.vars = "SelfEfficacy_W1")
ggplot(preddat3, aes(SelfEfficacy_W1, value, colour = variable, 
                     linetype = variable)) +
  geom_line(size = 2) +
  scale_color_viridis(discrete = TRUE) +
  scale_x_continuous("Self-Efficacy") +
  scale_y_continuous("Activity Probability", label = percent) +
  coord_cartesian(ylim = c(0, .6), expand = FALSE) +
  theme_tufte() +
  theme(legend.position = c(.7, .8),
        legend.key.width = unit(2, "cm"))

# Similar to ordered logistic regression, multinomial logistic regression also
# applies when there are more than 2 classes of response variable. However, the
# multinomial logistic model doesn't assume the order of categories. 
# This flexibility comes with compromises: it takes more variables, and it's more
# complicated to interpret the result.
# The employment status data in acl is a multinomial (response) variable.
acl[, EmployG_W2 := as.character(Employment_W2)]
acl[EmployG_W2 %in%c(
  "(2) 2500+HRS", "(3) 15002499",
  "(4) 500-1499", "(5) 1-499HRS"),
  EmployG_W2 := "(2) EMPLOYED"]
acl[, EmployG_W2 := factor(EmployG_W2)]
xtable(as.data.frame(table(acl$EmployG_W2)),
       caption = "Frequency table of employment",
       label = "tglm2-freqtab")
# The practice of multinomial logistic regression is much like that of ordered
# logistic regression, and the only difference is the 'family' argument in vglm()
# is 'multinomial'.
m0.mr <- vglm(EmployG_W2 ~ SelfEfficacy_W1,
              family = multinomial(),
              data = acl, model = TRUE)
# Estimate and apply IPWs
w <- ipwpoint(exposure = SelfEfficacy_W1,
              family = "gaussian",
              numerator = ~ 1,
              denominator = ~ 1 + Sex + RaceEthnicity + AGE_W1,
              data = acl)
m1.mr <- vglm(EmployG_W2 ~ SelfEfficacy_W1,
              family = multinomial(),
              data = acl, model = TRUE,
              weights = winsorizor(w$ipw.weights, .01))
# Generate a result.
# NOTICE: while the ordered logistic regression model assumes the explanatory
# variable has identical effect on all categories of response variable, the
# multinomial logistic model doesn't. Therefore, there will be an estimate of
# coefficients for every category.
screenreg(list(m0.mr, m1.mr))
# Similar to what we've done to the ordered logistic model, we generate plots
# to illustrate how the predicted probability of a category of employment changes
# along the self efficacy.
preddat4 <- data.table(SelfEfficacy_W1 = 
                         seq(from = min(acl$SelfEfficacy_W1, na.rm = TRUE),
                             to = max(acl$SelfEfficacy_W1, na.rm = TRUE),
                             length.out = 1000))
preddat4 <- cbind(preddat4, 
                  predict(m1.mr, newdata = preddat4, type = "response"))
preddat4 <- melt(preddat4, id.vars = "SelfEfficacy_W1")
ggplot(preddat4, aes(SelfEfficacy_W1, value, colour = variable, 
                     linetype = variable)) +
  geom_line(size = 2) +
  scale_color_viridis(discrete = TRUE) +
  scale_x_continuous("Self-Efficacy") +
  scale_y_continuous("Probability", label = percent) +
  coord_cartesian(ylim = c(0, .65), expand = FALSE) +
  theme_tufte() +
  theme(legend.position = c(.18, .82),
        legend.key.width = unit(2, "cm"))


# GLM Part 2 (3): Poisson Regression and Negative Binomial Regression
# Poisson regression suits best the response variables of counted values.
# In acl, the NChronic12_W1 and _W2 records the chronic disease in the last 12
# months, hence it is a ideal variable to be analysed using Poisson regression.
# To begin with, we produce table and graphic summary for the data.
egltable(c("NChronic12_W1", "NChronic12_W2"),
         data = acl, parametric = FALSE)
plot_grid(
  ggplot(acl, aes(NChronic12_W1)) +
  geom_bar() + theme_tufte(),
  ggplot(acl, aes(NChronic12_W2)) +
    geom_bar() + theme_tufte(),
  ncol = 1,
  label = c("Wave 1", "Wave 2"),
  label_x = .8
)s
# Poisson regression can also be performed using vglm() function, with argument
# 'family' being 'poissonff()'.
m0.pr <- vglm(NChronic12_W2 ~ SelfEfficacy_W1,
              family = poissonff(),
              data = acl, model = TRUE)
summary(m0.pr)
# Before continuing discussion, it's appropriate to check whether the assumptions
# of Poisson regression model: no over-dispersion and homogeneity. One of the
# best ways is simultaneously fit the negative binomial model and check levels
# of fitting.
m0.nbr <- vglm(NChronic12_W2 ~ SelfEfficacy_W1,
               family = negbinomial(),
               data = acl, model = TRUE)
# Then, we assess the Akaike Information Criteria (AIC) and Bayesian Information
# Criteria (BIC) between two models.
AIC(m0.nbr) - AIC(m0.pr)
BIC(m0.nbr) - BIC(m0.pr)
# The above results indicate that the negative binomial model has smaller AIC
# and BIC. Again, we apply IPW for adjusting confounding factors and compare
# the raw and adjusted models.
w <- ipwpoint(exposure = SelfEfficacy_W1,
              family = "gaussian",
              numerator = ~ 1,
              denominator = ~ 1 + Sex + RaceEthnicity + AGE_W1,
              data = acl)
m1.nbr <- vglm(NChronic12_W2 ~ SelfEfficacy_W1,
               family = negbinomial(),
               data = acl, model = TRUE,
               weights = winsorizor(w$ipw.weights, .01))

xtable(rbind(
  data.table(Type = "Raw",
             Labels = rownames(coef(summary(m0.nbr))),
             coef(summary(m0.nbr))),
  data.table(Type = "Adj",
             Labels = rownames(coef(summary(m1.nbr))),
             coef(summary(m1.nbr)))),
  digits = 2,
  caption = "Comparison of unadjusted (raw) and adjusted negative binomial regression models",
  label = "tglm2-nbrcompare"
)