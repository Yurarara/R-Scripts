library(checkpoint)
book_directory = "F:/R directories/book directory"
checkpoint_directory = "D:/Documents/R/win-library/4.1"
checkpoint("2021-06-16", R.version = "4.1.0", 
           project = book_directory, 
           checkpointLocation = checkpoint_directory, 
           scanForPackages = FALSE, 
           scan.rnw.with.knitr = TRUE, 
           use.knitr = TRUE)
library(knitr)
library(data.table)
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
  digits = 5
)


# 1. ANOVA application in R (1): identically distributed linear regression
# Data Generation
set.seed(1234)
example <- data.table(
  y = rnorm(9),
  Condition = factor(rep(c("A", "B", "Control"), each = 3), 
  levels = c("Control", "A", "B"))
)

# Using lm() for simple linear regression: ANOVA is GLM with all variables being
# dummy.
# The coefficients should be the difference among group averages, which is then 
# to be analysed for significance.
coef(lm(y ~ Condition, data = example))
# When simply calculating group means, the mean of Control group is the intercept
# of coefficients above.
# And, the rest two numbers will become the corresponding coefficients when 
# subtracted by this intercept. 
example[, .(M = mean(y)), by = Condition]
# To see the cross effect among variables, use model.matrix() and '*' operator.
mtcars <- as.data.table(mtcars)
mtcars[, ID := factor(1:.N)]
mtcars[, vs := factor(vs)]
mtcars[, am := factor(am)]
head(model.matrix(~ vs * am, data = mtcars))


# 1. ANOVA application in R (2): ANOVA in practice
# ezANOVA can be used for ANOVA based on multiple types of models.
# Specifically, the function takes an ID argument 'wid' and an argument 
# specifying groups.
# In this case, the example data of 3 conditions are from the same distribution:
# no significance.
example[, ID := factor(1:.N)]
print(ezANOVA(
  data = example, 
  dv = y,
  wid = ID,
  between = Condition,
  type = 3, 
  detailed = TRUE
))
# Alternatively, applying ANOVA to mtcars data produces results with some 
# significance.
# NOTICE: here we're studying not only the independent effects of vs and am, 
# but also their cross effects.
print(ezANOVA(
  data = mtcars, 
  dv = mpg,
  wid = ID,
  between = vs * am,
  type = 3, 
  detailed = TRUE
))


#2. GLM and Linear Regression (1): basics
# Open the file: this .RDS file is already modified for convinience purpose.
acl <- readRDS("advancedr_acl_data.RDS")
# Density plot for checking normality - generally normal.
plot(testDistribution(acl$SWL_W1, "normal", 
                      verlab = "Satisfaction with Life", plot = FALSE, 
                      extremevalues = "theoretical", 
                      adjust = 2))$DensityPlot
# Ordinary Least Square method for linear regression. 
m.ols <- ols(SWL_W1 ~ Sex + AGE_W1 + SESCategory, data = acl, x = TRUE)
# Denote the formation of output: texreg() for LATEX output, htmlreg() for HTML 
# output.
screenreg(m.ols, single.row = TRUE, label = "tglml-olstex")
# Variance of Inflation Factor: degree of collinearity. A VIF value close to 1 
# indicate little collinearity.
vif(m.ols)
# A data frame of fitted vs residuals to determine the normality of errors.
diagnostic.data <- data.table(
  fitted = fitted(m.ols), 
  resid = residuals(m.ols)
)
# Test normality with testDistribution() and check normality using density plot.
plot(testDistribution(diagnostic.data$resid, 
                 "normal", 
                 verlab = "Satisfaction with Life Residuals", plot = FALSE, 
                 extremevalues = "theoretical", adjust = 2))$DensityPlot
# Check homogeneity with quantile plots.
ggplot(diagnostic.data, aes(fitted, resid)) +
  geom_point(alpha = .2, colour = "grey50") +
  geom_quantile(quantiles = .5, colour = "black", size = 1) +
  geom_quantile(quantiles = c(.25, .75), colour = "blue", linetype = 2, size = 1) +
  geom_quantile(quantiles = c(.05, .95), colour = "black", linetype = 3, size = 1)
# glm() can also conduct the linear regression as ols(), but it's more flexible:
# it allows specifying various types of distribution and link functions.
# However, R^2 is not covered in the result of glm(), for this statistic doesn't
# always make sense given different types of distribution.
m.glm <- glm(SWL_W1 ~ Sex + AGE_W1 + SESCategory, data = acl, family = gaussian(link = "identity"))
m.glm
# Use summary() to get detailed output.
summary(m.glm)


# 2. GLM and Linear Regression (2): studying specific factor
# To study the effect of a certain variable in the model, use update() to alter
# the model.
# NOTICE: by surrounding the model with brackets, the analysis result is forced
# to be printed.
(m.ols2 <- update(m.ols, . ~ . + Employment_W1))
# In order to determine the significance of a certain variable, use anova().
# In this case, the newly introduced variable Employment_W1 is significant.
anova(m.ols2)
# Again, use update() if we want to rule out the non-significant variable Sex,
# and also to study the cross effect between Age and SES (social economy status).
# It turns out that the age and SES does have cross effect for those with 3 and 
# 4 categories of SES (p = 0.0088 & p = 0.0090).
(m.ols3 <- update(m.ols2, . ~ . - Sex + AGE_W1 * SESCategory))
# To further investigate the age-SES effect, use visreg() for visualised analysis.
# NOTICE: the arguments of visreg() are used as followed: 
# overlay - TRUE if all plots are to be integrated into one.
# partial - TRUE if the partial error plots is to be shown.
# rug - TRUE if the rug plot is to be shown (used to determine the data point on
# x-axis.)
# band - TRUE if the confidence interval is to be shown.
plot(visreg(m.ols3, xvar = "AGE_W1", by = "SESCategory", plot = FALSE), 
     overlay = TRUE, partial = FALSE, rug = FALSE, 
     xlab = "Age (years)", ylab = "Predicted Life Satisfaction", 
     line = list(lty = 1:4, col = c("black", "grey75", "grey50", "grey25")), 
     band = FALSE)


# 2. GLM and Linear Regression (3): high-performance regression
# When the amount of data for processing is big, the performance of linear
# regression becomes important. This usually happens in bootstrapping, where 
# large amount of simulations are needed.
# This part illustrates the application of some high-performance linear regression.
# To begin with, create a specified data table.
tmpdat <- na.omit(acl[, .(SWL_W1, AGE_W1, SESCategory, Employment_W1)])
# The following code realises a simple procedure:
# 1. Record run time using system.time().
# 2. Iterate, using sapply(), to generate index for every row.
# 3. Perform simulated linear fitting and get regression coefficients.
set.seed(12345)
t1 <- system.time(ols.boot <- sapply(1:500, function(i) {
  index <- sample(nrow(tmpdat), size = nrow(tmpdat), replace = TRUE)
  coef(ols(SWL_W1 ~ AGE_W1 * SESCategory + Employment_W1, data = tmpdat[index]))
  }))
# The processing time, on my PC, is 3.28s in total. It's not that long but still
# noticeable. It will certainly be a lot longer when processing larger data.
t1
# The fastLM() provided by RcppEigen package performes linear regression based on
# C++, hence the efficiency is significantly higher.
set.seed(12345)
t2 <- system.time(rcpp.boot1 <- sapply(1:500, function(i){
  index <- sample(nrow(tmpdat), size = nrow(tmpdat), replace = TRUE)
  coef(fastLm(SWL_W1 ~ AGE_W1 * SESCategory + Employment_W1, data = tmpdat[index]))
}))
# When switched to fastLM(), the processing time went down to 1.67s.
t2
# A more primal - or, pure version of fastLM() is fastLmPure(). It guarantees
# even shorter processing time, but it depends on the user input of variables and
# model matrix. In addition, this function does not support model interface in R.
# Hence when recording processing time, the calculating of variable vector and
# model matrix should be taken into account.
set.seed(12345)
t3 <- system.time({
  y <- tempdat[, SWL_W1]
  X <- model.matrix(~ AGE_W1 * SESCategory + Employment_W1, data = tmpdat)
  N <- nrow(tmpdat)
  rcpp.boot2 <- sapply(1:500, function(i){
    index <- sample.int(N, size = N, replace = TRUE)
    fastLmPure(X = X[index, ], y = y[index])$coefficients
  })
})
# The processing time goes down to 0.33s in this case.
t3
# Given this high efficiency, we try its performance when dealing with 10,000
# simulations.
set.seed(12345)
t4 <- system.time({
  y <- tempdat[, SWL_W1]
  X <- model.matrix(~ AGE_W1 * SESCategory + Employment_W1, data = tmpdat)
  N <- nrow(tmpdat)
  rcpp.boot2 <- sapply(1:10000, function(i){
    index <- sample.int(N, size = N, replace = TRUE)
    fastLmPure(X = X[index, ], y = y[index])$coefficients
  })
})
# It took in total 6.53s on my PC. Considering the ols() function would take up
# to over 60s, it's very impressive improvement on performance.
t4
# Finally, besides for the processing time, the result of linear fitting is also
# priority. To check the consistency of results, use all.equal().
# The following functions both return TRUE, indicating they have same results.
all.equal(ols.boot, rcpp.boot1, check.attributes = FALSE)
all.equal(ols.boot, rcpp.boot2, check.attributes = FALSE)


# 2. GLM and Linear Regression (4): controlling cross effects
# The CESD11_W2 column in data ACL indicates the level of depression reported by
# individuals, and SelfEfficacy_W1 represents the individual's assessment of the
# ability to change or control one's own life. Intuitively, it's of interest to 
# find out whether the level of self efficacy can predict the level of depression.
# The following model does just that.
m0 <- ols(CESD11_W2 ~ SelfEfficacy_W1, data = acl)
# The result gives a -0.36 coefficient with p<0.001 significance, indicating that
# the lower self efficacy one have, the higher level of depression he/she might
# experience.
screenreg(m0, label = "tglm1 - olsunadj")
# However, very importantly, this negative correlation doesn't necessarily lead 
# to causation - they might be as well the result of some other factors.
# This where the Inverse Probability Weight comes into play. The induction is as
# followed:
# 1. The reason of correlation not equal to causation is that the probability
# distribution P(X) - the ideal causation probability - is not viable.
# 2. The reason for that is that P(X) is conditioned on the confounding factor(s)
# W, the potential factors affecting Y. Hence, the real probability of X on Y is
# P(X|W),
# 3. To remove the influence of W, a sensible method is making this conditioned
# probability equal to 1. To achieve this, a coefficient(weight) is necessary,
# and its value is the reciprocal of the conditioned probability: 1/P(X|W).
# To cover the whole proving process in this note would be too off the topic, so
# here's the conclusion: by introducing IPW, the effect of confounding factors
# is removed, and we can focus on the actual effect posed by the factor of 
# interest.
# This can be done using the ipw package. The argument exposure indicates the
# factor of interest, and family denotes the distribution family to be used.
# In the numerator and denominator arguments, we specify the IPW model - notably
# the confounding variables.
w <- ipwpoint(
  exposure = SelfEfficacy_W1,
  family = "gaussian",
  numerator = ~ 1,
  denominator = ~ 1 + Sex + RaceEthnicity + AGE_W1 + NChronic12_W1,
  data = acl
)
# This weight can then be introduced into the ols model.
m1 <- ols(CESD11_W2 ~ SelfEfficacy_W1, data = acl, 
          weights = winsorizor(w$ipw.weights, .01))
# It can be observed that the coefficient produced by the updated model is 
# smaller than the previous one, but level of significance is not harmed.
screenreg(list(m0, m1), label = "tglm1-weight1")
# Add more confounding variables - they may be mixed or medium variables.
w2 <- ipwpoint(
  exposure = SelfEfficacy_W1, 
  family = "gaussian",
  numerator = ~ 1,
  denominator = ~ 1 + Sex+ RaceEthnicity + AGE_W1 + NChronic12_W1 + SESCategory
  + Employment_W1 + BMI_W1 + Smoke_W1 + PhysActCat_W1, 
  data = acl
)
m2 <- ols(CESD11_W2 ~ SelfEfficacy_W1, data = acl,
          weights = winsorizor(w2$ipw.weights, .01))
screenreg(list(m0, m1, m2), label = "tglm1-weight2")
# Another way of inspecting potential confounding factors is simply adding these
# factors (explicitly) into the model.
m1b <- ols(CESD11_W2 ~ Sex + RaceEthnicity + AGE_W1 + 
             NChronic12_W1 + SelfEfficacy_W1, data = acl)
m2b <- ols(CESD11_W2 ~ Sex + RaceEthnicity + AGE_W1 + 
             NChronic12_W1 + SESCategory +
             Employment_W1 + BMI_W1 + Smoke_W1 + PhysActCat_W1 +
             SelfEfficacy_W1, data = acl)
screenreg(list(m1b, m2b), label = "tglm1-extra")
# Doubly Robust Estimator: a robust estimator combining the above 2 methods.
# In another word, it performs linear regression with IPW, but also takes all
# other factors of interest into the model.
m1c <- ols(CESD11_W2 ~ Sex + RaceEthnicity + AGE_W1 + 
             NChronic12_W1 + SelfEfficacy_W1, data = acl,
           weights = winsorizor(w$ipw.weights, .01))
m2c <- ols(CESD11_W2 ~ Sex + RaceEthnicity + AGE_W1 + 
             NChronic12_W1 + SESCategory +
             Employment_W1 + BMI_W1 + Smoke_W1 + PhysActCat_W1 +
             SelfEfficacy_W1, data = acl, 
           weights = winsorizor(w2$ipw.weights, .01))
screenreg(list(m1b, m2b), label = "tglm1-robust")
# With all results above, we would like to define a function for extracting
# estimates and CIs.
extractor <- function(obj, label){
  b = coef(obj)
  ci = confint(obj)
  tb <- data.table(
          Type = label,
          B = b[["SelfEfficacy_W1"]],
          LL = ci["SelfEfficacy_W1", "2.5 %"],
          UL = ci["SelfEfficacy_W1", "97.5 %"]
        )
  return(tb)
}
allresults <- rbind(
  extractor(m0, "M0: Unadjusted"),
  extractor(m1, "M1: Partial IPW"),
  extractor(m1b, "M1: Partial Covs"),
  extractor(m1c, "M1: Partial Covs + IPW"),
  extractor(m2, "M2: Full IPW"),
  extractor(m2b, "M2: Full Covs"),
  extractor(m2c, "M2: Full Covs + IPW")
)
allresults[, Type := factor(Type, levels = Type)]
ggplot(allresults, aes(Type, y = B, ymin = LL, ymax = UL)) +
  geom_pointrange() +
  coord_flip() +
  xlab("") + ylab("Estimate + 95% CI")

