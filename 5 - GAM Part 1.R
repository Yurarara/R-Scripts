# Generalized Additive Model (GAM) is the expansion of GLM: it can also be used
# on continuous and discrete results. However, while GLM is a parametric model,
# GAL is a semi-parametric model: it allows for both parametric and non-parametric
# relationship between the result and predictions.
library(checkpoint)
book_directory = "F:/R directories/book directory"
checkpoint_directory = "D:/Documents/R/win-library/3.5.1"
checkpoint("2018-09-28", R.version = "3.5.1", 
           project = book_directory, 
           checkpointLocation = checkpoint_directory, 
           scanForPackages = FALSE, 
           scan.rnw.with.knitr = TRUE, 
           use.knitr = TRUE)
library(knitr)
library(data.table)
library(cowplot)
library(extraoperators)
library(ggplot2)
library(ggthemes)
library(scales)
library(viridis)
library(car)
library(mgcv)
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
acl <- readRDS("advancedr_acl_data.RDS")
# One important concept regarding GAM is smoothing spline, which allows for 
# simulating functions of unknown form (with high-rank polynomials).
# NOTICE: it's not essentially true that a polynomial with higher rank is better
# that one with lower rank. Although high rank polynomial models can simulate
# the performance of very complex functions, abnormalities tend to appear at
# extreme values - this consists of the main problem to be tackled when building
# a model with regression spline.

# The idea of splines can be inferred as the assembly of multiple models: a
# model with splines is in fact the collection of multiple polynomial models.
# To study such assemble of functions at different data points, we start by
# familiarizing with the %gle% operators in the 'extraoperators' package.
# These operators return a Boolean value indicating the coverage relationship
# between a (list of) number and a range.
# > and <
1:5 %gl% c(2,4)
# >= and <
1:5 %gel% c(2,4)
# > and <=
1:5 %gle% c(2,4)
# >= and <=
1:5 %gele% c(2,4)
# The following code illustrates some assembled uniform, linear, quadratic and 
# cubed polynomials.
ggplot(acl, aes(AGE_W1, CESD11_W1)) +
  stat_smooth(method = "lm",
              formula = y ~ 1 +
                ifelse(x %gle% c(42, 65), 1, 0) +
                ifelse(x %gle% c(42, 65), 1, 0),
              colour = viridis(6)[1], linetype = 1, se = FALSE) +
  stat_smooth(method = "lm",
              formula = y ~ bs(x, df = 3, degree = 1L),
              colour = viridis(6)[2], linetype = 2, se = FALSE) +
  stat_smooth(method = "lm",
              formula = y ~ bs(x, df = 4, degree = 2L),
              colour = viridis(6)[3], linetype = 3, se = FALSE) +
  stat_smooth(method = "lm",
              formula = y ~ bs(x, df = 5, degree = 3L),
              colour = viridis(6)[4], linetype = 4, se = FALSE)
# Our study of splines begins with B-spline, or basis spline. Its iconic feature,
# the ability to reduce overlay of basis functions in specific ranges, makes the
# corresponding computation much easier. The following code illustrates the
# overlay of the basis functions of B-splines, and their shapes at given knots.
knots <- c(33, 42, 57, 65, 72)
x <- seq(from = min(acl$AGE_W1), to = max(acl$AGE_W1), by = .01)
p1 <- ggplot(melt(bs(x, degree = 1, knots = knots, intercept = TRUE)),
             aes(Var1, value, colour = factor(Var2)))+
  geom_line() +
  scale_color_viridis("Basis", discrete = TRUE) +
  theme_tufte()
plot_grid(
  p1 +
    ggtitle("5 Knots, Degree = 1"),
  p1 %+% melt(bs(x, degree = 2, knots = knots, intercept = TRUE)) +
    ggtitle("5 Knots, Degree = 2"),
  p1 %+% melt(bs(x, degree = 2, knots = knots, intercept = TRUE)) +
    ggtitle("5 Knots, Degree = 3"),
  p1 %+% melt(bs(x, degree = 3, knots = knots, intercept = TRUE)) +
    ggtitle("5 Knots, Degree = 4"),
  p1 %+% melt(bs(x, degree = 4, knots = knots, intercept = TRUE)),
  ncol = 2
)
# Then we move onto the smooth spline. it's important that to build a smooth
# spline model, the degree of polynomial and knots are not predetermined, but
# acquired via auto learning.


# Next, we implement GAM models in R. In this part, we focus on the basic GAM
# with no cross effect.
# Here, the vgam() function comes into play. It is much like the previous vglm()
# function, but uses smooth spline, which is added into the model via another
# function s(). The 'df' argument in s() denotes the highest polynomial degree
# to be used in smooth splines. When the 'family' argument takes 'uninormal()',
# the model simulates the scale and position of the normal distribution.
mgam <- vgam(CESD11_W1 ~ Sex + s(AGE_W1, df = 3), 
             data = acl, family = uninormal(), model = TRUE)
# summary() returns the smoothing parameter and p values.
# NOTICE: the meaning of the significance of a smoothing parameter is that, if a
# smoothing parameter is statistically significant, then the model with this
# parameter would perform differently compared to the one with the linear form 
# of this parameter.
summary(mgam)
# Meanwhile, coef() returns the model's fitted coefficients. 
coef(mgam)
# Then, we want to test the coefficient for Sex. The following function in 'car'
# can do just that.
linearHypothesis(mgam, "Sex(2) FEMALE", coef. = coef(mgam), 
                 vcov = vcov(mgam))
# As powerful as this function, it can also test more complex linear assumptions.
# Here, it tests whether the coefficient for Sex and intercept are both zero.
linearHypothesis(mgam,
                 c("(Intercept):1", "Sex(2) FEMALE"),
                 coef. = coef(mgam),  vcov = vcov(mgam))
# To compare the GAM result with other previously mentioned predictions, we fit
# 2 ordinary GLM models: one with a linear parameter of AGE_W1, and another with
# a quadratic parameter of AGE_W1 with poly().
mlin <- vglm(CESD11_W1 ~ Sex + AGE_W1, 
             data = acl, family = uninormal(), MODEL = TRUE)
mquad <- vglm(CESD11_W1 ~ Sex + poly(AGE_W1, 2), 
             data = acl, family = uninormal(), MODEL = TRUE)
# The following code generates a graph for comparing results.
par(mfrow =c(1, 2))
plot(mgam, se = TRUE, which.term = 2,
     lcol = viridis(4)[1], scol = viridis(4)[1])
plot(as(mlin, "vgam"), se = TRUE, which.term = 2,
     lcol = viridis(4)[2], scol = viridis(4)[2],
     overlay = TRUE, add = TRUE)
plot(mgam, se = TRUE, which.term = 2,
     lcol = viridis(4)[1], scol = viridis(4)[1])
plot(as(mquad, "vgam"), se = TRUE, which.term = 2,
     lcol = viridis(4)[3], scol = viridis(4)[3],
     overlay = TRUE, add = TRUE)
# It can be inferred from the RHS graph that the linear model with quadratic 
# polynomial is good enough. However, another advantage of GAM is that it gives
# some unique inferences.
# Consider another example where we predict the depression in 2nd wave based on
# gender, the smooth spline of 1st wave depression and 1st wave age.
mgam2 <- vgam(CESD11_W2 ~ Sex + 
                s(CESD11_W1, df = 3) +
                s(AGE_W1, df = 3), data = acl,
              family = uninormal(), model = TRUE)
# NOTICE: the output of summary() gives a 0.1 p-value of the AGE_W1 smooth 
# spline. This indicates that the non-linearity of age is not statistically
# significant.( = probably linear)
summary(mgam2)
# Based on the above inference, we adjust the model and use the linear parameter.
mgam3 <- vgam(CESD11_W2 ~ Sex + 
                s(CESD11_W1, df = 3) +
                AGE_W1, data = acl,
              family = uninormal(), model = TRUE)
summary(mgam3)
# Similarly, we again use linearHypothesis() to test significancy of parameters.
# names() together with coef() can help acquiring names of parameters, which is
# to be used as arguments of linearHypothesis().
# Although altered to linear form, the age parameter is still not statistically
# significant.
names(coef(mgam3))
linearHypothesis(mgam3, "Sex(2) FEMALE", coef = coef(mgam3), vcov = vcov(mgam3))
linearHypothesis(mgam3, "AGE_W1", coef = coef(mgam3), vcov = vcov(mgam3))
# After getting these results, we would want to visualize them.
# The graph indicates that the lower level (less than 2) of 1st wave depression,
# to some extent, can predict a high value of the lower level of 2nd wave 
# depression (indicated by the flatter curve at higher level).
par(mfrow = c(2,2))
plot(mgam3, se = TRUE, lcol = viridis(4)[1],
     scol = viridis(4)[2])
# In the aspect of prediction, we still need to generate data for predictions.
# Here, we use whole range of sex and depression symptoms, and a five number
# summary of age (lowest, greatest and 25% quantiles.)
newdat<- as.data.table(expand.grid(
  Sex = levels(acl$Sex),
  CESD11_W1 = seq(
    from = min(acl$CESD11_W1, na.rm = TRUE),
    to = max(acl$CESD11_W1, na.rm = TRUE),
    length.out = 1000),
  AGE_W1 = fivenum(acl$AGE_W1)))
newdat$yhat <- predict(mgam3, newdata = newdat)
ggplot(newdat,
       aes(CESD11_W1, yhat, 
           colour = factor(AGE_W1), linetype = factor(AGE_W1))) +
  geom_line() +
  scale_color_viridis("Age", discrete = TRUE) +
  scale_linetype_discrete("AGE") +
  facet_wrap(~ Sex) +
  theme(legend.position = c(.75, .2),
        legend.key.width = unit(1.5, "cm")) +
  xlab("Depression Symptoms (Wave 1)") +
  ylab("Depression Symptoms (Wave 2)")


# Now, although the above analysis is fairly comprehensive, there are still some
# advance functions not included in the VGAM package, but implemented in the
# package named mgcv. The usage of functions in mgcv is much similar to using
# the vgam() function, especially there's also an s() function to introduce
# smooth spline.
# NOTICE: some functions in mgcv will collide with the ones in VGAM, so we would
# want to detach the the latter before library() the former.
detach("package:VGAM")
library(mgcv)
# In practice, the distribution family argument in gam() is 'gaussian()', 
# different from the 'uninormal()' in vgam(). In addition, while vgam() uses
# the 'df' argument in s() to control the maximum flexibility of smooth splines,
# the same parameter is named 'k' in gam().
mgam4 <- gam(CESD11_W2 ~ Sex +
               s(CESD11_W1, k = 3) +
               s(AGE_W1, k = 3), data = acl,
             family = gaussian())
# The output of summary() on gam() results contains more useful information 
# compared to vgam() results. Besides the estimated coefficients, it automatically
# tests for significance of smoothing parameters.
# Although the similar test is also included in the result of vgam(), the latter 
# only tests for its significance related to the linear form, but gam() also 
# provides the overall significance test: whether this parameter makes difference 
# in the model.
summary(mgam4)
# gam() also provides an easy way of acquiring the estimate of degrees of freedom.
# We change the k argument in s(), and observe the change in df for both parameters.
# We see that there's nearly 1 unit increase in df for the depression level.
mgam5 <- gam(CESD11_W2 ~ Sex +
               s(CESD11_W1, k = 4) +
               s(AGE_W1, k = 4), data = acl,
             family = gaussian())
summary(mgam5)
# Finally, we plot the result of both models and line them up. It seems that
# at higher k value, the flattened (even downward) pattern at higher depression 
# level is more apparent.
par(mfrow = c(2, 2))
plot(mgam4, se = TRUE, scale =0, mail = "k = 3")
plot(mgam5, se = TRUE, scale =0, mail = "k = 4")


# We now move on to study the GAM with interaction. To apply smooth splines in
# interaction parameters, we'll need to use the mgcv package, as VGAM doesn't
# support such analysis.
# Our basic assumption for interaction is that, the effect of depression or
# age is subject to the type of gender. To include a interaction factor in
# the fitting, simply use the 'by' argument in s() provided by mgcv.
mgam6 <- gam(CESD11_W2 ~ Sex +
               s(CESD11_W1, k = 4, by = Sex) +
               s(AGE_W1, k = 4, by = Sex),
             data = acl, 
             family = gaussian())
# The coefficient estimate from the new model is not much different to the one
# from models without interaction. In fact, the coefficient for either gender
# is much similar.
summary(mgam6)
# For comparing, we borrow a model from the previous script. This mgam5 model
# is identical to the new model, except for that it's without interaction. 
mgam5 <- gam(CESD11_W2 ~ Sex +
               s(CESD11_W1, k = 4) +
               s(AGE_W1, k = 4), data = acl,
             family = gaussian())
# We calculate the AIC and BIC to determine which model to prefer. The original
# model is superior for both criteria and is simpler (smaller df). Therefore,
# we prefer the model without interactions.
AIC(mgam5, mgam6)
BIC(mgam5, mgam6)

# The mgcv package also allows for checking whether the maximum smooth level
# is limited. This is directly related to the choice of k value in s() function.
# If the estimated df is far less than k-1, then there coul be barely any benefit 
# from further increasing the k value. On the other hand, if the estimated df
# is much close to k-1, increasing k value may present different result.
# In previous fittings (mgam5, where k = 4), the estimated df of 1st wave
# depression is very close to k-1 (2.99), which implies potential space for
# improvement.
summary(mgam5)
# To determine whether to increase the k value, we can use gam.check(). As this
# function depends on simulations, we set a seed for reproducibility. The result
# is presented in graphs.
par(mfrow = c(2, 2))
set.seed(12345)
gam.check(mgam5)
# The above code produces the conclusion that the p-value for basis dimension (k)
# checking is too low, indicating the need to increase k value.
# Hence, we increase the k value for both variables to 20. The fitting returns
# an estimated k of 13.99 for 1st wave depression.
mgam5b <- gam(CESD11_W2 ~ Sex +
                s(CESD11_W1, k = 20) +
                s(AGE_W1, k = 20), data = acl,
              family = gaussian())
summary(mgam5b)
# We can also plot the result. It's observable that the model with higher level
# of smoothing expresses higher flexibility.
par(mfrow = c(2, 2))
plot(mgam5, se = TRUE, scale = 0)
plot(mgam5b, se = TRUE, scale = 0)