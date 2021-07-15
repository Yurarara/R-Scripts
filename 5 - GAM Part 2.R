library(checkpoint)
book_directory = "F:/R directories/book directory"
checkpoint_directory = "F:/R directories/checkpoint directory"
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

# This part is about the behavior of non-continuous responsive variables in GAM.
# Similar to how it works in GLM, GAM uses the same distribution families to
# tackle with these problems.
# First, we study the non-linear relationship between the smoking status and age.
acl$CurSmoke = as.integer(acl$Smoke_W1 == "(1) Cur Smok")
mgam.lr1 <- vgam(CurSmoke ~ s(AGE_W1, df = 3), 
                 family = binomialff(link = "logitlink"),
                 data = acl, model = TRUE)
summary(mgam.lr1)
par(mfrow = c(1, 1))
# We plot the result to better understand it. It seems that the smoking habit
# changes very little at young ages, but drops at ages over 60.
plot(mgam.lr1, se = TRUE,
     lcol = viridis(4)[1],
     scol = viridis(4)[2])
# Then, we make predictions.
newdat <- as.data.table(expand.grid(
  AGE_W1 = seq(
    from = min(acl$AGE_W1, na.rm = TRUE),
    to = max(acl$AGE_W1, na.rm = TRUE),
    length.out = 1000
  )))
newdat$yhat <- predict(mgam.lr1,
                       newdata = newdat,
                       type = "responsive")
ggplot(newdat, aes(AGE_W1, yhat)) +
  geom_line() +
  scale_y_continuous(labels =  percent) +
  xlab("Age (years)") +
  ylab("Probability of Smoking") + 
  coord_cartesian(xlim = range(acl$AGE_W1),
                  ylim = c(0, .4),
                  expand = FALSE)
# A limitation of the VGAM package regarding predictions is that it doesn't have
# functions to calculate confidence intervals. We can manually do it via
# bootstrapping, which costs little time for relatively simple models. However,
# as the complexity of model increases, it may need parellel processing to speed
# it up.
nboot <- 500
out <- matrix(NA_real_, ncol = nboot, nrow = nrow(newdat))
start.time <- proc.time()
set.seed(12345)
for(i in 1:500){
  tmp <- vgam(CurSmoke ~ s(AGE_W1, df = 3), 
              family = binomialff(link = "logitlink"),
              data = acl[sample(nrow(acl), replace = TRUE)], model = TRUE)
  out[, i] <- predict(tmp, 
                      newdata = newdat,
                      type = "responsive")
}
stop.time <- proc.time()
stop.time - start.time
# Now that we have the simulated estimates, it's easy to calculate the CI. But
# before that, we want to know if the simulated value is close to the real one.
# The following code gives a micro level average absolute difference.
mean(abs(newdat$yhat - rowMeans(out)))
# We calculate CI, but using the quantiles of bootstrap samples.
newdat$LL <- apply(out, 1, quantile, probs = .025, na.rm = TRUE)
newdat$UL <- apply(out, 1, quantile, probs = .975, na.rm = TRUE)
# Finally, we plot again the result, in addition of the simulated CI.
# The CI is not necessarily smooth, because the number of bootstrapping is still
# very low (500). Even so, it provides information regarding the uncertainty of 
# predicted smoking probability throughout all ages.
ggplot(newdat, aes(AGE_W1, yhat)) +
  geom_ribbon(aes(ymin = LL, ymax = UL), fill = "grey80") +
  geom_line(size = 2) +
  scale_y_continuous(labels =  percent) +
  xlab("Age (years)") +
  ylab("Probability of Smoking") + 
  theme_tufte() +
  coord_cartesian(xlim = range(acl$AGE_W1),
                  ylim = c(0, .5),
                  expand = FALSE)


# The second sort of responsive variable we're looking at is the disordered 
# responsive variables. In the previous part regarding GLM, we use logistic  
# regression totackle such variables, and it can also be applied in the GAM 
# context.
# First, we alter the data to generate a variable illustrating the employment
# status.
acl[, EmployG_W2 := as.character(Employment_W2)]
acl[EmployG_W2 %in% c(
  "(2) 2500+HRS", "(3) 15002499",
  "(4) 500-1499", "(5) 1-499HRS"),
  EmployG_W2 := "(2) EMPLOYED"]
# Then, we fit a GAM model predicting the employment status using the smooth
# spline of age.
mgam.mr1 <- vgam(EmployG_W2 ~ s(AGE_W1, k = 5), 
                 family = multinomial(),
                 data = acl, model = TRUE)
# NOTICE: for disordered responsive variables, determining whether a smoothing 
# parameter has non-linearity cannot depend on one single test, but multiple
# tests for m-1 times, where m is the number of classes of the responsive 
# variable.
summary(mgam.mr1)
# We can also plot the result. The graph shows that some classes have larger
# non-linearity compared to the others, with significance. GAM with multinomial
# logistic regression allows the shape and flexibility of the smooth spline to
# vary by classes of responsive variable. 
par(mfrow = c(2, 2))
plot(mgam.mr1, se = TRUE,
     lcol = viridis(4)[1],
     scol = viridis(4)[2])
# After that, the prediction.
newdat <- as.data.table(expand.grid(
  AGE_W1 = seq(
    from = min(acl$AGE_W1, na.rm = TRUE),
    to = max(acl$AGE_W1, na.rm = TRUE),
    length.out = 1000)))
# The cbind() function is used here to merge the prediction estimates, for that
# the probabilities are not expressed in vectors, but in a matrix. Every element
# in the matrix is a probability prediction estimate for the corresponding
# class of responsive variable.
newdat <- cbind(newdat, predict(mgam.mr1,
                                newdata = newdat,
                                type = "responsive"))
# Then, from the above mentioned matrix, we transform the data into the long
# dataset needed for plotting probability prediction, which consists of the
# following variables: age, class of responsive variable, and the probability
# prediction.
newdat.long <- melt(newdat, id.vars = "AGE_W1")
summary(newdat.long)
# Finally, the plot. The plot gives the clues regarding some conclusions that's
# not attainable from a linear model: there's non-linearity in the age-employment
# relationship, because most people retire around 60 years old, with a small
# time window. This fact results in a sharp change in the employment probability
# along the otherwise flat patterns.
# We can spot such transferring process via GAM regression.
ggplot(newdat.long, aes(AGE_W1, value, 
                        colour = variable, linetype = variable)) +
  geom_line(size = 2) +
  scale_color_viridis(discrete = TRUE) +
  scale_x_continuous("Age (years)") +
  scale_y_continuous("Probability Predict", label = percent) +
  coord_cartesian(ylim = c(0, 1), expand = FALSE) +
  theme_tufte() +
  theme(legend.position = c(.2, 5),
        legend.key.width = unit(2, "cm"))


# The third type of responsive variable is the counts. As discussed previously
# in the GLM related part, they can be treated with Poisson or negative binomial
# distributions. The only unique part of GAM is the smooth spline, which works
# in the same manner as illustrated in the analysis of continuous normal
# distributed variables.
# The limitation of Poisson distribution, as discussed, is the often-appearing
# overdispersion. Therefore, we omit the Poisson analysis and focus on the
# negative binomial regression, which allows for overdispersion.
# This example aims to investigate the relationship between the appearance of 
# chronic diseases and gender and age.
mgam.nbr1 <- vgam(NChronic12_W2 ~ Sex + s(AGE_W1, k = 5),
                  family = negbinomial(),
                  data = acl, model = TRUE)
summary(mgam.nbr1)
# To better interpret the result, we plot them out. It seems that female has
# more chronic diseases than man, and the reported onsets of chronic diseases
# rise as age grows, especially faster at young ages.
par(mfrow = c(1, 2))
plot(mgam.nbr1, se = TRUE, 
     lcol = viridis(4)[1],
     scol = viridis(4)[2])
# Again, we generate data set for prediction.
newdat <- as.data.table(expand.grid(
  Sex = levels(acl$Sex), 
  AGE_W1 = seq(
    from = min(acl$AGE_W1, na.rm = TRUE),
    to = max(acl$AGE_W1, na.rm = TRUE),
    length.out = 1000)))
newdat$yhat <- predict(mgam.nbr1, newdata = newdat, type = "response")
# With predictions, we can now have plots. It can be observed that the difference
# of the effect of age towards chronic disease onsets increases along the age.
# However, there is no interaction between age and gender - the effect of age is
# the same to both genders.
ggplot(newdat, aes(AGE_W1, yhat, colour = Sex)) +
  geom_line(size = 2) +
  scale_color_viridis(discrete = TRUE) +
  xlab("Age (years)") +
  ylab("Chronic Diease Onset") +
  theme_tufte() +
  coord_cartesian(xlim = range(acl$AGE_W1),
                  ylim = c(0, 2.5), 
                  expand = FALSE) +
  theme(legend.position = c(.2, .8),
        legend.key.width = unit(1, "cm"))
# The above plot neglects the interaction between age and gender - a result of
# the VGAM package not supporting the interactions among smooth splines. To
# study the interactions, we detach VGAM and use the mgcv package.
detach("package:VGAM")
library(mgcv)
mgam.nbr2 <- gam(NChronic12_W2 ~ Sex + s(AGE_W1, k = 10, by = Sex),
                 family = nb(), data = acl)
summary(mgam.nbr2)
# And data for predictions and plots.
# It shows that females are not faster in entering the flat period. Even at 
# larger ages, females are expected to suffer from more chronic diseases.
newdat <- as.data.table(expand.grid(
  Sex = levels(acl$Sex), 
  AGE_W1 = seq(
    from = min(acl$AGE_W1, na.rm = TRUE),
    to = max(acl$AGE_W1, na.rm = TRUE),
    length.out = 1000)))
newdat$yhat <- predict(mgam.nbr2, 
                       newdata = newdat,
                       type = "response")
ggplot(newdat, aes(AGE_W1, yhat, colour = Sex)) +
  geom_line(size = 2) +
  scale_color_viridis(discrete = TRUE) +
  xlab("Age (years)") +
  ylab("Chronic Diease Onset") +
  theme_tufte() +
  coord_cartesian(xlim = range(acl$AGE_W1),
                  ylim = c(0, 2.7), 
                  expand = FALSE) +
  theme(legend.position = c(.2, .8),
        legend.key.width = unit(1, "cm"))
# To acquired CI, we need again to bootstrap the data as we did to the binomial
# GAM model, this time costing even longer time as the model complexity increases.
# Actually, the simulation takes nearly 3 minutes to complete on my PC.
nboot <- 500
out <- matrix(NA_real_, ncol = nboot, nrow = nrow(newdat))
start.time <- proc.time()
set.seed(12345)
for(i in 1:500){
  tmp <- gam(NChronic12_W2 ~ Sex + s(AGE_W1, k = 10, by = Sex),
             family = nb(), data = acl[sample(nrow(acl), replace = TRUE)])
  out[, i] <- predict(tmp, newdata = newdat, type = "response")
}
stop.time <- proc.time()
stop.time - start.time
# Check for systematic error
mean(abs(newdat$yhat - rowMeans(out)))
# Finally, calculate CI and plot.
newdat$LL <- apply(out, 1, quantile, 
                   probs = .025, na.rm = TRUE)
newdat$UL <- apply(out, 1, quantile, 
                   probs = .975, na.rm = TRUE)
ggplot(newdat, aes(AGE_W1, yhat)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = Sex), alpha = .2) +
  geom_line(aes(colour = Sex), size = 2) +
  scale_color_viridis(discrete = TRUE) +
  scale_fill_viridis(discrete = TRUE) +
  xlab("Age (years)") +
  ylab("Chronic Diease Onset") +
  theme_tufte() +
  coord_cartesian(xlim = range(acl$AGE_W1),
                  ylim = c(0, 4), 
                  expand = FALSE) +
  theme(legend.position = c(.2, .8),
        legend.key.width = unit(2, "cm"))

# From the above plot we see that there exists difference in patterns between
# male and female, but the prediction is with high level of uncertainty, 
# especially at high ages.
# Although the prediction indicates that male tend to enter a flatter tendency
# after 80 years old, the confidence interval is relatively wide, hence no 
# firm conclusion can be drawn. This is probably because of the small number of
# corresponding sample: there are only 27 male and 73 female samples whose age
# is above 80 in the first wave of survey that reported chronic disease status
# in the second wave.
xtabs(~Sex + I(AGE_W1 > 80), data = acl[!is.na(NChronic12_W2)])
