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
library(tidyverse)
library(rsample)
library(data.table)
library(boot)
library(parallel)
library(foreach)
library(doParallel)
setwd("F:/R directories")
options(
  width = 70,
  digits = 3
)
# Machine learning. It's said to be a very uncertain statistics toolkit. Although 
# it has numerous goals and purposes, the core of it remains being the various 
# types of pattern recognition.
# This part covers the introductory content of machine learning: the understand
# of sample structure, and the introduction of parallel calculation.
# In this part, we assume that the data we use is clean, small-scaled and tidy.
# Among the above introduced packages, the rsample package realizes resampling,
# the boot package provides methods for general bootstrapping, while the parallel
# package enables parallel calculation on multi-core CPUs. The foreach package 
# is a alternative of the iteration suited for parallel calculation, which is 
# empowered by the doParallel package.


# In this part, we'll be looking at the dataset named iris, which contains 150
# complete observation records of the plant.
# In machine learning, there is a risk that the machine learns too well towards
# the sample, but performs poorly when tested using real-world data. This risk
# is called over-fitting, or over-training. It's likely to be a result of the
# measurement of errors between model predictions and actual objects being too
# optimistic, so that the errors are always too small.
# A solution to this problem is to divide the data: the training group to be fed
# to the machine, and the reserved group for assessing  the performance of the 
# model when applied to new (wild) data. Usually the proportion between the
# training and reserved group should be around 80-20 to 70-30. We'll be using
# the former in this case.
set.seed(5)
case_data <- initial_split(data = iris, prop = 0.81)
case_data
# The practice of resampling is a random choosing process, which takes a part of
# data from the original dataset. It is a vital part in general because if the
# data is selected in a pattern, the model will be abnormally affected.
data_train <- training(case_data)
data_test<- testing(case_data)
# The glimpse() function in tidyverse allows for browsing data in the training
# group.
glimpse(data_train)
# The output of following function indicates hat there are 3 species of flowers
# within the iris dataset.
unique(data_train$Species)
# The focus of this part is how to combine sample dataset and the environment of
# a computer, in order to realize rapid machine learning. Therefore, here we 
# fit these numeric data with a simple linear model and analyse the sample data
# structure in 3 different ways.
length.lm <- lm(Petal.Length ~ Sepal.Length +
                  Sepal.Width + Petal.Width,
                data = data_train)
summary(length.lm)
mse_train <- mean(length.lm$residuals ^ 2)
mse_train
# The fitting, this time, gives a 0.309 residual standard error, and 0.0924 mean
# squared error, which is measured using the training data. 
# A model can perform excellently on an identical dataset after training, but 
# not necessarily so on the wild data, which is where our interest actually lies. 
# Therefore, we use following code to calculate the model's RSE and MSE on test 
# data. The result appears to be 0.376 and 0.132 respectively.
sqrt(
  sum(
    (predict(length.lm, data_test) - data_test$Petal.Length) ^ 2) /
    (nrow(data_test) - 2)
)
mse_test <- mean((predict(length.lm, data_test) -
                    data_test$Petal.Length) ^ 2)
mse_test
# Now we use plot to inspect the model's performance on real-world data.
# NOTICE: the original data of 150 observations is incomplete by 29 entries, for
# they are used as test data. This incompleteness may result in large difference
# in model's final performance.
# The plots show substantial difference between testing and sample data.
par(mfrow = c(2, 2))
# normal q-q plots for training and testing data.
plot(data_train$Sepal.Length, data_train$Petal.Length)
plot(data_test$Sepal.Length, data_test$Petal.Length)
qqnorm(data_train$Petal.Length,
       xlab = "Theoretical Quantiles of Training Data")
qqnorm(data_test$Petal.Length,
       xlab = "Theoretical Quantiles of Testing Data")
summary(data_train$Sepal.Length)
summary(data_test$Sepal.Length)


# From the above case we notice that the loss of data can result in further
# impairment to the model.
# One way of solving the problem is cross validation: to create multiple divided
# groups based on one dataset. In practice, we now divide the observations into
# not 2 but 5 groups. The first 4 groups are taken as training data, and the 
# last testing data. We then fit the data on training dataset, and calculate RSE
# and MSE. 
# This process will be repeated for multiple times (5 in this case), every time
# selecting a different training data group. The MSE produced every run will be
# calculated for average, which is named the cross MSE (or CV value).
crossData <- iris %>% sample_n(nrow(iris), replace = FALSE)
crossData <- add_column(crossData, 
                        Bin = cut(1:150, breaks = 5, labels = c(1:5)))
store <- tibble(Fold = 1:5, MSE = NA_real_)
for(i in 1:5){
  data_train <- crossData %>% filter(Bin != i)
  data_test <- crossData %>% filter(Bin == i)
  lengthFold.lm = lm(Petal.Length ~ Sepal.Length + 
                      Sepal.Width + Petal.Width, 
                     data = data_train)
  store[i,]$MSE <- mean((predict(lengthFold.lm, data_test) - 
                            data_test$Petal.Length) ^ 2)
}
# Here, the average training MSE is 0.108, similar to the MES acquired using the
# single training dataset. 
mse_k <- mean(store$MSE)
mse_k
# However, it's not the single-dataset model that is to be compared with. For
# cross validation, we construct a model fitted using all observations, and
# compare their MSE.
lengthFold.lm = lm(Petal.Length ~ Sepal.Length + 
                     Sepal.Width + Petal.Width, 
                   data = iris)
lengthFold.lm

# The MSE based on all observations is 0.099, a little smaller than the average
# MSE. 
mse_ALL <- mean(lengthFold.lm$residuals ^ 2)
mse_ALL
# We can also compare the MSE for every iteration.
store



# We now move onto the bootstrapping technique. Although K-folding helps acquiring
# a "real" MSE estimate, but as the model in every iteration comes from a 
# incomplete dataset, it could potentially have undetectable flaws. That's also
# the reason why although the MSE estimate is better than the single-dataset
# one, but still larger than the overall MSE.
# Bootstrapping solves the problem in another aspect. Presume that the sample
# data (the 150 observations) is selected from the whole population, then a 
# large number of re-sampling with replaces could generate a sample pool that's 
# very similar to the population.
# For more smooth processing, we now define a function to calculate MSE based on
# re-sampled data.
mse <- function(dat, i){
  lengthBoot.lm <- lm(Petal.Length ~ Sepal.Length + 
                        Sepal.Width + Petal.Width, 
                      data = dat[i,])
  return(mean(lengthBoot.lm$residuals ^ 2))
}
# Now we can perform bootstrapping using boot() in the boot package, which
# samples from iris 10,000 times with replacement and transit the samples to
# the mse() function defined above.
bootResults <- boot(data = iris, statistic = mse, R = 10000)
bootResults
# We can also plot the bootstrapping result. The plots show that the 95% CI of
# MSE is (0.08, 0.13).
plot(bootResults)
# We can confirm the CI using following code.
boot.ci(bootResults, conf = 0.95, type = "bca")



