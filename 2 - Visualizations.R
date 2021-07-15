library(checkpoint)
book_directory = "F:/R directories/book directory"
checkpoint_directory = "D:/Documents/R/win-library/4.1"
checkpoint("2021-06-14", R.version = "4.1.0", 
           project = book_directory, 
           checkpointLocation = checkpoint_directory, 
           scanForPackages = FALSE, 
           scan.rnw.with.knitr = TRUE, 
           use.knitr = TRUE)
library(ggplot2)
library(cowplot)
library(MASS)
library(mvtnorm)
library(mgcv)
library(quantreg)
library(JWileymisc)
library(data.table)

set.seed(1234)
d4 <- data.table(x = runif(500, 0, 5))
d4[, y1 := rnorm(500, mean = x+2, sd = 1)]
d4[, y2 := rnorm(500, mean = x+2, sd = x+.25)]

plot_grid(
  ggplot(d4, aes(x, y1)) +
    geom_point(colour = "grey70") +
    geom_quantile(quantiles = .5, colour = 'black') +
    geom_quantile(quantiles = c(.25, .75), colour = 'blue', linetype = 2) +
    geom_quantile(quantiles = c(.05, .95), colour = 'black', linetype = 3), 
  ggplot(d4, aes(x, y2)) +
    geom_point(colour = "grey70") +
    geom_quantile(quantiles = .5, colour = 'black') +
    geom_quantile(quantiles = c(.25, .75), colour = 'blue', linetype = 2) +
    geom_quantile(quantiles = c(.05, .95), colour = 'black', linetype = 3), 
  ncol = 2, lables = c("A", "B")
)
