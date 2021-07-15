# This case study aims to investigate the effect of multiple explanatory
# variables on one response variable. In the ACL data, there are two waves of 
# data presenting an individual's smoking habit. We're interested in the change
# of their habit, and the cause of this change.
library(checkpoint)
book_directory = "F:/R directories/book directory"
checkpoint_directory = "D:/Documents/R/win-library/4.1"
checkpoint("2021-06-28", R.version = "4.1.0", 
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
acl <- readRDS("advancedr_acl_data.RDS")
acl[, Smoke_W2W1 := NA_character_]
acl[Smoke_W1 == "(3) Nevr Smo" &
    SMOKE_W2 == "(3) W2 Never Smoker",
    Smoke_W2W1 := "Stable Never Smoker"]
acl[Smoke_W1 == "(2) Past Smo" &
    SMOKE_W2 == "(2) W2 Former Smoker",
    Smoke_W2W1 := "Stable Former Smoker"]
acl[Smoke_W1 == "(1) Cur Smok" &
    SMOKE_W2 == "(1) W2 Current Smoker",
    Smoke_W2W1 := "Stable Current Smoker"]
acl[Smoke_W1 %in% c("(2) Past Smo", "(3) Nevr Smo") &
    SMOKE_W2 == "(1) W2 Current Smoker",
    Smoke_W2W1 := "New Smoker"]
acl[Smoke_W1 == "(1) Cur Smok" &
    SMOKE_W2 == "(2) W2 Former Smoker",
    Smoke_W2W1 := "Recently Quit Smoker"]
acl[, Smoke_W2W1 := factor(Smoke_W2W1,
                    levels = c("Stable Never Smoker", "Stable Former Smoker", 
                               "Stable Current Smoker", "New Smoker", 
                               "Recently Quit Smoker"))]
xtable(as.data.frame(table(acl$Smoke_W2W1)),
                     caption = "Frequency table of smoking over time",
                     label = "tglm2-freqtab-smoke")
# Then, we look into different potential explanatory variables, construct model
# for each one of them, and compare AIC and BIC.
acl[, SES := as.numeric(SESCategory)]
mr.ses <- vglm(Smoke_W2W1 ~ Sex + SES + AGE_W1, 
               family = multinomial(), 
               data = acl, model = TRUE)
mr.psych <- vglm(Smoke_W2W1 ~ SWL_W1 + InformalSI_W1 + FormalSI_W1 +
                 SelfEfficacy_W1 + CESD11_W1,
                 family = multinomial(), 
                 data = acl, model = TRUE)
mr.health <- vglm(Smoke_W2W1 ~ PhysActCat_W1 + BMI_W1 + NChronic12_W1, 
                  family = multinomial(), 
                  data = acl, model = TRUE)
xtable(data.table(
  Model = c("Sociodemographics", "Phychosocial", "Health"),
  AIC = c(AIC(mr.ses), AIC(mr.psych), AIC(mr.health)),
  BIC = c(BIC(mr.ses), BIC(mr.psych), BIC(mr.health)),
  caption = "Model Comparisons",
  label = "tglm2-modelcomparisons"
))
# From the result above, the model with SES factors has the least AIC and BIC,
# hence should be the best prediction of the change of smoking over time.
summary(mr.ses)
# According to the fitting results, the age factor is statistically significant,
# but with small coefficients. We might want to alter the age into decade scale
# to observe larger change per unit.
acl[, AGE_W1 := AGE_W1/10]
