
## prepare data for analysis
## automatically setting working directories
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
if (!("cleaned.data" %in% ls())) source("dev/PreProcess_new.R")

require(texreg)
require(ggplot2)
require(jtools)
require(timevis)
require(cocor)
require(lavaan)

## we use 'cleaned.data' for the rest of the analysis
# > colnames(cleaned.data)
# [1] "id"                           "safe.disc.W1"
# [3] "dangerous.disc.W1"            "exp.disagr.offline.prcpt.W1"
# [5] "dangerous.disc.W1.wavesum"    "safe.disc.W1.wavesum"
# [7] "raw_sum.W1.wavesum"           "safe.disc.W2"
# [9] "dangerous.disc.W2"            "exp.disagr.offline.prcpt.W2"
# [11] "exp.disagr.online.prcpt.W2"   "dangerous.disc.W2.wavesum"
# [13] "safe.disc.W2.wavesum"         "raw_sum.W2.wavesum"
# [15] "exp.disagr.offline.prcpt.W3"  "exp.disagr.online.prcpt.W3"
# [17] "ego.netsize.W1"               "ego.netsize.W2"
# [19] "alter.centr.eigen.W1"         "alter.centr.eigen.W2"
# [21] "canpref.W1"                   "canpref.W2"
# [23] "canpref.W3"                   "age.years"
# [25] "female"                       "edu"
# [27] "household.income"             "residential.region"
# [29] "pol.know"                     "pref.certainty.W2"
# [31] "pref.certainty.W3"            "ideo.W1"
# [33] "ideo.W2"                      "ideo.W3"
# [35] "ideo_str.W1"                  "ideo_str.W2"
# [37] "ideo_str.W3"                  "perceived.opinion.climate.W2"
# [39] "perceived.opinion.climate.W3" "consistency.motivation"
# [41] "understanding.motivation"     "hedonic.motivation"
# [43] "internal.efficacy.W1"         "internal.efficacy.W2"
# [45] "internal.efficacy.W3"         "pol.interest.W1"
# [47] "pol.interest.W2"              "pol.interest.W3"
# [49] "internet.news.use.W2"         "newspaper.use.W2"
# [51] "tv.news.use.W2"               "media.exposure.W2"
# [53] "outgroup.negativity.bias.W2"  "outgroup.negativity.bias.W3"
# [55] "ingroup.favoritism.bias.W2"   "ingroup.favoritism.bias.W3"
# [57] "affective.polarization.W2"    "affective.polarization.W3"
# [59] "discussion.norm.W2"

## --------------------------- ##
## Timeline of data collection ##
## --------------------------- ##

timevis(data = data.frame(
  start = c("2012-11-27", "2012-12-11", "2012-12-21",
            "2012-12-07", "2012-11-27"),
  end = c("2012-11-29", "2012-12-13", "2012-12-23",
            "2012-12-19 23:59:59", "2012-12-10 23:59:59"),
  content = c("W1", "W2", "W3",
              "Wave 2 Exposure (vs. W3 self-report)",
              "Wave 1 Exposure (vs. W2 self-report)"),
  group = c(1, 1, 1, 3, 2)),
  groups = data.frame(id = 1:3,
                      content = c("Panel Survey", "Log data", "Log data")),
  showZoom = FALSE,
  options = list(editable = TRUE, height = "200px")
)

## ----------------------- ##
## Descriptive / Measures  ##
## ----------------------- ##

dat[, age] %>% descriptives(.)
dat[, table(canpref1.imputed)/.N] ## 0 = Park, 1 = Moon, 2 = else
dat[, sex] %>% descriptives(.)
dat[, edu] %>% descriptives(.)
dat[, income] %>% descriptives(.)
cleaned.data[, table(female)/sum(table(female))]


cleaned.data[, table(canpref.W1)/.N]
cleaned.data[, dangerous.disc.W1] %>% descriptives(.)
cleaned.data[, dangerous.disc.W2] %>% descriptives(.)

cleaned.data[, dangerous.disc.prcptn.W2] %>% descriptives(.)
cleaned.data[, dangerous.disc.prcptn.W3] %>% descriptives(.)

## other variables in the model, for regression/mediation model
cleaned.data[, perceived.opinion.climate.W2] %>% descriptives(.)
cleaned.data[, perceived.opinion.climate.W3] %>% descriptives(.)
cleaned.data[, affective.polarization.W2] %>% descriptives(.)
cleaned.data[, affective.polarization.W3] %>% descriptives(.)

cleaned.data[, ideo.W2] %>% descriptives(.)
cleaned.data[, media.exposure.W2] %>% descriptives(.)
cleaned.data[, pol.interest.W2] %>% descriptives(.)
cleaned.data[, discussion.norm.W2]  %>% descriptives(.)
cleaned.data[, log.netsize.W1] %>% descriptives(.)
cleaned.data[, log.netsize.W2] %>% descriptives(.)


## ----------------------- ##
## 1. Preliminary analysis ##
## ----------------------- ##

qq.out2 <- with(cleaned.data,
                qqplot(x = dangerous.disc.W1,
                       y = dangerous.disc.prcptn.W2,
                       plot.it = FALSE)) %>% as.data.frame(.) %>% setDT(.)

qq.out3 <- with(cleaned.data,
                qqplot(x = dangerous.disc.W2,
                       y = dangerous.disc.prcptn.W3,
                       plot.it = FALSE)) %>% as.data.frame(.) %>% setDT(.)

qq2 <- ggplot(qq.out2, aes(x = x, y = y)) +
  geom_jitter(width = 0.02, color = "grey") + theme_bw() +
  stat_smooth(aes(group = 1), color = "red", se = FALSE) +
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 100),
               lty = 2, color = "grey") +
  xlab("W1 Exposure log data (Mean proportion)") +
  ylab("W2 Perception (% of Exposure to disagreement)") +
  ggtitle("Quantile-Quantile plot, W1 Exposure vs. W2 Perception")

qq3 <- ggplot(qq.out3, aes(x = x, y = y)) +
  geom_jitter(width = 0.02, color = "grey") + theme_bw() +
  stat_smooth(aes(group = 1), color = "red", se = FALSE) +
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 100),
               lty = 2, color = "grey") +
  xlab("W2 Exposure log data (Mean proportion)") +
  ylab("W3 Perception (% of Exposure to disagreement)") +
  ggtitle("Quantile-Quantile plot, W2 Exposure - W3 Perception")

qq2 + qq3 + plot_layout(nrow = 1)

## check the correlation of measures
cleaned.data[, cor.test(dangerous.disc.W1, dangerous.disc.prcptn.W2)]
cleaned.data[, cor.test(dangerous.disc.W2, dangerous.disc.prcptn.W3)]
bivariate.perm.test(cleaned.data,
                    "dangerous.disc.W1", "dangerous.disc.prcptn.W2")
bivariate.perm.test(cleaned.data,
                    "dangerous.disc.W2", "dangerous.disc.prcptn.W3")

## create differences between perception and behavioral measures
## (+) values indicate the overestimation, and (-) means underestimation
cleaned.data[, dangerous.disc.W1.prop := dangerous.disc.W1*100]
cleaned.data[, diff.exp.disagree.W2 :=
               dangerous.disc.prcptn.W2 - dangerous.disc.W1.prop]
cleaned.data[, summary(diff.exp.disagree.W2)]
cleaned.data[, dangerous.disc.W2.prop := dangerous.disc.W2*100]
cleaned.data[, diff.exp.disagree.W3 :=
               dangerous.disc.prcptn.W3 - dangerous.disc.W2.prop]
cleaned.data[, summary(diff.exp.disagree.W3)]


## permutation test indicates that the difference between
## perception and objective behavior is significantly differ,
## in a way that people tend to overestimate the exposure to differences
diff.perm.test(cleaned.data, rep = 20000,
               "dangerous.disc.prcptn.W2", "dangerous.disc.W1.prop")
diff.perm.test(cleaned.data, rep = 20000,
               "dangerous.disc.prcptn.W3", "dangerous.disc.W2.prop")


## ------------------------------- ##
## some more recoding of variables ##
## ------------------------------- ##

cleaned.data[, log.total.exp.W1 := log(total.exp.W1 + 1)]
cleaned.data[, log.total.exp.W2 := log(total.exp.W2 + 1)]

cleaned.data[, accuracy.W2 :=
               (dangerous.disc.prcptn.W2 - dangerous.disc.W1.prop)/10]
cleaned.data[, accuracy.cat.W2 := car::recode(accuracy.W2,
               "0.1:hi = 'over'; else = 'accurate'", as.factor = T)]
cleaned.data[, table(accuracy.W2)]
cleaned.data[, table(accuracy.cat.W2)]


cleaned.data[, accuracy.W3 :=
               (dangerous.disc.prcptn.W3 - dangerous.disc.W2.prop)/10]
cleaned.data[, accuracy.cat.W3 := car::recode(accuracy.W3,
               "0.1:hi = 'over'; else = 'accurate'", as.factor = T)]
cleaned.data[, table(accuracy.W3)]
cleaned.data[, table(accuracy.cat.W3)]

# cleaned.data$canpref.W2 <- factor(cleaned.data$canpref.W2)
# cleaned.data$canpref.W3 <- factor(cleaned.data$canpref.W3)
## ----------------------------------------------------------- ##
## Does overreporting correlated with social desiability bias? ##
## we test this by interacting with discussion norm            ##
## ----------------------------------------------------------- ##

## cf. a-priory power analysis (using gpower)
## indicates for detecting small effect given sample size
## alpha is 0.07, therefore we stick to alpha = 0.5

## we can model the "latent" change score using univariate latent change score
## similar to latent growth model
## see https://www.tandfonline.com/doi/abs/10.1080/00049539208260150
## we can model latent "discrepancy" factor
## and subsequently regress on a set of exogenous variables

require(lavaan)

model1.luc.fm <- '

## baseline and descrepancy factor
    f_base =~ 1*dangerous.disc.W1.prop + 1*dangerous.disc.prcptn.W2
    f_diff =~ 1*dangerous.disc.prcptn.W2

## mean and variance of subjective/objective measures, all fixed to 0
   dangerous.disc.prcptn.W2 ~ 1*0
   dangerous.disc.prcptn.W2 ~~ 0*dangerous.disc.prcptn.W2
   dangerous.disc.W1.prop ~ 1*0
   dangerous.disc.W1.prop ~~ 0*dangerous.disc.W1.prop
## conditional intercept and variance of latent score
   f_diff ~ 1
   f_diff ~~ f_diff
   f_base ~ 1
   f_base ~~ f_base
## covariance of latent scores
   f_base ~~ f_diff

## regress objective exposure on Xs
   ## baseline total exposure
      f_base ~ log.total.exp.W1
   ## demographics
      f_base ~ age.years + female + edu + household.income
   ## partisanships
      f_base ~ canpref.W2 + ideo_str.W2
   ## motivation and ability factor
      f_base ~  pol.interest.W2 + pol.know + media.exposure.W2

## regress descprepancy on Xs
      f_diff ~ discussion.norm.W2 +
               perceived.opinion.climate.W2 +
               log.total.exp.W1 +
               age.years + female + edu + household.income +
               canpref.W2 + pol.interest.W2 + pol.know + ideo_str.W2 +
               media.exposure.W2
'

model1.luc <- sem(model1.luc.fm, data = cleaned.data)
summary(model1.luc, fit.measures = TRUE)
# modificationindices(model1.luc, sort. = T)

model2.luc.fm <- '

## baseline and descrepancy factor
    f_base =~ 1*dangerous.disc.W2.prop + 1*dangerous.disc.prcptn.W3
    f_diff =~ 1*dangerous.disc.prcptn.W3

## mean and variance of subjective/objective measures, all fixed to 0
   dangerous.disc.prcptn.W3 ~ 1*0
   dangerous.disc.prcptn.W3 ~~ 0*dangerous.disc.prcptn.W3
   dangerous.disc.W2.prop ~ 1*0
   dangerous.disc.W2.prop ~~ 0*dangerous.disc.W2.prop
## conditional intercept and variance of latent score
   f_diff ~ 1
   f_diff ~~ f_diff
   f_base ~ 1
   f_base ~~ f_base
## covariance of latent scores
   f_base ~~ f_diff

## regress objective exposure on Xs
   ## baseline total exposure
      f_base ~ log.total.exp.W2
   ## demographics
      f_base ~ age.years + female + edu + household.income
   ## partisanships
      f_base ~ canpref.W3 + ideo_str.W3
   ## motivation and ability factor
      f_base ~  pol.interest.W2 + pol.know + media.exposure.W2

## regress descprepancy on Xs
      f_diff ~ discussion.norm.W3 +
               perceived.opinion.climate.W3 +
               log.total.exp.W2 +
               age.years + female + edu + household.income +
               canpref.W3 + pol.interest.W3 + pol.know + ideo_str.W3 +
               media.exposure.W2
'


model2.luc <- sem(model2.luc.fm, data = cleaned.data, fixed.x = FALSE)
summary(model2.luc, fit.measures = TRUE) #standardized = TRUE)


model1 <- lm(accuracy.W2 ~
                 ## focal predictor
               discussion.norm.W2 +
               perceived.opinion.climate.W2 +
               log.total.exp.W1 +
                 ## demographic controls
                 age.years + female + edu + household.income +
                 ## political correlates
               canpref.W2 + pol.interest.W2 + pol.know + ideo_str.W2 +
                 ## media exposure
                 media.exposure.W2
               ,
               data = cleaned.data)

model1.cat <- glm(update.formula(model1, accuracy.cat.W2 ~ .),
                  binomial("logit"),
                       data = cleaned.data)

model2 <- lm(accuracy.W3 ~
              ## focal predictor
              discussion.norm.W3 +
              perceived.opinion.climate.W3 +
              log.total.exp.W2 +
              ## demographic controls
              age.years + female + edu + household.income +
              ## political correlates
              canpref.W3 + pol.interest.W2 +
              pol.know + ideo_str.W3 +
              ## media exposure
              media.exposure.W2,
             data = cleaned.data)

model2.cat <- glm(update.formula(model2, accuracy.cat.W3 ~ .),
                  binomial("logit"),
                       data = cleaned.data)

model1.luc.out <- model1.luc
model1.luc <- extract.lm(model1)
model1.luc@coef.names <- model1.luc@coef.names[-1]

test <- parameterEstimates(model1.luc.out) %>% setDT(.)
model1.luc@coef <- test[lhs == 'f_diff' & op == '~', est]
model1.luc@se <- test[lhs == 'f_diff' & op == '~', se]
model1.luc@pvalues <- test[lhs == 'f_diff' & op == '~', pvalue]
model1.luc@gof.names <- c("CFI", "TLI", "RMSEA", "P(RMSEA<.05)", "SRMR")
model1.luc@gof.decimal <- c(TRUE, TRUE, TRUE, TRUE, TRUE)
model1.luc@gof <- fitMeasures(model1.luc.out)[
                    c("cfi", "tli", "rmsea", "rmsea.pvalue", "srmr")]

model2.luc.out <- model2.luc
model2.luc <- extract.lm(model2)
model2.luc@coef.names <- model2.luc@coef.names[-1]

test <- parameterEstimates(model2.luc.out) %>% setDT(.)
model2.luc@coef <- test[lhs == 'f_diff' & op == '~', est]
model2.luc@se <- test[lhs == 'f_diff' & op == '~', se]
model2.luc@pvalues <- test[lhs == 'f_diff' & op == '~', pvalue]
model2.luc@gof.names <- c("CFI", "TLI", "RMSEA", "P(RMSEA<.05)", "SRMR")
model2.luc@gof.decimal <- c(TRUE, TRUE, TRUE, TRUE, TRUE)
model2.luc@gof <- fitMeasures(model2.luc.out)[
  c("cfi", "tli", "rmsea", "rmsea.pvalue", "srmr")]

require(texreg)
screenreg(list(model1, model1.cat, model1.luc,
               model2, model2.cat, model2.luc),
          stars = c(0.001, 0.01, 0.05, 0.10), digits = 3,
          custom.model.names = c("OLS W2", "GLM W2", "LCS W2",
                                 "OLS W3", "GLM W3", "LCS W3"),
          custom.coef.names = c(
            "(Intercept)", "Discussion norm W2/W3", "Prcvd Op Climate W2/W3",
            "Total Exp W1/W2 (log)", "Age (in years)", "Female", "Education",
            "HH income", "Candidate pref W2/W3", "Interest", "Knowledge",
            "Ideo Strength W2/W3", "Media Exposure",  "Discussion norm W2/W3",
            "Prcvd Op Climate W2/W3",  "Total Exp W1/W2 (log)",
            "Candidate pref W2/W3",  "Ideo Strength W2/W3"),
          reorder.coef = c(2:3, 9,12,10:11,13,4, 5:8, 1),
          groups = list("Focal predictors" = 1:4,  "Controls" = 5:8,
                        "Demographics" = 9:12, "Intercept" = 13))




## ---------------------------------------- ##
## What about cognitive bias and burdens?   ##
## (using an average of most recent 3-days) ##
##                                          ##
##           ANSWER IS NO!                  ##
## ---------------------------------------- ##

qq.out2.r <- with(cleaned.data,
                qqplot(x = recent.dangerous.disc.W1,
                       y = dangerous.disc.prcptn.W2,
                       plot.it = FALSE)) %>% as.data.frame(.) %>% setDT(.)

qq.out3.r <- with(cleaned.data,
                qqplot(x = recent.dangerous.disc.W2,
                       y = dangerous.disc.prcptn.W3,
                       plot.it = FALSE)) %>% as.data.frame(.) %>% setDT(.)

qq2r <- ggplot(qq.out2.r, aes(x = x, y = y)) +
  geom_jitter(width = 0.02, color = "grey") + theme_bw() +
  stat_smooth(aes(group = 1), color = "red", se = FALSE) +
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 100),
               lty = 2, color = "grey") +
  xlab("W1 Exposure log data (Mean proportion)") +
  ylab("W2 Perception (% of Exposure to disagreement)") +
  ggtitle("Quantile-Quantile plot, W1 Exposure vs. W2 Perception")

qq3r <- ggplot(qq.out3.r, aes(x = x, y = y)) +
  geom_jitter(width = 0.02, color = "grey") + theme_bw() +
  stat_smooth(aes(group = 1), color = "red", se = FALSE) +
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 100),
               lty = 2, color = "grey") +
  xlab("W2 Exposure log data (Mean proportion)") +
  ylab("W3 Perception (% of Exposure to disagreement)") +
  ggtitle("Quantile-Quantile plot, W2 Exposure - W3 Perception")

qq2r + qq3r + plot_layout(nrow = 1)


## test of equality of two correlation coefficients
## a comparison of two overlapping correlations based on dependent groups
require(cocor)

## Wave 2 measures
cocor(~ dangerous.disc.prcptn.W2 + recent.dangerous.disc.W1 |
        dangerous.disc.prcptn.W2 + dangerous.disc.W1, data = cleaned.data)

# Comparison between
#   r.jk (dangerous.disc.prcptn.W2, recent.dangerous.disc.W1) = 0.2433 and
#   r.jh (dangerous.disc.prcptn.W2, dangerous.disc.W1) = 0.4468
#   Difference: r.jk - r.jh = -0.2035
# 95% confidence interval for r.jk - r.jh: [-0.3002, -0.1068]
## RESULTS INDICATES 3-DAY BASED CORRELATIONS ARE SIGNIFICANTLY WEAKER!

## Wave 3 measures
cocor(~ dangerous.disc.prcptn.W3 + recent.dangerous.disc.W2 |
        dangerous.disc.prcptn.W3 + dangerous.disc.W2, data = cleaned.data)

# Comparison between:
#   r.jk (dangerous.disc.prcptn.W3, recent.dangerous.disc.W2) = 0.3631 and
#   r.jh (dangerous.disc.prcptn.W3, dangerous.disc.W2) = 0.4265
#   Difference: r.jk - r.jh = -0.0635
# 95% confidence interval for r.jk - r.jh: [-0.1411, 0.0136]
## RESULTS INDICATES THERE IS NO SIGNIFICANT DIFFERENCES





## ------------------ ##
## Outcome evaluation ##
## ------------------ ##




















## ------------------------------------- ##
## Predicting perceived opinion climates ##
## ------------------------------------- ##

## model predicting perceived opinion climate
model.M1 <- lm(perceived.opinion.climate.W2 ~
                ## focal predictor
                 safe.disc.W1.c +
                 dangerous.disc.W1.c +
                 log.raw_sum.W1.wavesum +
                 ## discussion motivation
                 discussion.norm.W2.c +
                 ## consistency.motivation + understanding.motivation +
                 ## demographic controls
                 age.years + female + edu + household.income +
                 ## political correlates
                 ideo.W2.c + pol.interest.W2.c + log.ego.netsize.W1.c + #pol.know.c +
                 ## media exposure
                 media.exposure.W2.c
              ,
              data = cleaned.data); summary(model.M1)


boot.M1 <- function(data, i){
  data.resample <- data[i,] # select obs. in bootstrap sample
  mod <- lm(formula(model.M1), data = data.resample)
  coefficients(mod) # return coefficient vector
  }

model.M1.b <- boot(cleaned.data, boot.M1, R = 10000,
                                parallel = "multicore", ncpus = 8)


## model predicting online exposure to disagreement, perception
model.Y1 <- lm(update.formula(model.M1,
                              ## DV = exp.disagr.online.prcpt.W3
                              exp.disagr.online.prcpt.W3 ~ .),
               data = cleaned.data); summary(model.Y1)
## model predicting online exposure to disagreement, perception
## controlling W1 lagged DV
model.Y1.lagged <- lm(update.formula(model.Y1,
                                     ## DV = perceived.opinion.climate.W2
                              . ~ . + exp.disagr.online.prcpt.W2),
               data = cleaned.data); summary(model.Y1.lagged)

## model predicting online exposure to diagreement, perception
## controlling W1 lagged DV and W2 IVs
model.Y1.CLPM <- lm(update.formula(model.Y1,
                                   ## DV = exp.disagr.online.prcpt.W3
                                  . ~ .
                                   ## controlling lagged DV
                                  + exp.disagr.online.prcpt.W2
                                   ## W2 IVs instead of W1 IVs
                                  + safe.disc.W2.c + dangerous.disc.W2.c
                                  - safe.disc.W1.c - dangerous.disc.W1.c),
                   data = cleaned.data); summary(model.Y1.CLPM)



## model predicting social-identity based affective process: no effects
model.M2 <- lm(update.formula(model.M1, affective.polarization.W2.c ~ .),
               data = cleaned.data); summary(model.M2)

## individual's ideology significantly interact with dangerous.disc
## in predicting perceived opinion climates
model.M1.int1 <- lm(update.formula(model.M1,
                                   . ~ . + dangerous.disc.W1.c*ideo.W2.c),
                    data = cleaned.data)
summary(model.M1.int1)
jtools::interact_plot(model.M1.int1,
                      pred = "dangerous.disc.W1.c", modx = "ideo.W2.c")

model.M1.int2 <- lm(update.formula(model.M1,
                                   . ~ . + safe.disc.W1.c*ideo.W2.c),
                    data = cleaned.data)
summary(model.M1.int2)

## affective polarization
# model.M2.int1 <- lm(
#   update.formula(model.M2, . ~ . + dangerous.disc.W1.c*discussion.norm.W2.c),
#   data = cleaned.data); summary(model.M2.int1) ## yet not likely to be real....
# jtools::interact_plot(model.M2.int1,
#                       pred = "dangerous.disc.W1.c", modx = "discussion.norm.W2.c")

model.M2.int2 <- lm(
  update.formula(model.M2, . ~ . + safe.disc.W1.c*ideo.W2.c),
  data = cleaned.data); summary(model.M2.int2)
jtools::interact_plot(model.M2.int2,
                      pred = "safe.disc.W1.c", modx = "ideo.W2.c")


## predicting self-reported exposure to disagreement
model.Y2 <- lm(update.formula(model.Y1, ## DV = exp.disagr.online.prcpt.W3
                              . ~ .
                              + perceived.opinion.climate.W2.c
                              + affective.polarization.W2.c),
                    data = cleaned.data); summary(model.Y2)

## cf. CLPM specification
model.Y2.CLPM <- lm(update.formula(model.Y1.CLPM,
                              ## DV = exp.disagr.online.prcpt.W3
               . ~ .
               + perceived.opinion.climate.W2.c + affective.polarization.W2.c),
              data = cleaned.data); summary(model.Y2.CLPM)

# CLPM.boot <- function(data, i) {
#   data.resample <- data[i, ]
#   fit <- lm(formula(model.Y2.CLPM), data = data.resample)
#   coef(fit)
# }
#
# require(boot)
# CLPM.boot.test <- boot(data = cleaned.data, statistic = CLPM.boot,
#                        R = 10000,
#                        parallel = "multicore", ncpus = 8)
# get.boot.stats(CLPM.boot.test, type = "perc")

## Basic mediation models
screenreg(list(model.M1, model.M2, model.Y2),
          single.row = T, digits = 3, leading.zero = FALSE,
          custom.model.names = c("M1: Prcvd Op Climate", "M2: Affective pol", "Y: Self-reptd Dis"),
          custom.coef.names = c("(Intercept)",
                                "In-prty msg Exp W1", "Out-prty msg Exp W1",
                                "Total Exp W1 (log)", "Discussion norm W2",
                                "Age (in years)", "Female", "Education", "HH income",
                                "Ideology W2", "Interest W2",
                                "Net size W1 (log)", "Media Exposure W2",
                                "Prcvd Op Climate W2", "Affective pol W2"),
          reorder.coef = c(2:3, 14:15, 4:5, 10:13, 6:9, 1),
          groups = list("Focal predictors" = 1:2, "Mediators" = 3:4,
                        "Controls" = 5:10, "Demographics" = 11:14, "Intercept" = 15))

## First stage moderation models
screenreg(list(model.M1, model.M1.int1, model.M2, model.M2.int2),
          single.row = T, digits = 3, leading.zero = FALSE,
          custom.model.names = c("Prcvd Op Climate", "POC Interaction",
                                 "Affective Pol", " AP Interaction"),
          custom.coef.names = c("(Intercept)",
                                "In-prty msg Exp W1", "Out-prty msg Exp W1",
                                "Total no. of exp (log)", "Discussion norm W2",
                                "Age (in years)", "Female", "Education", "HH income",
                                "Ideology W2", "Interest W2",
                                "Net size W2 (log)", "Media Exposure W2",
                                "Out-prty Exp X Ideo",
                                "In-prty Exp X Ideo"),
          reorder.coef = c(2:3, 10,14:15, 4:5, 11:13, 6:9, 1),
          groups = list("Focal predictors" = 1:2, "Interactions" = 3:5,
                        "Controls" = 6:10, "Demographics" = 11:14, "Intercept" = 15))

## -------------------------------------------------------- ##
## Nonparametric bootstrap-based indirect effect inferences ##
## -------------------------------------------------------- ##

## unconditional indirect effect
est.uncond.indirect

## estimation, with multicore processing
require(boot)
RNGkind("L'Ecuyer-CMRG")
set.seed(1234)
boot.test1 <- boot(dat = cleaned.data, statistic = est.uncond.indirect,
                   R = 20000, parallel = "multicore", ncpus = 8,
                   lm.model.M = model.M1, lm.model.Y = model.Y2,
                   pred = c("safe.disc.W1.c", "dangerous.disc.W1.c"),
                   m.name = "perceived.opinion.climate.W2.c")
out.uncond <- get.boot.stats(boot.test1, type = "perc")

## cf. when controlling for lagged effect of Y:
# model.Y1 <- lm(update.formula(model.Y, . ~ exp.disagr.online.prcpt.W2 + .),
#                data = cleaned.data)
# ## effect is still significant but at .90 level
# summary(model.Y1)
#
# set.seed(1234)
# boot.test1.lagged <- boot(dat = cleaned.data, statistic = est.uncond.indirect,
#                    R = 20000, parallel = "multicore", ncpus = 8,
#                    lm.model.M = model.M1, lm.model.Y = model.Y1,
#                    pred = c("safe.disc.W1", "dangerous.disc.W1"))
# out.uncond.lagged <- get.boot.stats(boot.test1.lagged,
#                                     type = "perc", conf = 0.90)

safe.disc.W1.c <- cleaned.data[, quantile(safe.disc.W1.c,
                                          seq(from = 1, to = 0, by = -0.1))]
dangerous.disc.W1.c <- cleaned.data[, quantile(dangerous.disc.W1.c,
                                          seq(from = 0, to = 1, by = 0.1))]

dat.predict <- data.frame(X = seq(from = 0, to = 1, by = 0.1),
                     safe.disc.W1.c = safe.disc.W1.c,
                     dangerous.disc.W1.c = dangerous.disc.W1.c,
                     log.raw_sum.W1.wavesum = 0,
                     discussion.norm.W2.c = 0,
                     age.years = 0,
                     female = 0,
                     edu = -0.316,
                     household.income = 0,
                     ideo.W2.c = -0.263,
                     pol.interest.W2.c = 0,
                     ego.netsize.W2.c = 0,
                     media.exposure.W2.c = 0) %>% setDT
dat.predict[, perceived.opinion.climate.W2 := predict(model.M1, dat.predict)]
dat.predict[, affective.polarization.W2 := predict(model.M2, dat.predict)]
dat.predict[, perceived.opinion.climate.W2.c :=
              perceived.opinion.climate.W2 - 56.54344]
dat.predict[, affective.polarization.W2.c :=
              affective.polarization.W2 - 0.6944282]
dat.predict[, exp.disagr.online.prcpt.W3 :=
              predict(model.Y2, dat.predict)]

ggplot(dat.predict, aes(x = X, y = exp.disagr.online.prcpt.W3)) +
  geom_bar(stat = "identity") + theme_bw()

## estimate conditional indirect effect
## as a function of moderators
require(boot)
est.cond.indirect

## ideological identification strength
set.seed(1234)
boot.test2 <- boot(cleaned.data, statistic = est.cond.indirect,
                   R = 20000, parallel = "multicore", ncpus = 8,
                   lm.model.M = model.M1.int1, lm.model.Y = model.Y2,
                   pred = "dangerous.disc.W1.c",
                   modx = "ideo.W2.c",
                   med = "perceived.opinion.climate.W2.c")
out.cond1 <- get.boot.stats(boot.test2, type = "perc")
out.cond1 <- cbind(var = rownames(out.cond1), out.cond1) %>% setDT(.)

  ## plot conditional indirect effects
  plot.cond.ind1 <- data.frame(
    moderator = select.modval(cleaned.data, "ideo.W2.c"),
    out.cond1[var %!in% c("index.modmed", "dir.effect"),]
    ) %>% setDT(.)
  ggplot(plot.cond.ind1, aes(x = moderator, y = coef)) + theme_bw() +
    geom_line() + geom_hline(yintercept = 0, color = "red", linetype = 2) +
    geom_ribbon(aes(ymin = llci, ymax = ulci), alpha = 0.25) +
    geom_vline(xintercept = -0.46, color = "red", lty = 2) +
    xlab("\nModerator: Ideological self placement") + ylab("theta") +
    scale_x_continuous(breaks = c(-3:3),
                       labels = c("Extremely Liberal",
                                  "Liberal", "Somewhat Liberal",
                                  "Middle", "Somewhat Conserv",
                                  "Conservative", "Extremely Conserv")) +
    ggtitle("Conditional indirect effect of actual exposure to out-party discussants' messages")






  ## difference estimator
  test.data <- cleaned.data

  ## DV: (+) means increase in exposure (perception)
  test.data[, Y := exp.disagr.online.prcpt.W3 - exp.disagr.online.prcpt.W2]

  ## IV: (+) means increase in exposure (actual)
  test.data[, X.dangerous := dangerous.disc.W2 - dangerous.disc.W1]
  test.data[, X.safe := safe.disc.W2 - safe.disc.W1]

  ## mediator: (+) means increase in measures
  test.data[, M.opc := perceived.opinion.climate.W3 - perceived.opinion.climate.W2]
  test.data[, M.afp := affective.polarization.W3 - affective.polarization.W2]

  lm(Y ~ X.dangerous +
         X.safe +
         log.raw_sum.W1.wavesum +
         ## mediator
         M.opc +
         M.afp +
         ## discussion motivation
         discussion.norm.W2.c +
         ## demographic controls
         age.years + female + edu + household.income +
         ## political correlates
         ideo.W2.c + pol.interest.W2.c + log.ego.netsize.W1.c +
         ## media exposure
         media.exposure.W2.c,
     data = test.data) %>% summary()

  lm(M.opc ~
       X.dangerous*internal.efficacy.W2 +
       X.safe +
       log.raw_sum.W1.wavesum +
       ## discussion motivation
       discussion.norm.W2.c +
       ## demographic controls
       age.years + female + edu + household.income +
       ## political correlates
       ideo.W2.c + pol.interest.W2.c + log.ego.netsize.W1.c +
       ## media exposure
       media.exposure.W2.c,
     data = test.data) %>% interact_plot(., pred = 'X.dangerous', modx = 'internal.efficacy.W2')
