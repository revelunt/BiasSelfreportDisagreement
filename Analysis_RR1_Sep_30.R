
## prepare data for analysis
## automatically setting working directories
rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
if (!("cleaned.data" %in% ls())) source("dev/PreProcess_RR1 temp.R")

require(texreg)
require(ggplot2)
require(jtools)
require(timevis)
require(cocor)
require(lavaan)
require(jtools)
require(interactions)
require(pscl)
require(ggthemes)

## we use 'cleaned.data' for the rest of the analysis
# > colnames(cleaned.data)
# [1] "id"                             "dangerous.disc.W1"
# [3] "total.exp.W1"                   "dangerous.disc.W2"
# [5] "total.exp.W2"                   "dangerous.disc.dlyavg.W1"
# [7] "dangerous.disc.dlyavg.W2"       "dangerous.disc.online.tally.W1"
# [9] "dangerous.disc.online.tally.W2" "dangerous.disc.prcptn.W2"
# [11] "safe.disc.prcptn.W2"            "dangerous.disc.prcptn.W3"
# [13] "safe.disc.prcptn.W3"            "log.netsize.W1"
# [15] "log.netsize.W2"                 "canpref.W1"
# [17] "canpref.W2"                     "canpref.W3"
# [19] "age.years"                      "female"
# [21] "edu"                            "household.income"
# [23] "residential.region"             "pol.know"
# [25] "pref.certainty.W2"              "pref.certainty.W3"
# [27] "tolerance.W3"                   "ideo.W1"
# [29] "ideo.W2"                        "ideo.W3"
# [31] "ideo_str.W1"                    "ideo_str.W2"
# [33] "ideo_str.W3"                    "perceived.opinion.climate.W2"
# [35] "perceived.opinion.climate.W3"   "consistency.motivation"
# [37] "understanding.motivation"       "hedonic.motivation"
# [39] "internal.efficacy.W1"           "internal.efficacy.W2"
# [41] "internal.efficacy.W3"           "pol.interest.W1"
# [43] "pol.interest.W2"                "pol.interest.W3"
# [45] "know.self.W3"                   "internet.news.use.W2"
# [47] "newspaper.use.W2"               "tv.news.use.W2"
# [49] "media.exposure.W2"              "outgroup.negativity.bias.W2"
# [51] "outgroup.negativity.bias.W3"    "ingroup.favoritism.bias.W2"
# [53] "ingroup.favoritism.bias.W3"     "affective.polarization.W2"
# [55] "affective.polarization.W3"      "discussion.norm.W2"
# [57] "discussion.norm.W3"             "ppart.W1"
# [59] "recent.dangerous.disc.W1"       "recent.total.exp.W1"
# [61] "recent.dangerous.disc.W2"       "recent.total.exp.W2"
# [63] "log.total.exp.W1"               "log.total.exp.W2"
# [65] "dis.accuracy.W2"                "dis.accuracy.W3"
# [67] "dis.accuracy.cat.W2"            "dis.accuracy.cat.W3"

## ----------------------- ##
## Descriptive / Measures  ##
## ----------------------- ##

## sample demographics
# dat[, sapply(.SD, descriptives),
#       .SDcols = c("age", "sex", "edu", "income")]

## "sex" is coded as 1 vs. 2, but the paper reports 0 vs. 1 scale

#                  age       sex      edu   income
# M          35.718475 1.4809384 7.683284 4.991202
# SD          9.858853 0.5003707 1.040072 1.882562
# range.low  16.000000 1.0000000 2.000000 1.000000
# range.high 59.000000 2.0000000 9.000000 8.000000

# ## ideology of user
# cleaned.data[, sapply(.SD, descriptives),
#                       .SDcols = ideo.W1:ideo.W3]
#
#
# ## cumulutive proportion benchmark
# cleaned.data[, sapply(.SD, descriptives),
#              .SDcols = c("dangerous.disc.W1.candpref",
#                          "dangerous.disc.W2.candpref",
#                          "dangerous.disc.W1.ideo3",
#                          "dangerous.disc.W2.ideo3",
#                          "dangerous.disc.W1.ideo2",
#                          "dangerous.disc.W2.ideo2")]
#
#
# ## perceived exposure to disagreement
# cleaned.data[, sapply(.SD, descriptives),
#              .SDcols = c("dangerous.disc.prcptn.W2.candpref",
#                          "dangerous.disc.prcptn.W3.candpref",
#                          "dangerous.disc.prcptn.W2.ideo3",
#                          "dangerous.disc.prcptn.W3.ideo3",
#                          "dangerous.disc.prcptn.W2.ideo2",
#                          "dangerous.disc.prcptn.W3.ideo2")]
#
# ## Social norm regarding exposure to disagreement
# cleaned.data[, sapply(.SD, descriptives),
#              .SDcols = c("discussion.norm.W2",
#                          "discussion.norm.W3",
#                          "need.for.approval.W2",
#                          "need.for.approval.W3")]
#
# ## motivation and ability (interest and knowledge)
# cleaned.data[, sapply(.SD, descriptives),
#              .SDcols = c("pol.interest.W2",
#                          "pol.interest.W3",
#                          "pol.know")]
#
# ## opinion climates
# cleaned.data[, sapply(.SD, descriptives),
#              .SDcols = c("perceived.opinion.climate.W2",
#                          "perceived.opinion.climate.W3")]
#
# ## other variables in the model, for regression/mediation model
# cleaned.data[, sapply(.SD, descriptives),
#              .SDcols = c("ideo_str.W2", "ideo_str.W3",
#                          "canpref.W2", "canpref.W3",
#                          "media.exposure.W2",
#                          "log.total.exp.W1", "log.total.exp.W2")]


## ----------------------- ##
## 1. Preliminary analysis ##
## ----------------------- ##

make_qqplot(type = "candpref")
make_qqplot(type = "ideo3")
make_qqplot(type = "ideo2")

## check the correlation of measures
cleaned.data[, cor.test(dangerous.disc.W1.candpref, dangerous.disc.prcptn.W2.candpref)]
cleaned.data[, cor.test(dangerous.disc.W2.candpref, dangerous.disc.prcptn.W3.candpref)]

cleaned.data[, cor.test(dangerous.disc.W1.ideo3, dangerous.disc.prcptn.W2.ideo3)]
cleaned.data[, cor.test(dangerous.disc.W2.ideo3, dangerous.disc.prcptn.W3.ideo3)]

cleaned.data[, cor.test(dangerous.disc.W1.ideo2, dangerous.disc.prcptn.W2.ideo2)]
cleaned.data[, cor.test(dangerous.disc.W2.ideo2, dangerous.disc.prcptn.W3.ideo2)]


## permutation test indicates that the difference between
## perception and objective behavior is significantly differ,
## in a way that people tend to overestimate the exposure to differences
RNGkind("L'Ecuyer-CMRG"); set.seed(12345)
diff.perm.test(cleaned.data, rep = 20000,
               "dangerous.disc.prcptn.W2", "dangerous.disc.W1")
RNGkind("L'Ecuyer-CMRG"); set.seed(12345)
diff.perm.test(cleaned.data, rep = 20000,
               "dangerous.disc.prcptn.W3", "dangerous.disc.W2")


## ----------------------------------------------------------- ##
## Does overreporting correlated with social desiability bias? ##
## we test this by interacting with discussion norm            ##
## ----------------------------------------------------------- ##

## cf. a-priory power analysis (using gpower)
## indicates for detecting small effect given sample size
## alpha is 0.07, therefore we stick to alpha = 0.5

## + on diff variable indicates overestimation of exposure to disagreement
cleaned.data[, descriptives(dis.accuracy.W2.candpref)]
cleaned.data[, descriptives(dis.accuracy.W3.candpref)]

cleaned.data[, descriptives(dis.accuracy.W2.ideo3)]
cleaned.data[, descriptives(dis.accuracy.W3.ideo3)]

cleaned.data[, descriptives(dis.accuracy.W2.ideo2)]
cleaned.data[, descriptives(dis.accuracy.W3.ideo2)]

model1.candpref <- lm(dis.accuracy.W2.candpref ~ ## predicting overestimation (+)
                        perceived.opinion.climate.W2 +
                        discussion.norm.W2 +
                        need.for.approval.W2 +
                        cand.pref.strength.W2 +
                        SNS.use + log.total.exp.W1 +
                        pol.interest.W2 + pol.know +
                        ## demographic controls
                        age.years + female + edu + household.income,
             data = cleaned.data)

model1.ideo3 <- lm(update.formula(model1.candpref, dis.accuracy.W2.ideo3 ~ . + ideo_str.W2 - cand.pref.strength.W2),
                  data = cleaned.data)

model1.ideo2 <- lm(update.formula(model1.candpref, dis.accuracy.W2.ideo2 ~ . + ideo_str.W2 - cand.pref.strength.W2),
                   data = cleaned.data)

model2.candpref <- lm(dis.accuracy.W3.candpref ~
                        perceived.opinion.climate.W3 +
                        discussion.norm.W3 +
                        need.for.approval.W3 +
                        cand.pref.strength.W3 +
                        SNS.use + log.total.exp.W2 +
                        pol.interest.W3 + pol.know +
                        ## demographic controls
                        age.years + female + edu + household.income,
               data = cleaned.data)

model2.ideo3 <- lm(update.formula(model2.candpref, dis.accuracy.W3.ideo3 ~ . + ideo_str.W3 - cand.pref.strength.W3),
                   data = cleaned.data)

model2.ideo2 <- lm(update.formula(model2.candpref, dis.accuracy.W3.ideo2 ~ . + ideo_str.W3 - cand.pref.strength.W3),
                   data = cleaned.data)

screenreg(list(model1.candpref, model1.ideo3, model1.ideo2,
               model2.candpref, model2.ideo3, model2.ideo2), digits = 3,
          custom.model.names = c("W2, candpref", "W2, ideo 3cat", "W2, ideo 2cat",
                                 "W3, candpref", "W3, ideo 3cat", "W3, ideo 2cat"),
          custom.coef.names = c(
            "(Intercept)", "Majority perception", "Social norm", "Need for socl approval",
            "Pref/Ideo Strength", "SNS use (min)", "Total Exp (log)",
            "Pol Interest", "Pol Knowledge", "Age (in years)", "Female", "Education", "HH income",
            "Pref/Ideo Strength", "Majority perception", "Social norm", "Need for socl approval",
            "Pref/Ideo Strength", "Total Exp (log)", "Pol Interest", "Pref/Ideo Strength"))

## ------------------------------------------------------------------------ ##
## Predicting perception of exposure directly at W2                         ##
## controlling actual exposure & mediation via perceived.opinion.climate.W2 ##
## ------------------------------------------------------------------------ ##

## total effect models
W2.total.candpref <- lm(dangerous.disc.prcptn.W2.candpref ~
                   dangerous.disc.W1.candpref +
                   discussion.norm.W2 +
                   need.for.approval.W2 + ## cf. interaction is not sig as well
                   ## partisan motivations
                   cand.pref.strength.W2 + SNS.use +
                   ## other controls
                   log.total.exp.W1 + pol.interest.W2 + pol.know +
                   ## demographic controls
                   age.years + female + edu + household.income, data = cleaned.data)

W2.total.ideo3 <- lm(update.formula(W2.total.candpref,
                         dangerous.disc.prcptn.W2.ideo3 ~ . + dangerous.disc.W1.ideo3 - dangerous.disc.W1.candpref
                         - cand.pref.strength.W2 + ideo_str.W2),
                         data = cleaned.data)
W2.total.ideo2 <- lm(update.formula(W2.total.candpref,
                         dangerous.disc.prcptn.W2.ideo2 ~ . + dangerous.disc.W1.ideo2 - dangerous.disc.W1.candpref
                         - cand.pref.strength.W2 + ideo_str.W2),
                         data = cleaned.data)
require(texreg)
screenreg(list(W2.total.candpref, W2.total.ideo3, W2.total.ideo2), digits = 3)


## Models predicting mediator (opinion climate perception)
W2.modelM.candpref <- lm(perceived.opinion.climate.W2 ~
                      dangerous.disc.W1.candpref +
                      discussion.norm.W2 +
                      need.for.approval.W2 + ## cf. interaction is not sig as well
                      ## partisan motivations
                        cand.pref.strength.W2 + SNS.use +
                      ## other controls
                      log.total.exp.W1 + pol.interest.W2 + pol.know +
                      ## demographic controls
                      age.years + female + edu + household.income, data = cleaned.data)

W2.modelM.ideo3 <- lm(update.formula(W2.modelM.candpref, . ~ . + dangerous.disc.W1.ideo3 - dangerous.disc.W1.candpref
                                     - cand.pref.strength.W2 + ideo_str.W2),
                     data = cleaned.data)
W2.modelM.ideo2 <- lm(update.formula(W2.modelM.candpref, . ~ . + dangerous.disc.W1.ideo2 - dangerous.disc.W1.candpref
                                     - cand.pref.strength.W2 + ideo_str.W2),
                     data = cleaned.data)

## Models predicting dependent along with X and M
W2.modelY.candpref <- lm(dangerous.disc.prcptn.W2.candpref ~
                      perceived.opinion.climate.W2 +
                      dangerous.disc.W1.candpref +
                      # poly(dangerous.disc.W1.candpref,2) + both are sig when poly^2 entered..
                      discussion.norm.W2 +
                      need.for.approval.W2 + ## cf. interaction is not sig as well
                      ## partisan motivations
                        cand.pref.strength.W2 + SNS.use +
                      ## other controls
                      log.total.exp.W1 + pol.interest.W2 + pol.know +
                      ## demographic controls
                      age.years + female + edu + household.income, data = cleaned.data)

W2.modelY.ideo3 <- lm(update.formula(W2.modelY.candpref,
                      dangerous.disc.prcptn.W2.ideo3 ~ . + dangerous.disc.W1.ideo3 - dangerous.disc.W1.candpref
                      - cand.pref.strength.W2 + ideo_str.W2),
                      data = cleaned.data)
W2.modelY.ideo2 <- lm(update.formula(W2.modelY.candpref,
                      dangerous.disc.prcptn.W2.ideo2 ~ . + dangerous.disc.W1.ideo2 - dangerous.disc.W1.candpref
                      - cand.pref.strength.W2 + ideo_str.W2),
                      data = cleaned.data)
screenreg(list(W2.modelM.candpref, W2.modelY.candpref,
               W2.modelM.ideo3, W2.modelY.ideo3,
               W2.modelM.ideo2, W2.modelY.ideo2), digits = 3,
          custom.model.names = c("Cand pref, M", "Cand pref, Y",
                                 "Ideo 3cat, M", "Ideo 3cat, Y",
                                 "Ideo 2cat, M", "Ideo 2cat, Y"),
          custom.coef.names = c(
            "(Intercept)", "Exposure (tracking)",
            "Social norm", "Need for socl approval",
            "Pref/Ideo Strength", "SNS use (min)",
            "Total Exp (log)", "Pol Interest", "Pol Knowledge",
            "Age (in years)", "Female", "Education", "HH income",
            "Majority perception",
            "Exposure (tracking)", "Pref/Ideo Strength", "Exposure (tracking)"),
          reorder.coef = c(1:2,14,3:13)
          )


## Statistical Mediation
## indirect effect, candidate pref model
require(boot)
RNGkind("L'Ecuyer-CMRG"); set.seed(12345)
W2.boot.candpref <- boot(cleaned.data, est.uncond.indirect, R = 10000,
                    lm.model.M = W2.modelM.candpref,
                    lm.model.Y = W2.modelY.candpref,
                    pred = "dangerous.disc.W1.candpref",
                    parallel = "multicore", ncpus = 8)
W2.boot.candpref %>% get.boot.stats(., type = "perc")

## indirect effect, 3-fold ideology model
RNGkind("L'Ecuyer-CMRG"); set.seed(12345)
W2.boot.ideo3 <- boot(cleaned.data, est.uncond.indirect, R = 10000,
                             lm.model.M = W2.modelM.ideo3,
                             lm.model.Y = W2.modelY.ideo3,
                             pred = "dangerous.disc.W1.ideo3",
                             parallel = "multicore", ncpus = 8)
W2.boot.ideo3 %>% get.boot.stats(., type = "perc")

## indirect effect, 2-fold ideology model
RNGkind("L'Ecuyer-CMRG"); set.seed(12345)
W2.boot.ideo2 <- boot(cleaned.data, est.uncond.indirect, R = 10000,
                             lm.model.M = W2.modelM.ideo2,
                             lm.model.Y = W2.modelY.ideo2,
                             pred = "dangerous.disc.W1.ideo2",
                             parallel = "multicore", ncpus = 8)

W2.boot.ideo2 %>% get.boot.stats(., type = "perc")

## cf. causal mediation and sensitivity analysis for confounding between M and Y
library("mediation")

med.out <- mediation::mediate(W2.modelM.candpref, W2.modelY.candpref,
                    treat = "dangerous.disc.W1.candpref", mediator = "perceived.opinion.climate.W2",
                    sims = 10000)
summary(med.out)
medsens(med.out, rho.by = 0.1, effect.type = "indirect", sims = 10000) %>%
  plot(., sens.par = "R2", sign.prod = "negative")

## ------------------------------------------------------------------------ ##
## Predicting perception of exposure directly at W3                         ##
## controlling actual exposure & mediation via perceived.opinion.climate.W3 ##
## ------------------------------------------------------------------------ ##

## Total effect models
W3.total.candpref <- lm(dangerous.disc.prcptn.W3.candpref ~
                          dangerous.disc.W2.candpref +
                          discussion.norm.W3 +
                          need.for.approval.W3 + ## cf. interaction is not sig as well
                          ## partisan motivations
                          cand.pref.strength.W3 + SNS.use +
                          ## other controls
                          log.total.exp.W2 + pol.interest.W3 + pol.know +
                          ## demographic controls
                          age.years + female + edu + household.income, data = cleaned.data)

W3.total.ideo3 <- lm(update.formula(W3.total.candpref,
                                    dangerous.disc.prcptn.W3.ideo3 ~ . + dangerous.disc.W2.ideo3 - dangerous.disc.W2.candpref
                                    - cand.pref.strength.W3 + ideo_str.W3),
                     data = cleaned.data)
W3.total.ideo2 <- lm(update.formula(W3.total.candpref,
                                    dangerous.disc.prcptn.W3.ideo2 ~ . + dangerous.disc.W2.ideo2 - dangerous.disc.W2.candpref
                                    - cand.pref.strength.W3 + ideo_str.W3),
                     data = cleaned.data)
require(texreg)
screenreg(list(W3.total.candpref, W3.total.ideo3, W3.total.ideo2), digits = 3)


## Models predicting mediator (opinion climate perception)
W3.modelM.candpref <- lm(perceived.opinion.climate.W3 ~
                           dangerous.disc.W2.candpref +
                           discussion.norm.W3 +
                           need.for.approval.W3 + ## cf. interaction is not sig as well
                           ## partisan motivations
                           cand.pref.strength.W3 + SNS.use +
                           ## other controls
                           log.total.exp.W2 + pol.interest.W3 + pol.know +
                           ## demographic controls
                           age.years + female + edu + household.income, data = cleaned.data)

W3.modelM.ideo3 <- lm(update.formula(W3.modelM.candpref, . ~ . + dangerous.disc.W2.ideo3 - dangerous.disc.W2.candpref
                                     - cand.pref.strength.W3 + ideo_str.W3),
                      data = cleaned.data)
W3.modelM.ideo2 <- lm(update.formula(W3.modelM.candpref, . ~ . + dangerous.disc.W2.ideo2 - dangerous.disc.W2.candpref
                                     - cand.pref.strength.W3 + ideo_str.W3),
                      data = cleaned.data)

## Models predicting dependent along with X and M
W3.modelY.candpref <- lm(dangerous.disc.prcptn.W3.candpref ~
                           perceived.opinion.climate.W3 +
                           dangerous.disc.W2.candpref +
                           # poly(dangerous.disc.W1.candpref,2) + both are sig when poly^2 entered..
                           discussion.norm.W3 +
                           need.for.approval.W3 + ## cf. interaction is not sig as well
                           ## partisan motivations
                           cand.pref.strength.W3 + SNS.use +
                           ## other controls
                           log.total.exp.W2 + pol.interest.W3 + pol.know +
                           ## demographic controls
                           age.years + female + edu + household.income, data = cleaned.data)

W3.modelY.ideo3 <- lm(update.formula(W3.modelY.candpref,
                                     dangerous.disc.prcptn.W3.ideo3 ~ . + dangerous.disc.W2.ideo3 - dangerous.disc.W2.candpref
                                     - cand.pref.strength.W3 + ideo_str.W3),
                      data = cleaned.data)
W3.modelY.ideo2 <- lm(update.formula(W3.modelY.candpref,
                                     dangerous.disc.prcptn.W3.ideo2 ~ . + dangerous.disc.W2.ideo2 - dangerous.disc.W2.candpref
                                     - cand.pref.strength.W3 + ideo_str.W3),
                      data = cleaned.data)

screenreg(list(W3.modelM.candpref, W3.modelY.candpref,
               W3.modelM.ideo3, W3.modelY.ideo3,
               W3.modelM.ideo2, W3.modelY.ideo2), digits = 3,
          custom.model.names = c("Cand pref, M", "Cand pref, Y",
                                 "Ideo 3cat, M", "Ideo 3cat, Y",
                                 "Ideo 2cat, M", "Ideo 2cat, Y"),
          custom.coef.names = c(
            "(Intercept)", "Exposure (tracking)",
            "Social norm", "Need for socl approval",
            "Pref/Ideo Strength", "SNS use (min)",
            "Total Exp (log)", "Pol Interest", "Pol Knowledge",
            "Age (in years)", "Female", "Education", "HH income",
            "Majority perception",
            "Exposure (tracking)", "Pref/Ideo Strength", "Exposure (tracking)"),
          reorder.coef = c(1:2,14,3:13)
)

## Statistical Mediation
require(boot)
RNGkind("L'Ecuyer-CMRG"); set.seed(12345)
W3.boot.candpref <- boot(cleaned.data, est.uncond.indirect, R = 10000,
                         lm.model.M = W3.modelM.candpref,
                         lm.model.Y = W3.modelY.candpref,
                         pred = "dangerous.disc.W2.candpref",
                         parallel = "multicore", ncpus = 8)
## indirect effect, candidate pref model
W3.boot.candpref %>% get.boot.stats(., type = "perc")

RNGkind("L'Ecuyer-CMRG"); set.seed(12345)
W3.boot.ideo3 <- boot(cleaned.data, est.uncond.indirect, R = 10000,
                      lm.model.M = W3.modelM.ideo3,
                      lm.model.Y = W3.modelY.ideo3,
                      pred = "dangerous.disc.W2.ideo3",
                      parallel = "multicore", ncpus = 8)
## indirect effect, 3-fold ideology model
W3.boot.ideo3 %>% get.boot.stats(., type = "perc")

RNGkind("L'Ecuyer-CMRG"); set.seed(12345)
W3.boot.ideo2 <- boot(cleaned.data, est.uncond.indirect, R = 10000,
                      lm.model.M = W3.modelM.ideo2,
                      lm.model.Y = W3.modelY.ideo2,
                      pred = "dangerous.disc.W2.ideo2",
                      parallel = "multicore", ncpus = 8)
## indirect effect, 2-fold ideology model
W3.boot.ideo2 %>% get.boot.stats(., type = "perc")


## --------------------------- ##
## Conditional indirect effect ##
## --------------------------- ##

summary(W2.modelM.candpref)
summary(W2.modelY.candpref)

## Biased processing?
## using candidate preference as criteria
W2.modelM.mod.candpref <- lm(formula = perceived.opinion.climate.W2 ~ dangerous.disc.W1.candpref*cand.pref.strength.W2 +
                               discussion.norm.W2 + need.for.approval.W2 + cand.pref.strength.W2 +
                               SNS.use + log.total.exp.W1 + pol.interest.W2 + pol.know +
                               age.years + female + edu + household.income, data = cleaned.data)
require(interactions)
interact_plot(model = W2.modelM.mod.candpref, pred = "dangerous.disc.W1.candpref", modx = "cand.pref.strength.W2")

W2.boot.condind.candpref <- boot(cleaned.data, est.cond.indirect, R = 10000,
                                             lm.model.M = W2.modelM.mod.candpref,
                                             lm.model.Y = W2.modelY.candpref,
                                             pred = "dangerous.disc.W1.candpref",
                                             med = "perceived.opinion.climate.W2",
                                             modx = "cand.pref.strength.W2")

W2.boot.condind.candpref %>% get.boot.stats(., type = "perc")
## index is not significant...

## using ideo 3 category
W2.modelM.mod.ideo3 <- lm(formula = perceived.opinion.climate.W2 ~ dangerous.disc.W1.ideo3*ideo_str.W2 +
                               discussion.norm.W2 + need.for.approval.W2 + ideo_str.W2 +
                               SNS.use + log.total.exp.W1 + pol.interest.W2 + pol.know +
                               age.years + female + edu + household.income, data = cleaned.data)
interact_plot(model = W2.modelM.mod.ideo3, pred = "dangerous.disc.W1.ideo3", modx = "ideo_str.W2")

W2.boot.condind.ideo3 <- boot(cleaned.data, est.cond.indirect, R = 10000,
                                lm.model.M = W2.modelM.mod.ideo3,
                                lm.model.Y = W2.modelY.ideo3,
                                pred = "dangerous.disc.W1.ideo3",
                                med = "perceived.opinion.climate.W2",
                                modx = "ideo_str.W2")
W2.boot.condind.ideo3 %>% get.boot.stats(., type = "perc")
## index is significant
## index.modmed = 0.06134339012 [0.0150675793, 0.12451138]

test2 <- W2.boot.condind.ideo3 %>% get.boot.stats(., type = "perc")
test2 <- cbind(mod.val = select.modval(cleaned.data, "ideo_str.W2"), test2[-c(1,303), ])
ggplot(test2, aes(x = mod.val, y = coef, ymin = llci, ymax = ulci)) +
  #geom_point(stat = "identity", color = "black") +
  geom_line(color = "black") +
  geom_ribbon(colour = "grey", alpha = 0.3) +
  geom_hline(yintercept = 0, colour = "black", lty = 2) +
  geom_vline(xintercept = 0.74, colour = "red", lty = 2) +
  xlab("Moderator: Ideology strength W2") +
  ylab("\u03b8 = Indirect effects of X on Y") + theme_bw()


## using ideo 2 category
W2.modelM.mod.idoe2 <- lm(formula = perceived.opinion.climate.W2 ~ dangerous.disc.W1.ideo2*ideo_str.W2 +
                               discussion.norm.W2 + need.for.approval.W2 + ideo_str.W2 +
                               SNS.use + log.total.exp.W1 + pol.interest.W2 + pol.know +
                               age.years + female + edu + household.income, data = cleaned.data)
interact_plot(model = W2.modelM.mod.idoe2, pred = "dangerous.disc.W1.ideo2", modx = "ideo_str.W2")

W2.boot.condind.ideo2 <- boot(cleaned.data, est.cond.indirect, R = 10000,
                              lm.model.M = W2.modelM.mod.idoe2,
                              lm.model.Y = W2.modelY.ideo2,
                              pred = "dangerous.disc.W1.ideo2",
                              med = "perceived.opinion.climate.W2",
                              modx = "ideo_str.W2")
W2.boot.condind.ideo2 %>% get.boot.stats(., type = "perc")
## index is significant
## index.modmed = 0.07466664 [0.01004325355, 0.1529417]

test3 <- W2.boot.condind.ideo2 %>% get.boot.stats(., type = "perc")
test3 <- cbind(mod.val = select.modval(cleaned.data, "ideo_str.W2"), test3[-c(1,303), ])
ggplot(test3, aes(x = mod.val, y = coef, ymin = llci, ymax = ulci)) +
  #geom_point(stat = "identity", color = "black") +
  geom_line(color = "black") +
  geom_ribbon(colour = "grey", alpha = 0.3) +
  geom_hline(yintercept = 0, colour = "black", lty = 2) +
  geom_vline(xintercept = 0.30, colour = "red", lty = 2) +
  xlab("Moderator: Ideology strength W2") +
  ylab("\u03b8 = Indirect effects of X on Y") + theme_bw()








## ------------------------------------------------------ ##
## Instantaneous indirect effect (Hayes & Preacher, 2010) ##
## (when one of the constituent path is nonlinear)        ##
## ------------------------------------------------------ ##

## How much Y is changing at the point X = x indirectly through X’s effect on M which, in turn, affects Y
## first, check linear model for M / for Y and possible quadratic relationships
W2.modelM.candpref %>% summary
W2.modelM.candpref.nonlinear <- lm(update.formula(W2.modelM.candpref,
                                                  . ~ . - dangerous.disc.W1.candpref + poly(dangerous.disc.W1.candpref, 2)),
                                   data = cleaned.data)
test <- cbind(X = cleaned.data[, dangerous.disc.W1.candpref],
              Y_hat = W2.modelM.candpref$fitted.values,
              Y_hat2 = W2.modelM.candpref.nonlinear$fitted.values,
              Y = cleaned.data[, perceived.opinion.climate.W2]) %>% as.data.frame(.)
ggplot(test, aes(x = X, y = Y)) + geom_jitter() +
  geom_smooth(aes(y = Y_hat), method = "lm", se = T) +
  geom_smooth(aes(y = Y_hat2), method = "lm", formula = y ~ poly(x,2), color = "red", se = T)

W2.modelY.candpref %>% summary
W2.modelY.candpref.nonlinear <- lm(update.formula(W2.modelY.candpref,
                                                  . ~ . - dangerous.disc.W1.candpref + poly(dangerous.disc.W1.candpref, 2)),
                                   data = cleaned.data)
test2 <- cbind(X = cleaned.data[, dangerous.disc.W1.candpref],
               Y_hat = W2.modelY.candpref$fitted.values,
               Y_hat2 = W2.modelY.candpref.nonlinear$fitted.values,
              Y = cleaned.data[, dangerous.disc.prcptn.W2.candpref]) %>% as.data.frame(.)
ggplot(test2, aes(x = X, y = Y)) + geom_jitter() +
  geom_smooth(aes(y = Y_hat), method = "lm", se = T) +
  geom_smooth(aes(y = Y_hat2), method = "lm", formula = y ~ poly(x,2), color = "red", se = T)

## center predictors, and create quadratic terms (for interpretation purpose)
cleaned.data[, X_cntrd := dangerous.disc.W1.candpref - mean(dangerous.disc.W1.candpref)]
cleaned.data[, X_cntrdsq := X_cntrd*X_cntrd]

W2.modelM.candpref.nonlinear <- lm(update.formula(W2.modelM.candpref,
                                    . ~ . - dangerous.disc.W1.candpref + X_cntrd + X_cntrdsq),
                                   data = cleaned.data)
W2.modelY.candpref.nonlinear <- lm(update.formula(W2.modelY.candpref,
                                                  . ~ . - dangerous.disc.W1.candpref + X_cntrd + X_cntrdsq),
                                   data = cleaned.data)


est.instantaneous.indirect <- function(dat, ## data frame to pass for bootstrapping
                                       i, ## i = the index value for resample
                                       lm.model.M, ## lm object estimating M
                                       lm.model.Y, ## lm object estimating Y
                                       pred, ## X
                                       pred_sq, ## X^2
                                       m.name = NULL) {  ## mediator name

  ## extract mediator automatically from the model of M if not provided
  if (is.null(m.name)) m.name <- formula.tools::lhs.vars(formula(lm.model.M))

  ## check mediator is indeed included in the model of Y
  check <- !(m.name %in% formula.tools::rhs.vars(formula(lm.model.Y)))
  if (isTRUE(check)) stop("lm.model.Y does not have proper mediator variable as in lm.model.M!")

  resample <- dat[i,]
  model.M.resample <- lm(formula(lm.model.M), data = resample)
  model.Y.resample <- lm(formula(lm.model.Y), data = resample)

  a1 <- summary(model.M.resample)$coef[pred, 1]
  a2 <- summary(model.M.resample)$coef[pred_sq, 1]
  b <- summary(model.Y.resample)$coef[m.name, 1]
  #c1 <- summary(model.Y.resample)$coef[pred, 1]
  #c2 <- summary(model.Y.resample)$coef[pred_sq, 1]

  ## effect decomposition ##
  ## define X value at every docile
  X <- dat[, pred, with = F] %>% sapply(., as.numeric) %>% quantile(., seq(from = 0, to = 1, by = 0.01))

  ## instantaneous indirect effect
  ind.effect <- (a1 + 2*a2*X)*b
  index <- a2*b
  ## instantaneous direct effect
  #dir.effect <- (c1 + 2*c2*X)

  return(c(ind.effect = ind.effect, index = index))
}

RNGkind("L'Ecuyer-CMRG"); set.seed(12345)
W3.boot.instantaneous.candpref <-
  boot(cleaned.data, est.instantaneous.indirect, R = 10000,
        lm.model.M = W2.modelM.candpref.nonlinear,
        lm.model.Y = W2.modelY.candpref.nonlinear,
        pred = "X_cntrd", pred_sq = "X_cntrdsq",
        parallel = "multicore", ncpus = 8)
## instantaneous indirect effect, at every docile
W3.boot.instantaneous.candpref %>% get.boot.stats(., type = "perc")

test3 <- cbind(X_centered = quantile(cleaned.data[, X_cntrd] %>% sapply(., as.numeric),
                                     seq(from = 0, to = 1, by = 0.01)),
               W3.boot.instantaneous.candpref %>% get.boot.stats(., type = "perc"))

ggplot(test3, aes(x = X_centered, y = coef, ymin = llci, ymax = ulci)) +
  #geom_point(stat = "identity", color = "black") +
  geom_line(color = "black") +
  geom_ribbon(colour = "grey", alpha = 0.3) +
  geom_hline(yintercept = 0, colour = "black", lty = 2) +
  geom_vline(xintercept = -0.077167559, colour = "red", lty = 2) +
  xlab("Exposure to disagreement (Tracking data), centered") +
  ylab("\u03b8 (Instantaneous indirect effects of X on Y)") + theme_bw()























































require(texreg)
## Table 1 in the main ms and A1 in the appendix
screenreg(list(model1, model1.cat, #model1.luc,
               model2, model2.cat), #model2.luc),
          stars = c(0.001, 0.01, 0.05, 0.10), digits = 3,
          custom.model.names = c("OLS W2", "GLM W2", #"LCS W2",
                                 "OLS W3", "GLM W3"), #"LCS W3"),
          custom.coef.names = c(
            "(Intercept)", "Discussion norm W2/W3",
            "Need for socl approval W2/W3", "Prcvd Op Climate W2/W3",
            "Candidate pref W2/W3", "Ideo Strength W2/W3",
            "Total Exp W1/W2 (log)", "Interest W2/W3", "Knowledge",
            "Age (in years)", "Female", "Education", "HH income",
            "Media Exposure",  "Discussion norm W2/W3",
            "Need for socl approval W2/W3", "Prcvd Op Climate W2/W3",
            "Candidate pref W2/W3", "Ideo Strength W2/W3",
            "Total Exp W1/W2 (log)", "Interest W2/W3"),
          override.ci.low = list(boot.model1[,2], boot.model1.cat[,2],
                                 boot.model2[,2], boot.model2.cat[,2]),
          override.ci.up = list(boot.model1[,3], boot.model1.cat[,3],
                                boot.model2[,3], boot.model2.cat[,3]),
          reorder.coef = c(2:3, 8:9, 4:6, 7,14, 10:13, 1),
          groups = list("Social desirability" = 1:2,
                        "Cognitive burden" = 3:4,
                        "Opinion Climate" = 5,
                        "Controls" = 6:9,
                        "Demographics" = 10:13))

## cf. Pseudo-Rsq
require(rcompanion)
nagelkerke(model1.cat)$Pseudo.R.squared.for.model.vs.null[3] # Nagelkerke R-sq
nagelkerke(model2.cat)$Pseudo.R.squared.for.model.vs.null[3]


## Table A3 in the appendix, predicting perceived exposure to disagreement
screenreg(list(model3.ols, model4.ols),
          stars = c(0.001, 0.01, 0.05, 0.10), digits = 3,
          single.row = T, leading.zero = F,
          custom.model.names = c("Prcv Dis W2", "Prcv Dis W3"),
          override.ci.low = list(boot.model3[,2], boot.model4[,2]),
          override.ci.up = list(boot.model3[,3], boot.model4[,3]),
          custom.coef.names = c(
          "(Intercept)", "Actual Exp to Dis % W1/W2", "Discussion norm W2/W3",
          "Need for socl approval W2/W3", "Prcvd Op Climate W2/W3",
          "Candidate pref W2/W3", "Ideo Strength W2/W3",
          "Total Exp W1/W2 (log)", "Interest W2/W3", "Knowledge",
          "Age (in years)", "Female", "Education", "HH income",
          "Media Exposure",  "Actual Exp to Dis % W1/W2", "Discussion norm W2/W3",
          "Need for socl approval W2/W3", "Prcvd Op Climate W2/W3",
          "Candidate pref W2/W3", "Ideo Strength W2/W3",
          "Total Exp W1/W2 (log)", "Interest W2/W3"),
          reorder.coef = c(2:4, 9:10, 5:7, 8,15, 11:14, 1),
          groups = list("Actual Exposure" = 1,
                        "Social desirability" = 2:3,
                        "Cognitive burden" = 4:5,
                        "Opinion Climate" = 6,
                        "Controls" = 7:10,
                        "Demographics" = 11:14))




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
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1),
               lty = 2, color = "grey") +
  xlab("W1 log data \n(Cumulative proportion, most recent three days)") +
  ylab("W2 Perception \n(Prop. of Exposure to diagreement)") +
  ggtitle("W1 Recent 3-day vs. W2 Perception")

qq3r <- ggplot(qq.out3.r, aes(x = x, y = y)) +
  geom_jitter(width = 0.02, color = "grey") + theme_bw() +
  stat_smooth(aes(group = 1), color = "red", se = FALSE) +
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1),
               lty = 2, color = "grey") +
  xlab("W2 log data \n(Cumulative proportion, most recent three days)") +
  ylab("W3 Perception \n(Prop. of Exposure to diagreement)") +
  ggtitle("W2 Recent 3-day vs. W3 Perception")

## Figure 2
qq2r + qq3r + plot_layout(ncol = 2)


## test of equality of two correlation coefficients
## a comparison of two overlapping correlations based on dependent groups
require(cocor)

## Wave 2 measures
RNGkind("L'Ecuyer-CMRG"); set.seed(12345)
cocor(~ dangerous.disc.prcptn.W2 + recent.dangerous.disc.W1 |
        dangerous.disc.prcptn.W2 + dangerous.disc.W1, data = cleaned.data)

# Comparison between
#   r.jk (dangerous.disc.prcptn.W2, recent.dangerous.disc.W1) = 0.3905 and
#   r.jh (dangerous.disc.prcptn.W2, dangerous.disc.W1) = 0.4481
#   Difference: r.jk - r.jh = -0.0576
# 95% confidence interval for r.jk - r.jh: [-0.0924, -0.0250]
## RESULTS INDICATES 3-DAY BASED CORRELATIONS ARE WEAKER!

## Wave 3 measures
cocor(~ dangerous.disc.prcptn.W3 + recent.dangerous.disc.W2 |
        dangerous.disc.prcptn.W3 + dangerous.disc.W2, data = cleaned.data)

# Comparison between:
#   r.jk (dangerous.disc.prcptn.W3, recent.dangerous.disc.W2) = 0.4392 and
#   r.jh (dangerous.disc.prcptn.W3, dangerous.disc.W2) = 0.4592
#   Difference: r.jk - r.jh = -0.0199
# 95% confidence interval for r.jk - r.jh: [-0.0574, 0.0167]
## RESULTS INDICATES THERE IS NO SIGNIFICANT DIFFERENCES

## replicate the models
model1r <- lm(update.formula(model1,
               recent.accuracy.W2 ~ .
               + log.recent.total.exp.W1 - log.total.exp.W1),
              data = cleaned.data)
model2r <- lm(update.formula(model2,
               recent.accuracy.W3 ~ .
               + log.recent.total.exp.W2 - log.total.exp.W2),
              data = cleaned.data)

RNGkind("L'Ecuyer-CMRG"); set.seed(12345)
boot.model1r <- boot(cleaned.data, statistic = boot.lm,
                    R = 10000, parallel = "multicore", ncpus = 8,
                    lm.fit = model1r) %>% get.boot.stats(., type = "perc")

RNGkind("L'Ecuyer-CMRG"); set.seed(12345)
boot.model2r <- boot(cleaned.data, statistic = boot.lm,
                    R = 10000, parallel = "multicore", ncpus = 8,
                    lm.fit = model2r) %>% get.boot.stats(., type = "perc")


require(texreg)
## Table 2 in the main ms and Table A4 in the appendix
screenreg(list(model1, model1r, model2, model2r),
          stars = c(0.001, 0.01, 0.05, 0.10), digits = 3,
          custom.model.names = c("OLS W2", "OLS W2 RECENT",
                                 "OLS W3", "OLS W3 RECENT"),
          custom.coef.names = c(
            "(Intercept)", "Discussion norm W2/W3",
            "Need for socl approval W2/W3", "Prcvd Op Climate W2/W3",
            "Candidate pref W2/W3", "Ideo Strength W2/W3",
            "Total Exp W1/W2 (log)", "Interest W2/W3", "Knowledge",
            "Age (in years)", "Female", "Education", "HH income",
            "Media Exposure", "Total Exp W1/W2 (log)",
            "Discussion norm W2/W3", "Need for socl approval W2/W3",
            "Prcvd Op Climate W2/W3", "Candidate pref W2/W3",
            "Ideo Strength W2/W3", "Total Exp W1/W2 (log)",
            "Interest W2/W3", "Total Exp W1/W2 (log)"),
          override.ci.low = list(boot.model1[,2], boot.model1r[,2],
                                 boot.model2[,2], boot.model2r[,2]),
          override.ci.up = list(boot.model1[,3], boot.model1r[,3],
                                boot.model2[,3], boot.model2r[,3]),
          reorder.coef = c(2:3, 8:9, 4:6, 7,14, 10:13, 1),
          groups = list("Social desirability" = 1:2,
                        "Cognitive burden" = 3:4,
                        "Opinion Climate" = 5,
                        "Controls" = 6:9,
                        "Demographics" = 10:13))


  ## ------------------ ##
  ## Outcome evaluation ##
  ## ------------------ ##

## Consequences of biased estimate of exposure to disagreement?
terms <- c("pref.certainty.W3", "pref.certainty.W2", "dangerous.disc.W2",
           "dangerous.disc.prcptn.W3", "log.total.exp.W2", "age.years",
           "female", "edu", "household.income", "canpref.W2",
           "pol.interest.W2", "pol.know", "ideo_str.W2",
           "internal.efficacy.W3", "media.exposure.W2")
index <- cleaned.data[, .SD, .SDcols = terms] %>% complete.cases(.)
cleaned.data[, dangerous.disc.prcptn.W3.r := dangerous.disc.prcptn.W3/100]

  model.certainty.1 <- lm(pref.certainty.W3 ~
                            ## lagged DV
                            pref.certainty.W2 +
                       ## focal X
                         dangerous.disc.prcptn.W3 +
                       # dangerous.disc.W2 +
                       log.total.exp.W2 +
                       ## demographic controls
                       age.years + female + edu + household.income +
                       ## political correlates
                       canpref.W2 + pol.interest.W2 +
                         pol.know + ideo_str.W2 + internal.efficacy.W3 +
                       ## media exposure
                       media.exposure.W2
                     , data = cleaned.data[index, ])


  model.certainty.2 <- lm(update.formula(model.certainty.1,
                                         . ~ . + dangerous.disc.W2
                                         - dangerous.disc.prcptn.W3),
                          data = cleaned.data[index, ])

## Table 3 in the main ms and Table A5 in the appendix
screenreg(list(model.certainty.1, model.certainty.2), digits = 3,
          custom.model.names = c("Pref Cernty Sbj", "Pref Cernty Obj"),
          custom.coef.names = c("(Intercept)", "Pref Certainty W2",
            "Exp Disagree (Sbj)", "Total Exp W2 (log)",
            "Age (in years)", "Female", "Education", "HH income",
            "Candidate pref", "Interest", "Knowledge", "Ideo Strength",
            "Efficacy", "Media Exposure", "Exp Disagree (Obj)"),
          reorder.coef = c(2, 3,15, 4,9:14, 5:8, 1),
          groups = list("Lagged DV" = 1, "Focal predictor" = 2:3,
                        "Correlates" = 4:10, "Demographics" = 11:14,
                        "Intercept" = 15))

coef.sbj <- coef(summary(model.certainty.1))['dangerous.disc.prcptn.W3', 1]
coef.obj <- coef(summary(model.certainty.2))['dangerous.disc.W2', 1]
relative.size.sbj.obs <- coef.sbj/coef.obj
bias.obs <- abs(coef.sbj - coef.obj)

p2_coef <- plot_summs(model.certainty.1, model.certainty.2,
                      #scale = TRUE, n.sd = 2,
                     colors = "Qual2",
                     model.names = c("Model w/ sbj measure",
                                     "Model w/ obj measure"),
                     legend.title = "DV: Pref certainty W3",
                     coefs = c("Pref Certainty W2" = "pref.certainty.W2",
                               "Exp to Disagree" = "dangerous.disc.prcptn.W3",
                               "Exp to Disagree" = "dangerous.disc.W2",
                               "Total Exp W2 (log)" = "log.total.exp.W2",
                               "Media Exposure" = "media.exposure.W2",
                               "Candidate pref W2" = "canpref.W2",
                               "Ideo Strength W2" = "ideo_str.W2",
                               "Political interest"= "pol.interest.W2",
                               "Political Knowledge" = "pol.know",
                               "Political Efficacy" = "internal.efficacy.W3",
                               "Age (in years)" = "age.years",
                               "Female" = "female",
                               "Education" = "edu",
                               "HH income" = "household.income"))

p2_coef + xlab("Unstandardized regression coef.") + ylab("") + theme_bw() +
  theme(legend.position = "bottom")


## For simulation inference
require(MASS)
require(data.table)
require(psych)
require(lm.beta)
require(corpcor)

var.names <- unique(c(names(coef(model.certainty.1)),
                      names(coef(model.certainty.2)))) %>% .[-1]
mu <- cleaned.data[index, apply(.SD, 2, mean), .SDcols = var.names]
sds <- cleaned.data[index, apply(.SD, 2, sd), .SDcols = var.names]
focal.vars <- c("dangerous.disc.W2", "dangerous.disc.prcptn.W3")
cor.obs <- cleaned.data[index, cor(.SD), .SDcols = var.names]

# source("power_cal.R")
## simulation conditions
cond <- expand.grid(
  sample_n = c(341, 1000, 5000),
  target.corr = seq(from = 0, to = 0.95, by = 0.05)
)

reps <- mapply(sim.MC,
               sample_n = cond$sample_n,
               target.corr = cond$target.corr,
               SIMPLIFY = FALSE)

sim.results <- do.call("rbind", reps) %>% setDT(.)
save(sim.results, file = "sim.results.Rdata")

## bias, relative.size.sbj, coef.obj, sbj.coef.agree.with.obj
cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
## whether significance of sbj measure agree with obj measures?
p3_1 <- sim.results[, .(agree = mean(sbj.coef.agree.with.obj),
                llci = prop.cis(mean(sbj.coef.agree.with.obj), 1000)[1],
                ulci = prop.cis(mean(sbj.coef.agree.with.obj), 1000)[2]),
            by = c("target.corr", "sample_n")] %>%
  ggplot(., aes(x = target.corr, y = agree, color = factor(sample_n))) +
  geom_smooth(aes(ymin = llci, ymax = ulci), alpha = 0.3) +
  xlab("Zero-order corr. between subjective and objective measure") +
  ylab("Proportion of two results agree") + theme_bw() +
  scale_x_continuous(breaks = seq(0, 0.95, 0.1)) +
  theme(legend.position = "none") + scale_color_brewer(palette = "Dark2") +
  geom_vline(xintercept = cor.obs[2,14], linetype = 2, col = "red") +
  ggtitle("Panel A: Proportions of two results with same statistical significance")

## absolute bias of sbj measure agree against obj measures
p3_2 <- sim.results[, .(bias = median(bias),
                llci = quantile(bias, 0.025),
                ulci = quantile(bias, 0.975)),
            by = c("target.corr", "sample_n")] %>%
  ggplot(., aes(x = target.corr, y = bias, group = factor(sample_n))) +
  geom_ribbon(aes(ymin = llci, ymax = ulci), fill = "grey70", alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE, aes(color = factor(sample_n))) +
  xlab("") + ylab("Mean bias, subjective measure") + theme_bw() +
  xlab("Zero-order corr. between subjective and objective measure") +
  geom_hline(yintercept = bias.obs, linetype = 2, col = "red") +
  geom_vline(xintercept = cor.obs[2,14], linetype = 2, col = "grey") +
  scale_color_brewer(palette = "Dark2") +
  facet_grid(~ sample_n) + theme(legend.position = "none") +
  ggtitle("\nPanel B: Absolute bias, coefficient from subjective measure")

## relative size of subjective measure
p3_3 <- sim.results[, .(median.relative.size = median(relative.size.sbj),
                llci = quantile(relative.size.sbj, 0.025),
                ulci = quantile(relative.size.sbj, 0.975)),
            by = c("target.corr", "sample_n")] %>%
  ggplot(., aes(x = target.corr, y = median.relative.size)) +
  geom_ribbon(aes(ymin = llci, ymax = ulci), fill = "grey70", alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE, aes(color = factor(sample_n))) +
  xlab("Zero-order corr. between subjective and objective measure") +
  ylab("Relative size, subjective measure") + theme_bw() +
  geom_hline(yintercept = relative.size.sbj.obs, linetype = 2, col = "red") +
  scale_colour_discrete(name = "Sample N") + scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "bottom") + facet_grid(~ sample_n) +
  ggtitle("\nPanel C: Relative size, coefficient from subjective measure")

p3_1 + p3_2 + p3_3 + plot_layout(nrow = 3)
