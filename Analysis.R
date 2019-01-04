
## prepare data for analysis
## automatically setting working directories
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
if (!("cleaned.data" %in% ls())) source("dev/PreProcess.R")

require(texreg)
require(ggplot2)
require(jtools)

## we use 'cleaned.data' and 'long.data' for the rest of the analysis
# > colnames(cleaned.data)
# [1] "id"                           "safe.disc.W1"
# [3] "dangerous.disc.W1"            "exp.disagr.offline.prcpt.W1"
# [5] "safe.disc.W2"                 "dangerous.disc.W2"
# [7] "exp.disagr.offline.prcpt.W2"  "exp.disagr.online.prcpt.W2"
# [9] "diff.exp.disagree.W2"         "safe.disc.W3"
# [11] "dangerous.disc.W3"            "exp.disagr.offline.prcpt.W3"
# [13] "exp.disagr.online.prcpt.W3"   "diff.exp.disagree.W3"
# [15] "ego.netsize.W1"               "ego.netsize.W2"
# [17] "ego.netsize.W3"               "alter.centr.eigen.W1"
# [19] "alter.centr.eigen.W2"         "alter.centr.eigen.W3"
# [21] "alter.centr.indeg.W1"         "alter.centr.indeg.W2"
# [23] "alter.centr.indeg.W3"         "canpref.W1"
# [25] "canpref.W2"                   "canpref.W3"
# [27] "age.years"                    "female"
# [29] "edu"                          "household.income"
# [31] "residential.region"           "pref.certainty.W2"
# [33] "pref.certainty.W3"            "ideo.W1"
# [35] "ideo.W2"                      "ideo.W3"
# [37] "ideo_str.W1"                  "ideo_str.W2"
# [39] "ideo_str.W3"                  "perceived.opinion.climate.W2"
# [41] "perceived.opinion.climate.W3" "consistency.motivation"
# [43] "understanding.motivation"     "hedonic.motivation"
# [45] "internal.efficacy.W1"         "internal.efficacy.W2"
# [47] "internal.efficacy.W3"         "pol.interest.W1"
# [49] "pol.interest.W2"              "pol.interest.W3"
# [51] "internet.news.use.W2"         "newspaper.use.W2"
# [53] "tv.news.use.W2"               "outgroup.negativity.bias.W2"
# [55] "outgroup.negativity.bias.W3"  "ingroup.favoritism.bias.W2"
# [57] "ingroup.favoritism.bias.W3"   "affective.polarization.W2"
# [59] "affective.polarization.W3"

## ----------------------- ##
## 1. Preliminary analysis ##
## ----------------------- ##


cleaned.data[, cor(dangerous.disc.W2, exp.disagr.offline.prcpt.W2)]
bivariate.perm.test(cleaned.data, "dangerous.disc.W2", "exp.disagr.offline.prcpt.W2", cor)
#         obs  llci.0.025  ulci.0.975
# -0.04797520 -0.09458145  0.10673502

cleaned.data[, cor(dangerous.disc.W2, exp.disagr.online.prcpt.W2)]
bivariate.perm.test(cleaned.data, "dangerous.disc.W2", "exp.disagr.online.prcpt.W2", cor)
#       obs llci.0.025 ulci.0.975
# 0.2851594 -0.1049099  0.0980455

## create differences between perception and behavioral measures
## (+) values indicate the overestimation, and (-) means underestimation
cleaned.data[, diff.exp.disagree.W2 := exp.disagr.online.prcpt.W2 - dangerous.disc.W2]
cleaned.data[, summary(diff.exp.disagree.W2)]

## permutation test indicates that the difference between
## perception and objective behavior is significantly differ,
## in a way that people tend to overestimate the exposure to differences
diff.perm.test(cleaned.data, "exp.disagr.online.prcpt.W2", "dangerous.disc.W2", rep = 10000)


cleaned.data[, cor(dangerous.disc.W3, exp.disagr.offline.prcpt.W3)]
bivariate.perm.test(cleaned.data, "dangerous.disc.W3", "exp.disagr.offline.prcpt.W3", cor)
#         obs   llci.0.025   ulci.0.975
# 0.002198055 -0.100956353  0.118166630

cleaned.data[, cor(dangerous.disc.W3, exp.disagr.online.prcpt.W3)]
bivariate.perm.test(cleaned.data, "dangerous.disc.W3", "exp.disagr.online.prcpt.W3", cor)
#       obs  llci.0.025  ulci.0.975
#0.01856405 -0.10507169  0.11222190

## create differences between perception and behavioral measures
## (+) values indicate the overestimation, and (-) means underestimation
cleaned.data[, diff.exp.disagree.W3 := exp.disagr.online.prcpt.W3 - dangerous.disc.W3]
cleaned.data[, summary(diff.exp.disagree.W3)]

## permutation test indicates that the difference between
## perception and objective behavior is significantly differ,
## in a way that people tend to overestimate the exposure to differences
diff.perm.test(cleaned.data, "exp.disagr.online.prcpt.W3", "dangerous.disc.W3", rep = 10000)




## ------------------------------------- ##
## Predicting perceived opinion climates ##
## ------------------------------------- ##

## model predicting perceived opinion climate
model.M1 <- lm(perceived.opinion.climate.W2 ~
                ## focal predictor
                safe.disc.W1 + dangerous.disc.W1 +
                 ## discussion motivation
                 consistency.motivation + understanding.motivation +
                 ## demographic controls
                 age.years + female + edu + household.income +
                 ## political correlates
                 canpref.W2 + ideo_str.W2 + pol.interest.W2 + ego.netsize.W2 +
                 ## media exposure
                 internet.news.use.W2 + newspaper.use.W2 + tv.news.use.W2
              ,
              data = cleaned.data); summary(model.M1)

## model predicting social-identity based affective process: no effects
model.M2 <- lm(update.formula(model.M1, affective.polarization.W2 ~ .),
               data = cleaned.data); summary(model.M2)

## individual's ideology str significantly interact with dangerous.disc
model.M1.int1 <- lm(update.formula(model.M1, . ~ . + dangerous.disc.W1*ideo_str.W2), data = cleaned.data)
summary(model.M1.int1)
jtools::interact_plot(model.M1.int1, pred = "dangerous.disc.W1", modx = "ideo_str.W2")

## connected alters' eigenvector and indegree centrality (measured prior to each wave)
## also significantly interact with dangerous.disc
model.M1.int2 <- lm(
  update.formula(model.M1, . ~ . + dangerous.disc.W1*alter.centr.indeg.W1), data = cleaned.data)
summary(model.M1.int2)
jtools::interact_plot(model.M1.int2, pred = "dangerous.disc.W1", modx = "alter.centr.indeg.W1")

model.M1.int3 <- lm(
  update.formula(model.M1, . ~ . + dangerous.disc.W1*alter.centr.eigen.W1), data = cleaned.data)
summary(model.M1.int3)
jtools::interact_plot(model.M1.int3, pred = "dangerous.disc.W1", modx = "alter.centr.eigen.W1")


## interactions with safe.disc are NOT significant
## for connected alters' eigenvector and indegree centrality (measured prior to each wave)



## predicting self-reported exposure to disagreement
model.Y <- lm(exp.disagr.online.prcpt.W3 ~
                ## lagged effect
                ## exp.disagr.online.prcpt.W2 +
                ## focal predictor
                safe.disc.W1 + dangerous.disc.W1 +
                ## demographic controls
                age.years + female + edu + household.income +
                ## political correlates
                canpref.W2 + ideo_str.W2 + internal.efficacy.W2 + pol.interest.W2 +
                ## media exposure
                internet.news.use.W2 + newspaper.use.W2 + tv.news.use.W2 +
                ## mediator
                perceived.opinion.climate.W2 + affective.polarization.W2
              ,
              data = cleaned.data); summary(model.Y)

screenreg(list(model.M1, model.M2, model.Y),
          custom.model.names = c("Prcvd Op Climate", "Affective pol", "Selfrep Dis"))
screenreg(list(model.M1, model.M1.int1, model.M1.int2, model.M1.int3),
          custom.model.names = c("Prcvd Op Climate", "Two-way I", "Two-way II", "Three-way"))

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
                   lm.model.M = model.M1, lm.model.Y = model.Y,
                   pred = c("safe.disc.W1", "dangerous.disc.W1"))
out.uncond <- get.boot.stats(boot.test1)






## estimate conditional indirect effect
## as a function of ideological identification strength
estimate.conditional.indirect

set.seed(1234)
boot.test2 <- boot(cleaned.data, estimate.conditional.indirect,
                   R = 20000, parallel = "multicore", ncpus = 8)
out.cond <- get.boot.stats(boot.test2)


## plot conditional indirect effects
plot.cond.ind <- cbind(moderator = as.vector(0:3), out.cond[3:6,])
require(data.table)
setDT(plot.cond.ind)
ggplot(plot.cond.ind, aes(x = moderator, y = coef)) +
  geom_line() + geom_hline(yintercept = 0, color = "red", linetype = 2) +
  geom_ribbon(aes(ymin = llci, ymax = ulci), alpha = 0.25) +
  xlab("Moderator: Strengths of ideology") + ylab("theta") +
  ggtitle("Conditional indirect effect of actual exposure to disagreeable discussants message")



## cf
cleaned.data[, Y := exp.disagr.online.prcpt.W3 - exp.disagr.online.prcpt.W2]
cleaned.data[, M1 := perceived.opinion.climate.W3 - perceived.opinion.climate.W2]
cleaned.data[, M2 := affective.polarization.W3 - affective.polarization.W2]
cleaned.data[, X1 := dangerous.disc.W3 - dangerous.disc.W2]
cleaned.data[, X2 := safe.disc.W3 - safe.disc.W2]

model.M1 <- lm(M1 ~
                 ## focal predictor
                 X1 + X2 +
                 ## discussion motivation
                 consistency.motivation + understanding.motivation +
                 ## demographic controls
                 age.years + female + edu + household.income +
                 ## political correlates
                 canpref.W2 + ideo_str.W2 + pol.interest.W2 + ego.netsize.W2 +
                 ## media exposure
                 internet.news.use.W2 + newspaper.use.W2 + tv.news.use.W2
               ,
               data = cleaned.data); summary(model.M1)

model.M2 <- lm(update.formula(model.M1, M2 ~ .),
               data = cleaned.data); summary(model.M2)

## individual's ideology str significantly interact with dangerous.disc
model.M1.int1 <- lm(update.formula(model.M1, . ~ . + X1*understanding.motivation), data = cleaned.data)
summary(model.M1.int1)
jtools::interact_plot(model.M1.int1, pred = "X1", modx = "understanding.motivation")

model.Y <- lm(Y ~
                ## focal predictor
                X1 + X2 +
                ## discussion motivation
                consistency.motivation + understanding.motivation +
                ## demographic controls
                age.years + female + edu + household.income +
                ## political correlates
                canpref.W2 + ideo_str.W2 + pol.interest.W2 + ego.netsize.W2 +
                ## media exposure
                internet.news.use.W2 + newspaper.use.W2 + tv.news.use.W2 +
                ## mediator
                M1 + M2
              ,
              data = cleaned.data)
model.Y %>% summary(.)

