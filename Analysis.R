
## prepare data for analysis
## automatically setting working directories
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
if (!("cleaned.data" %in% ls())) source("dev/PreProcess.R")

require(texreg)
require(ggplot2)
require(jtools)
require(plotROC)

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
bivariate.perm.test(cleaned.data, "dangerous.disc.W2", "exp.disagr.offline.prcpt.W2")
#         obs  llci.0.025  ulci.0.975
# -0.04797520  -0.1017732  0.1072070
# make the rounded percentage and rerun the correlation does not change the results!
cleaned.data[, dangerous.disc.W2.prop := (dangerous.disc.W2 %>% round(., digits = 2))*100]
bivariate.perm.test(cleaned.data, "dangerous.disc.W2.prop", "exp.disagr.offline.prcpt.W2")

cleaned.data[, cor(dangerous.disc.W2.prop, exp.disagr.online.prcpt.W2)]
bivariate.perm.test(cleaned.data, "dangerous.disc.W2.prop", "exp.disagr.online.prcpt.W2")
# obs.minus.perm llci.0.025 ulci.0.975
#      0.2851594  0.1758708  0.3888837

## create differences between perception and behavioral measures
## (+) values indicate the overestimation, and (-) means underestimation
cleaned.data[, diff.exp.disagree.W2 := exp.disagr.online.prcpt.W2 - dangerous.disc.W2.prop]
cleaned.data[, summary(diff.exp.disagree.W2)]

## permutation test indicates that the difference between
## perception and objective behavior is significantly differ,
## in a way that people tend to overestimate the exposure to differences
diff.perm.test(cleaned.data, "exp.disagr.online.prcpt.W2", "dangerous.disc.W2.prop", rep = 20000)


cleaned.data[, cor(dangerous.disc.W3, exp.disagr.offline.prcpt.W3)]
bivariate.perm.test(cleaned.data, "dangerous.disc.W3", "exp.disagr.offline.prcpt.W3")
# obs.minus.perm    llci.0.025   ulci.0.975
#     0.03105019   -0.06974810   0.14456759

cleaned.data[, cor(dangerous.disc.W3, exp.disagr.online.prcpt.W3)]
bivariate.perm.test(cleaned.data, "dangerous.disc.W3", "exp.disagr.online.prcpt.W3")
# obs.minus.perm     llci.0.025     ulci.0.975
#      0.2958441      0.1926712      0.4104359

## create differences between perception and behavioral measures
## (+) values indicate the overestimation, and (-) means underestimation
cleaned.data[, dangerous.disc.W3.prop := (dangerous.disc.W3 %>% round(., digits = 2))*100]
cleaned.data[, diff.exp.disagree.W3 := exp.disagr.online.prcpt.W3 - dangerous.disc.W3.prop]
cleaned.data[, summary(diff.exp.disagree.W3)]

## permutation test indicates that the difference between
## perception and objective behavior is significantly differ,
## in a way that people tend to overestimate the exposure to differences
diff.perm.test(cleaned.data, "exp.disagr.online.prcpt.W3", "dangerous.disc.W3.prop", rep = 20000)

## make figures
qq.out2 <- with(cleaned.data,
                qqplot(x = exp.disagr.online.prcpt.W2,
                       y = dangerous.disc.W2.prop,
                       plot.it = FALSE)) %>% as.data.frame(.) %>% setDT(.)

qq.out3 <- with(cleaned.data,
               qqplot(x = exp.disagr.online.prcpt.W3,
                      y = dangerous.disc.W3.prop,
                      plot.it = FALSE)) %>% as.data.frame(.) %>% setDT(.)

qq2 <- ggplot(qq.out2, aes(x = x, y = y)) +
  geom_jitter(width = 0.02, color = "grey") + theme_bw() +
  geom_boxplot(aes(group = x), outlier.colour = "grey", outlier.shape = 1) +
  stat_summary(fun.y = median, geom = "line", aes(group=1), color = "red") +
  geom_segment(aes(x = 0, xend = 100, y = 0, yend = 100),
               lty = 2, color = "grey") +
  xlab("Perception (% of Exposure to disagreement)") +
  ylab("Activity log (Mean proportion)") +
  ggtitle("Quantile-Quantile plot, Wave 2")

qq3 <- ggplot(qq.out3, aes(x = x, y = y)) +
  geom_jitter(width = 0.02, color = "grey") + theme_bw() +
  geom_boxplot(aes(group = x), outlier.colour = "grey", outlier.shape = 1) +
  stat_summary(fun.y = median, geom = "line", aes(group=1), color = "red") +
  geom_segment(aes(x = 0, xend = 100, y = 0, yend = 100),
               lty = 2, color = "grey") +
  xlab("Perception (% of Exposure to disagreement)") +
  ylab("Activity log (Mean proportion)") +
  ggtitle("Quantile-Quantile plot, Wave 3")

qq2 + qq3 + plot_layout(nrow = 1)

## ------------------------------------- ##
## Predicting perceived opinion climates ##
## ------------------------------------- ##

## model predicting perceived opinion climate
model.M1 <- lm(perceived.opinion.climate.W2 ~
                ## focal predictor
                safe.disc.W1 + dangerous.disc.W1 +
                 ## discussion motivation
                 ## consistency.motivation + understanding.motivation +
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


## interactions with dangerous.disc.W1 are NOT significant for affective polarization
model.M2.int1 <- lm(update.formula(model.M2, . ~ . + dangerous.disc.W1*ideo_str.W2), data = cleaned.data)
summary(model.M2.int1)
## yet marginally significant for safe discussion
model.M2.int2 <- lm(update.formula(model.M2, . ~ . + safe.disc.W1*ideo_str.W2), data = cleaned.data)
summary(model.M2.int1)

## predicting self-reported exposure to disagreement
model.Y <- lm(update.formula(model.M1,
               exp.disagr.online.prcpt.W3 ~ .
               + perceived.opinion.climate.W2 + affective.polarization.W2),
              data = cleaned.data); summary(model.Y)

## Basic mediation models
screenreg(list(model.M1, model.M2, model.Y),
          single.row = T, digits = 3, leading.zero = FALSE,
          custom.model.names = c("M1: Prcvd Op Climate", "M2: Affective pol", "Y: Self-reptd Dis"),
          custom.coef.names = c("(Intercept)", "In-prty msg Exp W1", "Out-prty msg Exp W1",
                                "Age (in years)", "Female", "Education", "HH income",
                                "Candidate Pref W2", "Ideo Strgth W2", "Interest W2",
                                "Net size W2", "Internet News Use W2", "NP News Use W2",
                                "TV News Use W2", "Prcvd Op Climate", "Affective pol"),
          reorder.coef = c(2:3, 15:16, 8:14, 4:7, 1),
          groups = list("Focal predictors" = 1:2, "Mediators" = 3:4,
                        "Controls" = 5:11, "Demographics" = 12:15, "Intercept" = 16))

## First stage moderation models
screenreg(list(model.M1, model.M1.int1, model.M1.int2, model.M1.int3),
          single.row = T, digits = 3, leading.zero = FALSE,
          custom.model.names = c("Prcvd Op Climate", "Ideo Strgth", "Alter Indegree", "Alter Eigenvector"),
          custom.coef.names = c("(Intercept)", "In-prty msg Exp W1", "Out-prty msg Exp W1",
                                "Age (in years)", "Female", "Education", "HH income",
                                "Candidate Pref W2", "Ideo Strgth W2", "Interest W2",
                                "Net size W2", "Internet News Use W2", "NP News Use W2",
                                "TV News Use W2",
                                "Out-prty Exp X Ideo Strgth",
                                "Alter Indegree W1", "Out-prty Exp X Indegree",
                                "Alter Eigenvec W1", "Out-prty Exp X Eigen"),
          reorder.coef = c(2:3, 9,15:19, 8,10:14, 4:7, 1),
          groups = list("Focal predictors" = 1:2, "Interactions" = 3:8,
                        "Controls" = 9:14, "Demographics" = 15:18, "Intercept" = 19))

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
out.uncond <- get.boot.stats(boot.test1, type = "perc")

## cf. when controlling for lagged effect of Y:
model.Y1 <- lm(update.formula(model.Y, . ~ exp.disagr.online.prcpt.W2 + .),
               data = cleaned.data)
## effect is still significant but at .90 level
summary(model.Y1)

set.seed(1234)
boot.test1.lagged <- boot(dat = cleaned.data, statistic = est.uncond.indirect,
                   R = 20000, parallel = "multicore", ncpus = 8,
                   lm.model.M = model.M1, lm.model.Y = model.Y1,
                   pred = c("safe.disc.W1", "dangerous.disc.W1"))
out.uncond.lagged <- get.boot.stats(boot.test1.lagged,
                                    type = "perc", conf = 0.90)




## estimate conditional indirect effect
## as a function of moderators
## using bcaboot packages
require(bcaboot)
est.cond.indirect

## ideological identification strength
set.seed(1234)
boot.test2 <- boot(cleaned.data, statistic = est.cond.indirect,
                   R = 20000, parallel = "multicore", ncpus = 8,
                   lm.model.M = model.M1.int1, lm.model.Y = model.Y,
                   pred = "dangerous.disc.W1",
                   modx = "ideo_str.W2")
out.cond1 <- get.boot.stats(boot.test2, type = "bca")
out.cond1 <- cbind(var = rownames(out.cond1), out.cond1) %>% setDT(.)

  ## plot conditional indirect effects
  plot.cond.ind1 <- data.frame(
    moderator = select.modval(cleaned.data, "ideo_str.W2"),
    out.cond1[var %!in% c("index.modmed", "dir.effect"),]
    ) %>% setDT(.)
  ggplot(plot.cond.ind1, aes(x = moderator, y = coef)) + theme_bw() +
    geom_line() + geom_hline(yintercept = 0, color = "red", linetype = 2) +
    geom_ribbon(aes(ymin = llci, ymax = ulci), alpha = 0.25) +
    geom_vline(xintercept = 0.52, color = "red", lty = 2) +
    xlab("Moderator: Strengths of ideology") + ylab("theta") +
    ggtitle("Conditional indirect effect of actual exposure to disagreeable discussants' messages")


## connected alters' indegree centrality --> significant with BCa...
  set.seed(1234)
  boot.test3 <- boot(cleaned.data, statistic = est.cond.indirect,
                     R = 10000, parallel = "multicore", ncpus = 8,
                     lm.model.M = model.M1.int2, lm.model.Y = model.Y,
                     pred = "dangerous.disc.W1",
                     modx = "alter.centr.indeg.W1")
  out.cond2 <- get.boot.stats(boot.test3, type = "bca")
  out.cond2 <- cbind(var = rownames(out.cond2), out.cond2) %>% setDT(.)

  ## plot conditional indirect effects
  plot.cond.ind2 <- data.frame(
    moderator = select.modval(cleaned.data, "alter.centr.indeg.W1"),
    out.cond2[var %!in% c("index.modmed", "dir.effect"),]
  ) %>% setDT(.)
  ggplot(plot.cond.ind2, aes(x = moderator, y = coef)) + theme_bw() +
    geom_line() + geom_hline(yintercept = 0, color = "red", linetype = 2) +
    geom_ribbon(aes(ymin = llci, ymax = ulci), alpha = 0.25) +
    #geom_vline(xintercept = 0.52, color = "red", lty = 2) +
    xlab("Moderator: Connected alters' indegree centrality") + ylab("theta") +
    ggtitle("Conditional indirect effect of actual exposure to disagreeable discussants' messages")


  ## connected alters' eigenvector centrality --> significant with BCa...
  set.seed(1234)
  boot.test4 <- boot(cleaned.data, statistic = est.cond.indirect,
                     R = 20000, parallel = "multicore", ncpus = 8,
                     lm.model.M = model.M1.int3, lm.model.Y = model.Y,
                     pred = "dangerous.disc.W1",
                     modx = "alter.centr.eigen.W1")
  out.cond3 <- get.boot.stats(boot.test4, type = "bca")
  out.cond3 <- cbind(var = rownames(out.cond3), out.cond3) %>% setDT(.)

  ## plot conditional indirect effects
  plot.cond.ind3 <- data.frame(
    moderator = select.modval(cleaned.data, "alter.centr.eigen.W1"),
    out.cond3[var %!in% c("index.modmed", "dir.effect"),]
  ) %>% setDT(.)
  ggplot(plot.cond.ind3, aes(x = moderator, y = coef)) + theme_bw() +
    geom_line() + geom_hline(yintercept = 0, color = "red", linetype = 2) +
    geom_ribbon(aes(ymin = llci, ymax = ulci), alpha = 0.25) +
    #geom_vline(xintercept = 0.52, color = "red", lty = 2) +
    xlab("Moderator: Connected alters' eigenvector centrality") + ylab("theta") +
    ggtitle("Conditional indirect effect of actual exposure to disagreeable discussants' messages")




















## cf. differencing methods
  # cleaned.data[, Y := exp.disagr.online.prcpt.W3 - exp.disagr.online.prcpt.W2]
  # cleaned.data[, M1 := perceived.opinion.climate.W3 - perceived.opinion.climate.W2]
  # cleaned.data[, M2 := affective.polarization.W3 - affective.polarization.W2]
  # cleaned.data[, X1 := dangerous.disc.W3 - dangerous.disc.W2]
  # cleaned.data[, X2 := safe.disc.W3 - safe.disc.W2]
  #
  # model.M1 <- lm(M1 ~
  #                  ## focal predictor
  #                  X1 + X2 +
  #                  ## discussion motivation
  #                  consistency.motivation + understanding.motivation +
  #                  ## demographic controls
  #                  age.years + female + edu + household.income +
  #                  ## political correlates
  #                  canpref.W2 + ideo_str.W2 + pol.interest.W2 + ego.netsize.W2 +
  #                  ## media exposure
  #                  internet.news.use.W2 + newspaper.use.W2 + tv.news.use.W2
  #                ,
  #                data = cleaned.data); summary(model.M1)
  #
  # model.M2 <- lm(update.formula(model.M1, M2 ~ .),
  #                data = cleaned.data); summary(model.M2)
  #
  # ## individual's ideology str significantly interact with dangerous.disc
  # model.M1.int1 <- lm(update.formula(model.M1, . ~ . + X1*understanding.motivation), data = cleaned.data)
  # summary(model.M1.int1)
  # jtools::interact_plot(model.M1.int1, pred = "X1", modx = "understanding.motivation")
  #
  # model.Y <- lm(Y ~
  #                 ## focal predictor
  #                 X1 + X2 +
  #                 ## discussion motivation
  #                 consistency.motivation + understanding.motivation +
  #                 ## demographic controls
  #                 age.years + female + edu + household.income +
  #                 ## political correlates
  #                 canpref.W2 + ideo_str.W2 + pol.interest.W2 + ego.netsize.W2 +
  #                 ## media exposure
  #                 internet.news.use.W2 + newspaper.use.W2 + tv.news.use.W2 +
  #                 ## mediator
  #                 M1 + M2
  #               ,
  #               data = cleaned.data)
  # model.Y %>% summary(.)

