
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


# make the rounded percentage and rerun the correlation does not change the results!
cleaned.data[, dangerous.disc.W2.prop := (dangerous.disc.W2 %>% round(., digits = 2))*100]
bivariate.perm.test(cleaned.data, "dangerous.disc.W2.prop", "exp.disagr.offline.prcpt.W2")
cleaned.data[, dangerous.disc.W2.sum.prop := (dangerous.disc.W2.sum %>%
                                                round(., digits = 2))*100]
bivariate.perm.test(cleaned.data, "dangerous.disc.W2.sum.prop", "exp.disagr.offline.prcpt.W2")

cleaned.data[, cor(dangerous.disc.W2.prop, exp.disagr.online.prcpt.W2)]
bivariate.perm.test(cleaned.data, "dangerous.disc.W2.prop", "exp.disagr.online.prcpt.W2")
# obs.minus.perm llci.0.025 ulci.0.975
#      0.2851594  0.1758708  0.3888837
cleaned.data[, cor(dangerous.disc.W2.sum.prop, exp.disagr.online.prcpt.W2)]
bivariate.perm.test(cleaned.data, "dangerous.disc.W2.sum.prop", "exp.disagr.online.prcpt.W2")
# obs.minus.perm     llci.0.025     ulci.0.975
#      0.2582314      0.1599817      0.3656424

## create differences between perception and behavioral measures
## (+) values indicate the overestimation, and (-) means underestimation
cleaned.data[, diff.exp.disagree.W2 := exp.disagr.online.prcpt.W2 - dangerous.disc.W2.prop]
cleaned.data[, summary(diff.exp.disagree.W2)]
cleaned.data[, diff.exp.disagree.W2.sum :=
               exp.disagr.online.prcpt.W2 - dangerous.disc.W2.sum.prop]
cleaned.data[, summary(diff.exp.disagree.W2.sum)]


## permutation test indicates that the difference between
## perception and objective behavior is significantly differ,
## in a way that people tend to overestimate the exposure to differences
diff.perm.test(cleaned.data, "exp.disagr.online.prcpt.W2", "dangerous.disc.W2.prop", rep = 20000)
diff.perm.test(cleaned.data, "exp.disagr.online.prcpt.W2", "dangerous.disc.W2.sum.prop", rep = 20000)



cleaned.data[, cor(dangerous.disc.W3, exp.disagr.online.prcpt.W3)]
bivariate.perm.test(cleaned.data, "dangerous.disc.W3", "exp.disagr.online.prcpt.W3")
# obs.minus.perm     llci.0.025     ulci.0.975
#      0.2958441      0.1926712      0.4104359
cleaned.data[, cor(dangerous.disc.W3.sum, exp.disagr.online.prcpt.W3)]
bivariate.perm.test(cleaned.data, "dangerous.disc.W3.sum", "exp.disagr.online.prcpt.W3")
# obs.minus.perm     llci.0.025     ulci.0.975
#      0.3624799      0.2601810      0.4709605

## create differences between perception and behavioral measures
## (+) values indicate the overestimation, and (-) means underestimation
cleaned.data[, dangerous.disc.W3.prop := (dangerous.disc.W3 %>% round(., digits = 2))*100]
cleaned.data[, dangerous.disc.W3.sum.prop := (dangerous.disc.W3.sum %>% round(., digits = 2))*100]
cleaned.data[, diff.exp.disagree.W3 := exp.disagr.online.prcpt.W3 - dangerous.disc.W3.prop]
cleaned.data[, summary(diff.exp.disagree.W3)]
cleaned.data[, diff.exp.disagree.W3.sum :=
               exp.disagr.online.prcpt.W3 - dangerous.disc.W3.sum.prop]
cleaned.data[, summary(diff.exp.disagree.W3.sum)]

## permutation test indicates that the difference between
## perception and objective behavior is significantly differ,
## in a way that people tend to overestimate the exposure to differences
diff.perm.test(cleaned.data, "exp.disagr.online.prcpt.W3", "dangerous.disc.W3.prop", rep = 20000)
diff.perm.test(cleaned.data, "exp.disagr.online.prcpt.W3", "dangerous.disc.W3.sum.prop", rep = 20000)

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

cleaned.data[, log.raw_sum.W1.wavesum := log(raw_sum.W1.wavesum + 1)][,
               log.raw_sum.W1.wavesum := log.raw_sum.W1.wavesum - mean(log.raw_sum.W1.wavesum)]
cleaned.data[, perceived.opinion.climate.W2 :=
               scales::rescale(perceived.opinion.climate.W2, to = c(0, 100))]
cleaned.data[, perceived.opinion.climate.W3 :=
               scales::rescale(perceived.opinion.climate.W3, to = c(0, 100))]

## mean-center all predictors
cleaned.data[, safe.disc.W1.c := safe.disc.W1 - mean(safe.disc.W1)]
cleaned.data[, dangerous.disc.W1.c := dangerous.disc.W1 - mean(dangerous.disc.W1)]
cleaned.data[, discussion.norm.W2.c := discussion.norm.W2 - mean(discussion.norm.W2)]
cleaned.data[, age.years := age.years - mean(age.years)]
cleaned.data[, edu := edu - median(edu)]
cleaned.data[, household.income := household.income - median(household.income)]
cleaned.data[, ideo.W2.c := ideo.W2 - 4] ## 4 == "middle of the road"
cleaned.data[, pol.interest.W2.c := pol.interest.W2 - mean(pol.interest.W2)]
cleaned.data[, media.exposure.W2.c := media.exposure.W2 - mean(media.exposure.W2)]
cleaned.data[, ego.netsize.W2.c := ego.netsize.W2 - mean(ego.netsize.W2)]
cleaned.data[, perceived.opinion.climate.W2.c :=
               perceived.opinion.climate.W2 - mean(perceived.opinion.climate.W2)]
cleaned.data[, affective.polarization.W2.c :=
               affective.polarization.W2 - mean(affective.polarization.W2)]


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
                 ideo.W2.c + pol.interest.W2.c + ego.netsize.W2.c +
                 ## media exposure
                 media.exposure.W2.c
              ,
              data = cleaned.data); summary(model.M1)

## model predicting online exposure to disagreement, perception
model.Y1 <- lm(update.formula(model.M1,
                              exp.disagr.online.prcpt.W3 ~ .),
               data = cleaned.data); summary(model.Y1)
## model predicting online exposure to disagreement, perception
model.Y1.lagged <- lm(update.formula(model.Y1,
                              . ~ . + exp.disagr.online.prcpt.W2),
               data = cleaned.data); summary(model.Y1.lagged)




## model predicting social-identity based affective process: no effects
model.M2 <- lm(update.formula(model.M1, affective.polarization.W2 ~ .),
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

## no effect on affective polarization
model.M2.int1 <- lm(
  update.formula(model.M2, . ~ . + dangerous.disc.W1.c*ideo.W2.c),
  data = cleaned.data); summary(model.M2.int1)
model.M2.int2 <- lm(
  update.formula(model.M2, . ~ . + safe.disc.W1.c*ideo.W2.c),
  data = cleaned.data); summary(model.M2.int2)


## predicting self-reported exposure to disagreement
model.Y2 <- lm(update.formula(model.M1,
               exp.disagr.online.prcpt.W3 ~ .
               + perceived.opinion.climate.W2.c + affective.polarization.W2.c),
              data = cleaned.data); summary(model.Y2)

## Basic mediation models
screenreg(list(model.M1, model.M2, model.Y2),
          single.row = T, digits = 3, leading.zero = FALSE,
          custom.model.names = c("M1: Prcvd Op Climate", "M2: Affective pol", "Y: Self-reptd Dis"),
          custom.coef.names = c("(Intercept)",
                                "In-prty msg Exp W1", "Out-prty msg Exp W1",
                                "Total no. of exp (log)", "Discussion norm W2",
                                "Age (in years)", "Female", "Education", "HH income",
                                "Ideology W2", "Interest W2",
                                "Net size W2", "Media Exposure W2", "Prcvd Op Climate", "Affective pol"),
          reorder.coef = c(2:3, 14:15, 4:5, 10:13, 6:9, 1),
          groups = list("Focal predictors" = 1:2, "Mediators" = 3:4,
                        "Controls" = 5:10, "Demographics" = 11:14, "Intercept" = 15))

## First stage moderation models
screenreg(list(model.M1, model.M1.int1, model.M1.int2),
          single.row = T, digits = 3, leading.zero = FALSE,
          custom.model.names = c("Prcvd Op Climate", "Interaction I", "Interaction II"),
          custom.coef.names = c("(Intercept)",
                                "In-prty msg Exp W1", "Out-prty msg Exp W1",
                                "Total no. of exp (log)", "Discussion norm W2",
                                "Age (in years)", "Female", "Education", "HH income",
                                "Ideology W2", "Interest W2",
                                "Net size W2", "Media Exposure W2",
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
## using bcaboot packages
require(bcaboot)
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

