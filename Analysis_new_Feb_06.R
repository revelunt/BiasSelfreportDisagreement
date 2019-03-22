
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
require(jtools)
require(interactions)
require(pscl)

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

## sample demographics
dat[, sapply(.SD, descriptives),
      .SDcols = c("age", "sex", "edu", "income")]

## "sex" is coded as 1 vs. 2, but the paper reports 0 vs. 1 scale

#                  age       sex      edu   income
# M          35.718475 1.4809384 7.683284 4.991202
# SD          9.858853 0.5003707 1.040072 1.882562
# range.low  16.000000 1.0000000 2.000000 1.000000
# range.high 59.000000 2.0000000 9.000000 8.000000

## candidate support W1 and W2
## 0 = Park, 1 = Moon, 2 = else
cleaned.data[, table(canpref.W1)/.N]
cleaned.data[, table(canpref.W2)/.N]


## cumulutive proportion benchmark
cleaned.data[, sapply(.SD, descriptives),
             .SDcols = c("dangerous.disc.W1",
                         "dangerous.disc.W2")]

## average of daily proportions
cleaned.data[, sapply(.SD, descriptives),
             .SDcols = c("dangerous.disc.dlyavg.W1",
                         "dangerous.disc.dlyavg.W2")]

## perceived exposure to disagreement
cleaned.data[, sapply(.SD, descriptives),
             .SDcols = c("dangerous.disc.prcptn.W2",
                         "dangerous.disc.prcptn.W3")]

## social desirability
cleaned.data[, sapply(.SD, descriptives),
             .SDcols = c("discussion.norm.W2",
                         "discussion.norm.W3",
                         "need.for.approval.W2",
                         "need.for.approval.W3")]

## motivation and ability (interest and knowledge)
cleaned.data[, sapply(.SD, descriptives),
             .SDcols = c("pol.interest.W2",
                         "pol.interest.W3",
                         "pol.know")]

## opinion climates
cleaned.data[, sapply(.SD, descriptives),
             .SDcols = c("perceived.opinion.climate.W2",
                         "perceived.opinion.climate.W3")]

## other variables in the model, for regression/mediation model
cleaned.data[, sapply(.SD, descriptives),
             .SDcols = c("ideo_str.W2", "ideo_str.W3",
                         "canpref.W2", "canpref.W3",
                         "media.exposure.W2",
                         "log.total.exp.W1", "log.total.exp.W2")]


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
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1),
               lty = 2, color = "grey") +
  xlab("W1 log data (Cumulative exposure in prop.)") +
  ylab("W2 Perception \n(Prop. of Exposure to disagreement)") +
  ggtitle("W1 Exposure vs. W2 Perception")

qq3 <- ggplot(qq.out3, aes(x = x, y = y)) +
  geom_jitter(width = 0.02, color = "grey") + theme_bw() +
  stat_smooth(aes(group = 1), color = "red", se = FALSE) +
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1),
               lty = 2, color = "grey") +
  xlab("W2 log data (Cumulative exposure in prop.)") +
  ylab("W3 Perception \n(Prop. of Exposure to disagreement)") +
  ggtitle("W2 Exposure vs. W3 Perception")

## cf. mean of daily average (dangerous.disc.dlyavg.W1)
qq.avg.out2 <- with(cleaned.data,
                qqplot(x = dangerous.disc.dlyavg.W1,
                       y = dangerous.disc.prcptn.W2,
                       plot.it = FALSE)) %>% as.data.frame(.) %>% setDT(.)

qq.avg.out3 <- with(cleaned.data,
                qqplot(x = dangerous.disc.dlyavg.W2,
                       y = dangerous.disc.prcptn.W3,
                       plot.it = FALSE)) %>% as.data.frame(.) %>% setDT(.)

qq2.avg <- ggplot(qq.avg.out2, aes(x = x, y = y)) +
  geom_jitter(width = 0.02, color = "grey") + theme_bw() +
  stat_smooth(aes(group = 1), color = "red", se = FALSE) +
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1),
               lty = 2, color = "grey") +
  xlab("W1 log data (Mean of daily proportion)") +
  ylab("W2 Perception \n(Prop. of Exposure to disagreement)") +
  ggtitle("W1 Exposure vs. W2 Perception")

qq3.avg <- ggplot(qq.avg.out3, aes(x = x, y = y)) +
  geom_jitter(width = 0.02, color = "grey") + theme_bw() +
  stat_smooth(aes(group = 1), color = "red", se = FALSE) +
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1),
               lty = 2, color = "grey") +
  xlab("W2 log data (Mean of daily proportion)") +
  ylab("W3 Perception \n(Prop. of Exposure to disagreement)") +
  ggtitle("W2 Exposure vs. W3 Perception")


## check the correlation of measures
cleaned.data[, cor.test(dangerous.disc.W1, dangerous.disc.prcptn.W2)]
cleaned.data[, cor.test(dangerous.disc.W2, dangerous.disc.prcptn.W3)]
bivariate.perm.test(cleaned.data,
                    "dangerous.disc.W1", "dangerous.disc.prcptn.W2")
bivariate.perm.test(cleaned.data,
                    "dangerous.disc.W2", "dangerous.disc.prcptn.W3")

cleaned.data[, cor.test(dangerous.disc.dlyavg.W1, dangerous.disc.prcptn.W2)]
cleaned.data[, cor.test(dangerous.disc.dlyavg.W2, dangerous.disc.prcptn.W3)]
bivariate.perm.test(cleaned.data,
                    "dangerous.disc.dlyavg.W1", "dangerous.disc.prcptn.W2")
bivariate.perm.test(cleaned.data,
                    "dangerous.disc.dlyavg.W2", "dangerous.disc.prcptn.W3")

## permutation test indicates that the difference between
## perception and objective behavior is significantly differ,
## in a way that people tend to overestimate the exposure to differences
diff.perm.test(cleaned.data, rep = 20000,
               "dangerous.disc.prcptn.W2", "dangerous.disc.W1")
diff.perm.test(cleaned.data, rep = 20000,
               "dangerous.disc.prcptn.W3", "dangerous.disc.W2")
diff.perm.test(cleaned.data, rep = 20000,
               "dangerous.disc.prcptn.W2", "dangerous.disc.dlyavg.W1")
diff.perm.test(cleaned.data, rep = 20000,
               "dangerous.disc.prcptn.W3", "dangerous.disc.dlyavg.W2")

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
#
# require(lavaan)
#
# model1.luc.fm <- '
#
# ## baseline and descrepancy factor
#     f_base =~ 1*dangerous.disc.W1 + 1*dangerous.disc.prcptn.W2
#     f_diff =~ 1*dangerous.disc.prcptn.W2
#
# ## mean and variance of subjective/objective measures, all fixed to 0
#    dangerous.disc.prcptn.W2 ~ 1*0
#    dangerous.disc.prcptn.W2 ~~ 0*dangerous.disc.prcptn.W2
#    dangerous.disc.W1 ~ 1*0
#    dangerous.disc.W1 ~~ 0*dangerous.disc.W1
# ## conditional intercept and variance of latent score
#    f_diff ~ 1
#    f_diff ~~ f_diff
#    f_base ~ 1
#    f_base ~~ f_base
# ## covariance of latent scores
#    f_base ~~ f_diff
#
# ## regress objective exposure on Xs
#    ## baseline total exposure
#       f_base ~ log.total.exp.W1
#    ## demographics
#       f_base ~ age.years + female + edu + household.income
#    ## partisanships
#       f_base ~ canpref.W2 + ideo_str.W2
#    ## motivation and ability factor
#       f_base ~  pol.interest.W2 + pol.know + media.exposure.W2
#
# ## regress descprepancy on Xs
#       f_diff ~ discussion.norm.W2 + need.for.approval.W2 +
#                perceived.opinion.climate.W2 +
#                log.total.exp.W1 +
#                age.years + female + edu + household.income +
#                canpref.W2 + pol.interest.W2 + pol.know + ideo_str.W2 +
#                media.exposure.W2
# '
#
# model1.luc <- sem(model1.luc.fm, data = cleaned.data)
# summary(model1.luc, fit.measures = TRUE)
# # modificationindices(model1.luc, sort. = T)

# model2.luc.fm <- '
#
# ## baseline and descrepancy factor
#     f_base =~ 1*dangerous.disc.W2 + 1*dangerous.disc.prcptn.W3
#     f_diff =~ 1*dangerous.disc.prcptn.W3
#
# ## mean and variance of subjective/objective measures, all fixed to 0
#    dangerous.disc.prcptn.W3 ~ 1*0
#    dangerous.disc.prcptn.W3 ~~ 0*dangerous.disc.prcptn.W3
#    dangerous.disc.W2 ~ 1*0
#    dangerous.disc.W2 ~~ 0*dangerous.disc.W2
# ## conditional intercept and variance of latent score
#    f_diff ~ 1
#    f_diff ~~ f_diff
#    f_base ~ 1
#    f_base ~~ f_base
# ## covariance of latent scores
#    f_base ~~ f_diff
#
# ## regress objective exposure on Xs
#    ## baseline total exposure
#       f_base ~ log.total.exp.W2
#    ## demographics
#       f_base ~ age.years + female + edu + household.income
#    ## partisanships
#       f_base ~ canpref.W3 + ideo_str.W3
#    ## motivation and ability factor
#       f_base ~  pol.interest.W2 + pol.know + media.exposure.W2
#
# ## regress descprepancy on Xs
#       f_diff ~ discussion.norm.W3 + need.for.approval.W3 +
#                perceived.opinion.climate.W3 +
#                log.total.exp.W2 +
#                age.years + female + edu + household.income +
#                canpref.W3 + pol.interest.W3 + pol.know + ideo_str.W3 +
#                media.exposure.W2
# '
#
#
# model2.luc <- sem(model2.luc.fm, data = cleaned.data, fixed.x = FALSE)
# summary(model2.luc, fit.measures = TRUE) #standardized = TRUE)

cleaned.data[, descriptives(dis.accuracy.W2)]
cleaned.data[, descriptives(dis.accuracy.W3)]

model1 <- lm(dis.accuracy.W2 ~ ## predicting overestimation
               ## social desirability
               discussion.norm.W2 +
               need.for.approval.W2 + ## cf. interaction is not sig as well
               ## partisan motivations
               perceived.opinion.climate.W2 +
               canpref.W2 + ideo_str.W2 +
               ## other controls
               log.total.exp.W1 + pol.interest.W2 + pol.know +
                 ## demographic controls
                 age.years + female + edu + household.income +
                 ## media exposure
                 media.exposure.W2
               ,
               data = cleaned.data)

model1.cat <- glm(update.formula(model1, dis.accuracy.cat.W2 ~ .),
                  binomial("logit"),
                       data = cleaned.data)

model2 <- lm(dis.accuracy.W3 ~
              ## focal predictor
              discussion.norm.W3 +
              need.for.approval.W3 + ## cf. interaction is not sig as well
              perceived.opinion.climate.W3 +
              canpref.W3 + ideo_str.W3 +
              ## other controls
              log.total.exp.W2 + pol.interest.W3 + pol.know +
              ## demographic controls
              age.years + female + edu + household.income +
              ## media exposure
              media.exposure.W2,
             data = cleaned.data)

model2.cat <- glm(update.formula(model2, dis.accuracy.cat.W3 ~ .),
                  binomial("logit"),
                       data = cleaned.data)


## predicting perception of exposure
## controlling actual exposure

model3.ols <- lm(dangerous.disc.prcptn.W2 ~
                   dangerous.disc.W1 +
                   discussion.norm.W2 +
                   need.for.approval.W2 + ## cf. interaction is not sig as well
                   ## partisan motivations
                   perceived.opinion.climate.W2 +
                   canpref.W2 + ideo_str.W2 +
                   ## other controls
                   log.total.exp.W1 + pol.interest.W2 + pol.know +
                   ## demographic controls
                   age.years + female + edu + household.income +
                   ## media exposure
                   media.exposure.W2, data = cleaned.data)


model4.ols <- lm(dangerous.disc.prcptn.W3 ~
                   dangerous.disc.W2 +
                   discussion.norm.W3 +
                   need.for.approval.W3 + ## cf. interaction is not sig as well
                   perceived.opinion.climate.W3 +
                   canpref.W3 + ideo_str.W3 +
                   ## other controls
                   log.total.exp.W2 + pol.interest.W3 + pol.know +
                   ## demographic controls
                   age.years + female + edu + household.income +
                   ## media exposure
                   media.exposure.W2,
                 data = cleaned.data)

# model1.luc.out <- model1.luc
# model1.luc <- extract.lm(model1)
# model1.luc@coef.names <- model1.luc@coef.names[-1]
#
# test <- parameterEstimates(model1.luc.out) %>% setDT(.)
# model1.luc@coef <- test[lhs == 'f_diff' & op == '~', est]
# model1.luc@se <- test[lhs == 'f_diff' & op == '~', se]
# model1.luc@pvalues <- test[lhs == 'f_diff' & op == '~', pvalue]
# model1.luc@gof.names <- c("CFI", "TLI", "RMSEA", "P(RMSEA<.05)", "SRMR")
# model1.luc@gof.decimal <- c(TRUE, TRUE, TRUE, TRUE, TRUE)
# model1.luc@gof <- fitMeasures(model1.luc.out)[
#                     c("cfi", "tli", "rmsea", "rmsea.pvalue", "srmr")]
#
# model2.luc.out <- model2.luc
# model2.luc <- extract.lm(model2)
# model2.luc@coef.names <- model2.luc@coef.names[-1]
#
# test <- parameterEstimates(model2.luc.out) %>% setDT(.)
# model2.luc@coef <- test[lhs == 'f_diff' & op == '~', est]
# model2.luc@se <- test[lhs == 'f_diff' & op == '~', se]
# model2.luc@pvalues <- test[lhs == 'f_diff' & op == '~', pvalue]
# model2.luc@gof.names <- c("CFI", "TLI", "RMSEA", "P(RMSEA<.05)", "SRMR")
# model2.luc@gof.decimal <- c(TRUE, TRUE, TRUE, TRUE, TRUE)
# model2.luc@gof <- fitMeasures(model2.luc.out)[
#   c("cfi", "tli", "rmsea", "rmsea.pvalue", "srmr")]


require(boot)
boot.model1 <- boot(cleaned.data, statistic = boot.lm,
                    R = 10000, parallel = "multicore", ncpus = 8,
                    lm.fit = model1) %>% get.boot.stats(., type = "perc")

boot.model1.cat <- boot(cleaned.data, statistic = boot.glm,
                        R = 10000, parallel = "multicore", ncpus = 8,
                        glm.fit = model1.cat) %>% get.boot.stats(., type = "perc")

boot.model2 <- boot(cleaned.data, statistic = boot.lm,
                    R = 10000, parallel = "multicore", ncpus = 8,
                    lm.fit = model2) %>% get.boot.stats(., type = "perc")

boot.model2.cat <- boot(cleaned.data, statistic = boot.glm,
                        R = 10000, parallel = "multicore", ncpus = 8,
                        glm.fit = model2.cat) %>% get.boot.stats(., type = "perc")

boot.model3 <- boot(cleaned.data, statistic = boot.lm,
                        R = 10000, parallel = "multicore", ncpus = 8,
                    lm.fit = model3.ols) %>% get.boot.stats(., type = "perc")

boot.model4 <- boot(cleaned.data, statistic = boot.lm,
                    R = 10000, parallel = "multicore", ncpus = 8,
                    lm.fit = model4.ols) %>% get.boot.stats(., type = "perc")

require(texreg)

## Table 2 in the main ms and A1 in the appendix
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

## Table A2 in the appendix, predicting perceived exposure to disagreement
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





# plotreg(list(model1, model2),
#         custom.model.names = c("Inaccuracy, W1 log vs. W2 perception",
#                                "Inaccuracy, W2 log vs. W3 perception"),
#         custom.coef.names = list(
#           c("(Intercept)", "Discussion norm W2",
#             "Need for approval W2", "Prcvd Op Climate W2",
#             "Candidate pref W2", "Ideo Strength W2",
#             "Total Exp W1 (log)", "Interest W2", "Knowledge",
#             "Age (in years)", "Female", "Education", "HH income",
#             "Media Exposure"),
#           ## Wave 2 model
#           c("(Intercept)", "Discussion norm W3", "Need for approval W3",
#             "Prcvd Op Climate W3", "Candidate pref W3", "Ideo Strength W3",
#             "Total Exp W2 (log)", "Interest W3", "Knowledge",
#             "Age (in years)", "Female", "Education", "HH income",
#             "Media Exposure")),
#         omit.coef = c("(Intercept)|(Candidate)"),
#         mfrow = T, lwd.inner = 5, custom.note = "Note: Bars denote 95% CIs",
#         lwd.zerobar = 2,
#         insignif.dark = "gray",
#         insignif.light = "gray80",
#         insignif.medium = "gray")

## standardized effects using jtools::plot_summs
require(jtools)
p_coef <- plot_summs(model1, model2, scale = TRUE, n.sd = 2,
           colors = "Qual2",
           model.names = c("W1 log-data vs. W2 perception",
                           "W2 log-data vs. W3 perception"),
           legend.title = "DV: Inaccuracy",
           coefs = c("Discussion norm" = "discussion.norm.W2",
                     "Discussion norm" = "discussion.norm.W3",
                     "Need for approval" = "need.for.approval.W2",
                     "Need for approval" = "need.for.approval.W3",
                     "Prcvd Op Climate" = "perceived.opinion.climate.W2",
                     "Prcvd Op Climate" = "perceived.opinion.climate.W3",
                     "Political Interest" = "pol.interest.W2",
                     "Political Interest" = "pol.interest.W3",
                     "Political Knowledge" = "pol.know"))

p_coef + xlab("Standardized effects (2SD difference)") + ylab("") + theme_bw() +
  theme(legend.position = "bottom")



## cf. Pseudo-Rsq
require(rcompanion)
nagelkerke(model1.cat)$Pseudo.R.squared.for.model.vs.null[3] # Nagelkerke R-sq
nagelkerke(model2.cat)$Pseudo.R.squared.for.model.vs.null[3]

# model1.agr <- lm(agr.accuracy.W2 ~
#                ## focal predictor
#                discussion.norm.W2 +
#                perceived.opinion.climate.W2 +
#                log.total.exp.W1 +
#                ## demographic controls
#                age.years + female + edu + household.income +
#                ## political correlates
#                canpref.W2 + pol.interest.W2 + pol.know + ideo_str.W2 +
#                ## media exposure
#                media.exposure.W2
#              ,
#              data = cleaned.data)
#
# model1.agr.cat <- glm(update.formula(model1.agr, agr.accuracy.cat.W2 ~ .),
#                   binomial("logit"),
#                   data = cleaned.data)
#
# model2.agr <- lm(agr.accuracy.W3 ~
#                ## focal predictor
#                discussion.norm.W3 +
#                perceived.opinion.climate.W3 +
#                log.total.exp.W2 +
#                ## demographic controls
#                age.years + female + edu + household.income +
#                ## political correlates
#                canpref.W3 + pol.interest.W2 +
#                pol.know + ideo_str.W3 +
#                ## media exposure
#                media.exposure.W2,
#              data = cleaned.data)
#
# model2.agr.cat <- glm(update.formula(model2.agr, agr.accuracy.cat.W3 ~ .),
#                   binomial("logit"),
#                   data = cleaned.data)
#
# screenreg(list(model1.agr, model1.agr.cat,
#                model2.agr, model2.agr.cat),
#           stars = c(0.001, 0.01, 0.05, 0.10), digits = 3,
#           custom.model.names = c("AGR OLS W2", "AGR GLM W2",
#                                  "AGR OLS W3", "AGR GLM W3"))


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
  ggtitle("W1 Recent three-day Exposure vs. W2 Perception")

qq3r <- ggplot(qq.out3.r, aes(x = x, y = y)) +
  geom_jitter(width = 0.02, color = "grey") + theme_bw() +
  stat_smooth(aes(group = 1), color = "red", se = FALSE) +
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1),
               lty = 2, color = "grey") +
  xlab("W2 log data \n(Cumulative proportion, most recent three days)") +
  ylab("W3 Perception \n(Prop. of Exposure to diagreement)") +
  ggtitle("W2 Recent three-day Exposure vs. W3 Perception")

## Figure 1
qq2 + qq3 + qq2.avg + qq3.avg + qq2r + qq3r + plot_layout(nrow = 3, ncol = 2)


## test of equality of two correlation coefficients
## a comparison of two overlapping correlations based on dependent groups
require(cocor)

## Wave 2 measures
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

boot.model1r <- boot(cleaned.data, statistic = boot.lm,
                    R = 10000, parallel = "multicore", ncpus = 8,
                    lm.fit = model1r) %>% get.boot.stats(., type = "perc")

boot.model2r <- boot(cleaned.data, statistic = boot.lm,
                    R = 10000, parallel = "multicore", ncpus = 8,
                    lm.fit = model2r) %>% get.boot.stats(., type = "perc")


require(texreg)
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



## ---------------------------------------- ##
## Predicting perceived opinion climates    ##
## (Moderated by one's political knowledge) ##
## ---------------------------------------- ##
#
# ## model predicting perceived opinion climate
# model.poc.1 <- lm(perceived.opinion.climate.W2 ~
#                 ## focal predictor
#                  dangerous.disc.W1*pol.know +
#                  log.total.exp.W1 +
#                  ## demographic controls
#                  age.years + female + edu + household.income +
#                  ## political correlates
#                  canpref.W2 + pol.interest.W2 + pol.know + ideo_str.W2 +
#                  ## media exposure
#                  media.exposure.W2
#               ,
#               data = cleaned.data); # summary(model.poc.1)
#
# require(boot)
# require(car)
# require(interactions)
#
# model.poc.1.boot <- car::Boot(model.poc.1, f = coef, method = "case",
#                               R = 10000, ncores = 8)
# model.poc.1.boot <- get.boot.stats(model.poc.1.boot, type = "bca")
#
# p5 <- interactions::interact_plot(model = model.poc.1,
#                       pred = "dangerous.disc.W1",
#                       modx = "pol.know")
# ## in W2, ideo str also moderates the exp to disagree,
# ## yet this is not replicated in W3
#
# model.poc.2 <- lm(perceived.opinion.climate.W3 ~
#                     ## focal predictor
#                     dangerous.disc.W2*pol.know +
#                     log.total.exp.W2 +
#                     ## demographic controls
#                     age.years + female + edu + household.income +
#                     ## political correlates
#                     canpref.W3 + pol.interest.W2 + pol.know + ideo_str.W3 +
#                     ## media exposure
#                     media.exposure.W2
#                   ,
#                   data = cleaned.data); summary(model.poc.2)
#
# model.poc.2.boot <- car::Boot(model.poc.2, f = coef, method = "case",
#                               R = 10000, ncores = 8)
# model.poc.2.boot <- get.boot.stats(model.poc.2.boot, type = "bca")
#
# p6 <- interactions::interact_plot(model = model.poc.2,
#                       pred = "dangerous.disc.W2",
#                       modx = "pol.know")
#
# p5 + p6 + plot_layout(nrow = 1)
#
# ## since DV distribution is not likely to be normal,
# ## we use bootstrapping (bca interval) instead of regular OLS
#
# # DV = Perceived Opinion Climate
# screenreg(list(model.poc.1, model.poc.2), single.row = T,
#           custom.model.names = c("POC OLS W2", "POC OLS W3"),
#           custom.coef.names = c(
#             "(Intercept)", "Out-party Exp W1/W2", "Knowledge",
#             "Total Exp W1/W2 (log)", "Age (in years)", "Female", "Education",
#             "HH income", "Candidate pref W2/W3", "Interest",
#             "Ideo Strength W2/W3", "Media Exposure", "Out Exp X Kn",
#             "Out-party Exp W1/W2", "Total Exp W1/W2 (log)",
#             "Candidate pref W2/W3", "Ideo Strength W2/W3", "Out Exp X Kn"),
#           override.ci.low = list(model.poc.1.boot[, 'llci'],
#                                  model.poc.2.boot[, 'llci']),
#           override.ci.up = list(model.poc.1.boot[, 'ulci'],
#                                 model.poc.2.boot[, 'ulci']),
#           reorder.coef = c(2:3,13, 4,9:12, 5:8, 1),
#           groups = list("Focal predictors" = 1:3,  "Controls" = 4:7,
#                         "Demographics" = 8:11))

## -------------------------------------------------------- ##
## Nonparametric bootstrap-based indirect effect inferences ##
## -------------------------------------------------------- ##

## Conditional indirect effect
## first, we need a new model predicting Y as not diff score, but
## perception measure alone, controlling for disagreement

# model.perc.1 <- lm(dangerous.disc.prcptn.W3 ~
#                      ## focal X
#                      dangerous.disc.W1 +
#                      log.total.exp.W1 +
#                      # Mediator
#                      perceived.opinion.climate.W2 +
#                      ## demographic controls
#                      age.years + female + edu + household.income +
#                      ## political correlates
#                      canpref.W2 + pol.interest.W2 + pol.know + ideo_str.W2 +
#                      ## media exposure
#                      media.exposure.W2
#                      , data = cleaned.data)
#
# ## estimation, with multicore processing
# require(boot)
# set.seed(1234)
# boot.test1 <- boot(cleaned.data, statistic = est.cond.indirect,
#                    R = 20000, parallel = "multicore", ncpus = 8,
#                    lm.model.M = model.poc.1, lm.model.Y = model.perc.1,
#                    pred = "dangerous.disc.W1",
#                    modx = "pol.know",
#                    med = "perceived.opinion.climate.W2")
# out.cond <- get.boot.stats(boot.test1, type = "perc")
# out.cond <- cbind(var = c("index.modmed",
#                           paste0("ind.", 1:1001),
#                           "dir.effect"), out.cond) %>% setDT(.)
#
# ## index of moderated mediation is sig.
# ## boot.ci(boot.out = boot.test1, type = "bca", index = 1)
# ## index = 1.16985 [0.099, 2.851]
#
#   ## plot conditional indirect effects
#   plot.cond.ind1 <- data.frame(
#     moderator = select.modval(cleaned.data, "pol.know"),
#     out.cond[var %!in% c("index.modmed", "dir.effect"),]
#     ) %>% setDT(.)
#   ggplot(plot.cond.ind1, aes(x = moderator, y = coef)) + theme_bw() +
#     geom_line() + geom_hline(yintercept = 0, color = "red", linetype = 2) +
#     geom_ribbon(aes(ymin = llci, ymax = ulci), alpha = 0.25) +
#     geom_vline(xintercept = 6.18, color = "red", lty = 2) +
#     xlab("\nModerator: Political Knowledge") + ylab("theta") + theme_bw()
#


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

## whether significance of sbj measure agree with obj measures?
p3_1 <- sim.results[, .(agree = mean(sbj.coef.agree.with.obj),
                llci = prop.cis(mean(sbj.coef.agree.with.obj), 1000)[1],
                ulci = prop.cis(mean(sbj.coef.agree.with.obj), 1000)[2]),
            by = c("target.corr", "sample_n")] %>%
  ggplot(., aes(x = target.corr, y = agree, color = factor(sample_n))) +
  geom_smooth(aes(ymin = llci, ymax = ulci), alpha = 0.3) +
  xlab("Zero-order correlation between subjective and objective measure") +
  ylab("Proportion of two results agree") + theme_bw() +
  scale_x_continuous(breaks = seq(0, 0.95, 0.1)) +
  theme(legend.position = "none") +
  geom_vline(xintercept = cor.obs[2,14], linetype = 2, col = "red")

## absolute bias of sbj measure agree against obj measures
p3_2 <- sim.results[, .(bias = median(bias),
                llci = quantile(bias, 0.025),
                ulci = quantile(bias, 0.975)),
            by = c("target.corr", "sample_n")] %>%
  ggplot(., aes(x = target.corr, y = bias, group = factor(sample_n))) +
  geom_ribbon(aes(ymin = llci, ymax = ulci), fill = "grey70", alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE, aes(color = factor(sample_n))) +
  xlab("") + ylab("Mean bias of subjective measure") + theme_bw() +
  geom_hline(yintercept = bias.obs, linetype = 2, col = "red") +
  geom_vline(xintercept = cor.obs[2,14], linetype = 2, col = "grey") +
  facet_grid(~ sample_n) + theme(legend.position = "none")

## relative size of subjective measure
p3_3 <- sim.results[, .(median.relative.size = median(relative.size.sbj),
                llci = quantile(relative.size.sbj, 0.025),
                ulci = quantile(relative.size.sbj, 0.975)),
            by = c("target.corr", "sample_n")] %>%
  ggplot(., aes(x = target.corr, y = median.relative.size)) +
  geom_ribbon(aes(ymin = llci, ymax = ulci), fill = "grey70", alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE, aes(color = factor(sample_n))) +
  xlab("Zero-order correlation between subjective and objective measure") +
  ylab("Relative size of subjective vs. objective measure") + theme_bw() +
  geom_hline(yintercept = relative.size.sbj.obs, linetype = 2, col = "red") +
  scale_colour_discrete(name = "Sample N") +
  theme(legend.position = "bottom") + facet_grid(~ sample_n)

p3_1 + p3_2 + p3_3 + plot_layout(nrow = 3)
