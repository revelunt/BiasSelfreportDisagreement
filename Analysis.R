
## prepare data for analysis
## automatically setting working directories
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
if (!("cleaned.data" %in% ls())) source("dev/PreProcess.R")

require(texreg)
require(ggplot2)
require(jtools)

## we use 'cleaned.data' and 'long.data' for the rest of the analysis

# > colnames(cleaned.data)
# [1] "id"                           "safe.disc.W1"                 "dangerous.disc.W1"
# [4] "exp.disagr.offline.prcpt.W1"  "safe.disc.W2"                 "dangerous.disc.W2"
# [7] "exp.disagr.offline.prcpt.W2"  "exp.disagr.online.prcpt.W2"   "diff.exp.disagree.W2"
# [10] "safe.disc.W3"                 "dangerous.disc.W3"            "exp.disagr.offline.prcpt.W3"
# [13] "exp.disagr.online.prcpt.W3"   "diff.exp.disagree.W3"         "centr.eigen.W1"
# [16] "centr.eigen.W2"               "centr.eigen.W3"               "canpref.W1"
# [19] "canpref.W2"                   "canpref.W3"                   "age.years"
# [22] "female"                       "edu"                          "household.income"
# [25] "residential.region"           "pref.certainty.W2"            "pref.certainty.W3"
# [28] "ideo.W1"                      "ideo.W2"                      "ideo.W3"
# [31] "ideo_str.W1"                  "ideo_str.W2"                  "ideo_str.W3"
# [34] "perceived.opinion.climate.W2" "perceived.opinion.climate.W3" "consistency.motivation"
# [37] "understanding.motivation"     "hedonic.motivation"           "internal.efficacy.W1"
# [40] "internal.efficacy.W2"         "internal.efficacy.W3"         "pol.interest.W1"
# [43] "pol.interest.W2"              "pol.interest.W3"              "internet.news.use.W2"
# [46] "newspaper.use.W2"             "tv.news.use.W2"               "outgroup.negativity.bias.W2"
# [49] "ingroup.favoritism.bias.W2"


## ------------------------------------- ##
## Predicting perceived opinion climates ##
## ------------------------------------- ##

## model predicting perceived.opinion.climate
model.M1 <- lm(perceived.opinion.climate.W2 ~
                ## focal predictor
                safe.disc.W1 + dangerous.disc.W1 +
                ## demographic controls
                age.years + female + edu + household.income +
                ## political correlates
                canpref.W2 + ideo_str.W2 + internal.efficacy.W2 + pol.interest.W2 +
                ## media exposure
                internet.news.use.W2 + newspaper.use.W2 + tv.news.use.W2
              ,
              data = cleaned.data); summary(model.M1)

## model predicting negative outgroup affect
model.M2 <- lm(update.formula(model.M1, affective.polarization ~ .),
               data = cleaned.data); summary(model.M2)

## individual's ideology str significantly interact with dangerous.disc
model.M1.int1 <- lm(update.formula(model.M1, . ~ . + dangerous.disc.W1*ideo_str.W2), data = cleaned.data)
jtools::interact_plot(model.M1.int1, pred = "dangerous.disc.W1", modx = "ideo_str.W2")
## eigenvector centrality (measured in prior wave) also significantly interact with dangerous.disc
model.M1.int2 <- lm(
  update.formula(model.M1, . ~ . + dangerous.disc.W1*centr.eigen.W1), data = cleaned.data)
jtools::interact_plot(model.M1.int2, pred = "dangerous.disc.W1", modx = "centr.eigen.W1")
## three-way interaction is also significant
model.M1.int3 <- lm(update.formula(model.M1, . ~ . + dangerous.disc.W1*centr.eigen.W1*ideo_str.W2),
               data = cleaned.data)
jtools::interact_plot(model.M1.int3, pred = "dangerous.disc.W1",
                      modx = "centr.eigen.W1", mod2 = "ideo_str.W2")

## predicting self-reported exposure to disagreement
model.Y <- lm(exp.disagr.online.prcpt.W3 ~
                ## focal predictor
                safe.disc.W1 + dangerous.disc.W1 + centr.eigen.W1 +
                ## demographic controls
                age.years + female + edu + household.income +
                ## political correlates
                canpref.W2 + ideo_str.W2 + internal.efficacy.W2 + pol.interest.W2 +
                ## media exposure
                internet.news.use.W2 + newspaper.use.W2 + tv.news.use.W2 +
                ## mediator
                 perceived.opinion.climate.W2 + ingroup.favoritism.bias.W2
              ,
              data = cleaned.data)

screenreg(list(model.M1, model.M2, model.Y),
          custom.model.names = c("Prcvd Op Climate", "Affective pol", "Selfrep Dis"))
screenreg(list(model.M1, model.M1.int1, model.M1.int2, model.M1.int3),
          custom.model.names = c("Prcvd Op Climate", "Two-way I", "Two-way II", "Three-way"))

## -------------------------------------------------------- ##
## Nonparametric bootstrap-based indirect effect inferences ##
## -------------------------------------------------------- ##

## unconditional indirect effect
estimate.unconditional.indirect

## estimation, with multicore processing
require(boot)
RNGkind("L'Ecuyer-CMRG")
set.seed(1234)
boot.test1 <- boot(cleaned.data, estimate.unconditional.indirect,
                   R = 20000, parallel = "multicore", ncpus = 8)
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

