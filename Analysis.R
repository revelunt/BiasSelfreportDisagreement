
if(!("texreg" %in% installed.packages()[,"Package"])) devtools::install_cran("texreg")
if(!("ggplot2" %in% installed.packages()[,"Package"])) devtools::install_cran("ggplot2")
require(texreg)
require(ggplot2)

## prepare data for analysis
## automatically setting working directories
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
if (!("cleaned.data" %in% ls())) source("dev/PreProcess.R")
## we use 'cleaned.data' and 'long.data' for the rest of the analysis

# > colnames(cleaned.data)
# [1] "id"                           "safe.disc.W1"                 "dangerous.disc.W1"
# [4] "exp.disagr.offline.prcpt.W1"  "safe.disc.W2"                 "dangerous.disc.W2"
# [7] "exp.disagr.offline.prcpt.W2"  "exp.disagr.online.prcpt.W2"   "diff.exp.disagree.W2"
# [10] "safe.disc.W3"                 "dangerous.disc.W3"            "exp.disagr.offline.prcpt.W3"
# [13] "exp.disagr.online.prcpt.W3"   "diff.exp.disagree.W3"         "canpref.W1"
# [16] "canpref.W2"                   "canpref.W3"                   "age.years"
# [19] "female"                       "edu"                          "household.income"
# [22] "residential.region"           "pref.certainty.W2"            "pref.certainty.W3"
# [25] "ideo.W1"                      "ideo.W2"                      "ideo.W3"
# [28] "ideo_str.W1"                  "ideo_str.W2"                  "ideo_str.W3"
# [31] "perceived.opinion.climate.W2" "perceived.opinion.climate.W3" "consistency.motivation"
# [34] "understanding.motivation"     "hedonic.motivation"           "internal.efficacy.W1"
# [37] "internal.efficacy.W2"         "internal.efficacy.W3"         "pol.interest.W1"
# [40] "pol.interest.W2"              "pol.interest.W3"              "internet.news.use.W2"
# [43] "newspaper.use.W2"             "tv.news.use.W2"               "outgroup.negativity.bias.W2"
# [46] "ingroup.favoritism.bias.W2"


## ------------------------------------- ##
## Predicting perceived opinion climates ##
## ------------------------------------- ##

model.M <- lm(perceived.opinion.climate.W2 ~
                ## focal predictor
                safe.disc.W1 + dangerous.disc.W1 +
                ## demographic controls
                age.years + female + edu + household.income +
                ## political correlates
                canpref.W2 + ideo_str.W2 + internal.efficacy.W2 + pol.interest.W2 +
                ## media exposure
                internet.news.use.W2 + newspaper.use.W2 + tv.news.use.W2
              ,
              data = cleaned.data)

## individual's ideology str significantly interact with dangerous.disc
model.M2 <- lm(update.formula(model.M, . ~ . + dangerous.disc.W1*ideo_str.W2), data = cleaned.data)
  ## cf.


## predicting self-reported exposure to disagreement
model.Y <- lm(exp.disagr.online.prcpt.W3 ~
                ## focal predictor
                safe.disc.W1 + dangerous.disc.W1 +
                ## demographic controls
                age.years + female + edu + household.income +
                ## political correlates
                canpref.W2 + ideo_str.W2 + internal.efficacy.W2 + pol.interest.W2 +
                ## media exposure
                internet.news.use.W2 + newspaper.use.W2 + tv.news.use.W2 +
                ## mediator
                 perceived.opinion.climate.W2
              ,
              data = cleaned.data)

screenreg(list(model.M, model.M2, model.Y))

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

plot.cond.ind <- cbind(moderator = as.vector(0:3), out.cond[3:6,])
require(data.table)
setDT(plot.cond.ind)

ggplot(plot.cond.ind, aes(x = moderator, y = coef)) +
  geom_line() + geom_hline(yintercept = 0, color = "red", linetype = 2) +
  geom_ribbon(aes(ymin = llci, ymax = ulci), alpha = 0.25) +
  xlab("Moderator: Strengths of ideology") + ylab("theta") +
  ggtitle("Conditional indirect effect of actual exposure to disagreeable discussants message")
