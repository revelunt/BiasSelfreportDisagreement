
## prepare data for analysis
## automatically setting working directories
options(scipen = 999)
rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
if (!("cleaned.data" %in% ls())) source("dev/PreProcess.R")

require(ggplot2)
require(ggthemes)
library(texreg)
library(data.table)
require(boot)
require(rstan)

## we use 'cleaned.data' for the rest of the analysis

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

## candidate support
cleaned.data[, table(canpref.W1)/.N]

## Activites on the forum
## unique number of users per day
net[, length(unique(c(unique(reader.id), unique(poster.id)))),
    by = reading.date][, .(M = mean(V1), SD = sd(V1))]

## unique messages
dat[, sum(쓰기_sum, na.rm = T)]

## average reading count
net[, count := 1]
net[, sum(count), by = reader.id][, .(mean(V1), sd(V1))]

## median of reading count divided by 27 days
net[, count := 1]
net[, sum(count), by = reader.id][, .(median(V1)/27, sd(V1)/27)]

## median of posting count divided by 27 days
dat[, .(median(쓰기_sum, na.rm = T), sd(쓰기_sum, na.rm = T))]/27

## number of those who posts anything at all
length(net[, unique(poster.id)])

## number of those who reads anything at all
length(net[, unique(reader.id)])

## whether activity levels are correlated with one's political interest/knowledge?
cor.test(net[, sum(count), by = reader.id][, V1], cleaned.data[, pol.know])
cor.test(net[, sum(count), by = reader.id][, V1], cleaned.data[, pol.interest.W1])
cor.test(net[, sum(count), by = reader.id][, V1], cleaned.data[, pol.interest.W2])
cor.test(net[, sum(count), by = reader.id][, V1], cleaned.data[, pol.interest.W3])



## Measures

## actual exposure (from behavioral tracking data)
cleaned.data[, sapply(.SD, descriptives),
             .SDcols = c("dangerous.disc.W1.candpref", "dangerous.disc.W2.candpref")]

## perceived exposure
cleaned.data[, sapply(.SD, descriptives),
             .SDcols = c("dangerous.disc.prcptn.W2.candpref", "dangerous.disc.prcptn.W3.candpref")]


## public opinion perception
cleaned.data[, sapply(.SD, descriptives),
             .SDcols = c("perceived.opinion.climate.W2", "perceived.opinion.climate.W3")]

## ----------------------- ##
## 1. Preliminary analysis ##
## ----------------------- ##

# make_qqplot(type = "candpref")
# make_ks_plot(type = "candpref")

test2 <- cleaned.data[, .(actual = dangerous.disc.W1.candpref,
                          perceived = dangerous.disc.prcptn.W2.candpref,
                          diff = dangerous.disc.prcptn.W2.candpref - dangerous.disc.W1.candpref,
                          avg = (dangerous.disc.prcptn.W2.candpref + dangerous.disc.W1.candpref)/2)]

p1a <- ggplot(test2, aes(x = actual, y = diff)) + geom_point(alpha = 0.5) +
  geom_hline(yintercept = mean(test2$diff), colour = "black", size = 0.5) +
  geom_hline(yintercept = 0, colour = "grey", lty = 2, size = 0.5) +
  #geom_hline(yintercept = mean(test2$diff) - (1.96 * sd(test2$diff)), colour = "red", size = 0.5) +
  #geom_hline(yintercept = mean(test2$diff) + (1.96 * sd(test2$diff)), colour = "red", size = 0.5) +
  ylab("Response inaccuracy \n(Perceived W2 minus Actual W1)") +
  xlab("Actual exposure W1") + theme_bw() +
  geom_smooth(method = 'lm')

wilcox.test(cleaned.data$dangerous.disc.W1.candpref,
            cleaned.data$dangerous.disc.prcptn.W2.candpref,
            paired = TRUE, alternative = "two.sided")


test3 <- cleaned.data[, .(actual = dangerous.disc.W2.candpref,
                          perceived = dangerous.disc.prcptn.W3.candpref,
                          diff = dangerous.disc.prcptn.W3.candpref - dangerous.disc.W2.candpref,
                          avg = (dangerous.disc.prcptn.W3.candpref + dangerous.disc.W2.candpref)/2)]

p1b <- ggplot(test3, aes(x = actual, y = diff)) + geom_point(alpha = 0.5) +
  geom_hline(yintercept = mean(test3$diff, na.rm = T), colour = "black", size = 0.5) +
  geom_hline(yintercept = 0, colour = "grey", lty = 2, size = 0.5) +
  #geom_hline(yintercept = mean(test2$diff) - (1.96 * sd(test2$diff)), colour = "red", size = 0.5) +
  #geom_hline(yintercept = mean(test2$diff) + (1.96 * sd(test2$diff)), colour = "red", size = 0.5) +
  ylab("Response inaccuracy \n(Perceived W3 minus Actual W2)") +
  xlab("Actual exposure W2") + theme_bw() +
  geom_smooth(method = 'lm')

wilcox.test(cleaned.data$dangerous.disc.W2.candpref,
            cleaned.data$dangerous.disc.prcptn.W3.candpref,
            paired = TRUE, alternative = "two.sided")

require(patchwork)
p1a + p1b + plot_layout(nrow = 1)


## check the correlation of measures
cleaned.data[, cor.test(dangerous.disc.W1.candpref, dangerous.disc.prcptn.W2.candpref)]
cleaned.data[, cor.test(dangerous.disc.W2.candpref, dangerous.disc.prcptn.W3.candpref)]

## permutation test indicates that the difference between
## perception and objective behavior is significantly differ,
## in a way that people tend to overestimate the exposure to differences
RNGkind("L'Ecuyer-CMRG"); set.seed(12345)
diff.perm.test(cleaned.data, rep = 20000,
               "dangerous.disc.prcptn.W2.candpref", "dangerous.disc.W1.candpref")
RNGkind("L'Ecuyer-CMRG"); set.seed(12345)
diff.perm.test(cleaned.data, rep = 20000,
               "dangerous.disc.prcptn.W3.candpref", "dangerous.disc.W2.candpref")


## ------------------------------------------------ ##
## Moving window other than 2 weeks prior to survey ##
## ------------------------------------------------ ##

require(cocor)

## Wave 2 measures
## list covers most recent N days from the date of the survey
moving.window.test.W2 <- lapply(dynamic.moving.window, function(dat_sub) {
  RNGkind("L'Ecuyer-CMRG"); set.seed(12345)
  cocor_test <- cocor(~ dangerous.disc.prcptn.W2.candpref + recent.dangerous.disc.W1 |
                        dangerous.disc.prcptn.W2.candpref + dangerous.disc.W1.candpref, data = dat_sub,
                      test = "zou2007")
  ## see Zou, G. Y. (2007). Toward using confidence intervals to compare correlations.
  ## in "Psychological Methods," vol 12, pp. 399-413. doi:10.1037/1082-989X.12.4.399

  return.dat <-
    data.frame(
      r.jk = cocor_test@r.jk, ## cor. b/w percept and object.recent.N.days
      r.jh = cocor_test@r.jh, ## cor. b/w percept and object.entire.days
      r.kh = cocor_test@r.kh, ## cor. b/w object.recent.N.days and object.entire.days
      sample.n = cocor_test@n,
      corr.diff = cocor_test@diff,
      test.type = cocor_test@test,
      alpha = 0.05,
      h0 = "r.jk == r.jh",
      h1 = "r.jk != r.jh", ## alternative hypothesis: correlations do differ
      llci = cocor_test@zou2007$conf.int[1],
      ulci = cocor_test@zou2007$conf.int[2],
      test.results = ifelse(setequal(sign(cocor_test@zou2007$conf.int)[1], sign(cocor_test@zou2007$conf.int)[2]),
                            "h1", "h0") ## if correlations do differ, return h1
    )

  return.dat
}) %>% do.call("rbind", .)
## Results indicates correlations using most recent N days do not differ, or even lower than entire 14-day window.

setDT(moving.window.test.W2)
moving.window.test.W2[, Ndays := 1:14]

p2a <- ggplot(moving.window.test.W2, aes(x = Ndays, y = corr.diff, colour = test.results)) +
  geom_point() + theme_bw() +
  scale_colour_manual(values = c("red", "grey50")) +
  geom_hline(yintercept = 0,
             colour = "grey", lty = 2, size = 0.5) +
  geom_errorbar(aes(ymin = llci, ymax = ulci), width = .1) +
  scale_x_continuous(name = "Most recent N days", breaks = c(1:14)) +
  ylab("Differences in \n two correlation coef.") + #ylim(c(0,.35)) +
  ggtitle("Panel A: Perceived (W2) vs. Actual (W1) disagreement") +
  theme(legend.position = "none")

## Wave 3 measures
moving.window.test.W3 <- lapply(dynamic.moving.window, function(dat_sub) {
  RNGkind("L'Ecuyer-CMRG"); set.seed(12345)
  cocor_test <- cocor(~ dangerous.disc.prcptn.W3.candpref + recent.dangerous.disc.W2 |
                        dangerous.disc.prcptn.W3.candpref + dangerous.disc.W2.candpref, data = dat_sub,
                      test = "zou2007")
  ## see Zou, G. Y. (2007). Toward using confidence intervals to compare correlations.
  ## in "Psychological Methods," vol 12, pp. 399-413. doi:10.1037/1082-989X.12.4.399

  return.dat <-
    data.frame(
      r.jk = cocor_test@r.jk, ## cor. b/w percept and object.recent.N.days
      r.jh = cocor_test@r.jh, ## cor. b/w percept and object.entire.days
      r.kh = cocor_test@r.kh, ## cor. b/w object.recent.N.days and object.entire.days
      sample.n = cocor_test@n,
      corr.diff = cocor_test@diff,
      test.type = cocor_test@test,
      alpha = 0.05,
      h0 = "r.jk == r.jh",
      h1 = "r.jk != r.jh", ## alternative hypothesis: correlations do differ
      llci = cocor_test@zou2007$conf.int[1],
      ulci = cocor_test@zou2007$conf.int[2],
      test.results = ifelse(setequal(sign(cocor_test@zou2007$conf.int)[1],
                                     sign(cocor_test@zou2007$conf.int)[2]),
                            "h1", "h0") ## if correlations do differ, return h1
    )

  return.dat
}) %>% do.call("rbind", .)
## Results indicates correlations using most recent N days do not differ.

setDT(moving.window.test.W3)
moving.window.test.W3[, Ndays := 1:14]

p2b <- ggplot(moving.window.test.W3, aes(x = Ndays, y = corr.diff, colour = test.results)) +
  geom_point() + theme_bw() + theme(legend.title = element_blank()) +
  scale_colour_manual(values = c("grey50", "red")) +
  geom_hline(yintercept = 0,
             colour = "grey", lty = 2, size = 0.5) +
  geom_errorbar(aes(ymin = llci, ymax = ulci), width = .1) +
  scale_x_continuous(name = "Most recent N days", breaks = c(1:14)) +
  ylab("Differences in \n two correlation coef.") + # ylim(c(0.35,.57)) +
  ggtitle("Panel B: Perceived (W3) vs. Actual (W2) disagreement") +
  theme(legend.position = "none")

(p2a / p2b)


## --------------------------------- ##
## Predictors of response inaccuracy ##
## --------------------------------- ##

## cf. a-priory power analysis (using gpower)
## indicates for detecting small effect given sample size
## alpha is 0.07, therefore we stick to alpha = 0.5

## accuracy: perception minus exposure, therefore (+) means overestimation
cleaned.data[, descriptives(dis.accuracy.W2.candpref)]
cleaned.data[, descriptives(dis.accuracy.W3.candpref)]

require(brms)

RNGkind("L'Ecuyer-CMRG")
set.seed(12345)
model1.brm <- brm(dis.accuracy.W2.candpref ~ ## predicting overestimation
                    ## social desirability
                    discussion.norm.W2 +
                    #need.for.approval.W2 + ## cf. interaction is not sig as well
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
                  data = cleaned.data, family = gaussian(),
                  prior = c(set_prior("normal(0, 100)", class = "Intercept"),
                            set_prior("normal(0, 10)", class = "b"),
                            set_prior("cauchy(0, 1)", class = "sigma")),
                  control = list(adapt_delta = 0.9),
                  warmup = 2000, iter = 4000, chains = 8, cores = 8)

RNGkind("L'Ecuyer-CMRG")
set.seed(12345)
model1.cat.brm <- brm(update.formula(formula(model1.brm), dis.accuracy.cat.W2 ~ .),
                      family = bernoulli("logit"),
                      data = cleaned.data,
                      prior = c(set_prior("normal(0, 100)", class = "Intercept"),
                                set_prior("normal(0, 10)", class = "b")),
                      control = list(adapt_delta = 0.9),
                      warmup = 2000, iter = 4000, chains = 8, cores = 8)

RNGkind("L'Ecuyer-CMRG")
set.seed(12345)
model2.brm <- brm(dis.accuracy.W3.candpref ~
                    ## focal predictor
                    discussion.norm.W3 +
                    #need.for.approval.W3 + ## cf. interaction is not sig as well
                    perceived.opinion.climate.W3 +
                    canpref.W3 + ideo_str.W3 +
                    ## other controls
                    log.total.exp.W2 + pol.interest.W3 + pol.know +
                    ## demographic controls
                    age.years + female + edu + household.income +
                    ## media exposure
                    media.exposure.W2,
                  data = cleaned.data,
                  family = gaussian(),
                  prior = c(set_prior("normal(0, 100)", class = "Intercept"),
                            set_prior("normal(0, 10)", class = "b"),
                            set_prior("cauchy(0, 1)", class = "sigma")),
                  control = list(adapt_delta = 0.9),
                  warmup = 2000, iter = 4000, chains = 8, cores = 8)

RNGkind("L'Ecuyer-CMRG")
set.seed(12345)
model2.cat.brm <- brm(update.formula(formula(model2.brm), dis.accuracy.cat.W3 ~ .),
                      family = bernoulli("logit"),
                      data = cleaned.data,
                      #prior = c(set_prior("normal(0, 100)", class = "Intercept"),
                      #          set_prior("normal(0, 10)", class = "b")),
                      control = list(adapt_delta = 0.9),
                      warmup = 2000, iter = 4000, chains = 8, cores = 8)

screenreg(list(model1.brm, model1.cat.brm,
               model2.brm, model2.cat.brm),
          stars = 0.05, digits = 4, leading.zero = FALSE,
          custom.model.names = c("OLS W2", "GLM W2",
                                 "OLS W3", "GLM W3"),
          custom.coef.names = c(
            "(Intercept)", "Discussion norm W2/W3",
            #"Need for socl approval W2/W3",
            "Prcvd Op Climate W2/W3",
            "Candidate pref W2/W3", "Ideo Strength W2/W3",
            "Total Exp W1/W2 (log)", "Interest W2/W3", "Knowledge",
            "Age (in years)", "Female", "Education", "HH income",
            "Media Exposure",  "Discussion norm W2/W3",
            #"Need for socl approval W2/W3",
            "Prcvd Op Climate W2/W3",
            "Candidate pref W2/W3", "Ideo Strength W2/W3",
            "Total Exp W1/W2 (log)", "Interest W2/W3"),
          reorder.coef = c(2:5, 7:8, 6,13, 9:12, 1),
          groups = list("Social desirability / Norm" = 1,
                        "Opinion Climate" = 2,
                        "Motivations / Abilities" = 3:6,
                        "Controls/Demographics" = 7:12),
          custom.note = "0 outside the 90% Highest Density Intervals")




## ------------------ ##
## Outcome evaluation ##
## ------------------ ##

## test of equality of two correlation coefficients
## a comparison of two overlapping correlations based on dependent groups

    ## political participation model testing -- DOES NOT WORK
    # require(pscl)
    # model.participation.1 <- lm(participation.W3 ~
    #                               # participation.W2 +
    #                                   #  dangerous.disc.prcptn.W3.candpref +
    #                                     dangerous.disc.W2.candpref +
    #                                    log.total.exp.W2 + ideo_str.W2 +
    #                                    ## demographic controls
    #                                    age.years + female + edu + household.income +
    #                                    ## political correlates
    #                                    pol.know + pol.interest.W2 + internal.efficacy.W2 +
    #                                    ## media exposure
    #                                    media.exposure.W2
    #                                  , data = cleaned.data) # family = pospoisson());
    # summary(model.participation.1)

## tolerance

## first, get model terms and mean-center variables -- required to be passed to sim function!
model.terms <-
  ~ ambi.W2 + ambi.W3 +
  dangerous.disc.W2.candpref +
  dangerous.disc.prcptn.W3.candpref +
  log.total.exp.W2 +
  age.years + female + edu + household.income +
  pol.interest.W2 + canpref.W2 + # ideo_str.W2 +
  pol.know + internal.efficacy.W2 +
  ## media exposure
  media.exposure.W2

variables <- attr(terms(model.terms), "term.labels")
dat.subset1 <- cleaned.data[, variables, with = F] %>% na.omit
# dat.subset1[, canpref.W2 := factor(canpref.W2)]
# dat.subset1[, canpref.W2 := factor(female)]

model.ambi.1 <- lm(ambi.W3 ~
                            ambi.W2 +
                       ## focal X
                     dangerous.disc.prcptn.W3.candpref*canpref.W2 +
                       log.total.exp.W2 +
                       ## demographic controls
                       age.years + female + edu + household.income +
                       ## political correlates
                       pol.interest.W2 +
                         pol.know + internal.efficacy.W2 +
                       ## media exposure
                       media.exposure.W2
                     , data = data.frame(dat.subset1)); summary(model.ambi.1)

candpref <- c(
  "1" = "Opposition party candidate supporters",
  "0" = "Ruling party candidate supporters"
  )

fig4 <- interactions::interact_plot(model.ambi.1, dangerous.disc.prcptn.W3.candpref, canpref.W2,
                                    interval = T,
                             colors = c("#F8766D", "#619CFF"),
                             legend.main = "Candidate preference",
                             modx.labels = c("Ruling party candidate",
                                             "Opposition party candidate")) +
   ggtitle("Marginal effects of Exposure to disagreement (self-reported)") +
   ylab("Ambivalence at W3") +
   scale_x_continuous(name = "Exposure to disagreement",
                      breaks = seq(from = 0, to = 1, by = 0.1)) +
   theme_few() + theme(legend.position = "none") +
   facet_grid(~ canpref.W2, labeller = labeller(canpref.W2 = candpref))

 sim_slopes(model.ambi.1, dangerous.disc.prcptn.W3.candpref, canpref.W2)


 model.ambi.2 <- lm(ambi.W3 ~
                      ambi.W2 +
                      ## focal X
                      dangerous.disc.W2.candpref*canpref.W2 +
                      log.total.exp.W2 +
                      ## demographic controls
                      age.years + female + edu + household.income +
                      ## political correlates
                      pol.interest.W2 +
                      pol.know + internal.efficacy.W2 +
                      ## media exposure
                      media.exposure.W2
                    , data = data.frame(dat.subset1)); summary(model.ambi.2)

## Table 3 in the main ms and Table A5 in the appendix
screenreg(list(model.ambi.1, model.ambi.2), digits = 3,
          custom.model.names = c("Ambi Sbj W3", "Ambi Obj W3"),
          custom.coef.names = c(
            "(Intercept)", "Ambivalence W2",
            "Exp to Dis (self-report)", "Candidate pref W2",
            "Total Exp W2 (log)", "Age (in years)",
            "Female", "Education", "HH income",
            "Interest W2", "Knowledge W1", "Efficacy W2",
            "Media Exposure W2",
            "Exp to Dis x Candidate pref",
            "Exp to Dis (behavioral)",
            "Exp to Dis x Candidate pref"),
          reorder.coef = c(2, 3,15, 4,14, 10:13,5, 6:9, 1),
          groups = list("Autogressive" = 1,
                        "Focal predictors" = 2:4,
                        "Interaction" = 5,
                        "Motivations / Abilities" = 6:8,
                        "Controls/Demographics" = 9:14))

## differences in interaction coef
coef.sbj <- coef(model.ambi.1)["dangerous.disc.prcptn.W3.candpref:canpref.W2"]
coef.obj <- coef(model.ambi.2)["dangerous.disc.W2.candpref:canpref.W2"]
relative.size.sbj.obs <- coef.sbj/coef.obj
bias.obs <- abs(coef.sbj - coef.obj)

## For simulation inference
require(MASS)
require(data.table)
require(psych)
require(lm.beta)
require(corpcor)

# source("power_cal.R")
## simulation conditions
cond <- expand.grid(
  sample_n = c(341, 1000, 2500),
  target.corr = seq(from = 0, to = 0.95, by = 0.05)
)

RNGkind("L'Ecuyer-CMRG")
set.seed(12345)
reps <- mapply(sim.MC,
               sample_n = cond$sample_n,
               target.corr = cond$target.corr,
               n_reps = 1000,
               SIMPLIFY = FALSE)

sim.results <- do.call("rbind", reps) %>% setDT(.)
# save(sim.results, file = "Rdata/sim.results.Rdata")
# load("Rdata/sim.results.Rdata")

## bias, relative.size.sbj, coef.obj, sbj.coef.agree.with.obj
## whether significance of sbj measure agree with obj measures?
sim.results[sample_n == 341, sig.coef.sbj := (sig.sbj < .05)]
sim.results[sample_n == 1000, sig.coef.sbj := (sig.sbj < .05)]
sim.results[sample_n == 2500, sig.coef.sbj := (sig.sbj < .01)]
sim.results[sample_n == 341, sig.coef.obj := (sig.obj < .05)]
sim.results[sample_n == 1000, sig.coef.obj := (sig.obj < .01)]
sim.results[sample_n == 2500, sig.coef.obj := (sig.obj < .001)]

p5_a <- sim.results[, .(target.corr, sample_n, results.agree =
                  ifelse((sign(coef.sbj) == sign(coef.obj) & (sig.coef.sbj == sig.coef.obj)),
                         1, 0))] %>%
  .[, .(agree = mean(results.agree),
        llci = prop.cis(mean(results.agree), 1000)[1],
        ulci = prop.cis(mean(results.agree), 1000)[2]),
    by = c("target.corr", "sample_n")] %>%
  ggplot(., aes(x = target.corr, y = agree, color = factor(sample_n))) +
  geom_smooth(aes(ymin = llci, ymax = ulci), alpha = 0.3) +
  xlab("Zero-order corr. between subjective and objective measure") +
  ylab("Pr. of two results agree") + theme_bw() +
  scale_x_continuous(breaks = seq(0, 0.95, 0.1)) +
  theme(legend.position = "none") + scale_color_brewer(palette = "Dark2") +
  geom_vline(xintercept = cor.obs[2,14], linetype = 2, col = "red") +
  ggtitle("Panel A: Proportions of two results with same statistical significance")


## absolute bias of sbj measure agree against obj measures
p5_b <- sim.results[, .(coef.bias = abs(coef.sbj - coef.obj)),
                     by = c("target.corr", "sample_n")] %>%
  .[, .(mean.bias = mean(coef.bias),
        llci = quantile(coef.bias, .025),
        ulci = quantile(coef.bias, .975)),
    by = c("target.corr", "sample_n")] %>%
  ggplot(., aes(x = target.corr, y = mean.bias, group = factor(sample_n))) +
  geom_ribbon(aes(ymin = llci, ymax = ulci), fill = "grey70", alpha = 0.3) +
  geom_smooth(se = FALSE, aes(color = factor(sample_n))) +
  xlab("") + ylab("Mean abs discrepancy") + theme_bw() +
  xlab("Zero-order corr. between subjective and objective measure") +
  geom_hline(yintercept = bias.obs, linetype = 2, col = "red") +
  geom_vline(xintercept = cor.obs[2,14], linetype = 2, col = "grey") +
  scale_color_brewer(palette = "Dark2") +
  facet_grid(~ sample_n) + theme(legend.position = "none") +
  ggtitle("\nPanel B: Mean absolute discrepancy, subjective vs. behavioral measures")


## relative size of subjective measure
p5_c <- sim.results[, .(coef.relative.size = (coef.sbj/coef.obj)),
            by = c("target.corr", "sample_n")] %>%
      .[, .(mean.relative.size = mean(coef.relative.size),
            llci = quantile(coef.relative.size, .025),
            ulci = quantile(coef.relative.size, .975)),
        by = c("target.corr", "sample_n")] %>%
  ggplot(., aes(x = target.corr, y = mean.relative.size, group = factor(sample_n))) +
  geom_ribbon(aes(ymin = llci, ymax = ulci), fill = "grey70", alpha = 0.3) +
  geom_smooth(se = FALSE, aes(color = factor(sample_n))) +
  xlab("Zero-order corr. between subjective and objective measure") +
  ylab("Relative size") + theme_bw() +
  geom_hline(yintercept = relative.size.sbj.obs, linetype = 2, col = "red") +
  geom_vline(xintercept = cor.obs[2,14], linetype = 2, col = "grey") +
  scale_color_brewer(palette = "Dark2") +
  #scale_colour_discrete(name = "Sample N") + scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "bottom") + labs(color = "Sample N") + facet_grid(~ sample_n) +
  ggtitle("\nPanel C: Relative size of coefficients, subjective vs. behavioral measures")

p5_a + p5_b + p5_c + plot_layout(nrow = 3)
