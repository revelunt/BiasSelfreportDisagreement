
## Online supplementary information

## ------------------------------ ##
## 1. Timeline of data collection ##
## ------------------------------ ##

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


## --------------------------------- ##
## 2. Bayesian estimation using brms ##
## --------------------------------- ##

require(brms)
require(sjstats)
require(loo)
require(texreg)

## set custom fuction to work with texreg
extract.brms <- function (model, include.rsquared = TRUE, include.nobs = TRUE,
                          include.loo.ic = TRUE, include.waic = TRUE, ...) {
  s <- summary(model, ...)$fixed
  names <- rownames(s)
  hdis <- sjstats::hdi(model, ...)
  hdis <- hdis[seq(1:length(names)),]
  co <- s[, 1]
  se <- s[, 2]
  rs <- brms::bayes_R2(model)[1]
  n <- nobs(model)
  gof <- numeric()
  gof.names <- character()
  gof.decimal <- logical()
  if (include.rsquared == TRUE) {
    gof <- c(gof, rs)
    gof.names <- c(gof.names, "R$^2$")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.nobs == TRUE) {
    gof <- c(gof, n)
    gof.names <- c(gof.names, "Num. obs.")
    gof.decimal <- c(gof.decimal, FALSE)
  }
  if (include.loo.ic == TRUE) {
    looic <- brms::loo(model)$estimates["looic","Estimate"]
    gof <- c(gof, looic)
    gof.names <- c(gof.names, "loo IC")
    gof.decimal <- c(gof.decimal, TRUE)
  }

  if (include.waic == TRUE) {
    waic <- brms::waic(model)$estimates["waic","Estimate"]
    gof <- c(gof, waic)
    gof.names <- c(gof.names, "WAIC")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  tr <- texreg::createTexreg(coef.names = names, coef = co, se = se,
                             ci.low = hdis$hdi.low, ci.up = hdis$hdi.high,
                             gof.names = gof.names, gof = gof, gof.decimal = gof.decimal)
  return(tr)
}
setMethod("extract", signature = className("brmsfit", "brm"),
          definition = extract.brms)

RNGkind("L'Ecuyer-CMRG")
set.seed(12345)
model1.brm <- brm(dis.accuracy.W2 ~ ## predicting overestimation
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
                  data = cleaned.data, family = gaussian(),
                  prior = c(set_prior("normal(0, 100)", class = "Intercept"),
                            set_prior("normal(0, 10)", class = "b"),
                            set_prior("cauchy(0, 1)", class = "sigma")),
                  control = list(adapt_delta = 0.9),
                  warmup = 2000, iter = 4000, chains = 8, cores = 8)

RNGkind("L'Ecuyer-CMRG")
set.seed(12345)
model1.cat.brm <- brm(update.formula(formula(model1), dis.accuracy.cat.W2 ~ .),
                      family = bernoulli("logit"),
                      data = cleaned.data,
                      prior = c(set_prior("normal(0, 100)", class = "Intercept"),
                                set_prior("normal(0, 10)", class = "b")),
                      control = list(adapt_delta = 0.9),
                      warmup = 2000, iter = 4000, chains = 8, cores = 8)

RNGkind("L'Ecuyer-CMRG")
set.seed(12345)
model2.brm <- brm(dis.accuracy.W3 ~
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
                  data = cleaned.data,
                  family = gaussian(),
                  prior = c(set_prior("normal(0, 100)", class = "Intercept"),
                            set_prior("normal(0, 10)", class = "b"),
                            set_prior("cauchy(0, 1)", class = "sigma")),
                  control = list(adapt_delta = 0.9),
                  warmup = 2000, iter = 4000, chains = 8, cores = 8)

RNGkind("L'Ecuyer-CMRG")
set.seed(12345)
model2.cat.brm <- brm(update.formula(formula(model2), dis.accuracy.cat.W3 ~ .),
                      family = bernoulli("logit"),
                      data = cleaned.data,
                      #prior = c(set_prior("normal(0, 100)", class = "Intercept"),
                      #          set_prior("normal(0, 10)", class = "b")),
                      control = list(adapt_delta = 0.9),
                      warmup = 2000, iter = 4000, chains = 8, cores = 8)

## Table A2 in the appendix
screenreg(list(model1.brm, model1.cat.brm, #model1.luc,
               model2.brm, model2.cat.brm), #model2.luc),
          stars = 0.05, digits = 3, leading.zero = FALSE,
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
          reorder.coef = c(2:3, 8:9, 4:6, 7,14, 10:13, 1),
          groups = list("Social desirability" = 1:2,
                        "Cognitive burden" = 3:4,
                        "Opinion Climate" = 5,
                        "Controls" = 6:9,
                        "Demographics" = 10:13),
          custom.note = "0 outside the 90% Highest Density Intervals")

## SI I - using alternative behavioral benchmark
## Here, we use average of daily proportion benchmark

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

require(patchwork)
## figure 1 in the ms
qq2.avg + qq3.avg + plot_layout(nrow = 1)

## Correlations
cleaned.data[, cor.test(dangerous.disc.dlyavg.W1, dangerous.disc.prcptn.W2)]
cleaned.data[, cor.test(dangerous.disc.dlyavg.W2, dangerous.disc.prcptn.W3)]

## Permutation tests
RNGkind("L'Ecuyer-CMRG"); set.seed(12345)
diff.perm.test(cleaned.data, rep = 20000,
               "dangerous.disc.prcptn.W2", "dangerous.disc.dlyavg.W1")
RNGkind("L'Ecuyer-CMRG"); set.seed(12345)
diff.perm.test(cleaned.data, rep = 20000,
               "dangerous.disc.prcptn.W3", "dangerous.disc.dlyavg.W2")


cleaned.data[, alt.dis.accuracy.W2 :=
               dangerous.disc.prcptn.W2 - dangerous.disc.dlyavg.W1]
cleaned.data[, alt.dis.accuracy.W3 :=
               dangerous.disc.prcptn.W3 - dangerous.disc.dlyavg.W2]

cleaned.data[, descriptives(alt.dis.accuracy.W2)]
cleaned.data[, descriptives(alt.dis.accuracy.W3)]


model1.SI <- lm(alt.dis.accuracy.W2 ~ ## predicting overestimation
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

model2.SI <- lm(alt.dis.accuracy.W3 ~
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

## SI II - using alternative public opinion perception
## Here, we use % of suppoter measure

model3.SI <- lm(dis.accuracy.W2 ~ ## predicting overestimation
               ## social desirability
               discussion.norm.W2 +
               need.for.approval.W2 + ## cf. interaction is not sig as well
               ## partisan motivations
               alt.perceived.opinion.climate.W2 +
               canpref.W2 + ideo_str.W2 +
               ## other controls
               log.total.exp.W1 + pol.interest.W2 + pol.know +
               ## demographic controls
               age.years + female + edu + household.income +
               ## media exposure
               media.exposure.W2
             ,
             data = cleaned.data)

model4.SI <- lm(dis.accuracy.W3 ~
               ## focal predictor
               discussion.norm.W3 +
               need.for.approval.W3 + ## cf. interaction is not sig as well
               alt.perceived.opinion.climate.W3 +
               canpref.W3 + ideo_str.W3 +
               ## other controls
               log.total.exp.W2 + pol.interest.W3 + pol.know +
               ## demographic controls
               age.years + female + edu + household.income +
               ## media exposure
               media.exposure.W2,
             data = cleaned.data)

model5.SI <- lm(alt.dis.accuracy.W2 ~ ## predicting overestimation
                  ## social desirability
                  discussion.norm.W2 +
                  need.for.approval.W2 + ## cf. interaction is not sig as well
                  ## partisan motivations
                  alt.perceived.opinion.climate.W2 +
                  canpref.W2 + ideo_str.W2 +
                  ## other controls
                  log.total.exp.W1 + pol.interest.W2 + pol.know +
                  ## demographic controls
                  age.years + female + edu + household.income +
                  ## media exposure
                  media.exposure.W2
                ,
                data = cleaned.data)

model6.SI <- lm(alt.dis.accuracy.W3 ~
                  ## focal predictor
                  discussion.norm.W3 +
                  need.for.approval.W3 + ## cf. interaction is not sig as well
                  alt.perceived.opinion.climate.W3 +
                  canpref.W3 + ideo_str.W3 +
                  ## other controls
                  log.total.exp.W2 + pol.interest.W3 + pol.know +
                  ## demographic controls
                  age.years + female + edu + household.income +
                  ## media exposure
                  media.exposure.W2,
                data = cleaned.data)


RNGkind("L'Ecuyer-CMRG"); set.seed(12345)
boot.model1.SI <- boot(cleaned.data, statistic = boot.lm,
                       R = 10000, parallel = "multicore", ncpus = 8,
                       lm.fit = model1.SI) %>% get.boot.stats(., type = "perc")
RNGkind("L'Ecuyer-CMRG"); set.seed(12345)
boot.model2.SI <- boot(cleaned.data, statistic = boot.lm,
                       R = 10000, parallel = "multicore", ncpus = 8,
                       lm.fit = model2.SI) %>% get.boot.stats(., type = "perc")
RNGkind("L'Ecuyer-CMRG"); set.seed(12345)
boot.model3.SI <- boot(cleaned.data, statistic = boot.lm,
                       R = 10000, parallel = "multicore", ncpus = 8,
                       lm.fit = model3.SI) %>% get.boot.stats(., type = "perc")
RNGkind("L'Ecuyer-CMRG"); set.seed(12345)
boot.model4.SI <- boot(cleaned.data, statistic = boot.lm,
                       R = 10000, parallel = "multicore", ncpus = 8,
                       lm.fit = model4.SI) %>% get.boot.stats(., type = "perc")
RNGkind("L'Ecuyer-CMRG"); set.seed(12345)
boot.model5.SI <- boot(cleaned.data, statistic = boot.lm,
                       R = 10000, parallel = "multicore", ncpus = 8,
                       lm.fit = model5.SI) %>% get.boot.stats(., type = "perc")
RNGkind("L'Ecuyer-CMRG"); set.seed(12345)
boot.model6.SI <- boot(cleaned.data, statistic = boot.lm,
                       R = 10000, parallel = "multicore", ncpus = 8,
                       lm.fit = model6.SI) %>% get.boot.stats(., type = "perc")

## Table A6 in the appendix
screenreg(list(model1.SI, model2.SI,
               model3.SI, model4.SI,
               model5.SI, model6.SI),
          stars = c(0.001, 0.01, 0.05, 0.10), digits = 3,
          custom.model.names = c("AltDV W2", "AltDV W3",
                                 "AltOP W2", "AltOP W3",
                                 "AltALL W2", "AltALL W3"),
          custom.coef.names = c(
            "(Intercept)", "Discussion norm W2/W3",
            "Need for socl approval W2/W3", "Prcvd Op Climate W2/W3",
            "Candidate pref W2/W3", "Ideo Strength W2/W3",
            "Total Exp W1/W2 (log)", "Interest W2/W3", "Knowledge",
            "Age (in years)", "Female", "Education", "HH income",
            "Media Exposure",  "Discussion norm W2/W3",
            "Need for socl approval W2/W3", "Prcvd Op Climate W2/W3",
            "Candidate pref W2/W3", "Ideo Strength W2/W3",
            "Total Exp W1/W2 (log)", "Interest W2/W3",
            "Prcvd Op Climate W2/W3", "Prcvd Op Climate W2/W3"),
          override.ci.low = list(boot.model1.SI[,2], boot.model2.SI[,2],
                                 boot.model3.SI[,2], boot.model4.SI[,2],
                                 boot.model5.SI[,2], boot.model6.SI[,2]),
          override.ci.up = list(boot.model1.SI[,3], boot.model2.SI[,3],
                                boot.model3.SI[,3], boot.model4.SI[,3],
                                boot.model5.SI[,3], boot.model6.SI[,3]),
          reorder.coef = c(2:3, 8:9, 4:6, 7,14, 10:13, 1),
          groups = list("Social desirability" = 1:2,
                        "Cognitive burden" = 3:4,
                        "Opinion Climate" = 5,
                        "Controls" = 6:9,
                        "Demographics" = 10:13))


## Time-window comparison (Using cumulative proportions)
## cumulative proportion with varying time-window,
## from entire 2 week to day before the survey

## Wave 1 exposure vs. Wave 2 perception
test_w1 <- lapply(5:18, function(i) {
  test_w1 <- net[, .(reading.date, id = reader.id, count = 1,
                     exp.disagree = !(dat[net[, reader.id], canpref2] == dat[net[, poster.id], canpref2])
  )][reading.date %in% date.range[seq(i, 18)],
     .(dangerous.disc.W1 = mean(exp.disagree, na.rm = T),
       total.exp.W1 = sum(count, na.rm = T),
       total.dangerous.disc.W1 = sum(exp.disagree, na.rm = T),
       total.safe.disc.W1 = sum(count, na.rm = T) - sum(exp.disagree, na.rm = T)),
     by = id] %>% setkey(., "id")

  test_w1 <- merge(test_w1,
                   cleaned.data[, .(id, dangerous.disc.prcptn.W2)],
                   by = "id", all = TRUE)
  out <- test_w1[, .(id, dangerous.disc.W1, dangerous.disc.prcptn.W2)]
  out
})

## Wave 2 exposure vs. Wave 3 perception
test_w2 <- lapply(15:27, function(i) {
  test_w2 <- net[, .(reading.date, id = reader.id, count = 1,
                     exp.disagree = !(dat[net[, reader.id], canpref3] == dat[net[, poster.id], canpref3])
  )][reading.date %in% date.range[seq(i, 27)],
     .(dangerous.disc.W2 = mean(exp.disagree, na.rm = T),
       total.exp.W2 = sum(count, na.rm = T),
       total.dangerous.disc.W2 = sum(exp.disagree, na.rm = T),
       total.safe.disc.W2 = sum(count, na.rm = T) - sum(exp.disagree, na.rm = T)),
     by = id] %>% setkey(., "id")

  test_w2 <- merge(test_w2,
                   cleaned.data[, .(id, dangerous.disc.prcptn.W3)],
                   by = "id", all = TRUE)
  out <- test_w2[, .(id, dangerous.disc.W2, dangerous.disc.prcptn.W3)]
  out
})


cor.mat <- data.frame(date.ranges = sapply(5:18, function(i) length(seq(i, 18))),
                      W1.W2 = ## correlations, W1 vs. W2
                        sapply(test_w1, function(x)
                          x[, cor(dangerous.disc.W1,
                                  dangerous.disc.prcptn.W2,
                                  use = "complete.obs")]),
                      W2.W3 = ## correlations, W2 vs. W3
                        c(NA,
                          sapply(test_w2, function(x)
                            x[, cor(dangerous.disc.W2,
                                    dangerous.disc.prcptn.W3,
                                    use = "complete.obs")]))
)

cor.mat <- melt(cor.mat, id.vars = "date.ranges",
                variable.name = "wave",
                value.name = "corr")
require(ggplot2)
ggplot(cor.mat, aes(x = date.ranges, y = corr, fill = factor(wave))) +
  geom_bar(stat = "identity", position = position_dodge(0.9)) +
  scale_x_reverse() + theme_bw() + theme(legend.title = element_blank(), legend.position = "bottom") +
  coord_cartesian(xlim = c(1,14)) + scale_x_continuous(breaks = 1:14) +
  scale_fill_discrete(
    breaks = c("W1.W2", "W2.W3"),
    labels = c(" W1 exposure vs. W2 perception  ", " W2 exposure vs. W3 perception ")) +
  ylab("Zero-order corr.") +
  xlab("Date ranges (i.e., recent k days until the survey date)") +
  ggtitle("Correlations b/w behavioral (cumulative proportions) vs. perceptional measures")


## Using daily averages
## Wave 1 exposure vs. Wave 2 perception
test_w1 <- lapply(5:18, function(i) {
  test_w1 <- net[, .(reading.date, id = reader.id,
                     exp.disagree = !(
                       dat[net[, reader.id], canpref2] ==
                         dat[net[, poster.id], canpref2])
                     )] %>%
             .[reading.date %in% date.range[seq(i, 18)],
                .(dangerous.disc.dlyavg.W1 = mean(exp.disagree, na.rm = T)),
               by = c("id", "reading.date")] %>%
             .[, .(dangerous.disc.dlyavg.W1 =
            ## daily average means we need to divide by no. of days
            ## but not by just simple mean....
            sum(dangerous.disc.dlyavg.W1)/14), by = id]

  test_w1 <- merge(test_w1,
                   cleaned.data[, .(id, dangerous.disc.prcptn.W2)],
                   by = "id", all = TRUE)
  out <- test_w1[, .(id, dangerous.disc.dlyavg.W1, dangerous.disc.prcptn.W2)]
  out
})

## Wave 2 exposure vs. Wave 3 perception
test_w2 <- lapply(15:27, function(i) {
  test_w2 <- net[, .(reading.date, id = reader.id,
                     exp.disagree = !(
                       dat[net[, reader.id], canpref3] ==
                         dat[net[, poster.id], canpref3])
                )] %>%
             .[reading.date %in% date.range[seq(i, 27)],
                .(dangerous.disc.dlyavg.W2 = mean(exp.disagree, na.rm = T)),
               by = c("id", "reading.date")] %>%
             .[, .(dangerous.disc.dlyavg.W2 =
            ## daily average means we need to divide by no. of days
            ## but not by just simple mean....
            sum(dangerous.disc.dlyavg.W2)/14), by = id]

  test_w2 <- merge(test_w2,
                   cleaned.data[, .(id, dangerous.disc.prcptn.W3)],
                   by = "id", all = TRUE)
  out <- test_w2[, .(id, dangerous.disc.dlyavg.W2, dangerous.disc.prcptn.W3)]
  out
})

cor.mat2 <- data.frame(date.ranges = sapply(5:18, function(i) length(seq(i, 18))),
                      W1.W2 = ## correlations, W1 vs. W2
                        sapply(test_w1, function(x)
                          x[, cor(dangerous.disc.dlyavg.W1,
                                  dangerous.disc.prcptn.W2,
                                  use = "complete.obs")]),
                      W2.W3 = ## correlations, W2 vs. W3
                        c(NA,
                          sapply(test_w2, function(x)
                            x[, cor(dangerous.disc.dlyavg.W2,
                                    dangerous.disc.prcptn.W3,
                                    use = "complete.obs")]))
)

cor.mat2 <- melt(cor.mat2, id.vars = "date.ranges",
                  variable.name = "wave",
                  value.name = "corr")
ggplot(cor.mat2, aes(x = date.ranges, y = corr, fill = factor(wave))) +
  geom_bar(stat = "identity", position = position_dodge(0.9)) +
  scale_x_reverse() + theme_bw() +
  theme(legend.title = element_blank(), legend.position = "bottom") +
  coord_cartesian(xlim = c(1,14)) + scale_x_continuous(breaks = 1:14) +
  scale_fill_discrete(
    breaks = c("W1.W2", "W2.W3"),
    labels = c(" W1 exposure vs. W2 perception  ", " W2 exposure vs. W3 perception ")) +
  ylab("Zero-order corr.") +
  xlab("Date ranges (i.e., recent k days until the survey date)") +
  ggtitle("Correlations b/w behavioral (daily averages) vs. perceptional measures")
