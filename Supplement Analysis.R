
## Sensitivity Analysis

## SI I - using alternative behavioral benchmark
## Here, we use average of daily proportion benchmark

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



boot.model1.SI <- boot(cleaned.data, statistic = boot.lm,
                       R = 10000, parallel = "multicore", ncpus = 8,
                       lm.fit = model1.SI) %>% get.boot.stats(., type = "perc")

boot.model2.SI <- boot(cleaned.data, statistic = boot.lm,
                       R = 10000, parallel = "multicore", ncpus = 8,
                       lm.fit = model2.SI) %>% get.boot.stats(., type = "perc")

boot.model3.SI <- boot(cleaned.data, statistic = boot.lm,
                       R = 10000, parallel = "multicore", ncpus = 8,
                       lm.fit = model3.SI) %>% get.boot.stats(., type = "perc")

boot.model4.SI <- boot(cleaned.data, statistic = boot.lm,
                       R = 10000, parallel = "multicore", ncpus = 8,
                       lm.fit = model4.SI) %>% get.boot.stats(., type = "perc")

boot.model5.SI <- boot(cleaned.data, statistic = boot.lm,
                       R = 10000, parallel = "multicore", ncpus = 8,
                       lm.fit = model5.SI) %>% get.boot.stats(., type = "perc")

boot.model6.SI <- boot(cleaned.data, statistic = boot.lm,
                       R = 10000, parallel = "multicore", ncpus = 8,
                       lm.fit = model6.SI) %>% get.boot.stats(., type = "perc")

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
