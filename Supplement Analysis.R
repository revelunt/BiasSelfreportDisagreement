
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
