
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


## ------------------------------ ##
## 2. models reported in Appendix ##
## ------------------------------ ##

model1.lm <- lm(dis.accuracy.W2.candpref ~ ## predicting overestimation
                    ## social desirability
                    discussion.norm.W2 +
                    ## partisan motivations
                    perceived.opinion.climate.W2 +
                    canpref.W2 + ideo_str.W2 +
                    ## other controls
                    log.total.exp.W1 + pol.interest.W2 + pol.know +
                    ## demographic controls
                    age.years + female + edu + household.income +
                    ## media exposure
                    media.exposure.W2, data = cleaned.data)

model1.glm <- glm(update.formula(formula(model1.lm), dis.accuracy.cat.W2 ~ .),
                      family = binomial("logit"),
                      data = cleaned.data)

model2.lm <- lm(dis.accuracy.W3.candpref ~
                    ## focal predictor
                    discussion.norm.W3 +
                    perceived.opinion.climate.W3 +
                    canpref.W3 + ideo_str.W3 +
                    ## other controls
                    log.total.exp.W2 + pol.interest.W3 + pol.know +
                    ## demographic controls
                    age.years + female + edu + household.income +
                    ## media exposure
                    media.exposure.W2,
                  data = cleaned.data)

model2.glm <- glm(update.formula(formula(model2.lm), dis.accuracy.cat.W3 ~ .),
                  family = binomial("logit"),
                  data = cleaned.data)

require(boot)
RNGkind("L'Ecuyer-CMRG"); set.seed(12345)
boot.model1 <- boot(cleaned.data, statistic = boot.lm,
                    R = 10000, parallel = "multicore", ncpus = 8,
                    lm.fit = model1.lm) %>% get.boot.stats(., type = "perc")

RNGkind("L'Ecuyer-CMRG"); set.seed(12345)
boot.model1.cat <- boot(cleaned.data, statistic = boot.glm,
                        R = 10000, parallel = "multicore", ncpus = 8,
                        glm.fit = model1.glm) %>% get.boot.stats(., type = "perc")

RNGkind("L'Ecuyer-CMRG"); set.seed(12345)
boot.model2 <- boot(cleaned.data, statistic = boot.lm,
                    R = 10000, parallel = "multicore", ncpus = 8,
                    lm.fit = model2.lm) %>% get.boot.stats(., type = "perc")

RNGkind("L'Ecuyer-CMRG"); set.seed(12345)
boot.model2.cat <- boot(cleaned.data, statistic = boot.glm,
                        R = 10000, parallel = "multicore", ncpus = 8,
                        glm.fit = model2.glm) %>% get.boot.stats(., type = "perc")

## Table A2 in the appendix
screenreg(list(model1.lm, model1.glm,
               model2.lm, model2.glm),
          stars = 0.05, digits = 3, leading.zero = FALSE,
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
          override.ci.low = list(boot.model1[,2], boot.model1.cat[,2],
                                 boot.model2[,2], boot.model2.cat[,2]),
          override.ci.up = list(boot.model1[,3], boot.model1.cat[,3],
                                boot.model2[,3], boot.model2.cat[,3]),
          reorder.coef = c(2:5, 7:8, 6,13, 9:12, 1),
          groups = list("Social desirability / Norm" = 1,
                        "Opinion Climate" = 2,
                        "Motivations / Abilities" = 3:6,
                        "Controls/Demographics" = 7:12),
          custom.note = "* = 0 outside the 95% bootstrap Confidence Intervals")



model3.lm <- lm(dangerous.disc.prcptn.W2.candpref ~
                   dangerous.disc.W1.candpref +
                   discussion.norm.W2 +
                   ## partisan motivations
                   perceived.opinion.climate.W2 +
                   canpref.W2 + ideo_str.W2 +
                   ## other controls
                   log.total.exp.W1 + pol.interest.W2 + pol.know +
                   ## demographic controls
                   age.years + female + edu + household.income +
                   ## media exposure
                   media.exposure.W2,
                 data = cleaned.data)

model4.lm <- lm(dangerous.disc.prcptn.W3.candpref ~
                  dangerous.disc.W2.candpref +
                  discussion.norm.W3 +
                  perceived.opinion.climate.W3 +
                  canpref.W3 + ideo_str.W3 +
                  ## other controls
                  log.total.exp.W2 + pol.interest.W3 + pol.know +
                  ## demographic controls
                  age.years + female + edu + household.income +
                  ## media exposure
                  media.exposure.W2,
                data = cleaned.data)

RNGkind("L'Ecuyer-CMRG"); set.seed(12345)
boot.model3 <- boot(cleaned.data, statistic = boot.lm,
                    R = 10000, parallel = "multicore", ncpus = 8,
                    lm.fit = model3.lm) %>% get.boot.stats(., type = "perc")

RNGkind("L'Ecuyer-CMRG"); set.seed(12345)
boot.model4 <- boot(cleaned.data, statistic = boot.lm,
                    R = 10000, parallel = "multicore", ncpus = 8,
                    lm.fit = model4.lm) %>% get.boot.stats(., type = "perc")

screenreg(list(model3.lm, model4.lm),
          stars = 0.05, digits = 3, leading.zero = FALSE,
          custom.model.names = c("Self-report W2", "Self-report W3"),
          custom.coef.names = c(
            "(Intercept)", "Exp to Dis (behavior) W2/W3",
            "Discussion norm W2/W3",
            "Prcvd Op Climate W2/W3",
            "Candidate pref W2/W3", "Ideo Strength W2/W3",
            "Total Exp W1/W2 (log)", "Interest W2/W3", "Knowledge",
            "Age (in years)", "Female", "Education", "HH income",
            "Media Exposure W2",  "Exp to Dis (behavior) W2/W3",
            "Discussion norm W2/W3",
            "Prcvd Op Climate W2/W3",
            "Candidate pref W2/W3", "Ideo Strength W2/W3",
            "Total Exp W1/W2 (log)", "Interest W2/W3"),
          override.ci.low = list(boot.model3[,2], boot.model4[,2]),
          override.ci.up = list(boot.model3[,3], boot.model4[,3]),
          reorder.coef = c(2:6,8:9, 14,7, 10:13, 1),
          groups = list("Actual behavior" = 1,
                        "Social desirability / Norm" = 2,
                        "Opinion Climate" = 3,
                        "Motivations / Abilities" = 4:7,
                        "Controls/Demographics" = 8:12),
          custom.note = "* = 0 outside the 95% bootstrap Confidence Intervals")
