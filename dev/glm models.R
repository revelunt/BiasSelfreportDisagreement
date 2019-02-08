

cleaned.data[, accuracy.W2 :=
               round(dangerous.disc.prcptn.W2 - dangerous.disc.W1.prop, digits = -1)/10]
cleaned.data[, accuracy.cat.W2 := car::recode(accuracy.W2,
               "2:hi = 1; lo:-1 = -1; else = 0", as.factor = T)]
cleaned.data[, table(accuracy.W2)]
cleaned.data[, table(accuracy.cat.W2)]

cleaned.data[, accuracy.W3 :=
               round(dangerous.disc.prcptn.W3 - dangerous.disc.W2.prop, digits = -1)]
cleaned.data[, accuracy.cat.W3 := car::recode(accuracy.W3,
               "20:hi = 1; else = 0", as.factor = T)]
cleaned.data[, accuracy.W3]

test1 <- glm(accuracy.W2 ~ log.raw_sum.W1.wavesum +
    ## discussion motivation
    discussion.norm.W2 +
    ## consistency.motivation + understanding.motivation +
    ## demographic controls
    age.years + female + edu + household.income +
    ## political correlates
    ideo.W2.c + pol.interest.W2 + log.ego.netsize.W1.c + pol.know +
    ## media exposure
    media.exposure.W2,
    family = binomial("logit"),
  data = cleaned.data)

test2 <- glm(accuracy.W3 ~ log.raw_sum.W2.wavesum +
              ## discussion motivation
              discussion.norm.W2 +
              ## consistency.motivation + understanding.motivation +
              ## demographic controls
              age.years + female + edu + household.income +
              ## political correlates
              ideo.W2.c + pol.interest.W2 + log.ego.netsize.W2.c + pol.know +
              ## media exposure
              media.exposure.W2,
            family = binomial("logit"),
            data = cleaned.data)

require(texreg)
screenreg(list(test1, test2))






