
## Bayesian estimation using brms

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
