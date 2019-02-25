
## For simulation inference
require(MASS)
require(data.table)
require(psych)
require(lm.beta)
require(corpcor)

var.names <- unique(c(names(coef(model.certainty.1)),
                 names(coef(model.certainty.2))))
var.names <- c(var.names[-1])
cor.obs <- cleaned.data[, cor(.SD, use = "pairwise.complete.obs"),
                          .SDcols = var.names]
mu <- cleaned.data[, apply(.SD, 2, mean, na.rm = T), .SDcols = var.names]
focal.vars <- c("dangerous.disc.W2", "dangerous.disc.prcptn.W3")


get.new.cor <- function(cor.obs, target.corr) {
  partial.cor <- corpcor::cor2pcor(cor.obs)
  colnames(partial.cor) <- rownames(partial.cor) <- var.names
  partial.cor["dangerous.disc.W2", "dangerous.disc.prcptn.W3"] <- target.corr
  partial.cor["dangerous.disc.prcptn.W3", "dangerous.disc.W2"] <- target.corr
  new.cor <- corpcor::pcor2cor(partial.cor)
  colnames(new.cor) <- rownames(new.cor) <- var.names
  new.cor
}



## define main simulation function
sim.MC <- function(N.sample, target.corr) {

  sim.run <- mclapply(seq_len(1000), function(k) { ## based on 5000 replications
  ## simulate the data given N based on mean and cov of exogenous variables
    ## simulate a data frame
    simulated.data <- mvrnorm(n = N.sample,
                              mu = mu,
                              Sigma = get.new.cor(cor.obs, target.corr),
                              empirical = TRUE)
    colnames(simulated.data) <- var.names
    simulated.data <- as.data.frame(simulated.data) %>% setDT(.)

    ## create pref.certainty.W3 based on model.certainty.2
    simulated.data[, pref.certainty.W3 :=
                     predict(model.certainty.2, newdata = simulated.data) +
                     rnorm(.N, mean = 0,
                           sd = sd(model.certainty.2$residuals))]


    ## Using simulated data, refit the model, recover coefficients
    model.sbj.resample <- lm(pref.certainty.W3 ~ pref.certainty.W2 +
                               dangerous.disc.prcptn.W3 + log.total.exp.W2 +
                               age.years + female + edu + household.income +
                               canpref.W2 + pol.interest.W2 + pol.know +
                               ideo_str.W2 + internal.efficacy.W3 +
                               media.exposure.W2,
                             data = simulated.data) ## DV = certainty
    model.obj.resample <- lm(pref.certainty.W3 ~ pref.certainty.W2 +
                               dangerous.disc.W2 +log.total.exp.W2 +
                               age.years + female + edu + household.income +
                               canpref.W2 + pol.interest.W2 + pol.know +
                               ideo_str.W2 + internal.efficacy.W3 +
                               media.exposure.W2,
                             data = simulated.data) ## DV = certainty

    coef.sbj <- coef(summary(lm.beta(model.sbj.resample)))['dangerous.disc.prcptn.W3', 1]
    coef.obj <- coef(summary(lm.beta(model.obj.resample)))['dangerous.disc.W2', 1]

    sig.sbj <- coef(summary(model.sbj.resample))['dangerous.disc.prcptn.W3', 4] < .05
    sig.obj <- coef(summary(model.obj.resample))['dangerous.disc.W2', 4] < .05

    est <- c(N.sample,
             target.corr,
             abs(coef.sbj - coef.obj), ## relative size of bias, abs of difference
             coef.sbj/coef.obj, ## relative size of subjective coef
             coef.obj, ## size of effect
             sig.obj, ## true effect significant?
             sig.obj == sig.sbj) ## whether results agree?
    names(est) <- c("N.sample", "target.corr", "bias", "relative.size.sbj",
                    "coef.obj", "coef.obj.sig", "sbj.coef.agree.with.obj")
    est

  }, mc.cores = parallel::detectCores(T))

  ## gather replication results
  sim.run <- do.call("rbind", sim.run)
  sim.run <- data.frame(sim.run) %>% setDT(.)
  sim.run
}

  cond <- expand.grid(
    N.sample = c(341, 1000, 5000),
    target.corr = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6)
  )

  reps <- mapply(sim.MC,
                 N.sample = cond$N.sample,
                 target.corr = cond$target.corr,
                 SIMPLIFY = FALSE)

sim.results <- do.call("rbind", reps) %>% setDT(.)

# sim.results[, zero.order.cor := sapply(target.corr, mod.zero.r)]
