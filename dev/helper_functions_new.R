
## ---------------- #
## Helper functions #
## ---------------- #

## ---------------------- ##
## 1. Data prep functions ##
## ---------------------- ##


## function to perform bivariate permutation test for correlation
bivariate.perm.test <- function(data, var1, var2,
                                 rep = 1000, probs = 0.95) {

  if (!('data.table' %in% class(data))) stop("data must be data.table type")
  if (is.null(probs)) probs = 0.95
  alpha1 <- (1 - probs)/2
  alpha2 <- 1 - alpha1

  var1 <- data[, get(var1)]
  var2 <- data[, get(var2)]

  cor.obs <- cor(var1, var2, use = "complete.obs")

  cor.perm <- sort(sapply(seq_len(rep), function(x) {
    var2.resample <- sample(var2, size = length(var2), replace = FALSE)
    cor.perm <- cor(var1, var2.resample, use = "complete.obs")
    cor.obs - cor.perm
  }))

  median.perm <- median(cor.perm)
  CIs <- quantile(cor.perm, c(alpha1, alpha2))
  out <- c(median.perm,  CIs[1], CIs[2])
  names(out) <- c("obs.minus.perm", paste('llci', alpha1, sep = "."), paste('ulci', alpha2, sep = "."))

  return(out)
}

## function to perform permutation test for difference scores of two variables
diff.perm.test <- function(data, var1, var2,
                           rep = 1000, probs = 0.95) {

  if (!('data.table' %in% class(data))) stop("data must be data.table type")
  if (is.null(probs)) probs = 0.95
  alpha1 <- (1 - probs)/2
  alpha2 <- 1 - alpha1

  var1 <- data[, get(var1)]
  var2 <- data[, get(var2)]

  diff.obs <- (var1 - var2)
  diff.obs.m <- mean(diff.obs)

  ## functions taken from wPerm
  perm <- sort(sapply(seq_len(rep), function(x) {
    b <- rbinom(length(var2), 1, 0.5)
    var1.resample <- b * var1 + (1 - b) * var2
    var2.resample <- (1 - b) * var1 + b * var2
    diff.perm <- (var1.resample - var2.resample)
    mean(diff.perm)
  }))

  CIs <- quantile(diff.obs.m - perm, c(alpha1, alpha2))
  out <- c(diff.obs.m, CIs[1], CIs[2])
  names(out) <- c("diff.obs", paste('llci', alpha1, sep = "."), paste('ulci', alpha2, sep = "."))

  return(out)
}


## function to process the data.frame adding demographic covariates

add.demographics <- function(data.to.process, dat) {

  if (!('dat' %in% ls(envir = .GlobalEnv))) stop("Plz load the raw data in the GlobalEnv!")
  if (!('data.table' %in% class(data.to.process))) stop("data must be data.table type!")

  data.copy <- data.to.process
  ## in wave 1, there are few Rs who did not indicated their initial candidate pref.
  ## we need to impute their preference using feeling thermo ratings
  dat[is.na(canpref1), pv254 - pv255] ## all zero
  ## or stated "would-be candidate" they might vote for Qs
  dat[, canpref1.imputed := canpref1]
  dat[is.na(canpref1), canpref1.imputed := car::recode(kv2, "1 = 0; 2 = 1; else = 2")]
  #dat[, table(canpref1, canpref1.imputed, exclude = NULL)]

  ## candidate preference (0 = conservative, 1 = liberal)
  data.copy[, canpref.W1 := dat[, canpref1.imputed]]
  data.copy[, canpref.W2 := dat[, canpref2]]
  data.copy[, canpref.W3 := dat[, canpref3]]
  ## age in years
  data.copy[, age.years := dat[, age]]
  ## gender (female = 1, male = 0)
  data.copy[, female := (dat[, sex]) - 1]
  ## education (ordinal, 1 = 'less than elementary' to 9 = 'post-graduate or more')
  ## with '5' (high school graduate) being the middle point
  data.copy[, edu := dat[, edu]]
  ## household income (ordinal, 1 to 8)
  data.copy[, household.income := dat[, income]]
  ## residential region
  data.copy[, residential.region := dat[, residence]]
  ## political knowledge
  data.copy[, pol.know :=
  dat[, .(kn1 = car::recode(pv177, "2 = 1; else = 0"), ## 18th Presidential election
          kn2 = car::recode(pv178, "1 = 1; else = 0"), ## Park-Oh is not the right answer
          kn3 = car::recode(pv179, "2 = 1; else = 0"), ## Abolition of pension for Ntnl Assembly
          kn4 = car::recode(pv180, "2 = 1; else = 0"), ## LEGISLATION & JUDICIARY COMMITTEE
          kn5 = car::recode(pv181, "4 = 1; else = 0"),
          kn6 = car::recode(pv182, "3 = 1; else = 0"),
          kn7 = car::recode(pv183, "4 = 1; else = 0"),
          kn8 = car::recode(pv184, "5 = 1; else = 0"),
          kn9 = car::recode(pv185, "2 = 1; else = 0"),
          kn10 = car::recode(pv185_1, "4 = 1; else = 0"))] %>%
    as.matrix(.) %>% #psych::alpha(, check.keys = T) ## alpha is .64, yet polkn is not scale..
    rowSums(.)]
  ## return data.copy
  data.copy
}

## ego centrality function

## a set of functions to get sum of connected alters' centrality
## in calculating sum of <<alters'>> centrality scores,
## measures are  weighted by the number of ego's choice (exposure)

## --------------------- ##
## 2. Analysis functions ##
## --------------------- ##

## print standardized beta
require(sjstats)
std.lm <- function(lm.model) {
  sjstats::std_beta(lm.model,  type = "std2")
}



require(formula.tools)
est.uncond.indirect <- function(dat, ## data frame to pass for bootstrapping
                                i, ## i = the index value for resample
                                lm.model.M, ## lm object estimating M
                                lm.model.Y, ## lm object estimating Y
                                pred, ## X (can accept multiple variables)
                                m.name = NULL) {  ## mediator name

  ## extract mediator automatically from the model of M if not provided
  if (is.null(m.name)) m.name <- formula.tools::lhs.vars(formula(lm.model.M))

  ## check mediator is indeed included in the model of Y
  check <- !(m.name %in% formula.tools::rhs.vars(formula(lm.model.Y)))
  if (isTRUE(check)) stop("lm.model.Y does not have proper mediator variable as in lm.model.M!")

  resample <- dat[i,]
  model.M.resample <- lm(formula(lm.model.M), data = resample)
  model.Y.resample <- lm(formula(lm.model.Y), data = resample)

  a <- summary(model.M.resample)$coef[pred, 1]
  b <- summary(model.Y.resample)$coef[m.name, 1]
  c <- summary(model.Y.resample)$coef[pred, 1]

  ## effect decomposition ##
  ## unconditional indirect effect
  ind.effect <- a*b
  ## unconditional direct effect
  dir.effect <- c

  # return effect decomposition
  return.vec <- c(ind.effect = ind.effect, dir.effect = dir.effect)
  return(return.vec)
}


## function to extract point estimates and 95% CIs from boot object
## and convert to data.frame object

get.boot.stats <- function(boot.out, type = "bca", ...) {
  ## identify dimension of the data
  n.now <- length(boot.out$t0)
  ## get CIs (default by bca, )
  require(parallel)
  require(boot)
  temp <- mclapply(1:n.now, function(i) {
    if (type == "perc") {
      cis <- boot.ci(boot.out, index = i, type = type, ...)
      cis[['percent']][4:5]
    } else {
      cis <- boot.ci(boot.out, index = i, type = type, ...)
      cis[[type]][4:5]
    }
  }, mc.cores = parallel::detectCores())

  ## collect stats and rename row/columns
  dat.out <- cbind(boot.out$t0, do.call("rbind", temp))
  dat.out <- as.data.frame(dat.out)
  rownames(dat.out) <- names(boot.out$t0)
  colnames(dat.out) <- c("coef", "llci", "ulci")

  dat.out
}

## function to find JN point of transition
# jnt <- function(.lm, pred, modx, alpha=.05) {
#   require(stringi)
#   b1 = coef(.lm)[pred]
#   b3 = coef(.lm)[stri_startswith_fixed(names(coef(.lm)), paste0(pred,":")) | stri_endswith_fixed(names(coef(.lm)), paste0(":",pred))]
#   se_b1 = coef(summary(.lm))[pred, 2]
#   se_b3 = coef(summary(.lm))[stri_startswith_fixed(names(coef(.lm)), paste0(pred,":")) | stri_endswith_fixed(names(coef(.lm)), paste0(":",pred)), 2]
#   COV_b1b3 = vcov(.lm)[pred, stri_startswith_fixed(names(coef(.lm)), paste0(pred,":")) | stri_endswith_fixed(names(coef(.lm)), paste0(":",pred))]
#   t_crit = qt(1-alpha/2, .lm$df.residual)
#   # see Bauer & Curran, 2005
#   a = t_crit^2 * se_b3^2 - b3^2
#   b = 2 * (t_crit^2 * COV_b1b3 - b1 * b3)
#   c = t_crit^2 * se_b1^2 - b1^2
#   jn = c(
#     (-b - sqrt(b^2 - 4 * a * c)) / (2 * a),
#     (-b + sqrt(b^2 - 4 * a * c)) / (2 * a)
#   )
#   JN = sort(unname(jn))
#   JN = JN[JN>=min(.lm$model[,modx]) & JN<=max(.lm$model[,modx])]
#   JN
# }

## 'not in' function
`%!in%` <- function (x, table) is.na(match(x, table, nomatch=NA_integer_))

## helper for automatically create sequence of mod val
select.modval <- function(dat, modx) {

  ## 'dat' expects data.table format
  ## 'modx' is the character of moderator in the model

  mod.val <- dat[, get(modx)]
  mod.val <- dat[, seq(from = range(mod.val)[1],
                       to = range(mod.val)[2],
                       ## automatically select 'by' arguments
                       ## 100 percentile interval vs. 0.01
                       by = min(0.01, diff(range(mod.val))/100))]
  mod.val <- unique(round(mod.val, digits = 2))
  mod.val
}

## mean/sd function
descriptives <- function(var) {
  m <- mean(var, na.rm = T)
  sd <- sd(var, na.rm = T)
  return(c(M = m, SD = sd))
}



## estimate conditional effects
est.cond.indirect <- function(dat, ## data frame to pass for bootstrapping
                              i, ## i = the index value for resample
                              lm.model.M, ## lm object estimating M
                              lm.model.Y, ## lm object estimating Y
                              pred, ## focal predictor, X (CANNOT accept multiple variables)
                              med = NULL,
                              modx) { ## moderator, only one at a time

  if (is.null(med)) {
  ## extract mediator automatically from the model of M
    m.name <- formula.tools::lhs.vars(formula(lm.model.M))
  } else {
    m.name <- med
  }

  ## check mediator is indeed included in the model of Y
  check <- !(m.name %in% formula.tools::rhs.vars(formula(lm.model.Y)))
  if (isTRUE(check)) stop("lm.model.Y does not have proper mediator variable as in lm.model.M!")

  resample <- dat[i,]
  model.M.resample <- lm(formula(lm.model.M), data = resample)
  model.Y.resample <- lm(formula(lm.model.Y), data = resample)

  interact_term1 <- paste(pred, modx, sep = ":")
  interact_term2 <- paste(modx, pred, sep = ":")

  select <- c(interact_term1, interact_term2) %in% names(coef(lm.model.M))
  interact_term <- c(interact_term1, interact_term2)[select]

  a1 <- summary(model.M.resample)$coef[pred, 1]
  a2 <- summary(model.M.resample)$coef[modx, 1]
  a3 <- summary(model.M.resample)$coef[interact_term, 1]
  b <- summary(model.Y.resample)$coef[m.name, 1]
  c1 <- summary(model.Y.resample)$coef[pred, 1]

  ## effect decomposition ##
  ## index of moderated mediation
  index.modmed <- a3*b

  ## conditional indirect effect at values of moderator
  mod.val <- select.modval(dat, modx)
  ind.effect <- (a1 + mod.val*a3)*b
  dir.effect <- c1

  return.vec <- c(index.modmed = index.modmed,
                  ind.effect = ind.effect,
                  dir.effect = dir.effect)
  return(return.vec)
}



## simulation functions
get.new.cor <- function(cor.obs, target.corr) {
  partial.cor <- corpcor::cor2pcor(cor.obs)
  colnames(partial.cor) <- rownames(partial.cor) <- var.names
  partial.cor["dangerous.disc.W2", "dangerous.disc.prcptn.W3"] <- target.corr
  partial.cor["dangerous.disc.prcptn.W3", "dangerous.disc.W2"] <- target.corr
  new.cor <- corpcor::pcor2cor(partial.cor)
  colnames(new.cor) <- rownames(new.cor) <- var.names
  new.cor
}

get.new.bivariate.cor <- function(target.corr) {
  new.cor <- get.new.cor(cor.obs = cor.obs, target.corr)
  new.bivariate.cor <- new.cor["dangerous.disc.prcptn.W3", "dangerous.disc.W2"]
  new.bivariate.cor
}


## define main simulation function
sim.MC <- function(N.sample, target.corr) {

  sim.run <- mclapply(seq_len(1000), function(k) { ## based on 5000 replications
    ## simulate the data given N based on mean and cov of exogenous variables
    ## and manipulate partial correlation of subjective vs. objective measure,
    ## derive modified zero-order correlation coef. and simulate dataset
    simulated.data <- mvrnorm(n = N.sample,
                              mu = mu,
                              Sigma = get.new.cor(cor.obs, target.corr),
                              empirical = TRUE)
    colnames(simulated.data) <- var.names
    simulated.data <- as.data.frame(simulated.data) %>% setDT(.)

    ## create pref.certainty.W3 based on model using obj measure
    ## and add random noise to simulate Y values
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

    ## grap standarzied coefficietns and their significance
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


