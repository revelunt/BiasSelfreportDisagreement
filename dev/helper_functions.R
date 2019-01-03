
## ---------------- #
## Helper functions #
## ---------------- #

## ---------------------- ##
## 1. Data prep functions ##
## ---------------------- ##

## function to calculate exposure to disagreement
## based on ego's and alters' candidate preferences

cal.exposure.disagree <- function(net, canpref, prop = TRUE) {

  out <- t(sapply(1:341, function(x) {
    tempvec <- net[x, ]
    exp.same <- canpref[x] == canpref
    safe.disc <- sum(tempvec[exp.same])
    dangerous.disc <- sum(tempvec[!exp.same])

    if (isTRUE(prop)) {
      safe.disc <- safe.disc/sum(tempvec)
      dangerous.disc <- dangerous.disc/sum(tempvec)
      if (is.nan(safe.disc)) safe.disc <- 0  ## no exposure
      if (is.nan(dangerous.disc)) dangerous.disc <- 0 ## no exposure
    }

    ## output: two-column matrix of safe.disc & dangerous.disc
    return(c(safe.disc = safe.disc, dangerous.disc = dangerous.disc))
  }))

  rownames(out) <- 1:341
  return(out)
}


## function to perform bivariate permutation test
## which produces test statistics of a given choice
## function (FUN) must accept two variable names as the arguments (e.g., cor)
bivariate.perm.test <- function(data, var1, var2, FUN,
                                 rep = 1000, probs = 0.95) {

  if (!('data.table' %in% class(data))) stop("data must be data.table type")
  if (is.null(probs)) probs = 0.95
  alpha1 <- (1 - probs)/2
  alpha2 <- 1 - alpha1

  var1 <- data[, get(var1)]
  var2 <- data[, get(var2)]

  FUN <- match.fun(FUN)
  obs <- FUN(var1, var2)

  perm <- sort(sapply(seq_len(rep), function(x) {
    var2.resample <- sample(var2, size = length(var2), replace = TRUE)
    FUN(var1, var2.resample)
  }))

  CIs <- quantile(perm, c(alpha1, alpha2))
  out <- c(obs, CIs[1], CIs[2])
  names(out) <- c("obs", paste('llci', alpha1, sep = "."), paste('ulci', alpha2, sep = "."))

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
  diff.obs.mean <- mean(diff.obs)

  perm <- sort(sapply(seq_len(rep), function(x) {
    var2.resample <- sample(var2, size = length(var2), replace = TRUE)
    diff.perm <- (var1 - var2.resample)
    mean(diff.perm)
  }))

  CIs <- quantile(perm, c(alpha1, alpha2))
  out <- c(diff.obs.mean, CIs[1], CIs[2])
  names(out) <- c("diff.obs", paste('llci', alpha1, sep = "."), paste('ulci', alpha2, sep = "."))

  return(out)
}


## function to process the data.frame adding demographic covariates

add.demographics <- function(data.to.process) {

  if (!('dat' %in% ls(envir = .GlobalEnv))) stop("Plz load the raw data in the GlobalEnv!")
  if (!('data.table' %in% class(data.to.process))) stop("data must be data.table type!")

  data.copy <- data.to.process
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

  ## return data.copy
  data.copy
}






## --------------------- ##
## 2. Analysis functions ##
## --------------------- ##

estimate.unconditional.indirect <- function(dat, i) {
  ## i is the index value for resample
  resample <- dat[i,]
  model.M.resample <- lm(formula(model.M1), data = resample)
  model.Y.resample <- lm(formula(model.Y), data = resample)

  a1 <- summary(model.M.resample)$coef['safe.disc.W1',1]
  a2 <- summary(model.M.resample)$coef['dangerous.disc.W1',1]

  b1 <- summary(model.Y.resample)$coef['perceived.opinion.climate.W2',1]
  c1 <- summary(model.Y.resample)$coef['safe.disc.W1',1]
  c2 <- summary(model.Y.resample)$coef['dangerous.disc.W1',1]

  ## effect decomposition ##
  ## unconditional indirect effect
  ## safe.exposure -> opinion climate -> self-reported exposure to disagree
  ind.safe.opi.climate <- a1*b1
  ## dangerous.exposure -> opinion climate -> self-reported exposure to disagree
  ind.dangerous.exposure.opi.climate <- a2*b1

  return.vec <-
    ## unconditional indirect effect
    c(ind.safe.opi.climate = ind.safe.opi.climate,
      ind.dangerous.exposure.opi.climate = ind.dangerous.exposure.opi.climate,
      direct.safe = c1,
      direct.dangerous = c2)

  # return indirect effects
  return(return.vec)
}

#### TO BE CONTINUTED -- revise based on moderated moderated mediation
#### ADD index of moderated moderated mediation

estimate.conditional.indirect <- function(dat, i) {
  ## i is the index value for resample
  resample <- dat[i,]
  model.M2.resample <- lm(formula(model.M2), data = resample)
  model.Y.resample <- lm(formula(model.Y), data = resample)

  a1 <- summary(model.M2.resample)$coef['safe.disc.W1',1]
  a2_1 <- summary(model.M2.resample)$coef['dangerous.disc.W1',1]
  # a2_2 <- summary(model.M2.resample)$coef['ideo_str.W2',1]
  a2_3 <- summary(model.M2.resample)$coef['dangerous.disc.W1:ideo_str.W2',1]

  b1 <- summary(model.Y.resample)$coef['perceived.opinion.climate.W2',1]
  c1 <- summary(model.Y.resample)$coef['safe.disc.W1',1]
  c2 <- summary(model.Y.resample)$coef['dangerous.disc.W1',1]

  ## effect decomposition ##
  ## safe.exposure -> opinion climate -> self-reported exposure to disagree
  ind.safe.opi.climate <- a1*b1
  ## index of moderated mediation
  ## dangerous.exposure -> opinion climate -> self-reported exposure to disagree
  ## moderated by ideo_strength
  indexModMed <- a2_3*b1
  ## conditional indirect effects at every values of ideo_strength
  cond.ind <- (a2_1 + c(0:3)*a2_3)*b1

  return.vec <-
    ## unconditional indirect effect
    c(ind.safe.opi.climate = ind.safe.opi.climate,
      indexModMed = indexModMed,
      cond.ind = cond.ind,
      direct.safe = c1,
      direct.dangerous = c2)

  # return indirect effects
  return(return.vec)
}


## function to extract point estimates and 95% CIs from boot object
## and convert to data.frame object

get.boot.stats <- function(boot.out, type = "bca", ...) {
  ## identify dimension of the data
  n.now <- length(boot.out$t0)
  ## get CIs (default by bca, )
  require(parallel)
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
