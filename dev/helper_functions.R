
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
  dat <- cbind(var1, var2) %>% as.data.frame
  dat <- na.omit(dat)

  diff.obs <- (dat$var1 - dat$var2)
  diff.obs.m <- mean(diff.obs)

  ## functions taken from wPerm
  perm <- sort(sapply(seq_len(rep), function(x) {
    b <- rbinom(length(dat$var2), 1, 0.5)
    var1.resample <- b * dat$var1 + (1 - b) * dat$var2
    var2.resample <- (1 - b) * dat$var1 + b * dat$var2
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
# require(sjstats)
# std.lm <- function(lm.model) {
#   sjstats::std_beta(lm.model,  type = "std2")
# }

boot.lm <- function(lm.fit, dat, i) {
  resample <- dat[i,]
  refit <- lm(formula(lm.fit), data = resample)
  coef(refit)
}

boot.glm <- function(glm.fit, dat, i) {
  resample <- dat[i,]
  refit <- glm(formula(glm.fit), family = binomial("logit"), data = resample)
  coef(refit)
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

# ## getting online tally by employing moving difference
# online.tally <- function(x) {
#
#   ## find first nonzero value in series
#   prior <- x[x > 0][1]
#   if (is.na(prior)) {
#     final.tally <- 0
#   } else {
#   ## retrieve location
#   index <- which(x %in% prior)[1]
#   ## form prior-relative exposure tally
#   tally <- x - prior
#   tally <- tally[-c(1:index)]
#   ## sum tally only after date of first prior
#     online.tally <- sum(tally, na.rm = T)
#     final.tally <- prior + online.tally
#   }
#   return(final.tally)
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
  range <- range(var, na.rm = T)
  des <- c(m, sd, range)
  names(des) <- c("M", "SD", "range.low", "range.high")
  return(des)
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
## define MC power function
sim.MC.power.obj <- function(sample_n, data = dat.subset1) {

  mu0 <- data[, apply(.SD, 2, mean, na.rm = T), .SDcols = variables]
  vars <- data[, apply(.SD, 2, var, na.rm = T), .SDcols = variables]
  sigma <- cor(data[, .SD, .SDcols = variables], use = "complete.obs")

  sim.run <- mclapply(seq_len(5000), function(k) { ## based on 5000 replications
    ## simulate the data given N based on mean and cov of exogenous variables
    ## and manipulate partial correlation of subjective vs. objective measure,
    ## derive modified zero-order correlation coef. and simulate dataset

    simulated.data <- BinNor::jointly.generate.binary.normal(no.rows = sample_n, no.bin = 2,
                                           no.nor = 12,
                                           prop.vec.bin = mu0[c(7,11)], # "female" & "canpref.W2"
                                           mean.vec.nor = mu0[-c(7, 11)],
                                           var.nor = vars[-c(7, 11)],
                                           sigma_star = sigma[c(7,11,1:6,8:10,12:14), c(7,11,1:6,8:10,12:14)]
                                           )
    colnames(simulated.data) <- names(mu0)[c(7,11,1:6,8:10,12:14)]
    simulated.data <- as.data.frame(simulated.data) %>% setDT(.)

    ## create pref.certainty.W3 based on model using obj measure
    ## and add random noise to simulate Y values
    simulated.data[, ambi.W3 :=
                     predict(model.ambi.2, newdata = simulated.data) +
                     rnorm(.N, mean = 0, sd = sd(model.ambi.2$residuals))]
    ## Using simulated data, refit the model, recover coefficients
    model.obj.resample <- lm(formula(model.ambi.2),
                             data = simulated.data) ## DV = certainty

    ## grap standarzied coefficietns and their significance
    coef <- coef(summary(model.obj.resample))['dangerous.disc.W2.candpref:canpref.W2', 1]
    sig <- coef(summary(model.obj.resample))['dangerous.disc.W2.candpref:canpref.W2', 4]

    est <- c(sample_n = sample_n, coef = coef, sig = sig)
    est

  }, mc.cores = parallel::detectCores(T))

  ## gather replication results
  sim.run <- do.call("rbind", sim.run)
  sim.run <- data.frame(sim.run) %>% setDT(.)
  sim.run
}

sim.MC.power.sbj <- function(sample_n, data = dat.subset1) {

  mu0 <- data[, apply(.SD, 2, mean, na.rm = T), .SDcols = variables]
  vars <- data[, apply(.SD, 2, var, na.rm = T), .SDcols = variables]
  sigma <- cor(data[, .SD, .SDcols = variables], use = "complete.obs")

  sim.run <- mclapply(seq_len(5000), function(k) { ## based on 1000 replications
    ## simulate the data given N based on mean and cov of exogenous variables
    ## and manipulate partial correlation of subjective vs. objective measure,
    ## derive modified zero-order correlation coef. and simulate dataset
    simulated.data <- BinNor::jointly.generate.binary.normal(no.rows = sample_n, no.bin = 2,
                                                             no.nor = 12,
                                                             prop.vec.bin = mu0[c(7,11)], # "female" & "canpref.W2"
                                                             mean.vec.nor = mu0[-c(7, 11)],
                                                             var.nor = vars[-c(7, 11)],
                                                             sigma_star = sigma[c(7,11,1:6,8:10,12:14), c(7,11,1:6,8:10,12:14)]
    )
    colnames(simulated.data) <- names(mu0)[c(7,11,1:6,8:10,12:14)]
    simulated.data <- as.data.frame(simulated.data) %>% setDT(.)


    ## create pref.certainty.W3 based on model using obj measure
    ## and add random noise to simulate Y values
    simulated.data[, ambi.W3 :=
                     predict(model.ambi.1, newdata = simulated.data) +
                     rnorm(.N, mean = 0, sd = sd(model.ambi.1$residuals))]
    ## Using simulated data, refit the model, recover coefficients
    model.sbj.resample <- lm(formula(model.ambi.1),
                             data = simulated.data) ## DV = certainty

    ## grap standarzied coefficietns and their significance
    coef <- coef(summary(model.sbj.resample))["dangerous.disc.prcptn.W3.candpref:canpref.W2", 1]
    sig <- coef(summary(model.sbj.resample))["dangerous.disc.prcptn.W3.candpref:canpref.W2", 4]

    est <- c(sample_n = sample_n, coef = coef, sig = sig)
    est

  }, mc.cores = parallel::detectCores(T))

  ## gather replication results
  sim.run <- do.call("rbind", sim.run)
  sim.run <- data.frame(sim.run) %>% setDT(.)
  sim.run
}


## define main simulation function
sim.MC <- function(sample_n, target.corr, n_reps,
                   data = dat.subset1, model_vars = variables) {

  ## Manipulate correlation of subjective vs. objective measure,
  ## derive modified covariance matrix
  model_vars <- model_vars[model_vars != "ambi.W3"]
  mu0 <- data[, apply(.SD, 2, mean, na.rm = T), .SDcols = model_vars]
  vars <- data[, apply(.SD, 2, var, na.rm = T), .SDcols = model_vars]
  sigma <- cor(data[, .SD, .SDcols = model_vars], use = "complete.obs")

  sigma_star <- sigma
  sigma_star["dangerous.disc.prcptn.W3.candpref", "dangerous.disc.W2.candpref"] <- target.corr
  sigma_star["dangerous.disc.W2.candpref", "dangerous.disc.prcptn.W3.candpref"] <- target.corr

  if (sample_n == 341) {
    alpha = 0.05
  } else {
    alpha = 0.01
  }

  sim.run <- mclapply(seq_len(n_reps), function(k) { ## based on "n_reps" no. of replications (e.g., 1000)
    ## simulate the data given N based on mean and cov from manipulated covmat
    simulated.data <- BinNor::jointly.generate.binary.normal(no.rows = sample_n,
                                no.bin = 2, no.nor = 11, prop.vec.bin = mu0[c(6,10)], # "female" & "canpref.W2"
                                mean.vec.nor = mu0[-c(6,10)], var.nor = vars[-c(6,10)],
                                sigma_star = sigma_star[c(6,10,1:5,7:9,11:13), c(6,10,1:5,7:9,11:13)]
    )
    colnames(simulated.data) <- names(mu0)[c(6,10,1:5,7:9,11:13)]
    simulated.data <- as.data.frame(simulated.data) %>% setDT(.)

    simulated.corr <- simulated.data[, cor(dangerous.disc.prcptn.W3.candpref, dangerous.disc.W2.candpref)]

    # ## create tolerance.W3 based on model using obj measure
    # ## and add random noise to simulate Y values
    simulated.data[, ambi.W3 :=
                     predict(model.ambi.2, newdata = simulated.data) +
                     rnorm(.N, mean = 0,
                           sd = sd(model.ambi.2$residuals))]

    ## Using simulated data, refit the model, recover coefficients
    model.sbj.resample <- lm(formula(model.ambi.1),
                             data = data.frame(simulated.data)) ## DV = ambivalence
    model.obj.resample <- lm(formula(model.ambi.2),
                             data = data.frame(simulated.data)) ## DV = ambivalence

    ## get unstandarzied coefficients and their significance levels
    coef.sbj <- coef(model.sbj.resample)["dangerous.disc.prcptn.W3.candpref:canpref.W2"]
    coef.obj <- coef(model.obj.resample)["dangerous.disc.W2.candpref:canpref.W2"]

    sim.slopes.sbj <- interactions::sim_slopes(model.sbj.resample, dangerous.disc.prcptn.W3.candpref, canpref.W2)
    sim.slopes.obj <- interactions::sim_slopes(model.obj.resample, dangerous.disc.W2.candpref, canpref.W2)

    cond.eff.sbj <- sim.slopes.sbj$slopes[2,2]
    cond.eff.obj <- sim.slopes.obj$slopes[2,2]

    ## smaller than alpha?
    sig.sbj <- coef(summary(model.sbj.resample))['dangerous.disc.prcptn.W3.candpref:canpref.W2', 4]
    sig.obj <- coef(summary(model.obj.resample))['dangerous.disc.W2.candpref:canpref.W2', 4]

    cond.eff.sig.sbj <- sim.slopes.sbj$slopes[2,7]
    cond.eff.sig.obj <- sim.slopes.obj$slopes[2,7]

    est <- c(sample_n = sample_n,
             target.corr = target.corr,
             simulated.corr = simulated.corr,
             coef.sbj = coef.sbj,
             coef.obj = coef.obj, ## size of effect
             cond.eff.sbj = cond.eff.sbj,
             cond.eff.obj = cond.eff.obj,
             sig.sbj = sig.sbj,
             sig.obj = sig.obj, ## true effect significant?
             cond.eff.sig.sbj = cond.eff.sig.sbj,
             cond.eff.sig.obj = cond.eff.sig.obj)

    names(est) <- c("sample_n", "target.corr", "simulated.corr",
                    "coef.sbj", "coef.obj",
                    "cond.eff.sbj", "cond.eff.obj",
                    "sig.sbj" , "sig.obj",
                    "cond.eff.sig.sbj", "cond.eff.sig.obj")
    est

  }, mc.cores = parallel::detectCores(T))

  ## gather replication results
  sim.run <- do.call("rbind", sim.run)
  sim.run <- data.frame(sim.run) %>% setDT(.)
  sim.run
}


prop.cis <- function(prop, n) {
  SE <- sqrt(prop * (1 - prop) / n)
  errors <- qnorm(.975)*SE
  CIs <- prop + c(-1*errors, errors)
  CIs
}


## QQplot function

make_qqplot <- function(data = cleaned.data, type = c("candpref", "ideo3", "ideo2")) {

  if(is.null(data)) data <- cleaned.data

  W2.xvar <- paste("dangerous.disc.W1", type, sep = ".")
  W2.yvar <- paste("dangerous.disc.prcptn.W2", type, sep = ".")
  W3.xvar <- paste("dangerous.disc.W2", type, sep = ".")
  W3.yvar <- paste("dangerous.disc.prcptn.W3", type, sep = ".")

  qq.out2 <- qqplot(x = sapply(data[, W2.xvar, with = F], as.numeric),
                    y = sapply(data[, W2.yvar, with = F], as.numeric),
                    plot.it = FALSE) %>% as.data.frame(.) %>% setDT(.)

  qq.out3 <- qqplot(x = sapply(data[, W3.xvar, with = F], as.numeric),
                    y = sapply(data[, W3.yvar, with = F], as.numeric),
                    plot.it = FALSE) %>% as.data.frame(.) %>% setDT(.)

  qq2 <- ggplot(qq.out2, aes(x = x, y = y)) +
    geom_jitter(width = 0.02, color = "grey") + theme_bw() +
    stat_smooth(aes(group = 1), color = "red", se = FALSE) +
    geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1),
                 lty = 2, color = "grey") +
    xlab("W1 log data") + ylab("W2 Perception") +
    ggtitle("W1 Exposure vs. W2 Perception")

  qq3 <- ggplot(qq.out3, aes(x = x, y = y)) +
    geom_jitter(width = 0.02, color = "grey") + theme_bw() +
    stat_smooth(aes(group = 1), color = "red", se = FALSE) +
    geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1),
                 lty = 2, color = "grey") +
    xlab("W2 log data") + ylab("W3 Perception") +
    ggtitle("W2 Exposure vs. W3 Perception")

  require(patchwork)
  ## figure 1 in the ms
  fig <- qq2 + qq3 + plot_layout(nrow = 1)
  print(fig)

}

## K-S plot function

minmax <- function(xvar, yvar) {

  cdf1 <- ecdf(xvar)
  cdf2 <- ecdf(yvar)
  # find min and max statistics to draw line between points of greatest distance
  minMax <- seq(min(xvar, yvar), max(xvar, yvar),  length.out = length(xvar))
  x0 <- minMax[which( abs(cdf1(minMax) - cdf2(minMax)) == max(abs(cdf1(minMax) - cdf2(minMax))) )]
  y0 <- cdf1(x0)
  y1 <- cdf2(x0)

  minmax <- data.frame(x0 = x0[1], y0 = y0[1], y1 = y1[1])
  print(minmax)

}

make_ks_plot <- function(data = cleaned.data,
                         type = c("candpref", "ideo3", "ideo2")) {

  if(is.null(data)) data <- cleaned.data

  W2.xvar <- paste("dangerous.disc.W1", type, sep = ".")
  W2.yvar <- paste("dangerous.disc.prcptn.W2", type, sep = ".")
  W3.xvar <- paste("dangerous.disc.W2", type, sep = ".")
  W3.yvar <- paste("dangerous.disc.prcptn.W3", type, sep = ".")

  W2.xvar <- sapply(data[, W2.xvar, with = F], as.numeric)
  W2.yvar <- sapply(data[, W2.yvar, with = F], as.numeric)

  test2 <- data.frame(KSD = c(W2.xvar, W2.yvar),
                     group = rep(c("Actual", "Perceived"), each = 341)) %>% setDT

  minmax2 <- minmax(W2.xvar, W2.yvar)

  p2 <- ggplot(test2, aes(x = KSD, group = group, color = group))+
    stat_ecdf(size = 0.7) + theme_bw() +
    theme(legend.position = "none") +
    xlab("Sample") +
    ylab("Empirial CDF") +
    #geom_line(size=1) +
    geom_segment(aes(x = minmax2$x0, y = minmax2$y0, xend = minmax2$x0, yend = minmax2$y1),
                 linetype = "dashed", color = "red") +
    geom_point(aes(x = minmax2$x0 , y = minmax2$y0), color ="red", size = 2) +
    geom_point(aes(x = minmax2$x0 , y = minmax2$y1), color ="red", size = 2) +
    ggtitle("K-S Test: Actual W1 vs. Perceived W2") +
    theme(legend.title = element_blank())

  W3.xvar <- sapply(data[, W3.xvar, with = F], as.numeric)
  W3.yvar <- sapply(data[, W3.yvar, with = F], as.numeric)

  W3.xvar[is.na(W3.xvar)] <- 0
  W3.yvar[is.na(W3.yvar)] <- 0

  test3 <- data.frame(KSD = c(W3.xvar, W3.yvar),
                      group = rep(c("Actual", "Perceived"), each = 341)) %>% setDT

  minmax3 <- minmax(W3.xvar, W3.yvar)

  p3 <- ggplot(test3, aes(x = KSD, group = group, color = group))+
    stat_ecdf(size = 0.7) + theme_bw() +
    theme(legend.position = "bottom") +
    xlab("Sample") +
    ylab("Empirial CDF") +
    #geom_line(size=1) +
    geom_segment(aes(x = minmax3$x0, y = minmax3$y0, xend = minmax3$x0, yend = minmax3$y1),
                 linetype = "dashed", color = "red") +
    geom_point(aes(x = minmax3$x0 , y = minmax3$y0), color ="red", size = 2) +
    geom_point(aes(x = minmax3$x0 , y = minmax3$y1), color ="red", size = 2) +
    ggtitle("K-S Test: Actual W2 vs. Perceived W3") +
    theme(legend.title = element_blank())

  require(patchwork)
  ## figure 1 in the ms
  fig <- p2 + p3 + plot_layout(nrow = 2)
  print(fig)
}




## Mediation via MC simulation
mcmed <- function(model.M, X, model.Y, M, rep = 20000, conf = 95, ...) {

  a <- summary(model.M)$coef[X,1]
  b <- summary(model.Y)$coef[M,1]
  pest <- c(a,b)
  var_a <- (summary(model.M)$coef[X,2])^2
  var_b <- (summary(model.Y)$coef[M,2])^2

  acov <- matrix(c(var_a, 0, 0, var_b),2,2)

  require(MASS)
  mcmc <- mvrnorm(rep, pest, acov, empirical=FALSE)

  ab <- mcmc[,1]*mcmc[,2]
  low <- (1-conf/100)/2
  upp <- ((1-conf/100)/2)+(conf/100)
  LL <- quantile(ab,low)
  UL <- quantile(ab,upp)
  LL4 <- format(LL,digits=4)
  UL4 <- format(UL,digits=4)

  ab <- data.frame(indirect = ab)
  xlab <- paste(conf,'% Monte Carlo Confidence Interval:\n','LL = ',LL4,'  UL = ',UL4)
  require(ggplot2)
  plot_ab <- ggplot(ab, aes(x = indirect)) + geom_histogram(...) +
    theme_bw() + xlab(xlab) + ggtitle("Distribution of Indirect Effect") +
    geom_vline(xintercept = LL, col = "red", lty = 2) +
    geom_vline(xintercept = UL, col = "red", lty = 2) +

  cat(paste(conf,'% Confidence Interval ','LL',LL4,'  UL',UL4))
  print(plot_ab)
  return(ab)

}

require(texreg)
extract.brmsfit <- function (model,
                             use.HDI = TRUE,
                             level = 0.9,
                             include.random = TRUE,
                             include.rsquared = TRUE,
                             include.nobs = TRUE,
                             include.loo.ic = TRUE,
                             reloo = FALSE,
                             include.waic = TRUE,
                             ...) {
  sf <- summary(model, ...)$fixed
  coefnames <- rownames(sf)
  coefs <- sf[, 1]
  se <- sf[, 2]
  if (isTRUE(use.HDI)) {
    hdis <- coda::HPDinterval(brms::as.mcmc(model, prob = level, combine_chains = TRUE))
    hdis <- hdis[seq(1:length(coefnames)), ]
    ci.low = hdis[, "lower"]
    ci.up = hdis[, "upper"]
  } else { # default using 95% posterior quantiles from summary.brmsfit
    ci.low = sf[, 3]
    ci.up = sf[, 4]
  }

  gof <- numeric()
  gof.names <- character()
  gof.decimal <- logical()
  if (isTRUE(include.random) & isFALSE(!nrow(model$ranef))) {
    sr <- summary(model, ...)$random
    sd.names <- character()
    sd.values <- numeric()
    for (i in 1:length(sr)) {
      sd <- sr[[i]][, 1]
      sd.names <- c(sd.names, paste0("SD: ", names(sr)[[i]], names(sd)))
      sd.values <- c(sd.values, sd)
    }
    gof <- c(gof, sd.values)
    gof.names <- c(gof.names, sd.names)
    gof.decimal <- c(gof.decimal, rep(TRUE, length(sd.values)))
  }
  if (isTRUE(include.rsquared)) {
    rs <- brms::bayes_R2(model)[1]
    gof <- c(gof, rs)
    gof.names <- c(gof.names, "R$^2$")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (isTRUE(include.nobs)) {
    n <- stats::nobs(model)
    gof <- c(gof, n)
    gof.names <- c(gof.names, "Num. obs.")
    gof.decimal <- c(gof.decimal, FALSE)
  }
  if (isTRUE(include.loo.ic)) {
    looic <- brms::loo(model, reloo = reloo)$estimates["looic", "Estimate"]
    gof <- c(gof, looic)
    gof.names <- c(gof.names, "loo IC")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (isTRUE(include.waic)) {
    waic <- brms::waic(model)$estimates["waic", "Estimate"]
    gof <- c(gof, waic)
    gof.names <- c(gof.names, "WAIC")
    gof.decimal <- c(gof.decimal, TRUE)
  }

  tr <- createTexreg(coef.names = coefnames,
                     coef = coefs,
                     se = se,
                     ci.low = ci.low,
                     ci.up = ci.up,
                     gof.names = gof.names,
                     gof = gof,
                     gof.decimal = gof.decimal)
  return(tr)
}

setMethod("extract",
          signature = className("brmsfit", "brms"),
          definition = extract.brmsfit)
