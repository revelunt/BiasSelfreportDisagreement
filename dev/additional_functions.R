
## function using bcajack from bootbca package
## this is rather slow, so not being used currently.

test.func <- function(dat, ## data frame to pass for bootstrapping
                              lm.model.M, ## lm object estimating M
                              lm.model.Y, ## lm object estimating Y
                              pred, ## focal predictor, X (CANNOT accept multiple variables)
                              med = NULL,
                              modx,
                              W = W) { ## moderator, only one at a time

  if (is.null(med)) {
    ## extract mediator automatically from the model of M
    m.name <- formula.tools::lhs.vars(formula(lm.model.M))
  } else {
    m.name <- med
  }

  ## check mediator is indeed included in the model of Y
  check <- !(m.name %in% formula.tools::rhs.vars(formula(lm.model.Y)))
  if (isTRUE(check)) stop("lm.model.Y does not have proper mediator variable as in lm.model.M!")

  model.M.resample <- lm(formula(lm.model.M), data = dat)
  model.Y.resample <- lm(formula(lm.model.Y), data = dat)

  interact_term1 <- paste(pred, modx, sep = ":")
  interact_term2 <- paste(modx, pred, sep = ":")

  select <- c(interact_term1, interact_term2) %in% names(coef(lm.model.M))
  interact_term <- c(interact_term1, interact_term2)[select]

  a1 <- summary(model.M.resample)$coef[pred, 1]
  a2 <- summary(model.M.resample)$coef[modx, 1]
  a3 <- summary(model.M.resample)$coef[interact_term, 1]
  b <- summary(model.Y.resample)$coef[m.name, 1]
  c1 <- summary(model.Y.resample)$coef[pred, 1]

  mod.val <- dat[, get(modx)]

  ## effect decomposition ##
  ## index of moderated mediation
  ## index.modmed <- a3*b
  ## conditional indirect effect at given W
  cond.ind.eff <- (a1 + a3*W)*b
  cond.ind.eff
}


est.con.ind.bcajack <- function(dat,
                                B = 5000,
                                lm.model.M,
                                lm.model.Y,
                                pred,
                                modx) {

  mod.val <- select.modval(dat, modx)

  cond.ind.bcajack <- mclapply(seq_along(mod.val), function(i) {
    result <- bcajack(x = dat, B = B, func = test.func, verbose = F,
                      lm.model.M = lm.model.M, lm.model.Y = lm.model.Y,
                      pred = "dangerous.disc.W1",
                      modx = "alter.centr.indeg.W1",
                      W = mod.val[i])

    return.vec <- c(est = result$stats['est', 'theta'],
                    llci = result$lims[1, "bca"],
                    ulci = result$lims[9, "bca"])
    return(return.vec)
  }, mc.cores = parallel::detectCores())

  cond.ind.bcajack <- do.call(rbind, cond.ind.bcajack)
  cond.ind.bcajack <- cbind(mod.val = mod.val, cond.ind.bcajack)

  data.table::setDT(cond.ind.bcajack)
  cond.ind.bcajack
}


test <- est.con.ind.bcajack(dat = cleaned.data,
                            B = 100,
                            lm.model.M = model.M1.int2,
                            lm.model.Y = model.Y,
                            pred = "dangerous.disc.W1",
                            modx = "alter.centr.indeg.W1")
