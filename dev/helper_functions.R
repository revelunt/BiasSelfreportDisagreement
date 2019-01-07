
## ---------------- #
## Helper functions #
## ---------------- #

## ---------------------- ##
## 1. Data prep functions ##
## ---------------------- ##

## function to calculate exposure to disagreement
## based on ego's and alters' candidate preferences

cal.exposure.disagree <- function(net, canpref, prop = FALSE) {

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


## function to perform bivariate permutation test for correlation
bivariate.perm.test <- function(data, var1, var2,
                                 rep = 1000, probs = 0.95) {

  if (!('data.table' %in% class(data))) stop("data must be data.table type")
  if (is.null(probs)) probs = 0.95
  alpha1 <- (1 - probs)/2
  alpha2 <- 1 - alpha1

  var1 <- data[, get(var1)]
  var2 <- data[, get(var2)]

  cor.obs <- cor(var1, var2)

  cor.perm <- sort(sapply(seq_len(rep), function(x) {
    var2.resample <- sample(var2, size = length(var2), replace = FALSE)
    cor.perm <- cor(var1, var2.resample)
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
  diff.obs.mean <- mean(diff.obs)

  perm <- sort(sapply(seq_len(rep), function(x) {
    var2.resample <- sample(var2, size = length(var2), replace = FALSE)
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


## a set of functions to get sum of connected alters' centrality
## in calculating sum of <<alters'>> centrality scores,
## measures are  weighted by the number of ego's choice (exposure)
get.alter.eigen.centr <- function() {

  ## check if "date.range" and "g" are present in the GlobalEnv
  if (!("date.range" %in% ls(envir = .GlobalEnv))) stop("date.range should be present in the GlobalEnv!")
  if (!("g" %in% ls(envir = .GlobalEnv))) stop("network data ('g') should be present in the GlobalEnv!")
  date.range <- get("date.range", envir = .GlobalEnv)
  g <- get("g", envir = .GlobalEnv)

  centr.eigen <- lapply(seq_len(length(date.range)), function(i) {

    ## extract daily exposure network
    ## make tempt network as matrix object through igraph conversion
    net.day.tempt <- net.day <- g[[i]] %>%
      igraph::graph_from_adjacency_matrix(., mode = "directed", weighted = NULL)

    ## for every ego, remove ego from the network and
    ## and calculate indegree of rest of alters
    ## this gives indegree of alters excluding ego's own connection
    alter.centr.eigen <- sapply(seq_len(341), function(v) {

      tempt <- delete_vertices(net.day.tempt, v)
      centr.eigen <- igraph::centr_eigen(tempt, directed = T)
      centr.eigen <- centr.eigen$vector

      index <- c(v, setdiff(1:341, v))
      centr.eigen <- c(0, centr.eigen)[index]

      ## bind with ego's own choice, and make a cross-product
      alter.centr.eigen <- net.day[v,] %*% centr.eigen
      alter.centr.eigen <- sum(alter.centr.eigen)
      alter.centr.eigen
    })

    centr.eigen <- scale(alter.centr.eigen)[,1]
    alter.centr.eigen <- cbind(id = 1:341, alter.centr.eigen = centr.eigen, day = i)
    alter.centr.eigen
  })

  alter.centr.eigen <- do.call(rbind, centr.eigen)
  require(data.table)
  alter.centr.eigen <- setDT(as.data.frame(alter.centr.eigen))
  alter.centr.eigen
}

## ego's own connection to alter is not considered in
## deriving the measure
get.alter.indegree.centr <- function() {

  ## check if "date.range" and "g" are present in the GlobalEnv
  if (!("date.range" %in% ls(envir = .GlobalEnv))) stop("date.range should be present in the GlobalEnv!")
  if (!("g" %in% ls(envir = .GlobalEnv))) stop("network data ('g') should be present in the GlobalEnv!")
  date.range <- get("date.range", envir = .GlobalEnv)
  g <- get("g", envir = .GlobalEnv)

  alter.centr <- lapply(seq_len(length(date.range)), function(i) {

    ## extract daily exposure network
    ## make tempt network as matrix object through igraph conversion
    net.day.tempt <- net.day <- g[[i]] %>%
      igraph::graph_from_adjacency_matrix(., mode = "directed", weighted = NULL)

    ## for every ego, remove ego from the network and
    ## and calculate indegree of rest of alters
    ## this gives indegree of alters exclusing ego's own connection
    alter.centr.ind <- sapply(seq_len(341), function(v) {

      tempt <- delete_vertices(net.day.tempt, v)
      centr.ind <- igraph::centr_degree(tempt, mode = "in", loops = F, normalized = F)
      centr.ind <- centr.ind$res

      index <- c(v, setdiff(1:341, v))
      centr.ind <- c(0, centr.ind)[index]

      ## bind with ego's own choice, and make a cross-product
      alter.centr.ind <- net.day[v,] %*% centr.ind
      alter.centr.ind <- sum(alter.centr.ind)
      alter.centr.ind
    })
    alter.centr.ind <- scale(alter.centr.ind)[,1]
    alter.centr.ind <- cbind(id = 1:341, alter.centr.ind = alter.centr.ind, day = i)
    alter.centr.ind
  })

  alter.centr.ind <- do.call(rbind, alter.centr)
  require(data.table)
  alter.centr.ind <- setDT(as.data.frame(alter.centr.ind))
  alter.centr.ind
}


## ego's network size
get.ego.netsize <- function() {

  ## check if "date.range" and "g" are present in the GlobalEnv
  if (!("date.range" %in% ls(envir = .GlobalEnv))) stop("date.range should be present in the GlobalEnv!")
  if (!("g" %in% ls(envir = .GlobalEnv))) stop("network data ('g') should be present in the GlobalEnv!")
  date.range <- get("date.range", envir = .GlobalEnv)
  g <- get("g", envir = .GlobalEnv)

  ego.netsize <- lapply(seq_len(length(date.range)), function(i) {
    net.day <- g[[i]] %>%
      igraph::graph_from_adjacency_matrix(., mode = "directed", weighted = TRUE)
    netsize <- igraph::degree(net.day, mode = "out", loops = F)
    netsize <- cbind(id = 1:341, netsize = netsize, day = i)
    netsize
    })

  ego.netsize <- do.call(rbind, ego.netsize)
  require(data.table)
  ego.netsize <- setDT(as.data.frame(ego.netsize))
  ego.netsize
}




## --------------------- ##
## 2. Analysis functions ##
## --------------------- ##

require(formula.tools)
est.uncond.indirect <- function(dat, ## data frame to pass for bootstrapping
                                i, ## i = the index value for resample
                                lm.model.M, ## lm object estimating M
                                lm.model.Y, ## lm object estimating Y
                                pred) { ## focal predictor, X (can accept multiple variables)

  ## extract mediator automatically from the model of M
  m.name <- formula.tools::lhs.vars(formula(lm.model.M))

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
jnt <- function(.lm, pred, modx, alpha=.05) {
  require(stringi)
  b1 = coef(.lm)[pred]
  b3 = coef(.lm)[stri_startswith_fixed(names(coef(.lm)), paste0(pred,":")) | stri_endswith_fixed(names(coef(.lm)), paste0(":",pred))]
  se_b1 = coef(summary(.lm))[pred, 2]
  se_b3 = coef(summary(.lm))[stri_startswith_fixed(names(coef(.lm)), paste0(pred,":")) | stri_endswith_fixed(names(coef(.lm)), paste0(":",pred)), 2]
  COV_b1b3 = vcov(.lm)[pred, stri_startswith_fixed(names(coef(.lm)), paste0(pred,":")) | stri_endswith_fixed(names(coef(.lm)), paste0(":",pred))]
  t_crit = qt(1-alpha/2, .lm$df.residual)
  # see Bauer & Curran, 2005
  a = t_crit^2 * se_b3^2 - b3^2
  b = 2 * (t_crit^2 * COV_b1b3 - b1 * b3)
  c = t_crit^2 * se_b1^2 - b1^2
  jn = c(
    (-b - sqrt(b^2 - 4 * a * c)) / (2 * a),
    (-b + sqrt(b^2 - 4 * a * c)) / (2 * a)
  )
  JN = sort(unname(jn))
  JN = JN[JN>=min(.lm$model[,modx]) & JN<=max(.lm$model[,modx])]
  JN
}


## estimate conditional effects
est.cond.indirect <- function(dat, ## data frame to pass for bootstrapping
                                i, ## i = the index value for resample
                                lm.model.M, ## lm object estimating M
                                lm.model.Y, ## lm object estimating Y
                                pred, ## focal predictor, X (CANNOT accept multiple variables)
                                modx) { ## moderator, only one at a time

  ## extract mediator automatically from the model of M
  m.name <- formula.tools::lhs.vars(formula(lm.model.M))

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
  c <- summary(model.Y.resample)$coef[pred, 1]

  mod.val <- dat[, get(modx)]

  ## effect decomposition ##
  ## index of moderated mediation
  index.modmed <- a3*b

  ## conditional indirect effect at every docile of moderator values
  ## plus any point of transition
  mod.val <- seq(from = range(mod.val)[1], to = range(mod.val)[2])
  critical.val <- jnt(lm.model.M, pred, modx)
  mod.val <- sort(unique(c(mod.val, critical.val)))

  ind.effect <- (a1 + mod.val*a3)*b
  ## unconditional direct effect
  dir.effect <- c

  # return effect decomposition
  return.vec <- c(index.modmed = index.modmed,
                  ind.effect = ind.effect,
                  dir.effect = dir.effect)
  return(return.vec)
}



