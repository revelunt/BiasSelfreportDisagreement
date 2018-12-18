
## ---------------- #
## Helper functions #
## ---------------- #


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
