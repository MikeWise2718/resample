expandCol <- function(dmin, dmax, intv, df) {
  # Calculate number of points, etc.
  tsecs <- as.numeric(difftime(dmax, dmin, units = "secs"))
  nsteps <- trunc(tsecs / intv)

  # now spread the points along the vector
  vn <- rep(-1, nsteps)
  tvek <- df[[1]]
  vvek <- df[[2]]
  jmin <- nsteps + 1
  vsum <- rep(0, nsteps)
  vcnt <- rep(0, nsteps)

  tn <- as.POSIXct(dmin + (1:nsteps)*intv)

  # loop over the input vector and accumulate the points 
  nr <- nrow(df)
  for (i in 1:nr) {
    j <- trunc(as.numeric(difftime(tvek[i], dmin, units = "secs")) / intv)
    if (0 < j && j < jmin) {
      jmin <- j
    }
    vsum[j] <- vsum[j] + vvek[i]
    vcnt[j] <- vcnt[j] + 1
  }

  # Now divide them back into the original 
  for (j in 1:nsteps) {
    if (vcnt[j] > 0) {
      vn[j] <- vsum[j]/vcnt[j]
    }
  }

  # now find the first non-negative point and spread it backwards
  if (jmin > 1) {
    for (i in 1:(jmin - 1)) {
      vn[i] <- vn[jmin]
    }
  }

  # now spread the values forward, skipping over the postive ones
  fsti <- max(2, jmin + 1)
  if (fsti < nsteps) {
    for (i in fsti:nsteps) {
      if (vn[i] < 0) {
        vn[i] <- vn[i - 1]
      }
    }
  }
  df <- data.frame(tn, vn)

  return(df)
}