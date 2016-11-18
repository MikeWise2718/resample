readAndMergeCols <- function(dirname, flst, alst, dlst, ext = "", trimpts = T, dmin = NULL, dmax = NULL) {
  # Loop through all the files (columns) specified in flst
  # and make them the same length, expanding as necessary
  # then combine them into a single file
  # keep the original data around
  urdat <- list()
  titles <- list()
  rdf <- NULL
  for (i in 1:length(flst)) {
    cv <- readCol(dirname, flst[[i]], alst[[i]], dlst[i], ext = ext)
    if (!is.null(cv)) {
      btrim <- dmin <= cv$dt & cv$dt <= dmax
      nnrow <- 0
      if (sum(btrim) > 0) {
        if (trimpts) {
          # trim the point dataframe
          cv <- cv[btrim,]
          newdf <- expandCol(dmin, dmax, 15 * 60, cv)
        } else {
          newdf <- expandCol(dmin, dmax, 15 * 60, cv[btrim,])
        }
        nnrow <- nrow(newdf)
        colname <- alst[[i]]
        if (is.null(rdf)) {
          rdf <- newdf
          colnames(rdf)[2] <- colname
        } else {
          rdf[[colname]] <- newdf[[2]]
        }
      }
      if (nrow(cv) > 0) {
        print(sprintf("%40s - rows:%6d nrows:%6d avg:%6.1f     start:%s  end:%s", flst[[i]], nrow(cv), nnrow, mean(cv[[2]]), min(cv[[1]]), max(cv[[1]])))
      } else {
        print(sprintf("%40s - rows:%6d nrows:%6d", flst[[i]], nrow(cv), nnrow))
      }
      urdat[[length(urdat) + 1]] <- cv
      titles[[length(titles) + 1]] <- sprintf("%s - %s", flst[[i]], dlst[[i]])
    } else {
      print(sprintf("could not find %s/%s", dirname, flst[[i]]))
    }
  }
  rv <- list()
  rv$urdat <- urdat
  rv$titles <- titles
  rv$rdf <- rdf
  return(rv)
}