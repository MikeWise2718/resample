plotCols <- function(rm, dcolor = NULL, trim = T, alf = 0.3, mcol = "red", lcol = "blue", tickdf = NULL) {
  rdf <- rm$rdf
  urdat <- rm$urdat
  titles <- rm$titles
  # plot all the columns
  pvars <- colnames(rdf)[2:(ncol(rdf))]
  i <- 1
  plots <- list()
  for (pv in pvars) {
    udf <- urdat[[i]]
    if (trim) {
      udf <- udf[dmin <= udf$dt & udf$dt <= dmax,]
    }
    rrdf <- data.frame(tn = rdf$tn, ppv = rdf[[pv]], colr = pv)
    gp <- ggplot(rrdf) + geom_line(aes(x = tn, y = ppv, color = colr), show.legend = T) +
                      labs(title = titles[[i]], x = "", y = "")
    if (nrow(udf) > 0) {
      gp <- gp + geom_point(data = udf, aes(x = dt, y = cname, color = "Measured"), alpha = alf, show.legend = T)
    }
    if (!is.null(tickdf)) {
      for (j in 1:nrow(tickdf)) {
        xval <- as.numeric(tickdf$dt[[j]])
        clr <- ifelse(tickdf$HotCold[[j]] == "C", "darkblue", "darkred")
        gp <- gp + geom_vline(xintercept = xval, color = clr)
      }
    }
    if (is.null(dcolor)) {
      vals <- c("Measured" = mcol)
      vals[[pv]] <- lcol
    } else {
      vals <- c(dcolor, "Measured" = mcol)
    }
    gp <- gp + scale_color_manual(name = "", values = vals)
    # gp <- gp + theme(axis.text.x=element_text(angle=30,hjust=1))
    #print(gp)
    plots[[length(plots) + 1]] <- gp
    i <- i + 1
  }
  return(plots)
}