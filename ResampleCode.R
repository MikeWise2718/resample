library(ggplot2)
library(grid)
library(gridExtra)
#
# Data prep script for CBRE HQ Hot/Cold call data
#  Mike Wise 11-Nov-2016
#
#setwd("./VAV 10")
#flst <- list.files()
#sprintf("c('%s')",paste0(flst,collapse="','"))

set.seed(1234)
options(warn = 2) # warnings are errors

version <- 1.5
starttime <- Sys.time()
startfmttime <- sprintf(format(starttime, "%d %b %Y - %H:%M:%S"))

versionstring <- sprintf("Version %3.2f- 15 July 2015 - starting:%s", version, startfmttime)
print(versionstring)

print(sprintf("Current Directory:%s", getwd()))

dmin <- as.POSIXct("2013-09-30 19:00:00", format = "%Y-%m-%d %H:%M:%S")
dmin <- as.POSIXct("2014-09-21 19:00:00", format = "%Y-%m-%d %H:%M:%S")
dmax <- as.POSIXct("2016-09-19 06:00:00", format = "%Y-%m-%d %H:%M:%S")
dmax <- as.POSIXct("2014-12-01 20:00:00", format = "%Y-%m-%d %H:%M:%S")
test <- T

if (test) {
  dname <- "./VAV TST"
  flst <- c('Discharge Airflow', 'Discharge Flow Heat Setpoint')
  alst <- c('ColDem', 'DisAir')
  dlst <- c('cfm', 'cfm')
  dmin <- as.POSIXct("2014-09-24 00:00:00", format = "%Y-%m-%d %H:%M:%S")
  dmax <- as.POSIXct("2014-09-24 23:45:00", format = "%Y-%m-%d %H:%M:%S")
} else {
  dname <- "../VAV 10"
  flst <- c('Cooling Demand', 'Discharge Airflow', 'Discharge Airflow  Setpoint',
          'Discharge Damper Command', 'Discharge Flow Cool Standby Setpoint', 'Discharge Flow Heat Setpoint',
          'Discharge Pressure', 'Discharge Temperature', 'Heating Demand',
          'Occupancy Sensor', 'Occupied Cool Setpoint', 'Occupied Heat Setpoint',
          'Occupied Mode', 'Re-Heating Valve  Command', 'Zone Standby Cool Setpoint',
          'Zone Standby Heat Setpoint', 'Zone Temperature', 'Zone Temperature  Setpoint',
          'Zone Unoccupied Cool Setpoint', 'Zone Unoccupied Heat Setpoint')

  alst <- c('ColDem', 'DisAir', 'DisAirSet',
          'DisDamCom', 'DisFloColSet', 'DisFloHetSet',
          'DisPrs', 'DisTmp', 'HetDem',
          'OccSen', 'OccColSet', 'OccHetSet',
          'OccMod', 'ReHetValCom', 'ZonStdColSet',
          'ZonStdHetSet', 'ZonTmp', 'ZonTmpSet',
          'ZonUnoCoolSet', 'ZonUnoHetSet')

  dlst <- c("%", "cfm", "cfm",
          "%", "cfm", "cfm",
          "H2O", "°F", "%",
          "TF", "°F", "°F",
          "", "%", "°F",
          "°F", "°F", "°F",
          "°F", "°F")
}
dcolor <- c('ColDem'='blue', 'DisAir'='green', 'DisAirSet'='green',
          'DisDamCom'='blue', 'DisFloColSet'='green', 'DisFloHetSet'='green',
          'DisPrs'='blue', 'DisTmp'='orange', 'HetDem'='blue',
          'OccSen'='blue', 'OccColSet'='orange', 'OccHetSet'='orange',
          'OccMod'='blue', 'ReHetValCom'='blue', 'ZonStdColSet'='orange',
          'ZonStdHetSet'='purple', 'ZonTmp'='purple', 'ZonTmpSet'='purple',
          'ZonUnoCoolSet'='darkblue', 'ZonUnoHetSet'='darkblue')

# Read om the columns

readcol <- function(dname, fname, cname, sname) {
  tfname <- sprintf("%s/%s", dname, fname)
  cv <- read.csv(tfname, encoding = "UTF-8")
  #  print(sprintf("Read %d values from %s",nrow(cv),tfname))
  dt <- as.POSIXct(cv$Timestamp, format = "%Y-%m-%dT%H:%M:%S")
  if (sname == "TF") {
    vv <- sub("true", "1", cv[[2]])
    vv <- sub("false", "0", vv)
  } else {
    vv <- gsub("[^0-9.\\-]", "", cv[[2]])
  }
  vv <- as.numeric(vv)
  rv <- data.frame(dt = dt, cname = vv)
  rv <- rv[complete.cases(rv),]
  return(rv)
}

expandcol <- function(dmin, dmax, intv, df) {
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

# Loop through all the files and make them the same length, expanding as necessary
urdat <- list()
tits <- list()
rdf <- NULL
for (i in 1:length(flst)) {
  cv <- readcol(dname, flst[[i]], alst[[i]], dlst[i])
  btrim <- dmin <= cv$dt & cv$dt <= dmax
  nnrow <- 0
  if (sum(btrim) > 0) {
    newdf <- expandcol(dmin, dmax, 15 * 60, cv[btrim,])
    nnrow <- nrow(newdf)
    colname <- alst[[i]]
    if (is.null(rdf)) {
      rdf <- newdf
      colnames(rdf)[2] <- colname
    } else {
      rdf[[ colname ]] <- newdf[[ 2 ]]
    }
  }
  if (nrow(cv) > 0) {
    print(sprintf("%40s - rows:%6d nrows:%6d avg:%6.1f     start:%s  end:%s", flst[[i]], nrow(cv), nnrow, mean(cv[[2]]), min(cv[[1]]), max(cv[[1]])))
  } else {
    print(sprintf("%40s - rows:%6d nrows:%6d", flst[[i]], nrow(cv), nnrow))
  }
  urdat[[length(urdat) + 1]] <- cv
  tits[[length(tits) + 1]] <- sprintf("%s - %s", flst[[i]], dlst[[i]])
}
elap <- as.numeric((Sys.time() - starttime)[1], units = "secs")
print(sprintf("DataPrep took %.1f secs", elap))


pvars <- colnames(rdf)[2:(ncol(rdf)-1+1)]
i <- 1
plots <- list()
for (pv in pvars) {
  udf <- urdat[[i]]
  udf <- udf[dmin <= udf$dt & udf$dt <= dmax,]
#  gp <- ggplot(rdf) + geom_line(aes_string(x = "tn", y = pv), color = dcolor[pv], show.legend = T) +
#                      labs(title = tits[[i]], x = "")
  rrdf <- data.frame(tn = rdf$tn, ppv = rdf[[pv]],colr=pv)
  gp <- ggplot(rrdf) + geom_line(aes(x = tn, y = ppv, color = colr), show.legend = T) +
                      labs(title=tits[[i]],x="",y="")
  if (nrow(udf) > 0) {
#    gp <- gp + geom_point(data = udf, aes(x = dt, y = cname), color = "red", alpha = 0.4, show.legend = T)
    gp <- gp + geom_point(data = udf, aes(x = dt, y = cname, color = "Measured"), alpha = 0.4, show.legend = T)
  }
  vals <- c(dcolor, "Measured" = "red")
  gp <- gp + scale_color_manual(name = "",values = vals )
  # gp <- gp + theme(axis.text.x=element_text(angle=30,hjust=1))
  plots[[length(plots)+1]] <- gp
  i <- i+1
}
grid.arrange(grobs=plots, ncol = 1, main = "Main title")

elap <- as.numeric((Sys.time() - starttime)[1], units = "secs")
print(sprintf("%s created on %s took %.1f secs", versionstring, startfmttime, elap))
