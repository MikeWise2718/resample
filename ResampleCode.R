library(ggplot2)
library(grid)
library(gridExtra)
#
# Resample Project
#  Mike Wise 15-Nov-2016
#  another comment
#

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
vavtest <- F

if (vavtest) {
  dname <- "./VAV TST"
  flst <- c('Discharge Airflow', 'Discharge Flow Heat Setpoint')
  alst <- c('ColDem', 'DisAir')
  dlst <- c('cfm', 'cfm')
  dmin <- as.POSIXct("2014-09-24 00:00:00", format = "%Y-%m-%d %H:%M:%S")
  dmax <- as.POSIXct("2014-09-24 23:45:00", format = "%Y-%m-%d %H:%M:%S")
  ext <- ""
  dcolor <- c('ColDem' = 'blue', 'DisAir' = 'green')
} else {
  dname <- "./SimpleTest"
  flst <- c('Flow1', 'Flow2', 'Flow3', 'Flow4')
  alst <- c('Flow1', 'Flow2', 'Flow3', 'Flow4')
  dlst <- c('cfm', 'cfm','cfm','cfm')
  dmin <- as.POSIXct("2014-09-24 00:00:00", format = "%Y-%m-%d %H:%M:%S")
  dmax <- as.POSIXct("2014-09-24 23:45:00", format = "%Y-%m-%d %H:%M:%S")
  ext <- "csv"
  dcolor <- NULL
  dcolor <- c('Flow1' = 'blue', 'Flow2' = 'green', 'Flow3' = 'purple',
              'Flow4'="brown")
}



source("readCol.R")
source("expandCol.R")

source("readAndMergeCols.R")
source("plotCols.R")


rm <- readAndMergeCols(dname, flst, alst, dlst,ext=ext)

elap <- as.numeric((Sys.time() - starttime)[1], units = "secs")
print(sprintf("DataPrep took %.1f secs", elap))


plots <- plotCols(rm,dcolor=dcolor,trim=F)
grid.arrange(grobs = plots, ncol = 1, main = "Main title")

elap <- as.numeric((Sys.time() - starttime)[1], units = "secs")
print(sprintf("%s created on %s took %.1f secs", versionstring, startfmttime, elap))
