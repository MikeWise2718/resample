readCol <- function(dname, fname, cname, sname,ext="") {
  tfname <- sprintf("%s/%s.%s", dname, fname,ext)
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