read_gridfile <- function(infile) {
  myGridInfo <- readLines(infile)
  keys <- vector(mode = "character")
  values <- vector(mode = "character")
  splitInfo <- strsplit(myGridInfo, " = ")
  for (info in splitInfo) {
    keys <- rbind(keys, info[1])
    values <- rbind(values, as.character(info[2]))
  }
  keys <- gsub(" ", "", keys)
  values[which(keys == "xsize")]

  if (values[which(keys == "gridtype")] == "lonlat") {
    x <- seq(as.numeric(values[which(keys == "xfirst")]),
             as.numeric(values[which(keys == "xfirst")])
             + (as.numeric(values[which(keys == "xinc")])
                * (as.numeric(values[which(keys == "xsize")]) - 1)),
             by = as.numeric(values[which(keys == "xinc")]))
    y <- seq(as.numeric(values[which(keys == "yfirst")]),
             as.numeric(values[which(keys == "yfirst")])
             + (as.numeric(values[which(keys == "yinc")])
                * (as.numeric(values[which(keys == "ysize")]) - 1)),
             by = as.numeric(values[which(keys == "yinc")]))
  }

  return(list(x, y))
}
