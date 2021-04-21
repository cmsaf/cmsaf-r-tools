check_infiles <- function(infiles) {

  for (file in infiles) {
    if (length(file) != 1) {
      stop("Input filepath must be of length one and not NULL")
    }
    if (!file.exists(file)) {
      stop(paste0("Input file ", file, " does not exist"))
    }
  }

}
