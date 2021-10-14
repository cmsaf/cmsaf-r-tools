check_infile <- function(file) {
  if (length(file) != 1) {
    stop("Input filepath must be of length one and not NULL")
  }

  if (!file.exists(file) && !is_url(file)) {
    stop("Input file does not exist")
  }
}
