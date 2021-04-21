check_outfile <- function(file) {
  if (length(file) != 1) {
    stop("Output filepath must be of length one and not NULL")
  }

  if (!is.character(file)) {
    stop("Output filepath must be of type character")
  }
}
