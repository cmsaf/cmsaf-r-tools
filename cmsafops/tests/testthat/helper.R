# Useful for creating temporary output files for tests.
tempfile_helper <- function(pattern) {
  tempfile(pattern, fileext = ".nc")
}

tempfile_helper_csv <- function(pattern) {
  tempfile(pattern, fileext = ".csv")
}
