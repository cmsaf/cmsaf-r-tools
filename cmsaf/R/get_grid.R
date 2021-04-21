get_grid <- function(filename) {
  grid_str <- substr(basename(filename), 21, 22)
  return(as.numeric(grid_str))
}
