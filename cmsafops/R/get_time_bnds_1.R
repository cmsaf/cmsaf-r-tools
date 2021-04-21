get_time_bounds_1 <- function(times) {
  time_bnds <- array(NA, dim = c(2, 1))
  time_bnds[1, 1] <- min(times, na.rm = TRUE)
  time_bnds[2, 1] <- max(times, na.rm = TRUE)

  return(time_bnds)
}
