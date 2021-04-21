get_time_bounds_run <- function(times, nts) {
  time_bnds <- array(NA, dim = c(2, length(times)-(nts-1)))
  
  for (j in 1:(length(times)-(nts-1))) {
    time_bnds[1, j] <- times[j]
    time_bnds[2, j] <- times[j+(nts-1)]
  }

  return(time_bnds)
}