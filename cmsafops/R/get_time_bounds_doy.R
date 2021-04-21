get_time_bounds_doy <- function(times, doy) {
  time_bnds <- array(NA, dim = c(2, length(unique(doy))))

  count <- 1
  for (j in sort(unique(doy))) {
    day_dummy <- which(doy == j)
    time_bnds[1, count] <- times[min(day_dummy)]
    time_bnds[2, count] <- times[max(day_dummy)]
    count <- count + 1
  }

  return(time_bnds)
}
