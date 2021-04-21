get_time_bounds <- function(times, months_all, months_unique) {
  time_bnds <- array(NA, dim = c(2, length(months_unique)))

  count <- 1
  for (j in seq_along(months_unique)) {
    mon_dummy <- which(months_all == months_unique[j])
    time_bnds[1, count] <- times[min(mon_dummy)]
    time_bnds[2, count] <- times[max(mon_dummy)]
    count <- count + 1
  }

  return(time_bnds)
}
