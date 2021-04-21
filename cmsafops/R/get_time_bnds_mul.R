get_time_bounds_mul <- function(times, test, test_count, mul) {
  time_bnds <- array(NA, dim = c(2, test_count))
  count <- 1
  for (i in seq_len(test_count)) {
    mon_dummy <- which(mul == test[i + 1])
    if (length(mon_dummy) >= 1) {
      time_bnds[1, count] <- times[min(mon_dummy)]
      time_bnds[2, count] <- times[max(mon_dummy)]
      count <- count + 1
    }
  }

  return(time_bnds)
}
