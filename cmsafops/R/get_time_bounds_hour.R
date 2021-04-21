get_time_bounds_hour <- function(times, hour) {
  time_bnds <- array(NA, dim = c(2, length(unique(hour))))
  
  count <- 1
  for (j in sort(unique(hour))) {
    hour_dummy <- which(hour == j)
    time_bnds[1, count] <- times[min(hour_dummy)]
    time_bnds[2, count] <- times[max(hour_dummy)]
    count <- count + 1
  }
  
  return(time_bnds)
}
