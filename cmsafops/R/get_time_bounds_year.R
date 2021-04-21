get_time_bounds_year <- function(times, years_all, years_unique) {
  time_bnds <- array(NA, dim = c(2, length(years_unique)))

  count <- 1
  for (j in seq_along(years_unique)) {
    year_dummy <- which(years_all == years_unique[j])
    time_bnds[1, count] <- times[min(year_dummy)]
    time_bnds[2, count] <- times[max(year_dummy)]
    count <- count + 1
  }

  return(time_bnds)
}
