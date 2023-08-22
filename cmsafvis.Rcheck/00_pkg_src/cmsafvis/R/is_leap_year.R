is_leap_year <- function(year) {
  year <- as.numeric(year)
  return(year %% 4 == 0 && (year %% 100 != 0 || year %% 400 == 0))
}
