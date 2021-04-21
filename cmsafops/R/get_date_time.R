#'Get dates and times from NetCDF type date format.
#'
#'@param times Timesteps from netcdf data (numeric).
#'@param unit Unit from netcdf data (character).
#'
#'@return A data frame with the columns years, months, days and times. Careful:
#'  The parts of the date are of numeric type, but the times are stored as
#'  characters (levels).
#'@importFrom assertthat assert_that is.string
#'
#' @examples
#'date_time <- get_date_time(times = c(159191, 5991820), 
#'  unit = "minutes since 1980-05-07")
#'date_time
#'date_time$years
#'@export
get_date_time <- function(times, unit) {
  assert_that(is.numeric(times))
  assert_that(is.string(unit))

  date_time <- get_time(unit, times)
  date <- as.Date(date_time)
  date_string <- as.character(date)

  years <- as.numeric(substr(date_string, 1, 4))
  months <- as.numeric(substr(date_string, 6, 7))
  days <- as.numeric(substr(date_string, 9, 10))

  times_string <- substr(date_time, 12, 19)

  result <- data.frame(years = years, months = months, days = days,
                       times = times_string)
  return(result)
}
