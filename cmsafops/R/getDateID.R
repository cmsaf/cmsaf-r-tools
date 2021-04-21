getDateID <- function(date.time) {
  doy <- as.numeric(strftime(date.time, format = "%j"))

  years <- as.numeric(strftime(date.time, format = "%Y"))
  for (year in seq_along(years)) {
    if (!is_leap_year(years[year])) {
      if (doy[year] >= 60) {
        doy[year] <- doy[year] + 1
      }
    }
  }

  return(doy)
}
