#' Convert time steps to POSIXct.
#'
#' Times in NetCDF data are generally given in form of a time step and a time
#' unit. This function uses both information to convert them to POSIXct time
#' values. For the unit 'months since' an approximation of 30.4375 d is used!
#'
#' @param time.unit Time unit, which is conform to the CF convention
#'   (character).
#' @param time.step Time steps in form of a numeric or integer vector.
#'
#' @return Time in form of POSIXct is returned. Default time zone is UTC.
#' @export
#'
#' @examples
#' get_time(time.unit = "hours since 1987-01-01", time.step = 249109)
#' get_time(time.unit = "days since 1987-01-01", time.step = 9109)
get_time <- function(time.unit, time.step) {
  stopifnot(is.character(time.unit))
  stopifnot(is.numeric(time.step) && !any(is.na(time.step)))

  # fix date and time if needed (adapted to LSA SAF data)
  
  if (nchar(unlist(strsplit(time.unit," "))[3]) == 20) {
    time.unit <- paste(unlist(strsplit(time.unit," "))[1], 
                        unlist(strsplit(time.unit," "))[2],
                        substr(unlist(strsplit(time.unit," "))[3],1,10),
                        substr(unlist(strsplit(time.unit," "))[3],12,19),
                        sep = " ")
  }
  
  # convert time.unit to POSIXct

  if (unlist(strsplit(time.unit, " "))[2] == "since") {
    t.unit <- unlist(strsplit(time.unit, " since "))[1]
    ref.date <- unlist(strsplit(time.unit, " since "))[2]
  } else {
    stop("Time unit is not conform to CF-Convention!")
  }

  # check for Julian date
  julian <- FALSE
  if (ref.date == "-4712-01-01 12:00:00") {
    ref.date <- "1900-01-01 00:00:00"
    julian <- TRUE
  }
  ref.date <- as.POSIXct(ref.date, tz = "UTC")

  # get factor to convert time.step to seconds
  # for months it is only an estimation for the average days per month
  factor <- 0

  # check reference time unit
  minutes <- c("minutes", "min", "mins")
  seconds <- c("seconds", "sec", "secs")
  hours <- c("hour", "hours")
  days <- c("days", "day")
  weeks <- c("week", "weeks")
  months <- c("month", "months")
  if (tolower(t.unit) %in% minutes)
    factor <- 60
  else if (tolower(t.unit) %in% seconds)
    factor <- 1
  else if (tolower(t.unit) %in% hours)
    factor <- 60 * 60
  else if (tolower(t.unit) %in% days)
    factor <- 24 * 60 * 60
  else if (tolower(t.unit) %in% weeks)
    factor <- 7 * 24 * 60 * 60
  else if (tolower(t.unit) %in% months)
    factor <- 30.4375 * 24 * 60 * 60

  if (factor == 0)
    stop("Non-compliant time unit: ", t.unit)

  # calculate times

  if (julian) {
    # 1582 conversion from julian to gregorian
    ts <- time.step - 2415020.5
    if (min(ts) < 0) (ts <- time.step - 2414982.5)
    time.step <- ts
  }
  check <- ifelse((time.step * factor) <= .Machine$integer.max, FALSE, TRUE)
  if (sum(check) > 0) {
    ffactor <- factor / 10
    times <- ref.date + (time.step * ffactor)
    for (i in 1:9) {
      times <- times + (time.step * ffactor)
    }
    check <- ifelse((time.step * ffactor) <= .Machine$integer.max, FALSE, TRUE)
    if (sum(check) > 0)
      warning("Some times exceed maximum integer value and may be wrong: ",
              times[check])
  } else {
    times <- ref.date + (time.step * factor)
  }

  return(times)
}
