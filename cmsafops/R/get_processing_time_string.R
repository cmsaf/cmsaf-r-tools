#' Get processing time string
#'
#' @param time_start start time of the process (of class "POSIXct" as given by
#'   "Sys.time()")
#' @param time_end end time of the process (of class "POSIXct" as given by
#'   "Sys.time()")
#'
#' @return a specialized string containing the processed time
#' @export
get_processing_time_string <- function(time_start, time_end) {
  stopifnot(length(time_start) == 1)
  stopifnot(length(time_start) == 1)
  stopifnot("POSIXct" %in% class(time_start))
  stopifnot("POSIXct" %in% class(time_start))
  paste0("processing time: ",
         round(as.numeric(time_end - time_start,
                          units = "secs"),
               digits = 2),
         " s")
}
