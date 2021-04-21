#' Designed for the CM SAF R Toolbox.
#'
#' This function is a helper function called by the CM SAF R Toolbox. Not for general use.
#' @param id id
#' @param dimnames dimnames
#' @param t_name t_name
#' @export
get_time_info <- function(id, dimnames, t_name) {
    if (t_name %in% dimnames) {
      t_calendar <- STANDARD
      t_units <- UNDEFINED
      attnames <- names(id$dim[[t_name]])
      if (ATTR_NAMES$UNITS %in% attnames) {
        t_units <- ncatt_get(id, t_name, ATTR_NAMES$UNITS)$value
      }
      if (ATTR_NAMES$CALENDAR %in% attnames) {
        t_calendar <- ncatt_get(id, t_name, ATTR_NAMES$CALENDAR)$value
      }
    }
  return(list(units = t_units, calendar = t_calendar))
}
