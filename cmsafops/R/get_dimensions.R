#' Designed for the CM SAF R Toolbox.
#'
#' This function is a helper function called by the CM SAF R Toolbox.
#' 
#' @param id An object of the class NetCDF4
#' @param dimnames Dimension names (data.frame)
#' @export
get_dimensions <- function(id, dimnames) {
  has_lon <- FALSE
  has_lat <- FALSE
  x <- NULL
  y <- NULL
  t <- NULL

  for (i in seq_along(dimnames)) {
    sn <- ncatt_get(id, dimnames[i], ATTR_NAMES$STANDARD_NAME)
    ln <- ncatt_get(id, dimnames[i], ATTR_NAMES$LONG_NAME)
    dn <- dimnames[i]

    is_x_dim <- any(c(sn, ln, dn) %in% X_NAMES)
    is_y_dim <- any(c(sn, ln, dn) %in% Y_NAMES)
    is_lon_dim <- any(c(sn, ln, dn) %in% LON_NAMES)
    is_lat_dim <- any(c(sn, ln, dn) %in% LAT_NAMES)
    is_time_dim <- any(c(sn, ln, dn) %in% TIME_NAMES)

    if (is_lon_dim) {
      has_lon <- TRUE
      x <- dn
    } else if (is_lat_dim) {
      has_lat <- TRUE
      y <- dn
    } else if (is_time_dim) {
      t <- dn
    } else if (is_x_dim) {
      x <- dn
    } else if (is_y_dim) {
      y <- dn
    }
  }

  # if(is.null(x) || is.null(y) || is.null(t)) {
  #   x <- dimnames[dimnames %in% X_NAMES]
  #   y <- dimnames[dimnames %in% Y_NAMES]
  #   t <- dimnames[dimnames %in% TIME_NAMES]
  # }

  names <- list(x = x, y = y, t = t)

  have_lonlat <- has_lon && has_lat

  return(list(names = names, have_lonlat = have_lonlat))
}
