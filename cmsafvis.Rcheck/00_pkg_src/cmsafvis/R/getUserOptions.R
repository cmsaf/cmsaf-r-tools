getUserOptions <- function(infile, nc = NULL) {
  if (!is.null(nc)) id <- nc
  else id <- ncdf4::nc_open(infile)

  vn <- names(id$var)
  dn <- names(id$dim)

  errorMsg <- "This script depends on files with variables 'lon', 'lat', and 'time'. Please name the accordings variables using this convention."

  # Get longitude range
  if ("lon" %in% c(dn, vn)) {
    lon_range <- range(ncdf4::ncvar_get(id, "lon"), na.rm = TRUE)
  } else {
    stop(errorMsg)
  }

  # Get latitude range
  if ("lat" %in% c(dn, vn)) {
    lat_range <- range(ncdf4::ncvar_get(id, "lat"), na.rm = TRUE)
  } else {
    stop(errorMsg)
  }

  # Get time range
  if ("time" %in% c(dn, vn)) {
    time_range <- as.Date(cmsafops::get_time(ncdf4::ncatt_get(id, "time", "units")$value, ncdf4::ncvar_get(id, "time")))
    time_range <- c(min(time_range), max(time_range))
  } else {
    stop(errorMsg)
  }

  # close nc file
  if (is.null(nc)) ncdf4::nc_close(id)

  return(list(
    variables = vn,
    dimensions = dn,
    lat_range = lat_range,
    lon_range = lon_range,
    time_range = time_range
  ))
}
