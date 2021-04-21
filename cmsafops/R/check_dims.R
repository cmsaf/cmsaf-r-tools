check_dims <- function(id) {
  dimnames <- names(id$dim)
  varnames <- names(id$var)

  has_time_bnds <- any(TIME_BOUNDS_NAMES %in% varnames)

  isRegGrid <- TRUE
  has_lon_lat <- TRUE

  lon_name <- dimnames[dimnames %in% LON_NAMES]
  lat_name <- dimnames[dimnames %in% LAT_NAMES]
  t_name <- dimnames[dimnames %in% TIME_NAMES]
  x_name <- dimnames[dimnames %in% X_NAMES]
  y_name <- dimnames[dimnames %in% Y_NAMES]

  if (!length(lon_name)) {
    isRegGrid <- FALSE
    lon_name <- varnames[varnames %in% LON_NAMES]
    lat_name <- varnames[varnames %in% LAT_NAMES]
    if (!length(lon_name)) {
      has_lon_lat <- FALSE
    }
  }
  dim_namelist <- list(lon_name = lon_name, lat_name = lat_name,
                       t_name = t_name, x_name = x_name, y_name = y_name)
  return(c(list(isRegGrid = isRegGrid, has_lon_lat = has_lon_lat,
                has_time_bnds = has_time_bnds, varnames = varnames),
           dim_namelist))
}
