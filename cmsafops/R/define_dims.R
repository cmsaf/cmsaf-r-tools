define_dims <- function(is_regular_grid, x_data, y_data, time_data, nb2,
                        time_unit, with_time_bnds = TRUE) {
  if (is_regular_grid) {
    x_name <- LON_NAMES$DEFAULT
    y_name <- LAT_NAMES$DEFAULT
    x_unit <- UNITS$DEGREES_EAST
    y_unit <- UNITS$DEGREES_NORTH
  } else {
    x_name <- X_NAMES$DEFAULT
    y_name <- Y_NAMES$DEFAULT
    x_unit <- UNITS$KILOMETER
    y_unit <- UNITS$KILOMETER
  }

  x <- ncdim_def(
    name = x_name,
    units = x_unit,
    vals = x_data
  )
  y <- ncdim_def(
    name = y_name,
    units = y_unit,
    vals = y_data
  )
  t <- ncdim_def(
    name = TIME_NAMES$DEFAULT,
    units = time_unit,
    vals = time_data,
    unlim = TRUE
  )
  if (with_time_bnds) {
    tb <- ncdim_def(
      name = NB2_NAME,
      units = UNITS$ONE,
      vals = nb2
    )
    dims <- list(x = x, y = y, t = t, tb = tb)
  } else {
    dims <- list(x = x, y = y, t = t)
  }

  return(dims)
}
