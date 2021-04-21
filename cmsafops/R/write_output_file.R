write_output_file <- function(outfile, force_v4,
                              vars, vars_data, var_name,
                              grid_vars, grid_vars_data,
                              cmsaf_info, calendar, var_atts, global_att,
                              with_time_bnds = TRUE, write_result = TRUE) {

  nc_out <- nc_create(outfile, c(vars, grid_vars), force_v4 = force_v4)

  if (write_result) {
    ncvar_put(nc_out, vars[[1]], vars_data$result)
  }
  if (with_time_bnds) {
    ncvar_put(nc_out, vars[[2]], vars_data$time_bounds)
  }

  # If there are grid variables (lon/lat), they should be kept unchanged.
  for (i in seq_along(grid_vars)) {
    ncvar_put(nc_out, grid_vars[[i]], grid_vars_data[[i]])
  }

  # Variable attributes
  ncatt_put(nc_out, var_name,
            ATTR_NAMES$STANDARD_NAME, var_atts$standard_name,
            prec = PRECISIONS_ATT$TEXT)

  ncatt_put(nc_out, var_name,
            ATTR_NAMES$LONG_NAME, var_atts$long_name,
            prec = PRECISIONS_ATT$TEXT)

  ncatt_put(nc_out, var_name,
            ATTR_NAMES$CMSAF_INFO, cmsaf_info,
            prec = PRECISIONS_ATT$TEXT)

  # Time attributes
  ncatt_put(nc_out, TIME_NAMES$DEFAULT,
            ATTR_NAMES$STANDARD_NAME, TIME_NAMES$DEFAULT,
            prec = PRECISIONS_ATT$TEXT)
  ncatt_put(nc_out, TIME_NAMES$DEFAULT,
            ATTR_NAMES$CALENDAR, calendar,
            prec = PRECISIONS_ATT$TEXT)
  if (with_time_bnds) {
    ncatt_put(nc_out, TIME_NAMES$DEFAULT,
              ATTR_NAMES$BOUNDS, TIME_BOUNDS_NAMES$DEFAULT,
              prec = PRECISIONS_ATT$TEXT)
  }


  if (LON_NAMES$DEFAULT %in% c(names(nc_out$dim), names(nc_out$var))) {
    ncatt_put(nc_out, LON_NAMES$DEFAULT,
              ATTR_NAMES$STANDARD_NAME, LON_NAMES$LONG_DEFAULT,
              prec = PRECISIONS_ATT$TEXT)
    ncatt_put(nc_out, LON_NAMES$DEFAULT,
              ATTR_NAMES$LONG_NAME, LON_NAMES$LONG_DEFAULT,
              prec = PRECISIONS_ATT$TEXT)
    if (LON_NAMES$DEFAULT %in% names(nc_out$dim)) {
    ncatt_put(nc_out, LON_NAMES$DEFAULT,
              ATTR_NAMES$AXIS, AXIS$X,
              prec = PRECISIONS_ATT$TEXT)
    }
  }

  if (LAT_NAMES$DEFAULT %in% c(names(nc_out$dim), names(nc_out$var))) {
    ncatt_put(nc_out, LAT_NAMES$DEFAULT,
              ATTR_NAMES$STANDARD_NAME, LAT_NAMES$LONG_DEFAULT,
              prec = PRECISIONS_ATT$TEXT)
    ncatt_put(nc_out, LAT_NAMES$DEFAULT,
              ATTR_NAMES$LONG_NAME, LAT_NAMES$LONG_DEFAULT,
              prec = PRECISIONS_ATT$TEXT)
    if (LAT_NAMES$DEFAULT %in% names(nc_out$dim)) {
    ncatt_put(nc_out, LAT_NAMES$DEFAULT,
              ATTR_NAMES$AXIS, AXIS$Y,
              prec = PRECISIONS_ATT$TEXT)
    }
  }

  if (X_NAMES$DEFAULT %in% names(nc_out$dim)) {
    ncatt_put(nc_out, X_NAMES$DEFAULT,
              ATTR_NAMES$STANDARD_NAME, X_NAMES$DEFAULT,
              prec = PRECISIONS_ATT$TEXT)
    ncatt_put(nc_out, X_NAMES$DEFAULT,
              ATTR_NAMES$LONG_NAME, X_NAMES$LONG_DEFAULT,
              prec = PRECISIONS_ATT$TEXT)
    ncatt_put(nc_out, X_NAMES$DEFAULT,
              ATTR_NAMES$AXIS, AXIS$X,
              prec = PRECISIONS_ATT$TEXT)
  }

  if (Y_NAMES$DEFAULT %in% names(nc_out$dim)) {
    ncatt_put(nc_out, Y_NAMES$DEFAULT,
              ATTR_NAMES$STANDARD_NAME, Y_NAMES$DEFAULT,
              prec = PRECISIONS_ATT$TEXT)
    ncatt_put(nc_out, Y_NAMES$DEFAULT,
              ATTR_NAMES$LONG_NAME, Y_NAMES$LONG_DEFAULT,
              prec = PRECISIONS_ATT$TEXT)
    ncatt_put(nc_out, Y_NAMES$DEFAULT,
              ATTR_NAMES$AXIS, AXIS$Y,
              prec = PRECISIONS_ATT$TEXT)
  }

  # Global attributes
  ncatt_put(nc_out, 0,
            ATTR_NAMES$INFO, INFO_STRING,
            prec = PRECISIONS_ATT$TEXT)
  for (i in seq_along(global_att)) {
    ncatt_put(nc_out, 0,
              names(global_att)[i], as.character(global_att[i][[1]]),
              prec = PRECISIONS_ATT$TEXT)
  }

  nc_close(nc_out)
}
