write_output_file_trend <- function(outfile, force_v4,
                                    vars, vars_data, var_name,
                                    grid_vars, grid_vars_data,
                                    sig, standard_name,
                                    cmsaf_info, calendar, var_atts, global_att) {
  nc_out <- nc_create(outfile, vars, force_v4 = force_v4)

  ncvar_put(nc_out, vars[[1]], vars_data$result$target)
  ncvar_put(nc_out, vars[[2]], vars_data$time_bounds)
  ncvar_put(nc_out, vars[[3]], vars_data$result$target_p)
  ncvar_put(nc_out, vars[[4]], vars_data$result$target2)

  # If there are grid variables (lon/lat), they should be kept unchanged.
  for (i in seq_along(grid_vars)) {
    nc_out <- ncvar_add(nc_out, grid_vars[[i]])
    ncvar_put(nc_out, grid_vars[[i]], grid_vars_data[[i]])
  }

  # Variable attributes

  nvar1 <- paste0(var_name, "_trend1")
  nvar2 <- paste0(var_name, "_trend2")

  long_name_1 <- paste0("linear trend in ", var_name, " multiplied by length of time series")
  long_name_2 <- paste0("linear trend in ", var_name)

  ncatt_put(nc_out, nvar1,
            ATTR_NAMES$STANDARD_NAME, var_atts$standard_name,
            prec = PRECISIONS_ATT$TEXT)
  ncatt_put(nc_out, nvar1,
            ATTR_NAMES$LONG_NAME, long_name_1,
            prec = PRECISIONS_ATT$TEXT)
  ncatt_put(nc_out, nvar1,
            ATTR_NAMES$CMSAF_INFO, cmsaf_info,
            prec = PRECISIONS_ATT$TEXT)

  ncatt_put(nc_out, nvar2,
            ATTR_NAMES$STANDARD_NAME, var_atts$standard_name,
            prec = PRECISIONS_ATT$TEXT)
  ncatt_put(nc_out, nvar2,
            ATTR_NAMES$LONG_NAME, long_name_2,
            prec = PRECISIONS_ATT$TEXT)
  ncatt_put(nc_out, nvar2,
            ATTR_NAMES$CMSAF_INFO, cmsaf_info,
            prec = PRECISIONS_ATT$TEXT)


  ncatt_put(nc_out, sig$name,
            ATTR_NAMES$STANDARD_NAME, sig$standard_name,
            prec = PRECISIONS_ATT$TEXT)
  ncatt_put(nc_out, sig$name,
            ATTR_NAMES$LONG_NAME, sig$long_name,
            prec = PRECISIONS_ATT$TEXT)
  ncatt_put(nc_out, sig$name,
            "description", sig$info,
            prec = PRECISIONS_ATT$TEXT)

  # Time attributes
  ncatt_put(nc_out, TIME_NAMES$DEFAULT,
            ATTR_NAMES$STANDARD_NAME, TIME_NAMES$DEFAULT,
            prec = PRECISIONS_ATT$TEXT)
  ncatt_put(nc_out, TIME_NAMES$DEFAULT,
            ATTR_NAMES$CALENDAR, calendar,
            prec = PRECISIONS_ATT$TEXT)
  ncatt_put(nc_out, TIME_NAMES$DEFAULT,
            ATTR_NAMES$BOUNDS, TIME_BOUNDS_NAMES$DEFAULT,
            prec = PRECISIONS_ATT$TEXT)

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
