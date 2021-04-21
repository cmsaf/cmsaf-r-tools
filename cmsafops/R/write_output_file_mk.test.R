# helper function for mk.test to write output file 
write_output_file_mk.test <- function(outfile, force_v4,
                                    vars, vars_data, var_name,
                                    grid_vars, grid_vars_data,
                                    S.value, Z.value, standard_name,
                                    cmsaf_info, calendar, var_atts, global_att) 
{
  nc_out <- nc_create(outfile, vars, force_v4 = force_v4)
  
  ncvar_put(nc_out, vars[[1]], vars_data$result$target.S)
  ncvar_put(nc_out, vars[[2]], vars_data$time_bounds)
  ncvar_put(nc_out, vars[[3]], vars_data$result$target.Z)
  
  # If there are grid variables (lon/lat), they should be kept unchanged.
  for (i in seq_along(grid_vars)) {
    nc_out <- ncvar_add(nc_out, grid_vars[[i]])
    ncvar_put(nc_out, grid_vars[[i]], grid_vars_data[[i]])
  }
  
  # Variable attributes
  ncatt_put(nc_out, S.value$name,
            ATTR_NAMES$STANDARD_NAME, S.value$standard_name,
            prec = PRECISIONS_ATT$TEXT)
  ncatt_put(nc_out, S.value$name,
            ATTR_NAMES$LONG_NAME, S.value$long_name,
            prec = PRECISIONS_ATT$TEXT)
  ncatt_put(nc_out, S.value$name,
            "description", S.value$info,
            prec = PRECISIONS_ATT$TEXT)
  
  ncatt_put(nc_out, Z.value$name,
            ATTR_NAMES$STANDARD_NAME, Z.value$standard_name,
            prec = PRECISIONS_ATT$TEXT)
  ncatt_put(nc_out, Z.value$name,
            ATTR_NAMES$LONG_NAME, Z.value$long_name,
            prec = PRECISIONS_ATT$TEXT)
  ncatt_put(nc_out, Z.value$name,
            "description", Z.value$info,
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