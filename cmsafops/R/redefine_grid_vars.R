redefine_grid_vars <- function(grid, dims, compression, new_data) {
  g_vars <- list()
  g_vars_data <- list()
  for (i in seq_along(grid$vars)) {
    var <- grid$vars[[i]]
    names_of_var <- c(var$name, var$longname, names(grid$vars)[i])
    if (any(names_of_var %in% LON_NAMES)) {
      g_vars[[LON_NAMES$DEFAULT]] <- ncvar_def(
        name = LON_NAMES$DEFAULT,
        units = UNITS$DEGREES_EAST,
        dim = dims[c("x", "y")],
        missval = -999,
        prec = PRECISIONS_VAR$DOUBLE,
        compression = compression
      )
      g_vars_data[[LON_NAMES$DEFAULT]] <- new_data[[i]]
    }else if (any(names_of_var %in% LAT_NAMES)) {
      g_vars[[LAT_NAMES$DEFAULT]] <- ncvar_def(
        name = LAT_NAMES$DEFAULT,
        units = UNITS$DEGREES_NORTH,
        dim = dims[c("x", "y")],
        missval = -999,
        prec = PRECISIONS_VAR$DOUBLE,
        compression = compression
      )
      g_vars_data[[LAT_NAMES$DEFAULT]] <- new_data[[i]]
    }
  }
  if (length(g_vars)) {
    return(list(vars = g_vars,
                           vars_data = g_vars_data,
                           is_regular = grid$is_regular))
  }else{
    return(grid)
  }
}
