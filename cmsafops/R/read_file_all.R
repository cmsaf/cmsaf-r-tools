read_file_all <- function(infile, var_name) {
  id <- nc_open(infile)

  global_att <- ncatt_get(id, 0)

  dim_names   <- names(id$dim)
  dimensions <- get_dimensions(id, dim_names)
  time_info <- get_time_info(id, dim_names, dimensions$names$t)

  # Variables
  varnames <- c(names(id$var))

  time_info$has_time_bnds <- TIME_BOUNDS_NAMES$DEFAULT %in% varnames

  variable <- list()
  if (!is.null(var_name)) {
    variable <- list(name = get_var_from_vars_all(c(varnames,dim_names), var_name))
  variable$prec <- get_var_prec(id$var[[variable$name]]$prec, variable$name)
  variable$chunksizes <- id$var[[variable$name]]$chunksizes
  variable$attributes <- ncatt_get(id, variable$name)

  if (is.null(variable$attributes$`_FillValue`)) {
    variable$attributes$`_FillValue` <- variable$attributes$missing_value
  }
  if (is.null(variable$attributes$missing_value)) {
    variable$attributes$missing_value <- variable$attributes$`_FillValue`
  }
  variable$attributes <- get_var_atts(variable$attributes, variable$name)
  }

  # Grid variables
  vars <- list()
  vars_data <- list()
  for (i in seq_along(varnames)) {
    var <- id$var[[i]]
    names_of_var <- c(var$name, var$longname, names(id$var)[i])
    if (any(names_of_var %in% LON_NAMES)) {
      vars[[LON_NAMES$DEFAULT]] <- id$var[[i]]
      vars_data[[LON_NAMES$DEFAULT]] <- ncvar_get(id, varnames[i])
    } else if (any(names_of_var %in% LAT_NAMES)) {
      vars[[LAT_NAMES$DEFAULT]] <- id$var[[i]]
      vars_data[[LAT_NAMES$DEFAULT]] <- ncvar_get(id, varnames[i])
    }
  }

  grid <- list(vars = vars,
               vars_data = vars_data,
               is_regular = dimensions$have_lonlat)

  dimension_data <- list(
    # fixed because of missing standard- and long-names in some files
    x = id$dim[[dimensions$names$x]]$vals,
    y = id$dim[[dimensions$names$y]]$vals,

    #x = ncvar_get(id, dimensions$names$x),
    #y = ncvar_get(id, dimensions$names$y),
    t = ncvar_get(id, dimensions$names$t)

  )
  nc_close(id)

  result <- list(
    global_att = global_att,
    time_info = time_info,
    variable = variable,
    dimension_data = dimension_data,
    grid = grid
  )

  return(result)
}
