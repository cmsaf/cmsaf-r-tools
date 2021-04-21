#'Change attributes of a NetCDF variable.
#'
#'This function can change the name, standard_name, long_name, units, _FillValue
#'and missing_value of a variable. There is no separate outfile, thus use this
#'function with care. The values for v_name, s_name, l_name, u_name, F_val and
#'m_val are optional and will only be changed if they are given. If an attribute
#'is not defined yet, it is added by the function.
#'
#'@param var Name of NetCDF variable (character).
#'@param infile Filename of input NetCDF file. This may include the directory
#'  (character).
#'@param v_name New variable name (character).
#'@param s_name New standard name (character).
#'@param l_name New long name (character).
#'@param u_name New units name (character).
#'@param F_val New fill value (numeric).
#'@param m_val New missing value (numeric).
#'@param val_prec Precision of the FillValue and missing value (character).
#'  Default is double.
#'@param verbose logical; if TRUE, progress messages are shown
#'
#'@return The variable information within the infile NetCDF is changed.
#'@export
#'
#'@examples
#'## Create an example NetCDF file with a similar structure as used by CM
#'## SAF. The file is created with the ncdf4 package.  Alternatively
#'## example data can be freely downloaded here: <https://wui.cmsaf.eu/>
#'
#'library(ncdf4)
#'
#'## create some (non-realistic) example data
#'
#'lon <- seq(5, 15, 0.5)
#'lat <- seq(45, 55, 0.5)
#'time <- seq(as.Date("2000-01-01"), as.Date("2010-12-31"), "month")
#'origin <- as.Date("1983-01-01 00:00:00")
#'time <- as.numeric(difftime(time, origin, units = "hour"))
#'data <- array(250:350, dim = c(21, 21, 132))
#'
#'## create NetCDF
#'
#'x <- ncdim_def(name = "lon", units = "degrees_east", vals = lon)
#'y <- ncdim_def(name = "lat", units = "degrees_north", vals = lat)
#'t <- ncdim_def(name = "time", units = "hours since 1983-01-01 00:00:00",
#'  vals = time, unlim = TRUE)
#'var1 <- ncvar_def("Data1", "W m-2", list(x, y, t), -1, prec = "short")
#'vars <- list(var1)
#'ncnew <- nc_create(file.path(tempdir(),"CMSAF_example_file.nc"), vars)
#'ncvar_put(ncnew, var1, data)
#'ncatt_put(ncnew, "lon", "standard_name", "longitude", prec = "text")
#'ncatt_put(ncnew, "lat", "standard_name", "latitude", prec = "text")
#'nc_close(ncnew)
#'
#'## Change the variable and standard name of the example CM SAF NetCDF
#'## file:
#'change_att(var = "Data1", infile = file.path(tempdir(),"CMSAF_example_file.nc"), v_name = "SIS",
#'  s_name = "surface_downwelling_shortwave_flux_in_air")
#'
#'unlink(file.path(tempdir(),"CMSAF_example_file.nc"))
change_att <- function(var, infile, v_name=NULL, s_name=NULL, l_name=NULL,
                       u_name=NULL, F_val=NULL, m_val=NULL, val_prec = "double",
                       verbose = FALSE) {

  check_variable(var)
  check_infile(infile)

  # open infile
  nc_in <- nc_open(infile, write = TRUE)

  # get information about variables
  varnames <- names(nc_in$var)

  if (!(var %in% varnames)) {
    stop(paste0("Variable ", var, " not found! File contains: ", varnames))
  }

  TO <- " changed to "

  if (!is.null(s_name)) {
    ncatt_put(nc_in, var, ATTR_NAMES$STANDARD_NAME, s_name, prec = PRECISIONS_ATT$TEXT)
    if (verbose) message(ATTR_NAMES$STANDARD_NAME, TO, s_name)
  }

  if (!is.null(l_name)) {
    ncatt_put(nc_in, var, ATTR_NAMES$LONG_NAME, l_name, prec = PRECISIONS_ATT$TEXT)
    if (verbose) message(ATTR_NAMES$LONG_NAME, TO, l_name)
  }

  if (!is.null(u_name)) {
    ncatt_put(nc_in, var, ATTR_NAMES$UNITS, u_name, prec = PRECISIONS_ATT$TEXT)
    if (verbose) message(ATTR_NAMES$UNITS, TO, u_name)
  }

  if (!is.null(F_val)) {
    ncatt_put(nc_in, var, ATTR_NAMES$FILL_VALUE, F_val, prec = val_prec)
    if (verbose) message(ATTR_NAMES$FILL_VALUE, TO, F_val)
  }

  if (!is.null(m_val)) {
    ncvar_change_missval(nc_in, var, m_val)
    if (verbose) message(ATTR_NAMES$MISSING_VALUE, TO, m_val)
  }

  if (!is.null(v_name)) {
    ncvar_rename(nc_in, old_varname = var, new_varname = v_name)
    if (verbose) message("variable name", TO, v_name)
  }

  ncatt_put(nc_in, 0, ATTR_NAMES$INFO, INFO_STRING, prec = PRECISIONS_ATT$TEXT)

  nc_close(nc_in)
}
