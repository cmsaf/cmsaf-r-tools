#'Read NetCDF variable.
#'
#'This simple function reads a variable of a NetCDF file into R.
#'
#'@param var Name of NetCDF variable (character).
#'@param infile Filename of input NetCDF file. This may include the directory
#'  (character).
#'@param verbose logical; if TRUE, progress messages are shown
#'
#'@return The output is a list object including the variable and the
#'  corresponding time variable. The dimension of the chosen variable is most
#'  commonly a two or three dimensional array.
#'@export
#'
#' @examples
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
#'## create example NetCDF
#'
#'x <- ncdim_def(name = "lon", units = "degrees_east", vals = lon)
#'y <- ncdim_def(name = "lat", units = "degrees_north", vals = lat)
#'t <- ncdim_def(name = "time", units = "hours since 1983-01-01 00:00:00",
#'  vals = time, unlim = TRUE)
#'var1 <- ncvar_def("SIS", "W m-2", list(x, y, t), -1, prec = "short")
#'vars <- list(var1)
#'ncnew <- nc_create(file.path(tempdir(),"CMSAF_example_file.nc"), vars)
#'ncvar_put(ncnew, var1, data)
#'ncatt_put(ncnew, "lon", "standard_name", "longitude", prec = "text")
#'ncatt_put(ncnew, "lat", "standard_name", "latitude", prec = "text")
#'nc_close(ncnew)
#'
#'## Load the data of variable 'SIS' of the example file into R.  To
#'## access the data use e.g., my.data$SIS
#'my.data <- read_ncvar(var = "SIS", infile = file.path(tempdir(),
#'  "CMSAF_example_file.nc"))
#'
#'unlink(file.path(tempdir(),"CMSAF_example_file.nc"))
read_ncvar <- function(var, infile, verbose = FALSE) {
  check_variable(var)
  check_infile(infile)

  calc_time_start <- Sys.time()

  # get file information
  file_data <- read_file_all(infile, var)

  id <- nc_open(infile)
  if (!(var %in% c(TIME_BOUNDS_NAMES$DEFAULT, NB2_NAME)) && var %in% DIM_NAMES && var %in% c(names(id$var), names(id$dim))) {
    file_data$variable$name <- var
  }

  # get details of file
  result  <- ncvar_get(id, file_data$variable$name)

  # extract time information
  date.time <- get_time(file_data$time_info$units, file_data$dimension_data$t)

  # create return list
  if (length(dim(result)) >= 3 && any(TIME_NAMES %in% c(names(id$var), names(id$dim)))) {
    output <- list(result, date.time)
    names(output) <- c(file_data$variable$name, TIME_NAMES$DEFAULT)
  } else {
    output <- list(result)
    names(output) <- c(file_data$variable$name)
  }
  nc_close(id)

  calc_time_end <- Sys.time()
  if (verbose) message(get_processing_time_string(calc_time_start, calc_time_end))
  return(output)
}
