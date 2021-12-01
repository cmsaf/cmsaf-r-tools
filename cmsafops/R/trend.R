#'Determine linear trends.
#'
#'The function determines the trend from data of a single CM SAF NetCDF input
#'file basing on a simple linear model. Depending on the file size, this
#'function could be very time consuming, thus there are two available options.
#'Option 1 (default) is using an apply approach and will read the whole data in
#'once. This option is quite fast, but requires enough memory. Option 2 is using
#'the same calculation, but reads the data pixel by pixel, which is very slow,
#'but can also be applied for large data files, which would not fit into the
#'memory at once.
#'
#'@param var Name of NetCDF variable (character).
#'@param infile Filename of input NetCDF file. This may include the directory
#'  (character).
#'@param outfile Filename of output NetCDF file. This may include the directory
#'  (character).
#'@param option The way of data handling. Option = 1 is fast but memory
#'  consuming (default). Option = 2 is slow, but needs much less memory. Input
#'  is either 1 or 2 (numeric).
#'@param nc34 NetCDF version of output file. If \code{nc34 = 3} the output file will be
#'  in NetCDFv3 format (numeric). Default output is NetCDFv4.
#'@param overwrite logical; should existing output file be overwritten?
#'@param verbose logical; if TRUE, progress messages are shown
#'@param nc Alternatively to \code{infile} you can specify the input as an
#'  object of class `ncdf4` (as returned from \code{ncdf4::nc_open}).
#'
#'@return A NetCDF file including three data layers is written. One layer
#'  (trend1) contains the linear trend multiplied by the number of time steps.
#'  In older versions of the package (<= 1.7) the trend was given in the same
#'  way as trend1. Another layer (trend2) contains just the calculated linear
#'  trend. An additional layer contains a measure for the significance of the
#'  calculated trends, which was derived using the 95 % confidence interval.
#'  The significance is calculated from the lower and upper value of the
#'  95% confidence interval:
#'  lower or upper value < 0: sig = 0 (not significant);
#'  lower and upper value < 0: sig = -1 (negative significant);
#'  lower and upper value > 0: sig = 1  (positive significant)
#'
#'@export
#'
#'@family temporal operators
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
#'## Determine the trend of the example CM SAF NetCDF file and write the
#'## output to a new file.
#'trend(var = "SIS", infile = file.path(tempdir(),"CMSAF_example_file.nc"), 
#'  outfile = file.path(tempdir(),"CMSAF_example_file_trend.nc"))
#'
#'unlink(c(file.path(tempdir(),"CMSAF_example_file.nc"), 
#'  file.path(tempdir(),"CMSAF_example_file_trend.nc")))
trend <- function(var, infile, outfile, option = 1, nc34 = 4,
                  overwrite = FALSE, verbose = FALSE, nc = NULL) {
  check_variable(var)

  if (is.null(nc)) check_infile(infile)
  check_outfile(outfile)

  outfile <- correct_filename(outfile)
  check_overwrite(outfile, overwrite)

  check_nc_version(nc34)

  calc_time_start <- Sys.time()

  sig <- list(name = "sig",
              standard_name = "significance",
              long_name = "significance based on 95% confidence interval",
              units = "1",
              info = "1 = positive significant, 0 = not significant, -1 = negative significant")

  ##### extract data from file #####
  file_data <- read_file(infile, var, nc = nc)
  file_data$variable$prec <- "float"

  time_bnds <- get_time_bounds_1(
    file_data$dimension_data$t
  )

  result <- calc_trend(infile, file_data, option, nc = nc)

  vars_data <- list(result = result, time_bounds = time_bnds)

  nc_format <- get_nc_version(nc34)
  cmsaf_info <- paste0("cmsafops::trend for variable ",
                       file_data$variable$name)

  time_data <- time_bnds[1, ]

  ##### prepare output #####
  global_att_list <- names(file_data$global_att)
  global_att_list <- global_att_list[toupper(global_att_list) %in% toupper(GLOBAL_ATT_DEFAULT)]
  global_attributes <- file_data$global_att[global_att_list]

  dims <- define_dims(file_data$grid$is_regular,
                      file_data$dimension_data$x,
                      file_data$dimension_data$y,
                      time_data,
                      NB2,
                      file_data$time_info$units)

  vars <- define_vars_trend(file_data$variable, dims, nc_format$compression, sig)

  write_output_file_trend(
    outfile,
    nc_format$force_v4,
    vars,
    vars_data,
    file_data$variable$name,
    file_data$grid$vars, file_data$grid$vars_data,
    sig, file_data$variable$attributes$standard_name,
    cmsaf_info,
    file_data$time_info$calendar,
    file_data$variable$attributes,
    global_attributes
  )

  calc_time_end <- Sys.time()
  if (verbose) message(get_processing_time_string(calc_time_start, calc_time_end))
}
