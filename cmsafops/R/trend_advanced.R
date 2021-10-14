#'Determine multiple linear trends.
#'
#'The function determines the trend from data of two CM SAF NetCDF input
#'files basing on a multiple linear model. Learn more 
#'<http://www.sthda.com/english/articles/40-regression-analysis/
#'168-multiple-linear-regression-in-r/>
#'
#'@param var1 Name of NetCDF variable of the first data set (character).
#'@param infile1 Filename of input NetCDF file. This may include the directory
#'  (character).
#'@param var2 Name of NetCDF variable of the second data set (character).
#'@param infile2 Filename of input NetCDF file. This may include the directory
#'  (character).
#'@param outfile Filename of output NetCDF file. This may include the directory
#'  (character).
#'@param nc34 NetCDF version of output file. If \code{nc34 = 3} the output file will be
#'  in NetCDFv3 format (numeric). Default output is NetCDFv4.
#'@param overwrite logical; should existing output file be overwritten?
#'@param verbose logical; if TRUE, progress messages are shown
#'@param nc1 Alternatively to \code{infile1} you can specify the input as an
#'  object of class `ncdf4` (as returned from \code{ncdf4::nc_open}).
#'@param nc2 Alternatively to \code{infile2} you can specify the input as an
#'  object of class `ncdf4` (as returned from \code{ncdf4::nc_open}).
#'
#'@return A NetCDF file including four data layers is written. One layer
#'  (trend1) contains the linear trend based on the time steps.
#'  Another layer (trend2) contains linear trend based on var2. 
#'  The two other layers contain a measure for the significance of the
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
#'## Create two example NetCDF files with a similar structure as used by CM
#'## SAF. The files are created with the ncdf4 package.  Alternatively
#'## example data can be freely downloaded here: <https://wui.cmsaf.eu/>
#'
#'library(ncdf4)
#'
#'## create some (non-realistic) example data
#'lon <- seq(10, 15, 0.5)
#'lat <- seq(50, 55, 0.5)
#'time <- as.Date("2000-05-31")
#'origin <- as.Date("1983-01-01 00:00:00")
#'time <- as.numeric(difftime(time, origin, units = "hour"))
#'data1 <- array(250:350, dim = c(11, 11, 1))
#'data2 <- array(230:320, dim = c(11, 11, 1))
#'
#'## create example NetCDF
#'x <- ncdim_def(name = "lon", units = "degrees_east", vals = lon)
#'y <- ncdim_def(name = "lat", units = "degrees_north", vals = lat)
#'t <- ncdim_def(name = "time", units = "hours since 1983-01-01 00:00:00",
#'              vals = time, unlim = TRUE)
#'var1 <- ncvar_def("SIS", "W m-2", list(x, y, t), -999, prec = "float")
#'vars <- list(var1)
#'ncnew_1 <- nc_create(file.path(tempdir(), "CMSAF_example_file_1.nc"), vars)
#'ncnew_2 <- nc_create(file.path(tempdir(), "CMSAF_example_file_2.nc"), vars)
#'
#'ncvar_put(ncnew_1, var1, data1)
#'ncvar_put(ncnew_2, var1, data2)
#'
#'ncatt_put(ncnew_1, "lon", "standard_name", "longitude", prec = "text")
#'ncatt_put(ncnew_1, "lat", "standard_name", "latitude", prec = "text")
#'
#'ncatt_put(ncnew_2, "lon", "standard_name", "longitude", prec = "text")
#'ncatt_put(ncnew_2, "lat", "standard_name", "latitude", prec = "text")
#'nc_close(ncnew_1)
#'nc_close(ncnew_2)
#'
#'## Determine the multiple linear trend of the example CM SAF NetCDF files and
#'## write the output to a new file.
#'trend_advanced(var1 = "SIS", infile1 = file.path(tempdir(),"CMSAF_example_file_1.nc"), 
#'       var2 = "SIS", infile2 = file.path(tempdir(), "CMSAF_example_file_2.nc"),
#'       outfile = file.path(tempdir(),"CMSAF_example_file_trend_advanced.nc"))
#'
#'unlink(c(file.path(tempdir(),"CMSAF_example_file_1.nc"), 
#'       file.path(tempdir(),"CMSAF_example_file_2.nc"),
#'       file.path(tempdir(),"CMSAF_example_file_trend_advanced.nc")))
trend_advanced <- function(var1, infile1, var2, infile2, outfile, nc34 = 4,
                  overwrite = FALSE, verbose = FALSE, nc1 = NULL, nc2 = NULL) {
  calc_time_start <- Sys.time()
  
  check_variable(var1)
  check_variable(var2)
  
  if (is.null(nc1)) check_infile(infile1)
  if (is.null(nc2)) check_infile(infile2)
  
  check_outfile(outfile)
  outfile <- correct_filename(outfile)
  check_overwrite(outfile, overwrite)
  check_nc_version(nc34)

  sig1 <- list(name = "sig1",
              standard_name = "significance 1",
              long_name = "significance based on 95% confidence interval (time)",
              units = "1",
              info = "1 = positive significant, 0 = not significant, -1 = negative significant")
  
  sig2 <- list(name = "sig2",
               standard_name = "significance 2",
               long_name = paste0("significance based on 95% confidence interval (",var2,")"),
               units = "1",
               info = "1 = positive significant, 0 = not significant, -1 = negative significant")

  ##### extract data from two files #####
  file_data <- read_file(infile1, var1, nc = nc1)
  file_data_two <- read_file(infile2, var2, nc = nc2)
  
  file_data$variable$prec <- "float"

  time_bnds <- get_time_bounds_1(
    file_data$dimension_data$t
  )
  
  result <- calc_trend_advanced(var1, infile1, var2, infile2, file_data, file_data_two, nc1 = nc1, nc2 = nc2)
  
  file_data$variable$attributes$missing_value <- -999
  
  vars_data <- list(result = result, time_bounds = time_bnds)

  nc_format <- get_nc_version(nc34)
  cmsaf_info <- paste0("cmsaf::trend_advanced for variable ",
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

  vars <- define_vars_trend_advanced(file_data$variable, dims, nc_format$compression, sig1, sig2)

  write_output_file_trend_advanced(
    outfile,
    nc_format$force_v4,
    vars,
    vars_data,
    file_data$variable$name,
    file_data_two$variable$name,
    file_data$grid$vars, file_data$grid$vars_data,
    sig1, sig2, file_data$variable$attributes$standard_name,
    cmsaf_info,
    file_data$time_info$calendar,
    file_data$variable$attributes,
    global_attributes
  )
  
  calc_time_end <- Sys.time()
  if (verbose) message(get_processing_time_string(calc_time_start, calc_time_end))
}
