#'This function determines the diurnal range.
#'
#'The function calculates the difference of maximum and minimum values of hourly
#'data from a single CM SAF NetCDF input file.
#'
#'@param var Name of NetCDF variable (character).
#'@param infile Filename of input NetCDF file. This may include the directory
#'  (character).
#'@param outfile Filename of output NetCDF file. This may include the directory
#'  (character).
#'@param nc34 NetCDF version of output file. If \code{nc34 = 3} the output file will be
#'  in NetCDFv3 format (numeric). Default output is NetCDFv4.
#'@param overwrite logical; should existing output file be overwritten?
#'@param verbose logical; if TRUE, progress messages are shown
#'@param nc Alternatively to \code{infile} you can specify the input as an
#'  object of class `ncdf4` (as returned from \code{ncdf4::nc_open}).
#'
#'@return A NetCDF file including a time series of the diurnal range is written
#'  (character).
#'@export
#'
#'@family daily statistics
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
#'time <- seq(ISOdate(2000, 1, 1), ISOdate(2000, 1, 6), "hours")
#'origin <- as.Date("1983-01-01 00:00:00")
#'time <- as.numeric(difftime(time, origin, units = "hour"))
#'data <- array(250:350, dim = c(21, 21, 121))
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
#'## Determine the diurnal range of the example CM SAF NetCDF file and
#'## write the output to a new file.
#'dayrange(var = "SIS", infile = file.path(tempdir(),"CMSAF_example_file.nc"), 
#'  outfile = file.path(tempdir(),"CMSAF_example_file_dayrange.nc"))
#'
#'unlink(c(file.path(tempdir(),"CMSAF_example_file.nc"), 
#'  file.path(tempdir(),"CMSAF_example_file_dayrange.nc")))
dayrange <- function(var, infile, outfile, nc34=4, overwrite = FALSE, verbose = FALSE,
                     nc = NULL) {
  check_variable(var)

  if (is.null(nc)) check_infile(infile)
  check_outfile(outfile)

  outfile <- correct_filename(outfile)
  check_overwrite(outfile, overwrite)

  check_nc_version(nc34)

  calc_time_start <- Sys.time()

  # get information about dimensions and attributes
  file_data <- read_file(infile, var, nc = nc)
  file_data$variable$prec <- "float"

  # extract time information
  date_time <- get_date_time(file_data$dimension_data$t,
                             file_data$time_info$units)
  days <- date_time$days
  months <- date_time$months
  years <- date_time$years

  mul <- years * months + days
  dummy_vec <- seq_along(days)

  testnum <- mul
  test_count <- 0
  test <- -999

  for (i in seq_along(mul)) {
    if (sum(testnum == mul[i]) >= 1) {
      test_count <- test_count + 1
      test <- cbind(test, mul[i])
      testnum[testnum == mul[i]] <- -999
    }
  }

  result <- array(file_data$variable$attributes$missing_value,
                  dim = c(length(file_data$dimension_data$x),
                          length(file_data$dimension_data$y), 1))
  time_bnds <- array(NA, dim = c(2, test_count))
  vars_data <- list(result = result, time_bounds = time_bnds[, 1])

  count <- 1
  for (i in seq_len(test_count)) {
    mon_dummy <- which(mul == test[i + 1])
    if (length(mon_dummy) >= 1) {
      time_bnds[1, count] <- file_data$dimension_data$t[min(mon_dummy)]
      time_bnds[2, count] <- file_data$dimension_data$t[max(mon_dummy)]
      count <- count + 1
    }
  }

  # create netcdf
  nc_format <- get_nc_version(nc34)
  cmsaf_info <- (paste0("cmsafops::dayrange for variable ", file_data$variable$name))

  time_data <- time_bnds[1, ]

  ##### prepare output #####
  global_att_list <- names(file_data$global_att)
  global_att_list <- global_att_list[toupper(global_att_list) %in% toupper(GLOBAL_ATT_DEFAULT)]
  global_attributes <- file_data$global_att[global_att_list]

  dims <- define_dims(file_data$grid$is_regular,
                      file_data$dimension_data$x,
                      file_data$dimension_data$y,
                      time_data[1],
                      NB2,
                      file_data$time_info$units)

  vars <- define_vars(file_data$variable, dims, nc_format$compression)

  write_output_file(
    outfile,
    nc_format$force_v4,
    vars,
    vars_data,
    file_data$variable$name,
    file_data$grid$vars, file_data$grid$vars_data,
    cmsaf_info,
    file_data$time_info$calendar,
    file_data$variable$attributes,
    global_attributes
  )

  dummy <- array(NA, dim = c(length(file_data$dimension_data$x), length(file_data$dimension_data$y), 1))
  if (!is.null(nc)) nc_in <- nc
  else nc_in <- nc_open(infile)
  nc_out <- nc_open(outfile, write = TRUE)

  count <- 1
  for (i in seq_len(test_count)) {
    day_dummy <- which(mul == test[i + 1])
    if (!length(day_dummy) >= 1) {
      stop("length of daily data not sufficient")
    }
    startt <- min(dummy_vec[day_dummy])
    countt <- length(day_dummy)
    dum_dat <- ncvar_get(nc_in, file_data$variable$name,
                         start = c(1, 1, startt), count = c(-1, -1, countt),
                         collapse_degen = FALSE)
    day_data <- apply(dum_dat, 1:2, "range")
    dummy[, , 1] <- day_data[2, , ] - day_data[1, , ]
    day_data <- dummy
    day_data[is.na(day_data)] <- file_data$variable$attributes$missing_value
    ncvar_put(nc_out, vars[[1]], day_data, start = c(1, 1, count),
              count = c(-1, -1, 1))
    ncvar_put(nc_out, dims$t, time_data[i], start = count, count = 1)
    ncvar_put(nc_out, vars[[2]], time_bnds[, i], start = c(1, count),
              count = c(-1, 1))
    count <- count + 1
  }
  if (is.null(nc)) nc_close(nc_in)
  nc_close(nc_out)

  calc_time_end <- Sys.time()
  if (verbose) message(get_processing_time_string(calc_time_start, calc_time_end))
}
