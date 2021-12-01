#'Determine multi-monthly means
#'
#'The function determines multi-monthly mean values from data of a single CM SAF
#'NetCDF input file. The months are given as a vector of integers from 1 to 12.
#'This allows means of user-defined seasons.
#'
#'@param var Name of NetCDF variable (character).
#'@param month  Months which should be averaged, in form of a comma separated
#'  vector of integer values from 1 to 12 (integer).
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
#'@return A NetCDF file including a time series of multi-monthly means is
#'  written.
#'@export
#'
#'@family monthly statistics
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
#'## Determine the mean of the monsoon seas from June to September of the
#'## example CM SAF NetCDF file and write the output to a new file.
#'multimonmean(var = "SIS", month = c(6, 7, 8, 9), infile = 
#'  file.path(tempdir(),"CMSAF_example_file.nc"), outfile =
#'  file.path(tempdir(),"CMSAF_example_file_multimonmean.nc"))
#'
#'unlink(c(file.path(tempdir(),"CMSAF_example_file.nc"), 
#'  file.path(tempdir(),"CMSAF_example_file_multimonmean.nc")))
multimonmean <- function(var, month = c(1), infile, outfile, nc34 = 4,
                         overwrite = FALSE, verbose = FALSE, nc = NULL) {
  calc_time_start <- Sys.time()

  check_variable(var)
  if (is.null(nc)) check_infile(infile)
  check_outfile(outfile)
  outfile <- correct_filename(outfile)
  check_overwrite(outfile, overwrite)
  check_nc_version(nc34)

  ##### extract data from file #####
  file_data <- read_file(infile, var, nc = nc)
  file_data$variable$prec <- "float"

  date_time <- get_date_time(file_data$dimension_data$t, file_data$time_info$units)
  months_all <- date_time$months
  months_unique <- sort(unique(months_all))
  years_all <- date_time$years
  years_unique <- sort(unique(years_all))

  nmonmeans <- length(years_unique) * length(months_unique)
  mul <- months_all * years_all

  if (!(length(month) == sum(month %in% months_all))) {
    stop(paste0("No match. Months are: ", months_all))
  }

  # Use placeholder for result so that it can be calculated later without the
  # need to have all input data in memory concurrently.
  data_placeholder <- array(
    file_data$variable$attributes$missing_value,
    dim = c(length(file_data$dimension_data$x),
            length(file_data$dimension_data$y),
            1)
  )
  time_bnds <- get_time_bounds_1(
    file_data$dimension_data$t
  )
  vars_data <- list(result = data_placeholder, time_bounds = time_bnds)

  nc_format <- get_nc_version(nc34)
  cmsaf_info <- paste0("cmsafops::multimonmean of month(s): ", paste(month, collapse = ", "), " for variable ", file_data$variable$name)

  ##### prepare output #####
  global_att_list <- names(file_data$global_att)
  global_att_list <- global_att_list[toupper(global_att_list) %in% toupper(GLOBAL_ATT_DEFAULT)]
  global_attributes <- file_data$global_att[global_att_list]

  dims <- define_dims(file_data$grid$is_regular,
                      file_data$dimension_data$x,
                      file_data$dimension_data$y,
                      0,
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

  ##### calculate and write result #####
  nc_out <- nc_open(outfile, write = TRUE)
  dummy_vec <- seq_along(months_all)
  dum_dat <- array(NA, dim = c(length(file_data$dimension_data$x), length(file_data$dimension_data$y), length(month)))
  if (!is.null(nc)) nc_in <- nc
  else nc_in <- nc_open(infile)
  count <- 1
  for (i in seq_along(years_unique)) {
    mon_dummy <- NULL
    for (j in seq_along(month)) {
      dum <- which(months_all == month[j] & years_all == years_unique[i])
      if (length(dum) > 0) {
        mon_dummy <- append(mon_dummy, dum)
        dumdum_dat <- ncvar_get(nc_in, file_data$variable$name, start = c(1, 1, dum[1]), count = c(-1, -1, length(dum)), collapse_degen = FALSE)
        dumdum_dat <- rowMeans(dumdum_dat, dims = 2, na.rm = TRUE)
        dum_dat[, , j] <- dumdum_dat
      }
    }
    if (sum(!is.na(dum_dat)) != 0) {
      mean_data <- rowMeans(dum_dat, dims = 2, na.rm = TRUE)
      mean_data[is.na(mean_data)] <- file_data$variable$attributes$missing_value
      tdum <- min(file_data$dimension_data$t[mon_dummy])
      time_bnds[1, 1] <- min(file_data$dimension_data$t[mon_dummy])
      time_bnds[2, 1] <- max(file_data$dimension_data$t[mon_dummy])
      ncvar_put(nc_out, vars[[1]], mean_data, start = c(1, 1, count), count = c(-1, -1, 1))
      ncvar_put(nc_out, dims$t, tdum, start = count, count = 1)
      ncvar_put(nc_out, vars[[2]], time_bnds, start = c(1, count), count = c(-1, 1))
      count <- count + 1
    }
  }
  nc_close(nc_out)
  if (is.null(nc)) nc_close(nc_in)

  calc_time_end <- Sys.time()
  if (verbose) message(get_processing_time_string(calc_time_start, calc_time_end))
}
