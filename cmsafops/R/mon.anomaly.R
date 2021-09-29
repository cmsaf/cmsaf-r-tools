#'Determine monthly anomalies
#'
#'The function subtracts from each timestep of a time series the corresponding
#'multi-year monthly mean. To get monthly anomalies, the input file should
#'contain monthly mean values.
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
#'@return A NetCDF file including a time series of differences is written.
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
#'lon <- seq(10, 15, 0.5)
#'lat <- seq(50, 55, 0.5)
#'time <- seq(as.Date("2000-01-01"), as.Date("2010-12-31"), "month")
#'origin <- as.Date("1983-01-01 00:00:00")
#'time <- as.numeric(difftime(time, origin, units = "hour"))
#'data <- array(250:350, dim = c(11, 11, 132))
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
#'## Determine the monthly anomalies of the example CM SAF NetCDF file and
#'## write the output to a new file.
#'mon.anomaly(var = "SIS", infile = file.path(tempdir(),"CMSAF_example_file.nc"),
#'  outfile = file.path(tempdir(),"CMSAF_example_file_mon.anomaly.nc"))
#'
#'unlink(c(file.path(tempdir(),"CMSAF_example_file.nc"), 
#'  file.path(tempdir(),"CMSAF_example_file_mon.anomaly.nc")))
mon.anomaly <- function(var, infile, outfile, nc34 = 4, overwrite = FALSE, verbose = FALSE,
                        nc = NULL) {
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
  months_all <- get_date_time(file_data$dimension_data$t, file_data$time_info$units)$months
  years_all <- get_date_time(file_data$dimension_data$t, file_data$time_info$units)$year
  mul <- months_all * years_all
  months_unique <- sort(unique(months_all))

  ### calculate monthly means
  monmean_file <- tempfile(pattern = "monmean", tmpdir = tempdir(), fileext = ".nc")
  monmean(var = file_data$variable$name, infile = infile, outfile = monmean_file, nc34 = nc34, overwrite = TRUE, verbose = verbose, nc = nc)
  nc_monmean <- nc_open(monmean_file)
  monmeans <- ncvar_get(nc_monmean, file_data$variable$name)
  monmeans_months_all <- get_date_time(ncvar_get(nc_monmean, "time"), ncatt_get(nc_monmean, "time", "units")$value)$months
  nc_close(nc_monmean)

  dates <- as.POSIXlt(get_time(file_data$time_info$units, file_data$dimension_data$t), format = "%Y-%m-%d")
  dates$mday <- rep(1, length(dates))
  dates <- as.Date(dates, format = "%Y-%m-%d")
  time_data <- unique(dates)
  unit_vec <- unlist(strsplit(file_data$time_info$units, split = " "))
  snc_index <- which(unit_vec == "since")
  time_data <- as.numeric(difftime(time_data, as.Date(unit_vec[snc_index + 1]), units = unit_vec[snc_index - 1]))

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

  # Use placeholder for result so that it can be calculated later without the
  # need to have all input data in memory concurrently.
  data_placeholder <- array(
    file_data$variable$attributes$missing_value,
    dim = c(length(file_data$dimension_data$x),
            length(file_data$dimension_data$y),
            length(time_data))
  )

    time_bnds <- get_time_bounds_mul(
      file_data$dimension_data$t, test, test_count, mul
    )

    vars_data <- list(result = data_placeholder, time_bounds = time_bnds)

  clim <- get_climatology(infile, file_data, nc = nc)

  nc_format <- get_nc_version(nc34)
  cmsaf_info <- paste0("cmsaf::mon.anomaly for variable ",
                       file_data$variable$name)

  #time_data <- file_data$dimension_data$t

  ##### prepare output #####
  global_att_list <- names(file_data$global_att)
  global_att_list <- global_att_list[toupper(global_att_list) %in% toupper(GLOBAL_ATT_DEFAULT)]
  global_attributes <- file_data$global_att[global_att_list]

  dims <- define_dims(file_data$grid$is_regular,
                      file_data$dimension_data$x,
                      file_data$dimension_data$y,
                      time_data,
                      NB2,
                      file_data$time_info$units,
                      with_time_bnds = file_data$time_info$has_time_bnds)

  vars <- define_vars(file_data$variable, dims, nc_format$compression, with_time_bnds = file_data$time_info$has_time_bnds)

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
    global_attributes,
    with_time_bnds = file_data$time_info$has_time_bnds
  )

  ##### calculate and write result #####
  nc_out <- nc_open(outfile, write = TRUE)
  dummy_vec <- seq_along(months_all)

  for (j in seq_along(months_unique)) {
    mon_dummy <- which(months_all == months_unique[j])
    monmean_dummy <- which(monmeans_months_all == months_unique[j])
    startt <- dummy_vec[mon_dummy]

    if (!is.null(nc)) nc_in <- nc
    else nc_in <- nc_open(infile)

    dum_dat <- array(NA, dim = c(length(file_data$dimension_data$x), length(file_data$dimension_data$y), length(startt)))

    for (i in seq_along(startt)) {
       dum_dat[, , i] <- ncvar_get(nc_in, file_data$variable$name, start = c(1, 1, startt[i]), count = c(-1, -1, 1), collapse_degen = FALSE)
    }

    if (is.null(nc)) nc_close(nc_in)

    if (verbose) message(paste0("apply monthly anomaly ", j,
                                " of ", length(months_unique)))

    mean_data <- rowMeans(dum_dat, dims = 2, na.rm = TRUE)
    #mean_data <- array(mean_data, dim = c(dim(mean_data), length(mon_dummy)))
    mean_data <- array(mean_data, dim = c(dim(mean_data), length(monmean_dummy)))
    anom_data <- array(monmeans[,,monmean_dummy], dim(mean_data)) - mean_data
    anom_data[is.na(anom_data)] <- file_data$variable$attributes$missing_value
    for (i in seq_along(monmean_dummy)) {
      ncvar_put(nc_out, vars[[1]], anom_data[,,i], start = c(1, 1, monmean_dummy[i]), count = c(-1, -1, 1))
	}
    }

  nc_close(nc_out)

  calc_time_end <- Sys.time()
  if (verbose) message(get_processing_time_string(calc_time_start, calc_time_end))
}
