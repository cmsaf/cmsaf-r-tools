#'Determine seasonal anomalies.
#'
#'The function determines the seasonal means of a time series and subtracts the
#'corresponding multi-seasonal means to get seasonal anomalies.
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
#'
#'@return A NetCDF file including a time series of seasonal anomalies is written.
#'@export
#'
#'@family seasonal statistics
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
#'## Determine the seasonal anomalies of the example CM SAF NetCDF file
#'## and write the output to a new file.
#'seas.anomaly(var = "SIS", infile = file.path(tempdir(),"CMSAF_example_file.nc"),
#'  outfile = file.path(tempdir(),"CMSAF_example_file_seas.anomaly.nc"))
#'
#'unlink(c(file.path(tempdir(),"CMSAF_example_file.nc"), 
#'  file.path(tempdir(),"CMSAF_example_file_seas.anomaly.nc")))
seas.anomaly <- function(var, infile, outfile, nc34 = 4, overwrite = FALSE, verbose = FALSE) {
  calc_time_start <- Sys.time()

  check_variable(var)
  check_infile(infile)
  check_outfile(outfile)
  outfile <- correct_filename(outfile)
  check_overwrite(outfile, overwrite)
  check_nc_version(nc34)

  ##### extract data from file #####
  file_data <- read_file(infile, var)
  file_data$variable$prec <- "float"

  date_time <- get_date_time(file_data$dimension_data$t, file_data$time_info$units)
  months_all <- date_time$months
  months_unique <- sort(unique(months_all))
  years_all <- date_time$years
  years_unique <- sort(unique(years_all))
  seasons <- set_seasons(years_unique, years_all, months_all)

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
  cmsaf_info <- paste0("cmsaf::seas.anomaly for variable ", file_data$variable$name)

  ##### prepare output #####
  global_att_list <- names(file_data$global_att)
  global_att_list <- global_att_list[toupper(global_att_list) %in% toupper(GLOBAL_ATT_DEFAULT)]
  global_attributes <- file_data$global_att[global_att_list]

  dims <- define_dims(file_data$grid$is_regular,
                      file_data$dimension_data$x,
                      file_data$dimension_data$y,
                      time_data = 0,
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
  nc_in <- nc_open(infile)
  nc_out <- nc_open(outfile, write = TRUE)
  dummy_vec <- seq_along(months_all)

  seas_clim <- array(NA, dim = c(length(file_data$dimension_data$x), length(file_data$dimension_data$y), 4))
  for (j in 1:4) {
    seas_dummy <- NA

    switch(j,
           {
             win <- which(months_all %in% c(1, 2, 12))
             if (length(win) >= 1) {
               seas_dummy <- win
             }
           },
           {
             spr <- which(months_all %in% c(3, 4, 5))
             if (length(spr) >= 1) {
               seas_dummy <- spr
             }
           },
           {
             sum <- which(months_all %in% c(6, 7, 8))
             if (length(sum) >= 1) {
               seas_dummy <- sum
             }
           },
           {
             aut <- which(months_all %in% c(9, 10, 11))
             if (length(aut) >= 1) {
               seas_dummy <- aut
             }
           }
    )

    dum_dat <- array(NA, dim = c(length(file_data$dimension_data$x), length(file_data$dimension_data$y), length(seas_dummy)))

    for (i in seq_along(seas_dummy)) {
      if (!is.na(seas_dummy[i])) {
        dum_dat[, , i] <- ncvar_get(nc_in, file_data$variable$name, start = c(1, 1, seas_dummy[i]), count = c(-1, -1, 1), collapse_degen = FALSE)
      }
    }
    seas_clim[, , j] <- rowMeans(dum_dat, dims = 2, na.rm = TRUE)
  }

  dum_dat <- array(NA, dim = c(length(file_data$dimension_data$x), length(file_data$dimension_data$y), 3))
  count <- 1
  for (i in seq_along(years_unique)) {
    for (j in 1:4) {
      seas_dummy <- NA

      if (j == 1) {
        win <- which((months_all %in% c(1, 2) & years_all == years_unique[i]) | (months_all == 12 & years_all == (years_unique[i] - 1)))
        if (length(win) >= 3) {
          seas_dummy <- win
        }
      }
      if (j == 2) {
        spr <- which(months_all %in% c(3, 4, 5) & years_all == years_unique[i])
        if (length(spr) >= 3) {
          seas_dummy <- spr
        }
      }
      if (j == 3) {
        sum <- which(months_all %in% c(6, 7, 8) & years_all == years_unique[i])
        if (length(sum) >= 3) {
          seas_dummy <- sum
        }
      }
      if (j == 4) {
        aut <- which(months_all %in% c(9, 10, 11) & years_all == years_unique[i])
        if (length(aut) >= 3) {
          seas_dummy <- aut
        }
      }

      if (!all(is.na(seas_dummy))) {
        dum_dat <- array(NA, dim = c(length(file_data$dimension_data$x), length(file_data$dimension_data$y), length(seas_dummy)))

        for (k in seq_along(seas_dummy)) {
          if (!is.na(seas_dummy[k])) {
            dum_dat[, , k] <- ncvar_get(nc_in, file_data$variable$name, start = c(1, 1, seas_dummy[k]), count = c(-1, -1, 1), collapse_degen = FALSE)
          }
        }

        mean_data <- rowMeans(dum_dat, dims = 2, na.rm = TRUE)
        mean_data <- mean_data - seas_clim[, , j]
        mean_data[is.na(mean_data)] <- file_data$variable$attributes$missing_value
        tdum <- min(file_data$dimension_data$t[seas_dummy], na.rm = TRUE)
        time_bnds[1, 1] <- min(file_data$dimension_data$t[seas_dummy], na.rm = TRUE)
        time_bnds[2, 1] <- max(file_data$dimension_data$t[seas_dummy], na.rm = TRUE)
        ncvar_put(nc_out, vars[[1]], mean_data, start = c(1, 1, count), count = c(-1, -1, 1))
        ncvar_put(nc_out, dims$t, tdum, start = count, count = 1)
        ncvar_put(nc_out, vars[[2]], time_bnds, start = c(1, count), count = c(-1, 1))
        count <- count + 1

      }
    }
  }

  nc_close(nc_out)
  nc_close(nc_in)

  calc_time_end <- Sys.time()
  if (verbose) message(get_processing_time_string(calc_time_start, calc_time_end))
}
