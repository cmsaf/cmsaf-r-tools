#'Concatenate datasets of several NetCDF input files.
#'
#'This function concatenates datasets of an arbitrary number of input files. All
#'input files have to have the same structure with the same variable and
#'different timesteps.
#'
#'@aliases cat
#'
#'@param var Name of NetCDF variable (character).
#'@param infiles Vector with filenames of input NetCDF files. The file names may
#'  include the directory (character).
#'@param outfile Filename of output NetCDF file. This may include the directory
#'  (character).
#'@param nc34 NetCDF version of output file. If \code{nc34 = 3} the output file will be
#'  in NetCDFv3 format (numeric). Default output is NetCDFv4.
#'@param overwrite logical; should existing output file be overwritten?
#'@param verbose logical; if TRUE, progress messages are shown
#'
#'@return A NetCDF file including the merged time series is written. The
#'  resulting file uses the meta data of the first input file.
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
#'time <- c(as.Date("2000-01-01"), as.Date("2001-02-01"))
#'origin <- as.Date("1983-01-01 00:00:00")
#'time <- as.numeric(difftime(time, origin, units = "hour"))
#'data1 <- array(250:350, dim = c(21, 21, 1))
#'data2 <- array(230:320, dim = c(21, 21, 1))
#'
#'## create two simple example NetCDF files
#'
#'x <- ncdim_def(name = "lon", units = "degrees_east", vals = lon)
#'y <- ncdim_def(name = "lat", units = "degrees_north", vals = lat)
#'t <- ncdim_def(name = "time", units = "hours since 1983-01-01 00:00:00",
#'  vals = time[1], unlim = TRUE)
#'var1 <- ncvar_def("SIS", "W m-2", list(x, y, t), -1, prec = "short")
#'vars <- list(var1)
#'ncnew <- nc_create(file.path(tempdir(),"CMSAF_example_file_1.nc"), vars)
#'ncvar_put(ncnew, var1, data1)
#'ncatt_put(ncnew, "lon", "standard_name", "longitude", prec = "text")
#'ncatt_put(ncnew, "lat", "standard_name", "latitude", prec = "text")
#'nc_close(ncnew)
#'t <- ncdim_def(name = "time", units = "hours since 1983-01-01 00:00:00",
#'  vals = time[2], unlim = TRUE)
#'ncnew <- nc_create(file.path(tempdir(),"CMSAF_example_file_2.nc"), vars)
#'ncvar_put(ncnew, var1, data2)
#'ncatt_put(ncnew, "lon", "standard_name", "longitude", prec = "text")
#'ncatt_put(ncnew, "lat", "standard_name", "latitude", prec = "text")
#'nc_close(ncnew)
#'
#'## Cut a region and merge both example CM SAF NetCDF files into one
#'## output file.  Get path information of working directory with getwd()
#'## command.
#'wd <- getwd()
#'cmsaf.cat(var = "SIS", infiles = c(file.path(tempdir(),
#'  "CMSAF_example_file_1.nc"), file.path(tempdir(),"CMSAF_example_file_2.nc")),
#'  outfile = file.path(tempdir(),"CMSAF_example_file_cat.nc"))
#'
#'unlink(c(file.path(tempdir(),"CMSAF_example_file_1.nc"), 
#'  file.path(tempdir(),"CMSAF_example_file_2.nc"),
#'  file.path(tempdir(),"CMSAF_example_file_cat.nc")))
cmsaf.cat <- function(var, infiles, outfile, nc34 = 4, overwrite = FALSE, verbose = FALSE) {
  check_variable(var)
  check_infiles(infiles)
  check_outfile(outfile)
  outfile <- correct_filename(outfile)
  check_overwrite(outfile, overwrite)
  check_nc_version(nc34)

  calc_time_start <- Sys.time()

  filelist <- infiles
  fdim <- length(filelist)

  if (fdim == 0) {
    stop("No files found that match the pattern")
  }

  filelist <- sort(filelist)
  fdim <- length(filelist)

  file <- filelist[1]

  # get information about dimensions and attributes
  file_data <- read_file(file, var)
  if (file_data$time_info$has_time_bnds) {
    time_bnds <- get_time_bounds_from_file(file)
  }

  # nc_in <- nc_open(file)
  # result <- ncvar_get(nc_in,file_data$variable$name,collapse_degen=FALSE)
  # nc_close(nc_in)
  result <- read_ncvar(file_data$variable$name, file)[[file_data$variable$name]]
  result[is.na(result)] <- file_data$variable$attributes$missing_value

  if (file_data$time_info$has_time_bnds) {
    vars_data <- list(result = result, time_bounds = time_bnds[, 1])
  }else{
    vars_data <- list(result = result)
  }

  # get time reference
  dt_ref <- get_time(file_data$time_info$units, 0)
  unit_ref <- unlist(strsplit(file_data$time_info$units, split = " "))[1]

  # check reference time unit
  unit_ref_test <- switch(
    substr(toupper(unit_ref), 1, 3),
    "MIN" = "mins",
    "SEC" = "secs",
    "HOU" = "hours",
    "DAY" = "days",
    "WEE" = "weeks",
    "MON" = "months",
    "auto"
  )

  # create netcdf
  nc_format <- get_nc_version(nc34)
  cmsaf_info <- (paste0("cmsafops::cmsaf.cat for variable ",
                        file_data$variable$name))

  ##### prepare output #####
  global_att_list <- names(file_data$global_att)
  global_att_list <- global_att_list[toupper(global_att_list) %in% toupper(GLOBAL_ATT_DEFAULT)]
  global_attributes <- file_data$global_att[global_att_list]

  dims <- define_dims(file_data$grid$is_regular,
                      file_data$dimension_data$x,
                      file_data$dimension_data$y,
                      file_data$dimension_data$t,
                      NB2,
                      file_data$time_info$units,
                      with_time_bnds = file_data$time_info$has_time_bnds)

  vars <- define_vars(file_data$variable, dims, nc_format$compression,
                      with_time_bnds = file_data$time_info$has_time_bnds)

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

  time_len <- length(file_data$dimension_data$t)
  time_sorting <- file_data$dimension_data$t
  file_num <- rep(1, length(file_data$dimension_data$t))

  if (fdim >= 2) {
    nc_out <- nc_open(outfile, write = TRUE)
    for (i in 2:fdim) {
      file <- filelist[i]
      nc_in <- nc_open(file)

      dum_dat <- ncvar_get(nc_in, file_data$variable$name, collapse_degen = FALSE)
      dum_time <- as.numeric(ncvar_get(nc_in, TIME_NAMES$DEFAULT))
      time_len <- time_len + length(dum_time)
      dum_t_units <- ncatt_get(nc_in, TIME_NAMES$DEFAULT, ATTR_NAMES$UNITS)$value
      dt_dum <- get_time(dum_t_units, dum_time)

      if (as.character(dt_ref) == "-4712-01-01 12:00:00") {
        dum_time2 <- (as.numeric(dt_dum) / 86400) + 2440587.5
      } else {
        if (unit_ref == "months") {
          dum_time2 <- as.numeric(round((difftime(dt_dum, dt_ref, units = c("days"))) / 30.4375))
        } else {
          dum_time2 <- difftime(dt_dum, dt_ref, units = c(unit_ref))
        }
      }
      if (file_data$time_info$has_time_bnds) {
        dum_tb <- get_time_bounds_from_file(file)
      }
      nc_close(nc_in)

      dum_dat[is.na(dum_dat)] <- file_data$variable$attributes$missing_value
      countt2 <- length(dum_time2)
      startt2 <- time_len - countt2 + 1

      if (file_data$time_info$has_time_bnds) {
        ncvar_put(nc_out, vars[[1]], dum_dat, start = c(1, 1, startt2),
                  count = c(-1, -1, countt2))
        ncvar_put(nc_out, vars[[2]], dum_tb, start = c(1, startt2),
                  count = c(-1, countt2))
        ncvar_put(nc_out, dims$t, dum_time2, start = startt2, count = countt2)
        nc_sync(nc_out)

      } else {

        ncvar_put(nc_out, vars[[1]], dum_dat, start = c(1, 1, startt2),
                  count = c(-1, -1, countt2))
        ncvar_put(nc_out, dims$t, dum_time2, start = startt2, count = countt2)
        nc_sync(nc_out)
      }

      time_sorting <- append(time_sorting, dum_time)
      file_num <- append(file_num, rep(i, length(dum_time)))
    }

    file_num <- file_num[order(time_sorting)]
    filelist <- filelist[unique(file_num)]
    nc_close(nc_out)
  }

  calc_time_end <- Sys.time()
  if (verbose) message(get_processing_time_string(calc_time_start, calc_time_end))
}
