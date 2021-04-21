#'Extract a list of dates.
#'
#'This function selects a time period from a time series.
#'
#'@param var Name of NetCDF variable (character).
#'@param start Start date as character in form of 'YYYY-MM-DD' (e.g.,
#'  '2001-12-31').
#'@param end End date as character in form of 'YYYY-MM-DD' (e.g., '2001-12-31').
#'@param infile Filename of input NetCDF file. This may include the directory
#'  (character).
#'@param outfile Filename of output NetCDF file. This may include the directory
#'  (character).
#'@param nc34 NetCDF version of output file. If \code{nc34 = 3} the output file will be
#'  in NetCDFv3 format (numeric). Default output is NetCDFv4.
#'@param overwrite logical; should existing output file be overwritten?
#'@param verbose logical; if TRUE, progress messages are shown
#'
#'@return A NetCDF file including the selected time period is written.
#'
#'@export
#'@family selection and removal functions
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
#'## Select a 13-months period of the example CM SAF NetCDF file and write
#'## the output to a new file.
#'selperiod(var = "SIS", start = "2001-01-01", end = "2002-01-01", 
#'  infile = file.path(tempdir(),"CMSAF_example_file.nc"),
#'  outfile = file.path(tempdir(),"CMSAF_example_file_selperiod.nc"))
#'
#'unlink(c(file.path(tempdir(),"CMSAF_example_file.nc"), 
#'  file.path(tempdir(),"CMSAF_example_file_selperiod.nc")))

selperiod <- local({
  count <- 1
  function(var, start, end, infile, outfile, nc34 = 4,
                        overwrite = FALSE, verbose = FALSE) {
    count <<- 1
    calc_time_start <- Sys.time()
    gc()
    check_variable(var)
    check_infile(infile)
    check_outfile(outfile)
    outfile <- correct_filename(outfile)
    check_overwrite(outfile, overwrite)
    check_nc_version(nc34)
    
    # get information about dimensions and attributes
    file_data <- read_file(infile, var)
    file_data$variable$prec <- "float"
    if (file_data$time_info$has_time_bnds) {
      time_bnds <- get_time_bounds_from_file(infile)
    }
  
    # extract time information
    if (startsWith(file_data$time_info$units, "hours") || startsWith(file_data$time_info$units, "seconds")) {
      end <- as.POSIXlt(paste(end, "23:59", sep = " "), tz = "UTC", format = "%Y-%m-%d %R")
      start <- as.POSIXlt(paste(start, "00:00", sep = " "), tz = "UTC", format = "%Y-%m-%d %R")
      date.time <- as.POSIXlt(get_time(file_data$time_info$units, file_data$dimension_data$t), format = "%Y-%m-%d %R", tz = "UTC")
    }else{
      date.time <- as.Date(get_time(file_data$time_info$units, file_data$dimension_data$t))
    }
    period <- date.time[which(date.time >= start & date.time <= end)]
    date_in <- which(date.time %in% period)
  
    if (!length(period)) {
      stop(paste0("No match. Dates are: ", date.time))
    }
  
    #result <- array(NA, dim = c(length(file_data$dimension_data$x), length(file_data$dimension_data$y), length(date_in)))
    #result[is.na(result)] <- file_data$variable$attributes$missing_value
  
    if (file_data$time_info$has_time_bnds) {
      #vars_data <- list(result = result, time_bounds = time_bnds[, date_in])
      vars_data <- list(time_bounds = time_bnds[, date_in])
    }else{
      #vars_data <- list(result = result)
      vars_data <- list(time_bounds = file_data$dimension_data$t)
    }
  
    # create netcdf
    nc_format <- get_nc_version(nc34)
    cmsaf_info <- (paste0("cmsaf::selperiod for variable ", file_data$variable$name))
    
    ##### prepare output #####
    global_att_list <- names(file_data$global_att)
    global_att_list <- global_att_list[toupper(global_att_list) %in% toupper(GLOBAL_ATT_DEFAULT)]
    global_attributes <- file_data$global_att[global_att_list]
  
    dims <- define_dims(file_data$grid$is_regular,
                        file_data$dimension_data$x,
                        file_data$dimension_data$y,
                        file_data$dimension_data$t[date_in],
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
      with_time_bnds = file_data$time_info$has_time_bnds,
      write_result = FALSE
    )
  
    # extract desired times from infile
    nc_in <- nc_open(infile)
    nc_out <- nc_open(outfile, write = TRUE)
  
    # dum_dat <- ncvar_get(nc_in, file_data$variable$name, start  = c(1, 1, date_in[1]), count = c(-1, -1, length(date_in)))
    # dum_dat[is.na(dum_dat)] <- file_data$variable$attributes$missing_value
    # ncvar_put(nc_out, vars[[1]], dum_dat, start = c(1, 1, 1), count = c(-1, -1, length(date_in)))
    # ncvar_put(nc_out, dims$t, file_data$dimension_data$t[date_in], start = 1, count = length(date_in))
    # if (file_data$time_info$has_time_bnds) {
    #   ncvar_put(nc_out, vars[[2]], time_bnds[, date_in], start = c(1, 1), count = c(-1, length(date_in)))
    # }
    count <<- 1
    for (i in seq_along(file_data$dimension_data$t)) {
      for (j in seq_along(period)) {
        if (date.time[i] == period[j]) {
          dum_dat <- ncvar_get(nc_in, file_data$variable$name, start  = c(1, 1, i), count  = c(-1, -1, 1))
          dum_dat[is.na(dum_dat)] <- file_data$variable$attributes$missing_value
          ncvar_put(nc_out, vars[[1]], dum_dat, start = c(1, 1, count), count = c(-1, -1, 1))
          ncvar_put(nc_out, dims$t, file_data$dimension_data$t[i], start = count, count = 1)
          if (file_data$time_info$has_time_bnds) {
            ncvar_put(nc_out, vars[[2]], time_bnds[, i], start = c(1, count), count = c(-1, 1))
          }
          count <<- count + 1
        }
      }
    }
  
    nc_close(nc_in)
    nc_close(nc_out)
  
    calc_time_end <- Sys.time()
    if (verbose) message(get_processing_time_string(calc_time_start, calc_time_end))
  }
})