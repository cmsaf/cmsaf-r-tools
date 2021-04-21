#'Determine mean monthly daily variations
#'
#'The function determines mean monthly daily variations values from data of a single CM SAF
#'NetCDF input file. This function is applicable to 3-dimensional NetCDF data.
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
#'@return A NetCDF file including a time series of mean monthly daily variations is written.
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
#'lon <- seq(5, 8, 0.5)
#'lat <- seq(45, 48, 0.5)
#'time <- seq(ISOdate(2000, 3, 1), ISOdate(2000, 5, 31), "hours")
#'origin <- as.Date("1983-01-01 00:00:00")
#'time <- as.numeric(difftime(time, origin, units = "hour"))
#'data <- array(250:272, dim = c(7, 7, 2185))
#'
#'x <- ncdim_def(name = "lon", units = "degrees_east", vals = lon)
#'y <- ncdim_def(name = "lat", units = "degrees_north", vals = lat)
#'t <- ncdim_def(name = "time", units = "hours since 1983-01-01 00:00:00",
#'              vals = time, unlim = TRUE)
#'var1 <- ncvar_def("SIS", "W m-2", list(x, y, t), -999, prec = "short",
#'                 longname = "Surface Incoming Shortwave Radiation")
#'vars <- list(var1)
#'ncnew <- nc_create(file.path(tempdir(), "CMSAF_example_file.nc"), vars)
#'ncvar_put(ncnew, var1, data)
#'ncatt_put(ncnew, "lon", "standard_name", "longitude", prec = "text")
#'ncatt_put(ncnew, "lat", "standard_name", "latitude", prec = "text")
#'ncatt_put(ncnew, "SIS", "standard_name", "SIS_standard", prec = "text")
#'nc_close(ncnew)
#'
#'## Determine the mean monthly daily variations of the example CM SAF NetCDF file and
#'## write the output to a new file.
#'mondaymean(var = "SIS", infile = file.path(tempdir(), "CMSAF_example_file.nc"), 
#'  outfile = file.path(tempdir(), "CMSAF_example_file_mondaymean.nc"))
#'
#'unlink(c(file.path(tempdir(),"CMSAF_example_file.nc"), 
#'  file.path(tempdir(),"CMSAF_example_file_mondaymean.nc")))
mondaymean <- function(var, infile, outfile, nc34 = 4, overwrite = FALSE, verbose = FALSE) {
  calc_time_start <- Sys.time()
  gc()
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
  time_steps <- date_time$times
  months_all <- date_time$months
  years_all <- date_time$years
  
  dateID <- paste(years_all, months_all, time_steps)
  
  warning_msg <- "The time steps must be available every 15min (at least 12 days per month), 30min (at least 6 days per month) or 1h (at least 3 days per month). One or more time steps do not meet the requirements. The result of these time steps is NA. "
  if(!("" %in% time_steps)) {
    hours_all_string <- substr(time_steps, 1, 2)
    hours_all_int <- strtoi(hours_all_string, base=10L)
    time_steps_hour <- hours_all_int[2] - ((hours_all_int[1] + 1)%%24)   # hourly time steps (time_steps_hour = 0)
    
    minutes_all_string <- substr(time_steps, 4, 5)
    minutes_all_int <- strtoi(minutes_all_string, base=10L)
    time_steps_minute <- minutes_all_int[1] + minutes_all_int[2] + minutes_all_int[3] + minutes_all_int[4]   # every 15 minutes (time_steps_minute = 90) or every 30 minutes (time_steps_minute = 60)
    
    
    # Use placeholder for result so that it can be calculated later without the
    # need to have all input data in memory concurrently.
    data_placeholder <- array(
      file_data$variable$attributes$missing_value,
      dim = c(length(file_data$dimension_data$x),
              length(file_data$dimension_data$y),
              length(unique(dateID)))
    )
    times_for_bnds <- file_data$dimension_data$t
    time_bnds <- array(NA, dim = c(2, length(unique(dateID))))
    
    count <- 1
    for (j in unique(dateID)) {
      dateID_dummy <- which(dateID == j)
      time_bnds[1, count] <- times_for_bnds[min(dateID_dummy)]
      time_bnds[2, count] <- times_for_bnds[max(dateID_dummy)]
      count <- count + 1
    }
    
    #time_bnds <- get_time_bounds_doy(file_data$dimension_data$t, dateID)
    
    vars_data <- list(result = data_placeholder, time_bounds = time_bnds)
    
    nc_format <- get_nc_version(nc34)
    
    cmsaf_info <- paste0("cmsaf::mondaymean for variable ", file_data$variable$name)
    
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
      global_attributes,
    )
    
    ##### calculate and write result #####
    nc_out <- nc_open(outfile, write = TRUE)
    warning_value <- TRUE
    for (j in seq_along(unique(dateID))) {
      day_dummy <- which(dateID == unique(dateID)[j])
      startt <- day_dummy
      day_counter <- 0
      
      dum_dat <- array(
        NA,
        dim = c(length(file_data$dimension_data$x),
                length(file_data$dimension_data$y),
                length(startt))
      )
      nc_in <- nc_open(infile)
     
      for (i in seq_along(startt)) {
        dum_dat[, , i] <- ncvar_get(
          nc_in,
          file_data$variable$name,
          start = c(1, 1, startt[i]),
          count = c(-1, -1, 1),
          collapse_degen = FALSE
        )
        if(all(!is.na(dum_dat[,,i])))
          day_counter <- day_counter + 1
      }
      nc_close(nc_in)
      bool_value <- (time_steps_minute == 90 & day_counter >= 12) | (time_steps_minute == 60 & day_counter >= 6) | (time_steps_hour == 0 & day_counter >= 3)
      if(bool_value) {
        if (verbose) message(paste0("apply mean monthly daily variation ", j))
        mean_data <- rowMeans(dum_dat, dims = 2, na.rm = TRUE)
      } else {
        if(warning_value){
          warning(paste(warning_msg))
          warning_value <- FALSE
        }
        if (verbose) message(paste0("apply mean monthly daily variation ", j))
        mean_data <- dum_dat[,,1]
        mean_data[which(is.na(mean_data) | !is.na(mean_data))] <- NA
      }
      mean_data[is.na(mean_data)] <- file_data$variable$attributes$missing_value
      ncvar_put(nc_out, vars[[1]], mean_data, start = c(1, 1, j), count = c(-1, -1, 1))
    }
    nc_close(nc_out)
  } 
  else {
    stop(warning_msg)
  }
  calc_time_end <- Sys.time()
  if (verbose) message(get_processing_time_string(calc_time_start, calc_time_end))
}
