#'Function to combine NetCDF files and simultaneously cut a region (and level).
#'
#'This function selects a region (and optionally a level) from a bunch of
#'NetCDF files that match the same pattern of the filename, and writes the
#'output to a new file. If no longitude and latitude values are given, files are
#'only merged. All input files have to have the same grid and the same variable.
#'The reference time of the output file is determined by the first input file.
#'
#'@param var Name of NetCDF variable (character).
#'@param path The directory of input NetCDF files without / at the end
#'  (character).
#'@param pattern A part of the filename, which is the same for all desired input
#'  files (character). The pattern has to be a character string containing a
#'  regular expression.
#'@param outfile Filename of output NetCDF file. This may include the directory
#'  (character).
#'@param lon1 Longitude of lower left corner (numeric).
#'@param lon2 Longitude of upper right left corner (numeric).
#'@param lat1 Latitude of lower left corner (numeric).
#'@param lat2 Latitude of upper right corner (numeric).  Longitude of upper
#'  right corner (numeric).
#'@param level Number of level that should be extracted (integer) or NULL.
#'@param nc34 NetCDF version of output file. If \code{nc34 = 3} the output file will be
#'  in NetCDFv3 format (numeric). Default output is NetCDFv4.
#'@param overwrite logical; should existing output file be overwritten?
#'@param verbose logical; if TRUE, progress messages are shown
#'
#'@return A NetCDF file including the merged time series of the selected region
#'  is written. The resulting file uses the meta data of the first input file.
#'@export
#'
#'@family data manipulation functions
#'
#'@examples
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
#'ncnew <- nc_create(file.path(tempdir(),"CMSAF_example_file_n1.nc"), vars)
#'ncvar_put(ncnew, var1, data1)
#'ncatt_put(ncnew, "lon", "standard_name", "longitude", prec = "text")
#'ncatt_put(ncnew, "lat", "standard_name", "latitude", prec = "text")
#'nc_close(ncnew)
#'t <- ncdim_def(name = "time", units = "hours since 1983-01-01 00:00:00",
#'  vals = time[2], unlim = TRUE)
#'ncnew <- nc_create(file.path(tempdir(),"CMSAF_example_file_n2.nc"), vars)
#'ncvar_put(ncnew, var1, data2)
#'ncatt_put(ncnew, "lon", "standard_name", "longitude", prec = "text")
#'ncatt_put(ncnew, "lat", "standard_name", "latitude", prec = "text")
#'nc_close(ncnew)
#'
#'## Cut a region and merge both example CM SAF NetCDF files into one
#'## output file.  Get path information of working directory with getwd()
#'## command.
#'box_mergetime(var = "SIS", path= tempdir(), pattern = "CMSAF_example_file_n",
#'  outfile = file.path(tempdir(),"CMSAF_example_file_box_mergetime.nc"), 
#'  lon1 = 8, lon2 = 12, lat1 = 48, lat2 = 52)
#'
#'unlink(c(file.path(tempdir(),"CMSAF_example_file_n1.nc"), 
#'  file.path(tempdir(),"CMSAF_example_file_n2.nc"),
#'  file.path(tempdir(),"CMSAF_example_file_box_mergetime.nc")))
box_mergetime <- function(var, path, pattern, outfile, lon1 = -180, lon2 = 180,
                          lat1 = -90, lat2 = 90, level = NULL, nc34 = 4,
                          overwrite = FALSE, verbose = FALSE) {
  
  if (is.null(path) || is.null(pattern) || missing(path) || missing(pattern)) {
    stop("Missing input: Please provide path and pattern.")
  }
  check_variable(var)

  check_outfile(outfile)

  outfile <- correct_filename(outfile)
  check_overwrite(outfile, overwrite)

  check_nc_version(nc34)

  calc_time_start <- Sys.time()

  filelist <- list.files(path = path, pattern = pattern, full.names = TRUE)
  fdim <- length(filelist)

  if (fdim == 0) {
    stop("No files found that match the pattern")
  }

  # Sort files by first time step of each file
  time_order <- NULL
  
  for (i in seq_along(filelist)) {
    file_info <- read_file(filelist[i], var)
    time_order <- rbind(time_order, c(i, file_info$dimension_data$t[1]))
  }
  
  time_order_id <- order(time_order[,2])
  #filelist <- sort(filelist)
  filelist <- filelist[time_order_id]
  fdim <- length(filelist)

  file <- filelist[1]

  # get information about dimensions and attributes
  file_data <- read_file(file, var)

  if (!(file_data$grid$is_regular || length(file_data$grid$vars))) {
    stop("No lon/lat information found in file, ",
         "please add by applying add_grid_info")
  }
  # if (is.null(file_data$variable$prec) || !(file_data$variable$prec %in% PRECISIONS_VAR)) {
  #   file_data$variable$prec <- "float"
  # }
  file_data$variable$prec <- "float" # fixed problem with wrong datatype
  if (!length(file_data$grid$vars)) {
    lon <- file_data$dimension_data$x
    lat <- file_data$dimension_data$y
  } else {
    lon <- file_data$grid$vars_data[[LON_NAMES$DEFAULT]]
    lat <- file_data$grid$vars_data[[LAT_NAMES$DEFAULT]]
  }

  lon_limit <- which(lon >= lon1 & lon <= lon2, arr.ind = TRUE)
  lat_limit <- which(lat >= lat1 & lat <= lat2, arr.ind = TRUE)

  # check for empty lon_limit or lat_limit
  if (length(lon_limit) == 0 || length(lat_limit) == 0) {
    stop("Selected region is outside target area!")
  }

  if (file_data$grid$is_regular) {
    file_data$dimension_data$x <- file_data$dimension_data$x[lon_limit]
    file_data$dimension_data$y <- file_data$dimension_data$y[lat_limit]

    startx <- min(lon_limit)
    starty <- min(lat_limit)
    countx <- length(lon_limit)
    county <- length(lat_limit)
    countt <- length(file_data$dimension_data$t)

    id <- nc_open(file)
    if (is.null(level)) {
      result <- NA
    } else {
      result <- NA
      # changed for inclusion of levbox_mergetime
    }
    nc_close(id)
  } else {
    lonlat_merge <- data.matrix(merge(lon_limit, lat_limit,
                                      by.x = c("row", "col"),
                                      by.y = c("row", "col"),
                                      out.class = matrix))

    x_range <- which(file_data$dimension_data$x %in% file_data$dimension_data$x[lonlat_merge[, 1]])
    y_range <- which(file_data$dimension_data$y %in% file_data$dimension_data$y[lonlat_merge[, 2]])

    file_data$dimension_data$x <- file_data$dimension_data$x[x_range]
    file_data$dimension_data$y <- file_data$dimension_data$y[y_range]

    file_data$grid$vars_data$lon <- file_data$grid$vars_data$lon[x_range, y_range]
    file_data$grid$vars_data$lat <- file_data$grid$vars_data$lat[x_range, y_range]

    result <- NA
  }

  result[is.na(result)] <- file_data$variable$attributes$missing_value

  if (file_data$time_info$has_time_bnds) {
    time_bnds <- get_time_bounds_from_file(file)
    vars_data <- list(result = result, time_bounds = time_bnds)
  } else {
    vars_data <- list(result = result)
  }

  # get time reference
  dt_ref <- get_time(file_data$time_info$units, 0)
  unit_ref <- unlist(strsplit(file_data$time_info$units, split = " "))[1]

  # check reference time unit
  unit_ref <- switch(
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
  if (is.null(level)) {
    cmsaf_info <- paste0("cmsafops::box_mergetime for variable ",
                         file_data$variable$name)
  } else {
    cmsaf_info <- paste0("cmsafops::levbox_mergetime for variable ",
                         file_data$variable$name)
  }

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
                      with_time_bnds = file_data$time_info$has_time_bnds
  )

  vars <- define_vars(file_data$variable, dims, nc_format$compression,
                      with_time_bnds = file_data$time_info$has_time_bnds)

  file_data$grid <- redefine_grid_vars(file_data$grid, dims, nc_format$compression, file_data$grid$vars_data)

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

  time_len <- 0 #length(file_data$dimension_data$t)
  time_sorting <- file_data$dimension_data$t
  file_num <- rep(1, time_len)
  
  counter_time <- 0 # used for secs
  counter_days <- 0
  
    nc_out <- nc_open(outfile, write = TRUE)
    for (i in seq_len(fdim)) {
      file <- filelist[i]
      nc_in <- nc_open(file)

      startt <- time_len

      dum_time <- as.numeric(ncvar_get(nc_in, TIME_NAMES$DEFAULT))

      dimensionality <- as.double(length(file_data$dimension_data$x)) *
                        as.double(length(file_data$dimension_data$y)) *
                        as.double(length(dum_time))

      countt <- length(dum_time)

      limit <- 2601 * 2601 * 31

      if (dimensionality <= limit) {
        if (file_data$grid$is_regular) {
          if (is.null(level)) {
            dum_dat <- ncvar_get(nc_in, file_data$variable$name,
                                 start = c(startx, starty, 1),
                                 count = c(countx, county, -1))
          } else {
            dum_dat <- ncvar_get(nc_in, file_data$variable$name,
                                 start = c(startx, starty, level, 1),
                                 count = c(countx, county, 1, -1))
            # changed for inclusion of levbox_mergetime
          }
        } else {
          vari <- ncvar_get(nc_in, file_data$variable$name)
          dum_dat <- vari[x_range, y_range]
        }

        dum_dat[is.na(dum_dat)] <- file_data$variable$attributes$missing_value
        ncvar_put(nc_out, vars[[1]], dum_dat, start = c(1, 1, time_len + 1),
                  count = c(-1, -1, countt))

      } else {
        nr_of_possible_t_dims <-
          round((limit / length(file_data$dimension_data$x)) / length(file_data$dimension_data$y))
        dimsteps_start <-
          seq(1,
              countt,
              nr_of_possible_t_dims)
        dimsteps_count <-
          rep(nr_of_possible_t_dims, length(dimsteps_start))
        cor <-
          nr_of_possible_t_dims * length(dimsteps_start) - countt
        dimsteps_count[length(dimsteps_start)] <-
          dimsteps_count[length(dimsteps_start)] - cor

        for (j in seq_along(dimsteps_start)) {
            dum_dat <- ncvar_get(
              nc_in,
              file_data$variable$name,
              start = c(1, 1, dimsteps_start[j]),
              count = c(-1, -1, dimsteps_count[j]),
              collapse_degen = FALSE
            )
            dum_dat[is.na(dum_dat)] <- file_data$variable$attributes$missing_value
            ncvar_put(
              nc_out,
              vars[[1]],
              dum_dat,
              start = c(1, 1, startt + dimsteps_start[j]),
              count = c(-1, -1, dimsteps_count[j])
            )
        }
      }

      dum_t_units <- ncatt_get(nc_in, TIME_NAMES$DEFAULT, ATTR_NAMES$UNITS)$value

      dt_dum <- get_time(dum_t_units, dum_time)

      if (as.character(dt_ref) == "-4712-01-01 12:00:00") {
        dum_time2 <- (as.numeric(dt_dum) / 86400) + 2440587.5
      } else {
        if (unit_ref == "months") {
          dum_time2 <- as.numeric(round((difftime(dt_dum, dt_ref,
                                                  units = c("days"))) / 30.4375))
        } else if(unit_ref == "secs"){
          if(fdim > 1){
            ### Not clear why this is done
            # file_data_1 <- read_file(file, var)
            # file_time_info_units <- file_data_1$time_info$units
            # extract_string <- substr(file_time_info_units, 1, 24)
            # 
            # dt_dum <- get_time(extract_string, counter_time)
            # counter_time <- counter_time + 3600
            dum_time2 <- difftime(dt_dum, dt_ref, units = c(unit_ref))
            # 
            # counter_days <- counter_days + 1
            # if(counter_days == 24)
            # {
            #   counter_days <- 0
            #   counter_time <- 0
            # }
          }
          else{
            dum_time2 <- difftime(dt_dum, dt_ref, units = c(unit_ref))
          }
        }
        else {
          dum_time2 <- difftime(dt_dum, dt_ref, units = c(unit_ref))
        }
      }
      if (file_data$time_info$has_time_bnds) {
        dum_tb <- get_time_bounds_from_file(file)
      }

      ncvar_put(nc_out, dims$t, dum_time2, start = time_len + 1, count = countt)
      if (file_data$time_info$has_time_bnds) {
        ncvar_put(nc_out, vars[[2]], dum_tb, start = c(1, time_len + 1),
                  count = c(-1, countt))

      }

      time_len <- time_len + length(dum_time)
      nc_sync(nc_out)

      nc_close(nc_in)

      time_sorting <- append(time_sorting, dum_time)
      file_num <- append(file_num, rep(i, length(dum_time)))
    }

    nc_close(nc_out)

    file_num <- file_num[order(time_sorting)]
    filelist <- filelist[unique(file_num)]

  calc_time_end <- Sys.time()
  if (verbose) message(get_processing_time_string(calc_time_start, calc_time_end))
}
