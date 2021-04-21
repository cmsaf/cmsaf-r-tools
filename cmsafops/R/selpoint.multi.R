#'Extract data at multiple points.
#'
#'This function extracts all data at given points. The points are given by a
#'pair of vectors with longitude and latitude coordinates. The function will
#'find the closest grid points to the given coordinates and extracts the data
#'for these points. For each point a separate output file is written. The
#'output-files can be optional in NetCDF or csv. Input can be a single NetCDF
#'file (given by the infile attribute) or a bunch of NetCDF files (given by the
#'path and pattern attributes).
#'
#'@param var Name of NetCDF variable (character).
#'@param infile Filename of input NetCDF file. This may include the directory
#'  (character). Infile is not needed if path and pattern are given.
#'@param path Directory of input files (character). Will not be used if infile
#'  is given.
#'@param pattern Pattern that all desired files in the 'path' directory have in
#'  common (character).
#'@param outpath Directory where output files will be stored (character).
#'@param lon1 Longitude vector of desired points (numeric vector). Must
#'  have the same length as \code{lat1}.
#'@param lat1 Latitude vector of desired points (numeric vector). Must have
#'  the same length as \code{lon1}.
#'@param station_names Optional vector of names, which will be used for the
#'  output files (character vector). Must have the same length as
#'  \code{lon1} and \code{lat1}.
#'@param format Intended output format. Options are \code{nc} or \code{csv}. Default is
#'  \code{nc} (character).
#'@param nc34 NetCDF version of output file. If \code{nc34 = 3} the output file will be
#'  in NetCDFv3 format (numeric). Default output is NetCDFv4.
#'@param verbose logical; if TRUE, progress messages are shown
#'
#'@return For each pair of longitude and latitude coordinates one separate
#'  NetCDF or csv file including the selected data is written. The csv files are
#'  tested for use in Excel and include four columns (Time ; Data ; Longitude ;
#'  Latitude), which are separated by ';'. If station_names are defined, the
#'  output files will be named according to this vector. Otherwise, the output
#'  files will be named as selpoint_longitude_latitude.format. Already existing
#'  files will be overwritten in case that station_names are given or renamed
#'  (e.g., \code{selpoint1_longitude_latitude.nc}) in case that no station_names are
#'  given.
#'@export
#'
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
#'## Select two points of the example CM SAF NetCDF file and write the
#'## output to a csv-file.
#'selpoint.multi(var = "SIS", infile = file.path(tempdir(),"CMSAF_example_file.nc"),
#'  outpath = tempdir(), lon1 = c(8, 9), lat1 = c(48, 49),
#'  station_names = c("A", "B"), format = "csv")
#'
#'unlink(c(file.path(tempdir(),"CMSAF_example_file.nc"), file.path(tempdir(),"A.csv"), 
#'  file.path(tempdir(),"B.csv")))
selpoint.multi <- function(var, infile, path, pattern, outpath, lon1, lat1,
                           station_names, format = "nc", nc34 = 4, verbose = FALSE) {
  check_variable(var)

  if (missing(infile) && (missing(path) || missing(pattern))) {
    stop(paste0("Missing input: Please provide either infile or path and pattern.",
                collapse = " "))
  }
  if (!missing(infile)) {
    check_infile(infile)
    case <- 1
    if (!missing(path) || !missing(pattern)) {
      if (verbose) message("infile defined, path and pattern will not be used")
    }
  }else{
    stopifnot(length(list.files(path = path, pattern = pattern)) > 0)
    case <- 2
  }
  stopifnot(length(lon1) == length(lat1))
  check_nc_version(nc34)
  check_format(format)
  calc_time_start <- Sys.time()

  if (case == 1) {
    file_data <- read_file(infile, var)
    if (file_data$time_info$has_time_bnds) {
      time_bnds <- get_time_bounds_from_file(infile)
    }
  }else{
    filelist <- list.files(path = path, pattern = pattern, full.names = TRUE)
    fdim <- length(filelist)

    if (fdim == 0) {
      stop("No files found that match the pattern")
    }

    filelist <- sort(filelist)
    file <- filelist[1]
    file_data <- read_file(file, var)
  }

  if (!(file_data$grid$is_regular || length(file_data$grid$vars))) {
    stop("No lon/lat information found in file, please add by applying add_grid_info")
  }

  # find closest point to target coordinates using sp package
  target_lon  <- NULL
  target_lat  <- NULL
  target_x <- NULL
  target_y <- NULL
  result_data <- NULL

  if (file_data$grid$is_regular) {
    dlon <- abs(file_data$dimension_data$x[1] - file_data$dimension_data$x[2])
    dlat <- abs(file_data$dimension_data$y[1] - file_data$dimension_data$y[2])

    for (n in seq_along(lon1)) {
      lon_limit <- which(file_data$dimension_data$x >= (lon1[n] - dlon) & file_data$dimension_data$x <= (lon1[n] + dlon))
      lat_limit <- which(file_data$dimension_data$y >= (lat1[n] - dlat) & file_data$dimension_data$y <= (lat1[n] + dlat))

      if (!(any(lon_limit) & any(lat_limit))) {
        stop("Coordinates outside of the domain.")
      }

      lon2 <- file_data$dimension_data$x[lon_limit]
      lat2 <- file_data$dimension_data$y[lat_limit]

      pos <- sp::SpatialPoints(cbind(lon1[n], lat1[n]), proj4string = sp::CRS("+proj=longlat +datum=WGS84"))
      dum_dist <- 1000
      for (i in seq_along(lon2)) {
        for (j in seq_along(lat2)) {
          dist <- sp::spDistsN1(pos, c(lon2[i], lat2[j]), longlat = FALSE)
          if (dist <= dum_dist) {
            dum_dist <- dist
            dumi <- i
            dumj <- j
          }
        }
      }

      lon_limit <- which(file_data$dimension_data$x == lon2[dumi])
      lat_limit <- which(file_data$dimension_data$y == lat2[dumj])


      if (!(any(lon_limit) & any(lat_limit))) {
        stop("Coordinates outside of the domain.")
      }

      target_lon <- append(target_lon, file_data$dimension_data$x[lon_limit])
      target_lat <- append(target_lat, file_data$dimension_data$y[lat_limit])
      target_x <- append(target_x, lon_limit)
      target_y <- append(target_y, lat_limit)

      if (case == 1) {
        id <- nc_open(infile)
        result <- ncvar_get(id, file_data$variable$name, start = c(lon_limit, lat_limit, 1), count = c(1, 1, -1))
        nc_close(id)
        result_data <- rbind(result_data, result)
        }

    } # end for
  }else{
    target_x_nearest <- NULL
    target_y_nearest <- NULL

    dlon <- LON_RANGE
    dlat <- LAT_RANGE

    for (n in seq_along(lon1)) {

      lon_limit <- which(file_data$grid$vars_data[[LON_NAMES$DEFAULT]] >= (lon1[n] - dlon) & file_data$grid$vars_data[[LON_NAMES$DEFAULT]] <= (lon1[n] + dlon), arr.ind = TRUE)
      lat_limit <- which(file_data$grid$vars_data[[LAT_NAMES$DEFAULT]] >= (lat1[n] - dlat) & file_data$grid$vars_data[[LAT_NAMES$DEFAULT]] <= (lat1[n] + dlat), arr.ind = TRUE)

      if (!(any(lon_limit) && any(lat_limit))) {
        stop("Coordinates outside of the domain.")
      }

      lonlat_merge <- data.matrix(merge(lon_limit, lat_limit, by.x = c("row", "col"), by.y =  c("row", "col"), out.class = matrix))

      dist <- sp::spDistsN1(cbind(file_data$grid$vars_data[[LON_NAMES$DEFAULT]][lonlat_merge], file_data$grid$vars_data[[LAT_NAMES$DEFAULT]][lonlat_merge]), c(lon1[n], lat1[n]), longlat = FALSE)
      mini <- which.min(dist)
      nearest <- lonlat_merge[mini, ]

      x_nearest <- nearest[1]
      y_nearest <- nearest[2]

      x_dim <- file_data$dimension_data$x[x_nearest]
      y_dim <- file_data$dimension_data$y[y_nearest]

      target_x_nearest <- append(target_x_nearest, x_nearest)
      target_y_nearest <- append(target_y_nearest, y_nearest)
      target_x <- rbind(target_x, x_dim)
      target_y <- rbind(target_y, y_dim)
      target_lon <- append(target_lon, file_data$grid$vars_data[[LON_NAMES$DEFAULT]][x_nearest, y_nearest])
      target_lat <- append(target_lat, file_data$grid$vars_data[[LAT_NAMES$DEFAULT]][x_nearest, y_nearest])

      if (case == 1) {
        id <- nc_open(infile)
        result <- ncvar_get(id, file_data$variable$name, start = c(x_nearest, y_nearest, 1), count = c(1, 1, -1))
        nc_close(id)
        result_data <- rbind(result_data, result)
      }

    } # end for
  }

  if (case == 1) {

    if (file_data$time_info$has_time_bnds) {
      time_bnds <- get_time_bounds_from_file(infile)
    }

  }else{

    time_sorting <- NULL
    time_bnds <- NULL
    for (j in seq_len(fdim)) {
      dummy <- NULL
      file <- filelist[j]
      id <- nc_open(file)
      for (i in seq_along(target_x)) {
        if (file_data$grid$is_regular) {
          dummy <- cbind(dummy, ncvar_get(id, file_data$variable$name, start = c(target_x[i], target_y[i], 1), count = c(1, 1, -1)))
        }else{
          result <- ncvar_get(id, file_data$variable$name, start = c(target_x_nearest[i], target_y_nearest[i], 1), count = c(1, 1, -1))
          dummy <- rbind(dummy, result)
        }
      }
      dum_time <- as.numeric(ncvar_get(id, TIME_NAMES$DEFAULT))
      time_sorting <- append(time_sorting, dum_time)
      result_data <- rbind(result_data, dummy)
      if (file_data$time_info$has_time_bnds) {
        tbnds1 <- ncvar_get(id, TIME_BOUNDS_NAMES$DEFAULT)
        time_bnds <- rbind(time_bnds, tbnds1)
      }
      nc_close(id)
    }

    file_data$dimension_data$t <- time_sorting
    result_data <- aperm(result_data, c(2, 1))
    result_data <- result_data[, order(time_sorting)]
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


  target_lon <- round(target_lon, digits = 3)
  target_lat <- round(target_lat, digits = 3)

  if (is.null(result_data)) {
    stop("Some Error occured with your data")
  }
  if (is.null(dim(result_data)[1])) {
    result_data <- array(result_data, dim = c(1, length(result_data)))
  }

  for (i in seq_len(dim(result_data)[1])) {

    format <- ifelse(toupper(format) == "CSV", "csv", "nc")

    # create filename
    index <- 1
    if (!missing(station_names)) {
      if (length(station_names) == length(lon1)) {
        outfile <- file.path(outpath, paste0(station_names[i], ".", format))
      } else {
        if (verbose) message("station_names not used because length is not equal lon1.")
        outfile <- file.path(outpath, paste0("selpoint_", target_lon[i], "_", target_lat[i], ".", format))
        while (file.exists(outfile)) {
          outfile <- file.path(outpath, paste0("selpoint", index, "_", target_lon[i], "_", target_lat[i], ".", format))
          index <- index + 1
        }
      }
    } else {
      outfile <- file.path(outpath, paste0("selpoint_", target_lon[i], "_", target_lat[i], ".", format))
      while (file.exists(outfile)) {
        outfile <- file.path(outpath, paste0("selpoint", index, "_", target_lon[i], "_", target_lat[i], ".", format))
        index <- index + 1
      }
    }

    if (format == "nc") {
      # create netcdf
      nc_format <- get_nc_version(nc34)
      result <- result_data[i, ]
      result[is.na(result)] <- file_data$variable$attributes$missing_value

      cmsaf_info <- (paste0("cmsaf::selpoint.multi for variable ", file_data$variable$name))

      if (file_data$time_info$has_time_bnds) {
        vars_data <- list(result = result, time_bounds = time_bnds)
      }else{
        vars_data <- list(result = result)
      }

      ##### prepare output #####
      global_att_list <- names(file_data$global_att)
      global_att_list <- global_att_list[toupper(global_att_list) %in% toupper(GLOBAL_ATT_DEFAULT)]
      global_attributes <- file_data$global_att[global_att_list]

      x_data <- ifelse(file_data$grid$is_regular, target_lon[i], target_x[i])
      y_data <- ifelse(file_data$grid$is_regular, target_lat[i], target_y[i])

      dims <- define_dims(file_data$grid$is_regular,
                          x_data,
                          y_data,
                          file_data$dimension_data$t,
                          NB2,
                          file_data$time_info$units,
                          with_time_bnds = file_data$time_info$has_time_bnds)

      vars <- define_vars(file_data$variable, dims, nc_format$compression, with_time_bnds = file_data$time_info$has_time_bnds)

      if (!file_data$grid$is_regular) {
        new_data <- list()
        new_data[[which(names(file_data$grid$vars) %in% LON_NAMES)]] <- target_lon[i]
        new_data[[which(names(file_data$grid$vars) %in% LAT_NAMES)]] <- target_lat[i]
        file_data$grid <- redefine_grid_vars(file_data$grid, dims, nc_format$compression, new_data)
      }

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

    } else {# csv
      result <- result_data[i, ]
      lon <- target_lon[i]
      lat <- target_lat[i]
      lon_rep <- rep(lon, length(file_data$dimension_data$t))
      lat_rep <- rep(lat, length(file_data$dimension_data$t))
      dataframe <- data.frame(get_time(file_data$time_info$units, file_data$dimension_data$t), result, lon_rep, lat_rep)
      dum_fname <- unlist(strsplit(outfile, "\\."))
      if (dum_fname[length(dum_fname)] != "csv") (outfile <- paste0(outfile, ".csv"))
      utils::write.table(dataframe, file = outfile, row.names = FALSE, sep = ";")
    }

  } # end for

  calc_time_end <- Sys.time()
  if (verbose) message(get_processing_time_string(calc_time_start, calc_time_end))
}
