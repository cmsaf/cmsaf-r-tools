#'Select a region by longitude and latitude.
#'
#'This function cuts a region from data of a CM SAF NetCDF file. The region is
#'selected by giving the coordinates of the lower left and upper right corner of
#'an rectangular grid area.
#'
#'@param var Name of NetCDF variable (character).
#'@param infile Filename of input NetCDF file. This may include the directory
#'  (character).
#'@param outfile Filename of output NetCDF file. This may include the directory
#'  (character).
#'@param lon1 Longitude of lower left corner (numeric).
#'@param lon2 Longitude of upper right left corner (numeric).
#'@param lat1 Latitude of lower left corner (numeric).
#'@param lat2 Latitude of upper right corner (numeric).
#'@param nc34 NetCDF version of output file. If \code{nc34 = 3} the output file will be
#'  in NetCDFv3 format (numeric). Default output is NetCDFv4.
#'@param overwrite logical; should existing output file be overwritten?
#'@param verbose logical; if TRUE, progress messages are shown
#'@param nc Alternatively to \code{infile} you can specify the input as an
#'  object of class `ncdf4` (as returned from \code{ncdf4::nc_open}).
#'
#'@return A NetCDF file including the selected region is written.
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
#'## Cut a region of the example CM SAF NetCDF file and write the output
#'## to a new file.
#'sellonlatbox(var = "SIS", infile = file.path(tempdir(),"CMSAF_example_file.nc"),
#'  outfile = file.path(tempdir(),"CMSAF_example_file_sellonlatbox.nc"), 
#'  lon1 = 8, lon2 = 12, lat1 = 48, lat2 = 52)
#'
#'unlink(c(file.path(tempdir(),"CMSAF_example_file.nc"), 
#'  file.path(tempdir(),"CMSAF_example_file_sellonlatbox.nc")))
sellonlatbox <- function(var, infile, outfile, lon1 = -180, lon2 = 180,
                         lat1 = -90, lat2 = 90, nc34 = 4, overwrite = FALSE, verbose = FALSE,
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

  if (!(file_data$grid$is_regular || length(file_data$grid$vars))) {
    stop("No lon/lat information found in file, please add by applying add_grid_info")
  }
  if (is.null(file_data$variable$prec) || !(file_data$variable$prec %in% PRECISIONS_VAR)) {
    file_data$variable$prec <- "float"
  }

  if (!length(file_data$grid$vars)) {
    lon <- file_data$dimension_data$x
    lat <- file_data$dimension_data$y
  }else{
    lon <- file_data$grid$vars_data[[LON_NAMES$DEFAULT]]
    lat <- file_data$grid$vars_data[[LAT_NAMES$DEFAULT]]
  }

  lon_limit <- which(lon >= lon1 & lon <= lon2, arr.ind = TRUE)
  lat_limit <- which(lat >= lat1 & lat <= lat2, arr.ind = TRUE)

  # check for empty lon_limit or lat_limit
  if (!length(lon_limit) || !length(lat_limit)) {
    stop("Selected region is outside target area!")
  }


  if (file_data$grid$is_regular) {
    file_data$dimension_data$x <- file_data$dimension_data$x[lon_limit]
    file_data$dimension_data$y <- file_data$dimension_data$y[lat_limit]

    startx <- min(lon_limit)
    starty <- min(lat_limit)
    countx <- length(lon_limit)
    county <- length(lat_limit)

    #id <- nc_open(infile)
    #result <- ncvar_get(id,file_data$variable$name,start = c(startx, starty, 1), count = c(countx, county, 1))
    result <- array(NA, dim = c(countx, county, 1))
    #nc_close(id)
  } else{
    lonlat_merge <- data.matrix(merge(lon_limit, lat_limit, by.x = c("row", "col"), by.y = c("row", "col"), out.class = matrix))

    x_range <- which(file_data$dimension_data$x %in% file_data$dimension_data$x[lonlat_merge[, 1]])
    y_range <- which(file_data$dimension_data$y %in% file_data$dimension_data$y[lonlat_merge[, 2]])

    file_data$dimension_data$x <- file_data$dimension_data$x[x_range]
    file_data$dimension_data$y <- file_data$dimension_data$y[y_range]

    file_data$grid$vars_data$lon <- file_data$grid$vars_data$lon[x_range, y_range]
    file_data$grid$vars_data$lat <- file_data$grid$vars_data$lat[x_range, y_range]

    #id <- nc_open(infile)
    #vari <- ncvar_get(id, file_data$variable$name)
    #result <- vari[x_range, y_range]
    #nc_close(id)

    result <- array(NA, dim = c(length(x_range), length(y_range), 1))
  }

  # if (length(file_data$dimension_data$t)==1) {
  #   dummy <- array(NA,dim=c(dim(result)[1],dim(result)[2],1))
  #   dummy[,,1] <- result
  #   result <- dummy
  # }

  result[is.na(result)] <- file_data$variable$attributes$missing_value

  if (file_data$time_info$has_time_bnds) {
    time_bnds <- get_time_bounds_from_file(infile, nc = nc)[, 1]
    vars_data <- list(result = result, time_bounds = time_bnds)
  }else{
    vars_data <- list(result = result)
  }

  # create netcdf
  nc_format <- get_nc_version(nc34)
  cmsaf_info <- (paste0("cmsafops::sellonlatbox for variable ", file_data$variable$name))

  ##### prepare output #####
  global_att_list <- names(file_data$global_att)
  global_att_list <- global_att_list[toupper(global_att_list) %in% toupper(GLOBAL_ATT_DEFAULT)]
  global_attributes <- file_data$global_att[global_att_list]

  dims <- define_dims(file_data$grid$is_regular,
                      file_data$dimension_data$x,
                      file_data$dimension_data$y,
                      file_data$dimension_data$t[1],
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
    with_time_bnds = file_data$time_info$has_time_bnds
  )

  if (file_data$time_info$has_time_bnds) {
    time_bnds <- get_time_bounds_from_file(infile, nc = nc)
  }

  if (!is.null(nc)) nc_in <- nc
  else nc_in <- nc_open(infile)
  nc_out <- nc_open(outfile, write = TRUE)


  for (i in seq_len(length(file_data$dimension_data$t))) {
    if (file_data$grid$is_regular) {
      result <- ncvar_get(nc_in, file_data$variable$name, start = c(startx, starty, i), count = c(countx, county, 1))
    } else{
      vari <- ncvar_get(nc_in, file_data$variable$name, start = c(1, 1, i), count = c(-1, -1, 1))
      result <- vari[x_range, y_range]
    }

    result[is.na(result)] <- file_data$variable$attributes$missing_value
    ncvar_put(nc_out, vars[[1]], result, start = c(1, 1, i), count = c(-1, -1, 1))
    ncvar_put(nc_out, dims$t, file_data$dimension_data$t[i], start = i, count = 1)
    if (file_data$time_info$has_time_bnds) {
      ncvar_put(nc_out, vars[[2]], time_bnds[, i], start = c(1, i),
                count = c(-1, 1))
    }
  }
  if (is.null(nc)) nc_close(nc_in)
  nc_close(nc_out)

  calc_time_end <- Sys.time()
  if (verbose) message(get_processing_time_string(calc_time_start, calc_time_end))
}
