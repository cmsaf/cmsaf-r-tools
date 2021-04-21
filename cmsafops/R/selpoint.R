#'Extract data at a given point.
#'
#'This function extracts all data at a given point. A point is given by a pair
#'of longitude and latitude coordinates. The function will find the closest grid
#'point to the given coordinates and extracts the data for this point. The
#'output-file can be optional in NetCDF or csv. The outfile is checked for the
#'correct file extension.
#'
#'@param var Name of NetCDF variable (character).
#'@param infile Filename of input NetCDF file. This may include the directory
#'  (character).
#'@param outfile Filename of output NetCDF file. This may include the directory
#'  (character).
#'@param lon1 Longitude of desired point (numeric).
#'@param lat1 Latitude of desired point (numeric).
#'@param format Intended output format. Options are \code{nc} or \code{csv}. Default is
#'  \code{nc} (character).
#'@param nc34 NetCDF version of output file. If \code{nc34 = 3} the output file will be
#'  in NetCDFv3 format (numeric). Default output is NetCDFv4.
#'@param overwrite logical; should existing output file be overwritten?
#'@param verbose logical; if TRUE, progress messages are shown
#'
#'@return A NetCDF or csv file including the selected point is written. The
#'  csv file is tested for use in Excel and includes two columns (Time and
#'  Data), which are separated by ';'.
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
#'## Select a point of the example CM SAF NetCDF file and write the output
#'## to a csv-file.
#'selpoint(var = "SIS", infile = file.path(tempdir(),"CMSAF_example_file.nc"), 
#'  outfile = file.path(tempdir(),"CMSAF_example_file_selpoint.nc"),
#'  lon1 = 8, lat1 = 48, format = "csv")
#'
#'unlink(c(file.path(tempdir(),"CMSAF_example_file.nc"), 
#'  file.path(tempdir(),"CMSAF_example_file_selpoint.nc.csv")))
selpoint <- function(var, infile, outfile, lon1 = 0, lat1 = 0, format = "nc",
                     nc34 = 4, overwrite = FALSE, verbose = FALSE) {
  check_variable(var)

  check_infile(infile)

  check_overwrite(outfile, overwrite)

  check_nc_version(nc34)
  check_format(format)
  calc_time_start <- Sys.time()

  # get information about dimensions and attributes
  file_data <- read_file(infile, var)
  if (file_data$time_info$has_time_bnds) {
    time_bnds <- get_time_bounds_from_file(infile)
  }

  if (!(file_data$grid$is_regular || length(file_data$grid$vars))) {
    stop("No lon/lat information found in file, please add by applying add_grid_info")
  }

  # find closest point to target coordinates using sp package
  if (file_data$grid$is_regular) {
    dlon <- abs(file_data$dimension_data$x[1] - file_data$dimension_data$x[2])
    dlat <- abs(file_data$dimension_data$y[1] - file_data$dimension_data$y[2])

    lon_limit <- which(file_data$dimension_data$x >= (lon1 - dlon)
                       & file_data$dimension_data$x <= (lon1 + dlon))
    lat_limit <- which(file_data$dimension_data$y >= (lat1 - dlat)
                       & file_data$dimension_data$y <= (lat1 + dlat))

    if (!(any(lon_limit) & any(lat_limit))) {
      stop("Coordinates outside of the domain.")
    }

    lon2 <- file_data$dimension_data$x[lon_limit]
    lat2 <- file_data$dimension_data$y[lat_limit]

    pos <- sp::SpatialPoints(cbind(lon1, lat1),
                             proj4string = sp::CRS("+proj=longlat +datum=WGS84"))
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

    file_data$dimension_data$x <- file_data$dimension_data$x[lon_limit]
    file_data$dimension_data$y <- file_data$dimension_data$y[lat_limit]

    id <- nc_open(infile)
    result <- ncvar_get(id, file_data$variable$name,
                        start = c(lon_limit, lat_limit, 1), count = c(1, 1, -1))
    nc_close(id)
  } else {
    # sets range in which to look for a nearest neighbour, can be set in constants.R
    dlon <- LON_RANGE
    dlat <- LAT_RANGE

    lon_limit <- which(file_data$grid$vars_data[[LON_NAMES$DEFAULT]]
                       >= (lon1 - dlon)
                       & file_data$grid$vars_data[[LON_NAMES$DEFAULT]]
                       <= (lon1 + dlon), arr.ind = TRUE)
    lat_limit <- which(file_data$grid$vars_data[[LAT_NAMES$DEFAULT]]
                       >= (lat1 - dlat)
                       & file_data$grid$vars_data[[LAT_NAMES$DEFAULT]]
                       <= (lat1 + dlat), arr.ind = TRUE)

    if (!(any(lon_limit) & any(lat_limit))) {
      stop("Coordinates outside of the domain.")
    }

    lonlat_merge <- data.matrix(merge(lon_limit, lat_limit,
                                      by.x = c("row", "col"),
                                      by.y = c("row", "col"),
                                      out.class = matrix))

    dist <- sp::spDistsN1(cbind(file_data$grid$vars_data[[LON_NAMES$DEFAULT]][lonlat_merge],
                                file_data$grid$vars_data[[LAT_NAMES$DEFAULT]][lonlat_merge]),
                          c(lon1, lat1), longlat = FALSE)
    mini <- which.min(dist)
    nearest <- lonlat_merge[mini, ]

    x_nearest <- nearest[1]
    y_nearest <- nearest[2]

    file_data$dimension_data$x <- file_data$dimension_data$x[x_nearest]
    file_data$dimension_data$y <- file_data$dimension_data$y[y_nearest]

    file_data$grid$vars_data[[LON_NAMES$DEFAULT]] <- file_data$grid$vars_data[[LON_NAMES$DEFAULT]][x_nearest, y_nearest]
    file_data$grid$vars_data[[LAT_NAMES$DEFAULT]] <- file_data$grid$vars_data[[LAT_NAMES$DEFAULT]][x_nearest, y_nearest]

    id <- nc_open(infile)
    result <- ncvar_get(id, file_data$variable$name,
                        start = c(x_nearest, y_nearest, 1), count = c(1, 1, -1))
    nc_close(id)
  }

  if (length(file_data$dimension_data$t) == 1) {
    dummy <- array(NA, dim = c(1, 1, 1))
    dummy[1, 1, 1] <- result
    result <- dummy
  }

  # file output
  format <- ifelse(toupper(format) == "CSV", "csv", "nc")

  if (format == "nc") {
    # create netcdf
    nc_format <- get_nc_version(nc34)
    cmsaf_info <- (paste0("cmsaf::selpoint for variable ", file_data$variable$name))

    dum_fname <- unlist(strsplit(outfile, "\\."))
    if (dum_fname[length(dum_fname)] != "nc")
      outfile <- paste0(outfile, ".nc")

    result[is.na(result)] <- file_data$variable$attributes$missing_value

    if (file_data$time_info$has_time_bnds) {
      vars_data <- list(result = result, time_bounds = time_bnds)
    } else {
      vars_data <- list(result = result)
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
                        with_time_bnds = file_data$time_info$has_time_bnds)

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

  } else {
    dum_fname <- unlist(strsplit(outfile, "\\."))
    if (dum_fname[length(dum_fname)] != "csv")
      outfile <- paste0(outfile, ".csv")
    dataframe <- data.frame(get_time(file_data$time_info$units,
                                     file_data$dimension_data$t), result)
    utils::write.table(dataframe, file = outfile, row.names = FALSE, sep = ";")
  }

  calc_time_end <- Sys.time()
  if (verbose)
    message(get_processing_time_string(calc_time_start, calc_time_end))
}
