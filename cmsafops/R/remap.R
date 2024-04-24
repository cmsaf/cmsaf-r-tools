#' Grid interpolation.
#'
#' The function interpolates the data of infile1 to the grid of infile2. From
#' infile2 only the grid information is used.
#' By default, a nearest neighbor interpolation provided by
#' \code{\link[FNN:get.knn]{get.knnx}} is used. For interpolation between
#' regular grids a simple bilinear interpolation as provided by
#' \code{\link[fields:interp.surface]{interp.surface.grid}} as well as a conservative
#' remapping as provided by \code{\link[rainfarmr:remapcon]{remapcon}} can be chosen.
#'
#' @param var Name of NetCDF variable (character).
#' @param infile1 Filename of first input NetCDF file. This may include the
#'   directory (character). The data of infile1 are interpolated.
#' @param infile2 Filename of second input file. This may include the directory
#'   (character). The grid information of infile2 are the target grid for the
#'   interpolation. This File may also be an ASCII-File containing the grid
#'   information.
#' @param outfile Filename of output NetCDF file. This may include the directory
#'    (character).
#' @param method Method used for remapping (character).
#'  Options are "bilinear" for bilinear interpolation,
#'  "conservative" for conservative remapping (only for regular grids, respectively)
#'  and "nearest" for nearest-neighbor interpolation.
#'  Default is "nearest".
#' @param dxy_factor In case of nearest neighbor all grid points with distance > (dxy * dxy_factor) 
#'  are set to NA (numeric). Default is 1.
#' @param nc34 NetCDF version of output file. If \code{nc34 = 3} the output file will be
#'  in NetCDFv3 format (numeric). Default output is NetCDFv4.
#' @param overwrite logical; should existing output file be overwritten?
#' @param verbose logical; if TRUE, progress messages are shown
#' @param nc1 Alternatively to \code{infile1} you can specify the input as an
#'  object of class `ncdf4` (as returned from \code{ncdf4::nc_open}).
#' @param nc2 Alternatively to \code{infile2} you can specify the input as an
#'  object of class `ncdf4` (as returned from \code{ncdf4::nc_open}).
#'
#' @return A NetCDF file including the interpolated data of infile1 on the grid of
#' infile2 is written.
#' @export
#'
#'@family data manipulation functions
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
#'lon2 <- seq(5, 15, 1)
#'lat2 <- seq(45, 55, 1)
#'time <- c(as.Date("2000-01-01"), as.Date("2001-02-01"))
#'origin <- as.Date("1983-01-01 00:00:00")
#'time <- as.numeric(difftime(time, origin, units = "hour"))
#'data1 <- array(250:350, dim = c(21, 21, 1))
#'data2 <- array(230:320, dim = c(21, 21, 1))
#'
#'## create two example NetCDF files
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
#'
#'x <- ncdim_def(name = "lon", units = "degrees_east", vals = lon2)
#'y <- ncdim_def(name = "lat", units = "degrees_north", vals = lat2)
#'t <- ncdim_def(name = "time", units = "hours since 1983-01-01 00:00:00",
#'  vals = time[1], unlim = TRUE)
#'ncnew <- nc_create(file.path(tempdir(),"CMSAF_example_file_2.nc"), vars)
#'ncvar_put(ncnew, var1, data2)
#'ncatt_put(ncnew, "lon", "standard_name", "longitude", prec = "text")
#'ncatt_put(ncnew, "lat", "standard_name", "latitude", prec = "text")
#'nc_close(ncnew)
#'
#'## Interpolate the fields of both example CM SAF NetCDF file 1 to the
#'## coarser grid of file 2 and write the result into one output file.
#'remap(var = "SIS", infile1 = file.path(tempdir(),"CMSAF_example_file_1.nc"), 
#'  infile2 = file.path(tempdir(),"CMSAF_example_file_2.nc"),
#'  outfile = file.path(tempdir(),"CMSAF_example_file_remap.nc"))
#'
#'unlink(c(file.path(tempdir(),"CMSAF_example_file_1.nc"), 
#'  file.path(tempdir(),"CMSAF_example_file_2.nc"),
#'  file.path(tempdir(),"CMSAF_example_file_remap.nc")))
remap <- function(var, infile1, infile2, outfile, method = "nearest", nc34 = 4,
                  overwrite = FALSE, verbose = FALSE, nc1 = NULL, nc2 = NULL) {
  calc_time_start <- Sys.time()

  check_variable(var)
  if (is.null(nc1)) check_infile(infile1)
  if (is.null(nc2)) check_infile(infile2)
  check_outfile(outfile)
  outfile <- correct_filename(outfile)
  check_overwrite(outfile, overwrite)
  check_nc_version(nc34)
  stopifnot(method %in% c("bilinear", "conservative", "nearest"))

  ##### extract data from file #####
  file_data1 <- read_file(infile1, var, nc = nc1)
  file_data1$variable$prec <- PRECISIONS_VAR$FLOAT
  if (file_data1$time_info$has_time_bnds) {
    time_bnds <- get_time_bounds_from_file(infile1, nc = nc1)
  }

  nc_format <- get_nc_version(nc34)

  if (endsWith(infile2, ".txt")) {
    lonlat <- read_gridfile(infile2)
    file_data2 <- list()
    file_data2$grid <- list()
    file_data2$dimension_data <- list(x = lonlat[[1]], y = lonlat[[2]])
    file_data2$grid$is_regular <- TRUE

  }else{
    file_data2 <- read_file(infile2, NULL, nc = nc2)
  }

  isReg1 <- file_data1$grid$is_regular
  isReg2 <- file_data2$grid$is_regular

  if (method %in% c("conservative", "bilinear")  && !(isReg1 && isReg2)) {
    stop("Conservative or bilinear remapping only available for regular grids!")
  }

  if (isReg1) {
    ref <- list(file_data1$dimension_data$x, file_data1$dimension_data$y)
  }else{
    ref <- list(file_data1$grid$vars_data[[LON_NAMES$DEFAULT]],
                file_data1$grid$vars_data[[LAT_NAMES$DEFAULT]])
  }

  if (isReg2) {
    ref2 <- list(file_data2$dimension_data$x, file_data2$dimension_data$y)
  }else{
    ref2 <- list(file_data2$grid$vars_data[[LON_NAMES$DEFAULT]],
                 file_data2$grid$vars_data[[LAT_NAMES$DEFAULT]])
  }

  if (max(ref[[1]], na.rm = TRUE) > 180) {
    ref[[1]] <-
      ifelse(ref[[1]] > 180, -360 + ref[[1]], ref[[1]])
  }

  dxy <- LON_RANGE

  lon_limit <- which(ref2[[1]] > (min(ref[[1]], na.rm = TRUE) - dxy) & ref2[[1]]
                     < (max(ref[[1]], na.rm = TRUE) + dxy), arr.ind = TRUE)
  lat_limit <- which(ref2[[2]] > (min(ref[[2]], na.rm = TRUE) - dxy) & ref2[[2]]
                     < (max(ref[[2]], na.rm = TRUE) + dxy), arr.ind = TRUE)

  # check for empty lon_limit or lat_limit
  if (length(lon_limit) == 0 || length(lat_limit) == 0) {
    stop("Selected grids do not have any overlap")
  }

  if (isReg2) {
    x_dim_name <- LON_NAMES$DEFAULT
    y_dim_name <- LAT_NAMES$DEFAULT
    x_dim_unit <- UNITS$DEGREES_EAST
    y_dim_unit <- UNITS$DEGREES_NORTH

    file_data2$dimension_data$x <- file_data2$dimension_data$x[lon_limit]
    file_data2$dimension_data$y <- file_data2$dimension_data$y[lat_limit]

  }else{
    x_dim_name <- X_NAMES$DEFAULT
    y_dim_name <- Y_NAMES$DEFAULT
    x_dim_unit <- UNITS$KILOMETER
    y_dim_unit <- UNITS$KILOMETER

    lonlat_merge <- data.matrix(merge(lon_limit, lat_limit,
                                      by.x = c("row", "col"),
                                      by.y = c("row", "col"),
                                      out.class = matrix))

    x_range <- which(file_data2$dimension_data$x %in% file_data2$dimension_data$x[lonlat_merge[, 1]])
    y_range <- which(file_data2$dimension_data$y %in% file_data2$dimension_data$y[lonlat_merge[, 2]])

    file_data2$dimension_data$x <- file_data2$dimension_data$x[x_range]
    file_data2$dimension_data$y <- file_data2$dimension_data$y[y_range]

    file_data2$grid$vars_data[[LON_NAMES$DEFAULT]] <- file_data2$grid$vars_data$lon[x_range, y_range]
    file_data2$grid$vars_data[[LAT_NAMES$DEFAULT]] <- file_data2$grid$vars_data$lat[x_range, y_range]
  }

  result <- array(NA, dim = c(length(file_data2$dimension_data$x),
                           length(file_data2$dimension_data$y),
                           1))
  result[is.na(result)] <- file_data1$variable$attributes$missing_value

  if (file_data1$time_info$has_time_bnds) {
    vars_data <- list(result = result, time_bounds = time_bnds[, 1])
  }else{
    vars_data <- list(result = result)
  }

  cmsaf_info <- (paste0("cmsafops::remap for variable ", file_data1$variable$name))

  ##### prepare output #####
  global_att_list <- names(file_data1$global_att)
  global_att_list <- global_att_list[toupper(global_att_list) %in% toupper(GLOBAL_ATT_DEFAULT)]
  global_attributes <- file_data1$global_att[global_att_list]

  dims <- define_dims(file_data2$grid$is_regular,
                      file_data2$dimension_data$x,
                      file_data2$dimension_data$y,
                      file_data1$dimension_data$t[1],
                      NB2,
                      file_data1$time_info$units,
                      with_time_bnds = file_data1$time_info$has_time_bnds
  )

  vars <- define_vars(file_data1$variable, dims, nc_format$compression,
                      with_time_bnds = file_data1$time_info$has_time_bnds)

  file_data2$grid <- redefine_grid_vars(file_data2$grid, dims, nc_format$compression, file_data2$grid$vars_data)

  write_output_file(
    outfile,
    nc_format$force_v4,
    vars,
    vars_data,
    file_data1$variable$name,
    file_data2$grid$vars, file_data2$grid$vars_data,
    cmsaf_info,
    file_data1$time_info$calendar,
    file_data1$variable$attributes,
    global_attributes,
    with_time_bnds = file_data1$time_info$has_time_bnds
  )

  ##### calculate and write result #####
  if (!is.null(nc1)) nc_in <- nc1
  else nc_in <- nc_open(infile1)
  nc_out <- nc_open(outfile, write = TRUE)

  ref_vec1 <- as.vector(ref[[1]])
  ref_vec2 <- as.vector(ref[[2]])

  ref[[1]] <- ref_vec1[!is.na(ref_vec1)]
  ref[[2]] <- ref_vec2[!is.na(ref_vec2)]

  if (file_data1$time_info$has_time_bnds) {
    vars_data$time_bounds <- time_bnds
  }

  if (method == "nearest") {
    if (isReg1 && isReg2) {
      fnn_a <- FNN::get.knnx(ref[[1]], file_data2$dimension_data$x, k = 1)
      fnn_b <- FNN::get.knnx(ref[[2]], file_data2$dimension_data$y, k = 1)
    }else if (isReg2) {
      target_gr <- expand.grid(file_data2$dimension_data$x,
                               file_data2$dimension_data$y)
      ref_m <- cbind(ref[[1]], ref[[2]])
      fnn <- FNN::get.knnx(ref_m, target_gr, k = 1)
    }else{
      target1 <- as.vector(file_data2$grid$vars_data[[LON_NAMES$DEFAULT]])
      target2 <- as.vector(file_data2$grid$vars_data[[LAT_NAMES$DEFAULT]])
      not_na <- which(!(target1 <= -999 | is.na(target1) | target2 <= -999
                        | is.na(target2)))
      target_ref1 <- target1[not_na]
      target_ref2 <- target2[not_na]
      target_vec <- cbind(target_ref1, target_ref2)
      ref_m <- cbind(ref[[1]], ref[[2]])
      fnn <- FNN::get.knnx(ref_m, target_vec, k = 1)
      fnn_target <- match(target_ref1[fnn$nn.index], target1)
    }
  }

  for (i in seq_len(length(file_data1$dimension_data$t))) {
    rdata <- ncvar_get(nc_in, file_data1$variable$name, start = c(1, 1, i), count = c(-1, -1, 1))

    switch(method,
           nearest = {
             if (isReg1 && isReg2) {
               result <- rdata[fnn_a$nn.index, fnn_b$nn.index]
             }else if (isReg2) {
               rdata <- as.vector(rdata)
               rdata <- rdata[!is.na(ref_vec1)]
               result <- array(rdata[fnn$nn.index], dim = dim(result))
               result[fnn$nn.dist > dxy * abs(dxy_factor)] <- NA
             }else{
               rdata <- as.vector(rdata)
               rdata <- rdata[!is.na(ref_vec1)]
               result <- array(rdata[fnn_target], dim = dim(result))
               result[fnn$nn.dist > dxy * abs(dxy_factor)] <- NA
             }
           },
           conservative =
             {
               result <- rainfarmr::remapcon(ref[[1]],
                                             ref[[2]],
                                             rdata,
                                             file_data2$dimension_data$x,
                                             file_data2$dimension_data$y
               )
             },
           bilinear =
             {
               result <- fields::interp.surface.grid(list(x = ref[[1]],
                                                          y = ref[[2]],
                                                          z = rdata),
                                                     list(x = file_data2$dimension_data$x,
                                                          y = file_data2$dimension_data$y
                                                     ))$z
             })

    result[is.na(result)] <- file_data1$variable$attributes$missing_value
    ncvar_put(nc_out, vars[[1]], result, start = c(1, 1, i), count = c(-1, -1, 1))
    ncvar_put(nc_out, dims$t, file_data1$dimension_data$t[i], start = i, count = 1)
    if (file_data1$time_info$has_time_bnds) {
      ncvar_put(nc_out, vars[[2]], vars_data$time_bounds[, i], start = c(1, i), count = c(-1, 1))
    }
  }

  nc_close(nc_out)
  if (is.null(nc1)) nc_close(nc_in)

  calc_time_end <- Sys.time()
  if (verbose) message(get_processing_time_string(calc_time_start, calc_time_end))
}
