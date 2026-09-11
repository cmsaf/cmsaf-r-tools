
remap4quicklook <- function(var, infile, auxfile, dxy = 0.05, method = "nearest", verbose = FALSE) {

  cmsafops:::check_variable(var)
  cmsafops:::check_infile(infile)
  cmsafops:::check_infile(auxfile)
  stopifnot(method %in% c("bilinear", "conservative", "nearest"))
  
  # define constants and names
  LON_NAMES <- list(
    LONG_DEFAULT = "longitude",
    "Longitude",
    "Lon",
    DEFAULT = "lon"
  )
  
  LAT_NAMES <- list(
    LONG_DEFAULT = "latitude",
    "Latitude",
    "Lat",
    DEFAULT = "lat"
  )

  ##### extract data from file #####
  file_data1 <- cmsafops::read_file(infile, var)
  
  ### check if georef variable is available
  goc <- NULL
  nc <- ncdf4::nc_open(infile)
    if ("georef_offset_corrected" %in% names(nc$var)) {
	  goc <- ncdf4::ncvar_get(nc, "georef_offset_corrected")
	  goc <- goc + 1
	}   
  ncdf4::nc_close(nc)
  
  ##### extract data from file #####
  nc_aux <- ncdf4::nc_open(auxfile)
    lon <- ncdf4::ncvar_get(nc_aux, "lon")  #TODO be more resistant to different spellings
	  if (!is.null(goc) && !is.na(dim(lon)[3])) {
	    if (dim(lon)[3] > 1) lon <- lon[,,goc]
	  }

    lat <- ncdf4::ncvar_get(nc_aux, "lat")
	  if (!is.null(goc) && !is.na(dim(lat)[3])) {
	    if (dim(lat)[3] > 1) lat <- lat[,,goc]
	  }
    ncdf4::nc_close(nc_aux)
  gc()

  file_data2 <- list()
  file_data2$grid <- list()
  file_data2$dimension_data <- list(x = seq(min(lon,na.rm=T), max(lon, na.rm=T), dxy),
                                    y = seq(min(lat,na.rm=T), max(lat, na.rm=T), dxy))
  file_data2$grid$is_regular <- TRUE

  isReg1 <- file_data1$grid$is_regular
  isReg2 <- file_data2$grid$is_regular

  if (method %in% c("conservative", "bilinear")  && !(isReg1 && isReg2)) {
    stop("Conservative or bilinear remapping only available for regular grids!")
  }

  if (isReg1) {
    ref <- list(file_data1$dimension_data$x, file_data1$dimension_data$y)
  } else if (!is.null(goc) || !is.null(lon)) {
      ref <- list(lon, lat)
  } else{
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

  lon_limit <- which(ref2[[1]] > (min(ref[[1]], na.rm = TRUE) - dxy) & ref2[[1]]
                     < (max(ref[[1]], na.rm = TRUE) + dxy), arr.ind = TRUE)
  lat_limit <- which(ref2[[2]] > (min(ref[[2]], na.rm = TRUE) - dxy) & ref2[[2]]
                     < (max(ref[[2]], na.rm = TRUE) + dxy), arr.ind = TRUE)

  # check for empty lon_limit or lat_limit
  if (length(lon_limit) == 0 || length(lat_limit) == 0) {
    stop("Selected grids do not have any overlap")
  }

  if (isReg2) {
    file_data2$dimension_data$x <- file_data2$dimension_data$x[lon_limit]
    file_data2$dimension_data$y <- file_data2$dimension_data$y[lat_limit]

  }else{
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

  vars_data <- list(result = result)

  ##### prepare output #####
  ##### calculate and write result #####
  nc_in <- ncdf4::nc_open(infile)
  
  ref_vec1 <- as.vector(ref[[1]])
  ref_vec2 <- as.vector(ref[[2]])

  ref[[1]] <- ref_vec1[!is.na(ref_vec1)]
  ref[[2]] <- ref_vec2[!is.na(ref_vec2)]

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

    rdata <- ncdf4::ncvar_get(nc_in, file_data1$variable$name, start = c(1, 1, 1), count = c(-1, -1, 1))

    switch(method,
           nearest = {
             if (isReg1 && isReg2) {
               result <- rdata[fnn_a$nn.index, fnn_b$nn.index]
             }else if (isReg2) {
               rdata <- as.vector(rdata)
               rdata <- rdata[!is.na(ref_vec1)]
               result <- array(rdata[fnn$nn.index], dim = dim(result))
             }else{
               rdata <- as.vector(rdata)
               rdata <- rdata[!is.na(ref_vec1)]
               result <- array(rdata[fnn_target], dim = dim(result))
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

    result[result == file_data1$variable$attributes$missing_value] <- NA  
    ncdf4::nc_close(nc_in)
    
    result <- result[,,1]
    lo1 <- file_data2$dimension_data$x
    la1 <- file_data2$dimension_data$y
    
    remap_data <- list(lon = lo1, lat = la1, data = result)
    gc()
    return(remap_data)
}