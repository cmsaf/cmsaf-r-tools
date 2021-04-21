get_new_dimension <- function(dimension, grid_box_dimension, amount_grid_box, size_modulo) {
  counter <- grid_box_dimension
  counter_start <- 1
  result_vector <- c()
  for(i in 1:amount_grid_box) {
    if(size_modulo != 0 & i == amount_grid_box){
      if(size_modulo != 1){
        counter <- counter - grid_box_dimension + size_modulo
        lat_new <- dimension[counter_start] + (dimension[counter]-dimension[counter_start])/2
        result_vector <- append(result_vector, lat_new)
      }
      else{
        result_vector <- append(result_vector, dimension[counter_start])
      }
    }
    else{
      lat_new <- dimension[counter_start] + (dimension[counter]-dimension[counter_start])/2
      result_vector <- append(result_vector, lat_new)
      counter <- counter + grid_box_dimension
      counter_start <- counter_start + grid_box_dimension
    }
  }
  return(result_vector)
}

get_data_grid_operator <- function(op, data_operator, j, verbose) {
  switch(op,
         {
           if (verbose) message(paste0("apply gridbox maximum ", j))
           ifelse( !all(is.na(data_operator)),   # only if all values are na, write na
                   {
                     result_value <- max(data_operator, na.rm = TRUE)
                   }, 
                   {
                     result_value <- max(data_operator, na.rm = FALSE)
                   })
         },
         {
           if (verbose) message(paste0("apply gridbox minimum ", j))
           
           ifelse( !all(is.na(data_operator)),   # only if all values are na, write na
                   {
                     result_value <- min(data_operator, na.rm = TRUE)
                   }, 
                   {
                     result_value <- min(data_operator, na.rm = FALSE)
                   })
         },
         {
           if (verbose) message(paste0("apply gridbox range ", j))
           # max function
           ifelse( !all(is.na(data_operator)),   # only if all values are na, write na
                   {
                     data_max <- max(data_operator, na.rm = TRUE)
                   }, 
                   {
                     data_max <- max(data_operator, na.rm = FALSE)
                   })
           
           # min function
           ifelse( !all(is.na(data_operator)),   # only if all values are na, write na
                   {
                     data_min <- min(data_operator, na.rm = TRUE)
                   }, 
                   {
                     data_min <- min(data_operator, na.rm = FALSE)
                   })
           
           result_value <- data_max - data_min
         },
         {
           if (verbose) message(paste0("apply gridbox sum ", j))
           result_value <- sum(data_operator, na.rm = TRUE)
         },
         {
           if (verbose) message(paste0("apply gridbox mean ", j))
           result_value <- mean(data_operator, na.rm = TRUE)
         },
         {
           if (verbose) message(paste0("apply gridbox standard deviation ", j))
           result_value <- stats::sd(data_operator, na.rm = TRUE)
         },
         {
           if (verbose) message(paste0("apply gridbox variance ", j))
           result_value <- (stats::sd(data_operator, na.rm = TRUE))^2
         }
  )
  return(result_value)
}

gridboxx_wrapper <- function(op, var, lonGrid, latGrid, infile, outfile, nc34, overwrite, verbose) {
  calc_time_start <- Sys.time()
  gc()
  
  if((lonGrid %% 1 == 0) & (latGrid %% 1 == 0)) {
    check_variable(var)
    check_infile(infile)
    check_outfile(outfile)
    outfile <- correct_filename(outfile)
    check_overwrite(outfile, overwrite)
    check_nc_version(nc34)
    
    ##### extract data from file #####
    file_data <- read_file(infile, var)
    
    if (op > 2) {
      file_data$variable$prec <- "float"
    }
    dateID <- unique(file_data$dimension_data$t)
  
    lonSize <- length(file_data$dimension_data$x)
    latSize <- length(file_data$dimension_data$y)
    
    # error messages
    if(lonSize < lonGrid && latSize < latGrid)
      stop("The index for the longitude and for the latitude is greater than in the data. Please choose a smaller index. ")
    else if(lonSize < lonGrid)
      stop("The index for the longitude is greater than in the data. Please choose a smaller index for the longitude. ")
    else if(latSize < latGrid)
      stop("The index for the latitude is greater than in the data. Please choose a smaller index for the latitude. ")
    else if(lonGrid <= 0 || latGrid <= 0)
      stop("The index must be greater than zero. ")
    else if(round(lonGrid) != lonGrid || round(latGrid) != latGrid)
      stop("Please use integer values for the index. ")
    
    lonSizeModulo <-  lonSize %% lonGrid
    latSizeModulo <-  latSize %% latGrid
    
    lonGridBoxes <- (lonSize - lonSizeModulo)/lonGrid
    latGridBoxes <- (latSize - latSizeModulo)/latGrid
    
    if(latSizeModulo != 0){
      latGridBoxes <- latGridBoxes + 1
    }
    if(lonSizeModulo != 0){
      lonGridBoxes <- lonGridBoxes + 1
    }
    
    result_vector_x <- get_new_dimension(file_data$dimension_data$x, lonGrid, lonGridBoxes, lonSizeModulo)
    result_vector_y <- get_new_dimension(file_data$dimension_data$y, latGrid, latGridBoxes, latSizeModulo)
   
    # Use placeholder for result so that it can be calculated later without the
    # need to have all input data in memory concurrently.
    data_placeholder <- array(
      file_data$variable$attributes$missing_value,
      dim = c(length(result_vector_x),
              length(result_vector_y),
              length(dateID))
    )
    
    vars_data <- list(result = data_placeholder, time_bounds = file_data$dimension_data$t)
    nc_format <- get_nc_version(nc34)
    
    cmsaf_info <- switch(
      op,
      paste0("cmsaf::gridboxmax for variable ", file_data$variable$name),
      paste0("cmsaf::gridboxmin for variable ", file_data$variable$name),
      paste0("cmsaf::gridboxrange for variable ", file_data$variable$name),
      paste0("cmsaf::gridboxsum for variable ", file_data$variable$name),
      paste0("cmsaf::gridboxmean for variable ", file_data$variable$name),
      paste0("cmsaf::gridboxsd for variable ", file_data$variable$name),
      paste0("cmsaf::gridboxvar for variable ", file_data$variable$name)
    )
   
    ##### prepare output #####
    global_att_list <- names(file_data$global_att)
    global_att_list <- global_att_list[toupper(global_att_list) %in% toupper(GLOBAL_ATT_DEFAULT)]
    global_attributes <- file_data$global_att[global_att_list]
    
    dims <- define_dims(file_data$grid$is_regular,
                        result_vector_x,
                        result_vector_y,
                        file_data$dimension_data$t,
                        NB2,
                        file_data$time_info$units,
                        with_time_bnds = FALSE)
   
    vars <- define_vars(file_data$variable, dims, nc_format$compression, with_time_bnds = FALSE)
    
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
      with_time_bnds = FALSE
    )
    
    ##### calculate and write result #####
    nc_out <- nc_open(outfile, write = TRUE)
    nc_in <- nc_open(infile)
    
    for (j in seq_along(dateID)) {
      dum_dat <- ncvar_get(nc_in, file_data$variable$name, start = c(1, 1, j), count = c(-1, -1, 1), collapse_degen = FALSE)
      
      startGridBoxLon <- 1
      endGridBoxLon <- lonGrid
      startGridBoxLat <- 1
      endGridBoxLat <- latGrid
      
      data <- array(numeric(),c(lonGridBoxes, latGridBoxes, 1))
      for(lat_index in 1:latGridBoxes){
        for(lon_index in 1:lonGridBoxes) {
          if(latSizeModulo == 0 && lonSizeModulo == 0){   # if the grid match
            dataTemp <- dum_dat[startGridBoxLon:endGridBoxLon, startGridBoxLat:endGridBoxLat, 1]
            data[lon_index, lat_index, 1] <- get_data_grid_operator(op, dataTemp, j, verbose)
          } 
          else if(latSizeModulo != 0 && lonSizeModulo == 0) {   # if lat not match with grid box
            ifelse(lat_index != latGridBoxes,
                  {
                    dataTemp <- dum_dat[startGridBoxLon:endGridBoxLon, startGridBoxLat:endGridBoxLat, 1]
                    data[lon_index, lat_index, 1] <- get_data_grid_operator(op, dataTemp, j, verbose)
                  },
                  {
                    dataTemp <- dum_dat[startGridBoxLon:endGridBoxLon, startGridBoxLat:(endGridBoxLat - latGrid + latSizeModulo), 1]
                    data[lon_index, lat_index, 1] <- get_data_grid_operator(op, dataTemp, j, verbose)
                  })
          } 
          else if(latSizeModulo == 0 && lonSizeModulo != 0) {   # if lon not match with grid box
            ifelse(lon_index != lonGridBoxes, 
                  {
                    dataTemp <- dum_dat[startGridBoxLon:endGridBoxLon, startGridBoxLat:endGridBoxLat, 1]
                    data[lon_index, lat_index, 1] <- get_data_grid_operator(op, dataTemp, j, verbose)
                  },
                  {
                    endGridBoxLon <- endGridBoxLon - lonGrid + lonSizeModulo
                    dataTemp <- dum_dat[startGridBoxLon:endGridBoxLon, startGridBoxLat:endGridBoxLat, 1]
                    data[lon_index, lat_index, 1] <- get_data_grid_operator(op, dataTemp, j, verbose)
                  })
          }
          else if(lonSizeModulo != 0 && latSizeModulo != 0) {   # if lon and lat not match with grid box
            if (lon_index != lonGridBoxes && lat_index != latGridBoxes) {
              dataTemp <- dum_dat[startGridBoxLon:endGridBoxLon, startGridBoxLat:endGridBoxLat, 1]
              data[lon_index, lat_index, 1] <- get_data_grid_operator(op, dataTemp, j, verbose)
            } 
            else if(lon_index == lonGridBoxes && lat_index != latGridBoxes) {
              endGridBoxLon <- endGridBoxLon - lonGrid + lonSizeModulo
              dataTemp <- dum_dat[startGridBoxLon:endGridBoxLon, startGridBoxLat:endGridBoxLat, 1]
              data[lon_index, lat_index, 1] <- get_data_grid_operator(op, dataTemp, j, verbose)
            } 
            else if(lon_index != lonGridBoxes && lat_index == latGridBoxes) {
              dataTemp <- dum_dat[startGridBoxLon:endGridBoxLon, startGridBoxLat:(endGridBoxLat - latGrid + latSizeModulo), 1]
              data[lon_index, lat_index, 1] <- get_data_grid_operator(op, dataTemp, j, verbose)
            }
            else if(lon_index == lonGridBoxes && lat_index == latGridBoxes) {
              endGridBoxLon <- endGridBoxLon - lonGrid + lonSizeModulo
              endGridBoxLat <- endGridBoxLat - latGrid + latSizeModulo
              dataTemp <- dum_dat[startGridBoxLon:endGridBoxLon, startGridBoxLat:endGridBoxLat, 1]
              data[lon_index, lat_index, 1] <- get_data_grid_operator(op, dataTemp, j, verbose)
            }
          }
          startGridBoxLon <- startGridBoxLon + lonGrid
          endGridBoxLon <- endGridBoxLon + lonGrid
        }
        startGridBoxLat <- startGridBoxLat + latGrid
        endGridBoxLat <- endGridBoxLat + latGrid
        startGridBoxLon <- 1
        endGridBoxLon <- lonGrid
      }
      data[is.na(data)] <- file_data$variable$attributes$missing_value
     
      ncvar_put(nc_out, vars[[1]], data, start = c(1, 1, j), count = c(-1, -1, 1))
    }
    nc_close(nc_in)
    nc_close(nc_out)
  }
  else {
    stop("The parameters lonGrid and latGrid must be integer values. ")
  }
  calc_time_end <- Sys.time()
  if (verbose) message(get_processing_time_string(calc_time_start, calc_time_end))
}
