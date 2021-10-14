#' Routine to adjust the time dimensions and coordinates in two files. 
#' 
#' Designed for CM SAF R Toolbox.
#' 
#' @param var1 Name of NetCDF variable of the first data set (character).
#' @param infile1 Filename of first input NetCDF file. This may include the directory
#'  (character).
#' @param var2 Name of NetCDF variable of the second data set (character).
#' @param infile2 Filename of second input NetCDF file. This may include the directory
#'  (character).
#' @param outfile1 Filename of first output NetCDF file. This may include the directory
#'  (character).
#' @param outfile2 Filename of second output NetCDF file. This may include the directory
#'  (character).
#' @param nc34 NetCDF version of output file. If \code{nc34 = 3} the output file will be
#'  in NetCDFv3 format (numeric). Default output is NetCDFv4.
#' @param overwrite logical; should existing output file be overwritten?
#' @param verbose logical; if TRUE, progress messages are shown
#'@param nc1 Alternatively to \code{infile1} you can specify the input as an
#'  object of class `ncdf4` (as returned from \code{ncdf4::nc_open}).
#'@param nc2 Alternatively to \code{infile2} you can specify the input as an
#'  object of class `ncdf4` (as returned from \code{ncdf4::nc_open}).
#' 
#' @return Two NetCDF files with the same time period and coordinate system are the result.
#' 
#'@export
cmsaf.adjust.two.files <- function(var1, infile1, var2, infile2, outfile1, outfile2, nc34 = 4, overwrite = FALSE, verbose = FALSE,
                                   nc1 = NULL, nc2 = NULL) {
  gc()
  
  calc_time_start <- Sys.time()
  
  check_variable(var1)
  check_variable(var2)
  
  if (is.null(nc1)) check_infile(infile1)
  if (is.null(nc2)) check_infile(infile2)
  
  check_outfile(outfile1)
  check_outfile(outfile2)
  
  outfile <- correct_filename(outfile1)
  check_overwrite(outfile1, overwrite)
  
  outfile <- correct_filename(outfile2)
  check_overwrite(outfile2, overwrite)
  
  check_nc_version(nc34)
  
  ##### extract data from two files #####
  timestep_string <- c("m","h", "d", "m", "y")
  time_agg_func <- c("hourmean", "hourmean", "daymean", "monmean", "yearmean")
  
  # infile1
  if (!is.null(nc1)) id <- nc1
  else id <- nc_open(infile1)
  dim_names   <- names(id$dim)
  dimensions <- get_dimensions(id, dim_names)
  time_info <- get_time_info(id, dim_names, dimensions$names$t)
  timestep_infile1 <- substr(time_info$units, 1, 1)
  if (is.null(nc1)) nc_close(id)
  
  # infile2
  id <- nc_open(infile2)
  dim_names   <- names(id$dim)
  dimensions <- get_dimensions(id, dim_names)
  time_info <- get_time_info(id, dim_names, dimensions$names$t)
  timestep_infile2 <- substr(time_info$units, 1, 1)
  nc_close(id)
  
  temp_dir <- file.path(tempdir(), "cmsaf_adjust_two_files_tmp.nc")
  temp_infile1 <- file.path(tempdir(), "cmsaf_adjust_two_files_infile1_tmp.nc")
  temp_infile2 <- file.path(tempdir(), "cmsaf_adjust_two_files_infile2_tmp.nc")
  temp_remap_infile1 <- file.path(tempdir(), "cmsaf_adjust_two_files_infile1_remap_tmp.nc")
  temp_remap_infile2 <- file.path(tempdir(), "cmsaf_adjust_two_files_infile2_remap_tmp.nc")
  
  if(file.exists(temp_dir)){
    unlink(temp_dir)
  }
  if(file.exists(temp_infile1)){
    unlink(temp_infile1)
  }
  if(file.exists(temp_infile2)){
    unlink(temp_infile2)
  }
  if(file.exists(temp_remap_infile1)){
    unlink(temp_remap_infile1)
  }
  if(file.exists(temp_remap_infile2)){
    unlink(temp_remap_infile2)
  }
  
  newinfile1 <- FALSE
  newinfile2 <- FALSE
  # Adjust time resolution
  if((timestep_infile2 %in% timestep_string) && (timestep_infile1 %in% timestep_string)) { 
    if(timestep_infile2 != timestep_infile1) {
      position_timestep1 <- match(timestep_infile1, timestep_string)
      position_timestep2 <- match(timestep_infile2, timestep_string)
      
      time_agg_level_max <- max(position_timestep1, position_timestep2)
      time_agg_level_min <- min(position_timestep1, position_timestep2)
      
      fun <- get(time_agg_func[time_agg_level_max], asNamespace("cmsafops"))
      
      if(position_timestep1 == time_agg_level_min) {   # apply aggregation func to infile1
        argumentList <- list(
          var = var1, 
          infile = infile1, 
          outfile = temp_dir, 
          nc34 = 4, 
          overwrite = TRUE,
          nc = nc1
        )
        do.call(fun, argumentList)
        newinfile1 <- TRUE
      }
      else {    # apply aggregation func to infile2
        argumentList <- list(
          var = var2, 
          infile = infile2, 
          outfile = temp_dir, 
          nc34 = 4, 
          overwrite = TRUE,
          nc = nc2
        )
       
        do.call(fun, argumentList)
        newinfile2 <- TRUE
      }
    }
  }
  
  ### Identification of overlapping time periods and generated files with correct time period ###
  if(newinfile1 == TRUE) {   # first data set is new
    # first file
    file_data_one <- read_file(temp_dir, var1)
    file_data_one$variable$prec <- "float"
    date_time_one <- as.Date(get_time(file_data_one$time_info$units, file_data_one$dimension_data$t))
    
    # second file
    file_data_second <- read_file(infile2, var2, nc = nc2)
    file_data_second$variable$prec <- "float"
    date_time_two <- as.Date(get_time(file_data_second$time_info$units, file_data_second$dimension_data$t))
    
    result <- which(unique(date_time_one) %in% unique(date_time_two))   # get overlapping time period
    start_date <- min(date_time_one[min(result)])   # get start date
    end_date <- max(date_time_one[max(result)])   # get end date
    
    cmsafops::selperiod(var = var1, start = start_date, end = end_date, infile = temp_dir, outfile = temp_infile1, nc34 = nc34, overwrite = TRUE)
    cmsafops::selperiod(var = var2, start = start_date, end = end_date, infile = infile2, outfile = temp_infile2, nc34 = nc34, overwrite = TRUE, nc = nc2)
  }
  if(newinfile2 == TRUE) {   # second data set is new
    # first file
    file_data_one <- read_file(infile1, var1)
    file_data_one$variable$prec <- "float"
    #nc_1 <- nc_open(infile1)
    date_time_one <- as.Date(get_time(file_data_one$time_info$units, file_data_one$dimension_data$t))
    
    # second file
    file_data_second <- read_file(temp_dir, var2)
    file_data_second$variable$prec <- "float"
    #nc_2 <- nc_open(temp_dir)
    date_time_two <- as.Date(get_time(file_data_second$time_info$units, file_data_second$dimension_data$t))
    
    
    result <- which(unique(date_time_one) %in% unique(date_time_two))   # get overlapping time period
    start_date <- min(date_time_one[min(result)])   # get start date
    end_date <- max(date_time_one[max(result)])   # get end date
    
    cmsafops::selperiod(var = var1, start = start_date, end = end_date, infile = infile1, outfile = temp_infile1, nc34 = nc34, overwrite = TRUE, nc = nc1)
    cmsafops::selperiod(var = var2, start = start_date, end = end_date, infile = temp_dir, outfile = temp_infile2, nc34 = nc34, overwrite = TRUE)
  }
  if(newinfile1 != TRUE && newinfile2 != TRUE) {
    # first file
    file_data_one <- read_file(infile1, var1, nc = nc1)
    file_data_one$variable$prec <- "float"
    date_time_one <- as.Date(get_time(file_data_one$time_info$units, file_data_one$dimension_data$t))
    
    # second file
    file_data_second <- read_file(infile2, var2, nc = nc2)
    file_data_second$variable$prec <- "float"
    date_time_two <- as.Date(get_time(file_data_second$time_info$units, file_data_second$dimension_data$t))
    
    
    result <- which(unique(date_time_one) %in% unique(date_time_two))   # get overlapping time period
    start_date <- min(date_time_one[min(result)])   # get start date
    end_date <- max(date_time_one[max(result)])   # get end date
    
    cmsafops::selperiod(var = var1, start = start_date, end = end_date, infile = infile1, outfile = temp_infile1, nc34 = nc34, overwrite = TRUE, nc = nc1)
    cmsafops::selperiod(var = var2, start = start_date, end = end_date, infile = infile2, outfile = temp_infile2, nc34 = nc34, overwrite = TRUE, nc = nc2)
  }
  if(file.exists(temp_dir)){
    unlink(temp_dir)
  }
  
  ### Spatial adjustment ###
  file_data_one <- read_file(temp_infile1, var1)
  file_data_second <- read_file(temp_infile2, var2)

  # get spatial steps
  file1_x_steps <- abs(file_data_one$dimension_data$x[2] - file_data_one$dimension_data$x[1])
  file2_x_steps <- abs(file_data_second$dimension_data$x[2] - file_data_second$dimension_data$x[1])
 
  infile1_after_remap <- ""
  infile2_after_remap <- ""

  if(file1_x_steps > file2_x_steps) {
    remap(var2, temp_infile2, temp_infile1, temp_remap_infile2, method = "nearest", nc34 = nc34, overwrite = TRUE)
    infile1_after_remap <- temp_infile1
    infile2_after_remap <- temp_remap_infile2
  } else if(file1_x_steps < file2_x_steps) {
    remap(var1, temp_infile1, temp_infile2, temp_remap_infile1, method = "nearest", nc34 = nc34, overwrite = TRUE)
    infile1_after_remap <- temp_remap_infile1
    infile2_after_remap <- temp_infile2
  } else {
    infile1_after_remap <- temp_infile1
    infile2_after_remap <- temp_infile2
  }
  
  # adjust lon / lat grid
  file_data_one <- read_file(infile1_after_remap, var1)
  file_data_one$dimension_data$x
  file_data_one$dimension_data$y
  
  file_data_second <- read_file(infile2_after_remap, var2)
  file_data_second$dimension_data$x
  file_data_second$dimension_data$y
  
  result <- which(unique(file_data_one$dimension_data$x) %in% unique(file_data_second$dimension_data$x))   # get overlapping x-dimension
  start_x <- min(file_data_one$dimension_data$x[min(result)])   # get start x-dimension
  end_x <- max(file_data_one$dimension_data$x[max(result)])   # get end x-dimension
  
  result <- which(unique(file_data_one$dimension_data$y) %in% unique(file_data_second$dimension_data$y))   # get overlapping y-dimension
  start_y <- min(file_data_one$dimension_data$y[min(result)])   # get start y-dimension
  end_y <- max(file_data_one$dimension_data$y[max(result)])   # get end y-dimension
  
  cmsafops::sellonlatbox(var = var1, 
                         infile = infile1_after_remap, 
                         outfile = outfile1, 
                         lon1 = start_x, lon2 = end_x, 
                         lat1 = start_y, lat2 = end_y, 
                         nc34 = nc34, overwrite = TRUE)   # select the correct lon / lat grid in file one
  cmsafops::sellonlatbox(var = var2, 
                         infile = infile2_after_remap, 
                         outfile = outfile2, 
                         lon1 = start_x, lon2 = end_x, 
                         lat1 = start_y, lat2 = end_y, 
                         nc34 = nc34, overwrite = TRUE)   # select the correct lon / lat grid in file one
  
  if(file.exists(temp_infile1)){
    unlink(temp_infile1)
  }
  if(file.exists(temp_infile2)){
    unlink(temp_infile2)
  }
}
