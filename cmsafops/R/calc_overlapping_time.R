#'Routine to calculate overlapping time periods in two files. 
#'
#'Designed for CMSAF Toolbox.
#'
#'@param var1 Name of NetCDF variable of the first data set (character).
#'@param infile1 Filename of first input NetCDF file. This may include the directory
#'  (character).
#'@param var2 Name of NetCDF variable of the second data set (character).
#'@param infile2 Filename of second input NetCDF file. This may include the directory
#'  (character). Also supported formats for station data are .csv and .RData files.
#'  
#'@return Start date and end date are the result (list).
#'
#'@export
calc_overlapping_time <- function(var1, infile1, var2 = NULL, infile2) {
  gc()
  
  calc_time_start <- Sys.time()
  
  if(endsWith(infile1, ".nc") && endsWith(infile2, ".nc")) {
    check_variable(var1)
    check_variable(var2)
    
    check_infile(infile1)
    check_infile(infile2)
    
    ##### extract data from two files #####
    timestep_string <- c("m","h", "d", "m", "y")
    time_agg_func <- c("hourmean", "hourmean", "daymean", "monmean", "yearmean")
    
    # infile1
    id <- nc_open(infile1)
    dim_names   <- names(id$dim)
    dimensions <- get_dimensions(id, dim_names)
    time_info <- get_time_info(id, dim_names, dimensions$names$t)
    timestep_infile1 <- substr(time_info$units, 1, 1)
    nc_close(id)
    
    # infile2
    id <- nc_open(infile2)
    dim_names   <- names(id$dim)
    dimensions <- get_dimensions(id, dim_names)
    time_info <- get_time_info(id, dim_names, dimensions$names$t)
    timestep_infile2 <- substr(time_info$units, 1, 1)
    nc_close(id)
    
    temp_dir <- file.path(tempdir(), "cmsaf_overlap_two_files_tmp.nc")
    
    if(file.exists(temp_dir)){
      unlink(temp_dir)
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
            overwrite = TRUE
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
            overwrite = TRUE
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
      file_data_second <- read_file(infile2, var2)
      file_data_second$variable$prec <- "float"
      date_time_two <- as.Date(get_time(file_data_second$time_info$units, file_data_second$dimension_data$t))
      
      result <- which(unique(date_time_one) %in% unique(date_time_two))   # get overlapping time period
      start_date <- min(date_time_one[min(result)])   # get start date
      end_date <- max(date_time_one[max(result)])   # get end date
    }
    if(newinfile2 == TRUE) {   # second data set is new
      # first file
      file_data_one <- read_file(infile1, var1)
      file_data_one$variable$prec <- "float"
      date_time_one <- as.Date(get_time(file_data_one$time_info$units, file_data_one$dimension_data$t))
      
      # second file
      file_data_second <- read_file(temp_dir, var2)
      file_data_second$variable$prec <- "float"
      date_time_two <- as.Date(get_time(file_data_second$time_info$units, file_data_second$dimension_data$t))
      
      
      result <- which(unique(date_time_one) %in% unique(date_time_two))   # get overlapping time period
      start_date <- min(date_time_one[min(result)])   # get start date
      end_date <- max(date_time_one[max(result)])   # get end date
    }
    if(newinfile1 != TRUE && newinfile2 != TRUE) {
      # first file
      file_data_one <- read_file(infile1, var1)
      file_data_one$variable$prec <- "float"
      date_time_one <- as.Date(get_time(file_data_one$time_info$units, file_data_one$dimension_data$t))
      
      # second file
      file_data_second <- read_file(infile2, var2)
      file_data_second$variable$prec <- "float"
      date_time_two <- as.Date(get_time(file_data_second$time_info$units, file_data_second$dimension_data$t))
      
      
      result <- which(unique(date_time_one) %in% unique(date_time_two))   # get overlapping time period
      start_date <- min(date_time_one[min(result)])   # get start date
      end_date <- max(date_time_one[max(result)])   # get end date
    }
    if(file.exists(temp_dir)){
      unlink(temp_dir)
    }
  } else if(endsWith(infile1, ".nc") && (endsWith(infile2, ".csv") || endsWith(infile2, ".RData"))){
    check_variable(var1)
    check_infile(infile1)
    
    file_data_one <- read_file(infile1, var1)
    file_data_one$variable$prec <- "float"
    date_time_one <- as.Date(get_time(file_data_one$time_info$units, file_data_one$dimension_data$t))
    start_date <- min(date_time_one)
    end_date <- max(date_time_one)
  } else {
    stop("The input files are in an unsupported format. Supported formats are infile1: .nc files and infile2: .nc or .csv or .RData files.")
  }
  return(list(start_date, end_date))
}