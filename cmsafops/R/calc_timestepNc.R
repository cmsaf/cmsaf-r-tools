#' Designed for the CM SAF R Toolbox.
#'
#' This function is a helper function called by the CM SAF R Toolbox.
#' 
#' @param result.fileslist A data frame containing all meta data (data.frame).
#' @param ordpath NetCDF file path
#'@export
calc_timestepNc <- function(result.fileslist, ordpath){
  institution <- NULL
  timestep <- NULL
  infile <- paste0(ordpath, "/", result.fileslist[1])
  id <- nc_open(infile)
  global_att <- ncatt_get(id, 0)
  institution <- global_att$institution
  
  dim_names   <- names(id$dim)
  dimensions <- get_dimensions(id, dim_names)
  time_info <- get_time_info(id, dim_names, dimensions$names$t)
  
  if(!is.null(institution)){
    if(institution == "EUMETSAT/CMSAF"){
      # Timestep extraction from filename 
      timestep <- substr(result.fileslist[1], 4, 4)
    }
    else{
      #timestep <- substr(time_info$units, 1, 1)
      counter.month <- 0
      temp.time <- 0
      temp.time.first <- 0
      temp.time.second <- 0
      
      for(i in 1:2){
        infile <- paste0(ordpath, "/", result.fileslist[i])
        nc_in <- nc_open(infile)
        dim_names   <- names(nc_in$dim)
        dimensions <- get_dimensions(nc_in, dim_names)
        time_info <- get_time_info(nc_in, dim_names, dimensions$names$t)
        
        dimension.data.t <- nc_in$dim[[dimensions$names$t]]$vals
        
        
        date_time_one <- as.Date(get_time(time_info$units, dimension.data.t))
        
        if(counter.month != 0){
          temp.time <- as.integer(substr(date_time_one, 6, 7))
          if(counter.month < temp.time)
          {
            timestep <- "m"
          }
          else{
            timestep <- "d"
          }
        }
        
        if(length(date_time_one) > 1){
          temp.time.first <- substr(date_time_one[1], 6, 7)
          temp.time.second <- substr(date_time_one[2], 6, 7)
          if(temp.time.first < temp.time.second)
          {
            timestep <- "m"
          }
          else{
            timestep <- "d"
          }
          break
        }
        else{
          counter.month <- as.integer(substr(date_time_one, 6, 7))
        }
      
        nc_close(nc_in)
      }
    }
  }
  else{
    #timestep <- substr(time_info$units, 1, 1)
    counter.month <- 0
    temp.time <- 0
    temp.time.first <- 0
    temp.time.second <- 0
    
    for(i in 1:2){
      infile <- paste0(ordpath, "/", result.fileslist[i])
      nc_in <- nc_open(infile)
      dim_names   <- names(nc_in$dim)
      dimensions <- get_dimensions(nc_in, dim_names)
      time_info <- get_time_info(nc_in, dim_names, dimensions$names$t)
      
      dimension.data.t <- nc_in$dim[[dimensions$names$t]]$vals
      
      date_time_one <- as.Date(get_time(time_info$units, dimension.data.t))
      
      if(counter.month != 0){
        temp.time <- as.integer(substr(date_time_one, 6, 7))
        if(counter.month < temp.time)
        {
          timestep <- "m"
        }
        else{
          timestep <- "d"
        }
      }
      
      if(length(date_time_one) > 1){
        temp.time.first <- substr(date_time_one[1], 6, 7)
        temp.time.second <- substr(date_time_one[2], 6, 7)
        if(temp.time.first < temp.time.second)
        {
          timestep <- "m"
        }
        else{
          timestep <- "d"
        }
        break
      }
      else{
        counter.month <- as.integer(substr(date_time_one, 6, 7))
      }
      
      nc_close(nc_in)
    }
    
  }
  nc_close(id)
  return(timestep)
}