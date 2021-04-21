#' Designed for the CM SAF R Toolbox.
#'
#' This function is a helper function called by the CM SAF R Toolbox.
#' 
#' @param result.fileslist A data frame containing all meta data (data.frame).
#' @param ordpath NetCDF file path
#'@export
calc_allDatesNc <- function(result.fileslist, ordpath){
  institution <- NULL
  timestep <- NULL
  allDates <- NULL
  infile <- paste0(ordpath, "/", result.fileslist[1])
  infile.last.file <- paste0(ordpath, "/", result.fileslist[1])
  
  ##### extract data from file #####
  id <- nc_open(infile)
  global_att <- ncatt_get(id, 0)
  institution <- global_att$institution
  
  if(!is.null(institution)){
    if(institution == "EUMETSAT/CMSAF"){
      # Timestep extraction from filename 
      allDates <- as.character(substr(result.fileslist, 6, 13))
    }
    else{
      # read timestep from file
      for(i in 1:length(result.fileslist)){
        allDates <- append(allDates,gsub("-","",getTimeRange(paste0(ordpath, "/", result.fileslist[i]))))
      }
    }
  }
  else{
    # read timestep from file
    for(i in 1:length(result.fileslist)){
      allDates <- append(allDates,gsub("-","",getTimeRange(paste0(ordpath, "/", result.fileslist[i]))))
    }
  }
  return(allDates)
}