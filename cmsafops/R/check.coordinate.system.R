#' Designed for the CM SAF R Toolbox.
#'
#' This function is a helper function called by the CM SAF R Toolbox.
#' 
#' @param nc_path Path to NetCDF files which should be converted
#' @param nc_temp_path Destination NetCDF file path
#' @param var Name of NetCDF variable (character)
#' @param filelist NetCDF file names (data.frame)
#'@export
check.coordinate.system <- function(nc_path, nc_temp_path, var, filelist){
  
  # if coordinate system have to convert
  dir_nc <- dirname(nc_path)
  infile <- nc_path # location of first file in nc_temp folder
  
  # check coordinate system
  # read .nc-file 
  file_data <- read_file(infile, var)
  nc_in <- nc_open(infile)
  
  # read data from infile
  dum_dat <- ncvar_get(
    nc_in,
    file_data$variable$name,
    collapse_degen = FALSE
  )
  
  nc_close(nc_in)
  
  if(max(file_data$dimension_data$x) > 180) # for coordinate system 0...360
  {
    for(i in 1:length(filelist)){
        cmsaf.transform.coordinate.system(file.path(dir_nc, filelist[i]), var, file.path(nc_temp_path, filelist[i]))
    }
  }
 else{
   for(i in seq_along(filelist)){
     file.copy(file.path(dir_nc, filelist[i]), nc_temp_path)
   }
 }
}