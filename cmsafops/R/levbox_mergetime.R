#'Function to combine NetCDF files and simultaneously cut a region and level.
#'
#'This function selects a region and a level from a bunch of CM SAF NetCDF files
#'that match the same pattern of the filename, and writes the output to a new
#'file. If no longitude and latitude values are given, files are only merged.
#'All input files have to have the same rectangular grid and the same variable.
#'The reference time of the output file is determined by the first input file.
#'
#'@param var Name of NetCDF variable (character).
#'@param path The directory of input NetCDF files without / at the end
#'  (character).
#'@param pattern A part of the filename, which is the same for all desired input
#'  files (character). The pattern has to be a character string containing a
#'  regular expression.
#'@param outfile Filename of output NetCDF file. This may include the directory
#'  (character).
#'@param level Number of level that should be extracted (integer).
#'@param lon1 Longitude of lower left corner (numeric).
#'@param lon2 Longitude of upper right left corner (numeric).
#'@param lat1 Latitude of lower left corner (numeric).
#'@param lat2 Latitude of upper right corner (numeric).  Longitude of upper
#'  right corner (numeric).
#'@param nc34 NetCDF version of output file. If \code{nc34 = 3} the output file will be
#'  in NetCDFv3 format (numeric). Default output is NetCDFv4.
#'@param overwrite logical; should existing output file be overwritten?
#'@param verbose logical; if TRUE, progress messages are shown
#'
#'@return A NetCDF file including the merged time series of the selected region
#'  is written. The output NetCDF file contains only the selected level.
#'@export
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
#'time <- c(as.Date("2000-01-01"), as.Date("2001-02-01"))
#'origin <- as.Date("1983-01-01 00:00:00")
#'time <- as.numeric(difftime(time, origin, units = "hour"))
#'level <- c(1:5)
#'data1 <- array(250:350, dim = c(21, 21, 5, 1))
#'data2 <- array(230:320, dim = c(21, 21, 5, 1))
#'
#'## create two example NetCDF files
#'
#'x <- ncdim_def(name = "lon", units = "degrees_east", vals = lon)
#'y <- ncdim_def(name = "lat", units = "degrees_north", vals = lat)
#'l <- ncdim_def(name = "level", units = "1", vals = level)
#'t <- ncdim_def(name = "time", units = "hours since 1983-01-01 00:00:00",
#'  vals = time[1], unlim = TRUE)
#'var1 <- ncvar_def("SIS", "W m-2", list(x, y, l, t), -1, prec = "short")
#'vars <- list(var1)
#'ncnew <- nc_create(file.path(tempdir(),"CMSAF_example_file_n1.nc"), vars)
#'ncvar_put(ncnew, var1, data1)
#'ncatt_put(ncnew, "lon", "standard_name", "longitude", prec = "text")
#'ncatt_put(ncnew, "lat", "standard_name", "latitude", prec = "text")
#'ncatt_put(ncnew, "level", "standard_name", "level", prec = "text")
#'nc_close(ncnew)
#'
#'t <- ncdim_def(name = "time", units = "hours since 1983-01-01 00:00:00",
#'  vals = time[2], unlim = TRUE)
#'ncnew <- nc_create(file.path(tempdir(),"CMSAF_example_file_n2.nc"), vars)
#'ncvar_put(ncnew, var1, data2)
#'ncatt_put(ncnew, "lon", "standard_name", "longitude", prec = "text")
#'ncatt_put(ncnew, "lat", "standard_name", "latitude", prec = "text")
#'ncatt_put(ncnew, "level", "standard_name", "level", prec = "text")
#'nc_close(ncnew)
#'
#'## Cut a region and levl, and merge both example CM SAF NetCDF files
#'## into one output file. First get path information of working
#'## directory.
#'levbox_mergetime(var = "SIS", level = 1, path = tempdir(), 
#'  pattern = "CMSAF_example_file_n", outfile = file.path(tempdir(),
#'  "CMSAF_example_file_levbox_mergetime.nc"), lon1 = 8, lon2 = 12, 
#'  lat1 = 48, lat2 =52)
#'
#'unlink(c(file.path(tempdir(),"CMSAF_example_file_n1.nc"), 
#'  file.path(tempdir(),"CMSAF_example_file_n2.nc"),
#'  file.path(tempdir(),"CMSAF_example_file_levbox_mergetime.nc")))
levbox_mergetime <- function(var, level = 1, path, pattern, outfile,
                             lon1 = -180, lon2 = 180, lat1 = -90, lat2 = 90,
                             nc34 = 4, overwrite = FALSE, verbose = FALSE) {
  box_mergetime(var, path, pattern, outfile, lon1, lon2, lat1, lat2, level,
                nc34, overwrite, verbose)
}
