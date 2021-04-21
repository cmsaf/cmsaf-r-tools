#'Determine daily means
#'
#'The function determines daily means from data of a single CM SAF NetCDF input
#'file. There is a difference between the operators daymean and dayavg.
#'The mean is regarded as a statistical function, whereas the average is found 
#'simply by adding the sample members and dividing the result by the sample size. 
#'For example, the mean of 1, 2, miss and 3 is (1 + 2 + 3)/3 = 2, 
#'whereas the average is (1 + 2 + miss + 3)/4 = miss/4 = miss. 
#'If there are no missing values in the sample, the average and mean are identical.
#'
#'@param var Name of NetCDF variable (character).
#'@param infile Filename of input NetCDF file. This may include the directory
#'  (character).
#'@param outfile Filename of output NetCDF file. This may include the directory
#'  (character).
#'@param nc34 NetCDF version of output file. If \code{nc34 = 3} the output file will be
#'  in NetCDFv3 format (numeric). Default output is NetCDFv4.
#'@param overwrite logical; should existing output file be overwritten?
#'@param verbose logical; if TRUE, progress messages are shown
#'
#'@return A NetCDF file including a time series of daily means is written.
#'@export
#'
#'@family daily statistics
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
#'time <- seq(ISOdate(2000, 1, 1), ISOdate(2000, 1, 6), "hours")
#'origin <- as.Date("1983-01-01 00:00:00")
#'time <- as.numeric(difftime(time, origin, units = "hour"))
#'data <- array(250:350, dim = c(21, 21, 121))
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
#'## Determine the daily means of the example CM SAF NetCDF file and
#'## write the output to a new file.
#'daymean(var = "SIS", infile = file.path(tempdir(),"CMSAF_example_file.nc"), 
#'  outfile = file.path(tempdir(),"CMSAF_example_file_daymean.nc"))
#'
#'unlink(c(file.path(tempdir(),"CMSAF_example_file.nc"), 
#'  file.path(tempdir(),"CMSAF_example_file_daymean.nc")))
daymean <- function(var, infile, outfile, nc34 = 4, overwrite = FALSE, verbose = FALSE) {
  dayx_wrapper(3, var, infile, outfile, nc34, overwrite, verbose = verbose)
}