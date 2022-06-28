#'Get information about the content of a NetCDF file.
#'
#'Shows the content of a NetCDF file in three different detail levels.
#'
#'@param infile Filename of input NetCDF file. This may include the directory
#'  (character).
#'@param info The output can be: long ('l'), medium ('m') and short ('s')
#'  (character). Default is short ('s'). The option 'l' additionally returns a
#'  list object with file information.
#'@param verbose logical; if TRUE, progress messages are shown
#'@param nc Alternatively to \code{infile} you can specify the input as an
#'  object of class `ncdf4` (as returned from \code{ncdf4::nc_open}).
#'
#'@return prints the content of the infile NetCDF.
#'@export
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
#'time <- seq(as.Date("2000-01-01"), as.Date("2010-12-31"), "month")
#'origin <- as.Date("1983-01-01 00:00:00")
#'time <- as.numeric(difftime(time, origin, units = "hour"))
#'data <- array(250:350, dim = c(21, 21, 132))
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
#'## Get information on a medium detail level of the example CM SAF NetCDF
#'## file:
#'ncinfo(infile = file.path(tempdir(),"CMSAF_example_file.nc"), info = "m")
#'
#'unlink(file.path(tempdir(),"CMSAF_example_file.nc"))
ncinfo <- function(infile, info = "s", verbose = FALSE, nc = NULL) {
  # define standard names of variables and dimensions

  t_name <- TIME_NAMES$DEFAULT
  t_units <- UNDEFINED

  # get file information

  if (!is.null(nc)) { 
    nc_in <- nc
  } else {
    nc_in <- nc_open(infile)
  }

  dimnames <- names(nc_in$dim)
  varnames <- names(nc_in$var)

  if (info == "l") {
    cat(utils::str(nc_in), "\n")
    return(invisible(nc_in))
  }

  if (info == "m") {
    print(nc_in)
  }

  if (info == "s") {
    cat("The file:", nc_in$filename, "contains:", "\n")
    if (length(varnames) == 1) {
      cat("\n", "Variable:", sep = "", "\n")
      cat(varnames[1], "\n")
    } else {
      cat("\n", "Variables:", sep = "", "\n")
      for (i in seq_along(varnames)) {
        cat(varnames[i], "\n")
      }
    }

    # check standard_names of dimensions
    for (i in seq_along(dimnames)) {
      invisible(utils::capture.output(sn <- ncatt_get(nc_in, dimnames[i], ATTR_NAMES$STANDARD_NAME)))
      # sn <- ncatt_get(nc_in, dimnames[i], ATTR_NAMES$STANDARD_NAME)
      if (length(sn) > 0) {
        sn <- sn$value
        if (sn == TIME_NAMES$DEFAULT) t_name <- dimnames[i]
      }
    }

    cat("\n", "With following dimensions:", sep = "", "\n")
    for (i in seq_along(dimnames)) {
      if (dimnames[i] == t_name) {
        for (j in seq_along(dimnames)) {
          if (t_name %in% dimnames) {
            attnames <- names(nc_in$dim[[i]])
            if (ATTR_NAMES$UNITS %in% attnames) {
              t_units <- ncatt_get(nc_in, t_name, ATTR_NAMES$UNITS)$value
            }
          }
        }
        time1 <- ncvar_get(nc_in, TIME_NAMES$DEFAULT)
        date.time <- as.Date(get_time(t_units, time1))
        cat("time with length ", length(time1), " (range ", as.character(min(date.time)), " to ", as.character(max(date.time)), ")", sep = "", "\n")
      } else {
        cat(dimnames[i], " with length ", nc_in$dim[[i]]$len, " (range ", min(nc_in$dim[[i]]$vals), " to ", max(nc_in$dim[[i]]$vals), ")", sep = "", "\n")
      }
    }
  }

  if (is.null(nc)) nc_close(nc_in)
}
