# Script which creates test data for running unit tests on the cmsaf package.
library("ncdf4")
testdata_dir <- file.path("tests", "testdata")

# Create some (non-realistic) example data. Inspired by "?cmsaf.add".
create_examples_normal <- function() {
  filename1 <- file.path(testdata_dir, "ex_normal1.nc")
  filename2 <- file.path(testdata_dir, "ex_normal2.nc")
  lon <- seq(5, 8, 0.5)
  lat <- seq(45, 48, 0.5)
  time <- c(as.Date("2000-01-01"), as.Date("2001-02-01"))
  origin <- as.Date("1983-01-01 00:00:00")
  time <- as.numeric(difftime(time, origin, units = "hour"))
  data1 <- array(250:272, dim = c(7, 7, 1))
  data2 <- array(230:252, dim = c(7, 7, 1))

  x <- ncdim_def(name = "lon", units = "degrees_east", vals = lon)
  y <- ncdim_def(name = "lat", units = "degrees_north", vals = lat)
  t <- ncdim_def(name = "time", units = "hours since 1983-01-01 00:00:00",
                 vals = time[1], unlim = TRUE)
  var1 <- ncvar_def("SIS", "W m-2", list(x, y, t), -999, prec = "short",
                    longname = "Surface Incoming Shortwave Radiation")
  vars <- list(var1)
  ncnew <- nc_create(filename1, vars)
  ncvar_put(ncnew, var1, data1)
  ncatt_put(ncnew, "lon", "standard_name", "longitude", prec = "text")
  ncatt_put(ncnew, "lat", "standard_name", "latitude", prec = "text")
  ncatt_put(ncnew, "SIS", "standard_name", "SIS_standard", prec = "text")
  nc_close(ncnew)

  x <- ncdim_def(name = "lon", units = "degrees_east", vals = lon)
  y <- ncdim_def(name = "lat", units = "degrees_north", vals = lat)
  t <- ncdim_def(name = "time", units = "hours since 1983-01-01 00:00:00",
                 vals = time[2], unlim = TRUE)
  var1 <- ncvar_def("SIS", "W m-2", list(x, y, t), -999, prec = "short",
                    longname = "Surface Incoming Shortwave Radiation")
  vars <- list(var1)
  ncnew <- nc_create(filename2, vars)
  ncvar_put(ncnew, var1, data2)
  ncatt_put(ncnew, "lon", "standard_name", "longitude", prec = "text")
  ncatt_put(ncnew, "lat", "standard_name", "latitude", prec = "text")
  ncatt_put(ncnew, "SIS", "standard_name", "SIS_standard", prec = "text")
  nc_close(ncnew)
}

# Create some (non-realistic) example data where len(time) > 1.
create_examples_time <- function() {
  filename1 <- file.path(testdata_dir, "ex_time_dim1.nc")
  filename2 <- file.path(testdata_dir, "ex_time_dim2.nc")
  lon <- seq(5, 8, 0.5)
  lat <- seq(45, 48, 0.5)
  time <- c(as.Date("2000-01-01"), as.Date("2001-02-01"), as.Date("2002-03-01"),
            as.Date("2003-04-01"))
  origin <- as.Date("1983-01-01 00:00:00")
  time <- as.numeric(difftime(time, origin, units = "hour"))
  data1 <- array(250:272, dim = c(7, 7, 2))
  data2 <- array(230:252, dim = c(7, 7, 2))

  x <- ncdim_def(name = "lon", units = "degrees_east", vals = lon)
  y <- ncdim_def(name = "lat", units = "degrees_north", vals = lat)
  t <- ncdim_def(name = "time", units = "hours since 1983-01-01 00:00:00",
                 vals = time[1:2], unlim = TRUE)
  var1 <- ncvar_def("SIS", "W m-2", list(x, y, t), -999, prec = "short",
                    longname = "Surface Incoming Shortwave Radiation")
  vars <- list(var1)
  ncnew <- nc_create(filename1, vars)
  ncvar_put(ncnew, var1, data1)
  ncatt_put(ncnew, "lon", "standard_name", "longitude", prec = "text")
  ncatt_put(ncnew, "lat", "standard_name", "latitude", prec = "text")
  ncatt_put(ncnew, "SIS", "standard_name", "SIS_standard", prec = "text")
  nc_close(ncnew)

  x <- ncdim_def(name = "lon", units = "degrees_east", vals = lon)
  y <- ncdim_def(name = "lat", units = "degrees_north", vals = lat)
  t <- ncdim_def(name = "time", units = "hours since 1983-01-01 00:00:00",
                 vals = time[3:4], unlim = TRUE)
  var1 <- ncvar_def("SIS", "W m-2", list(x, y, t), -999, prec = "short",
                    longname = "Surface Incoming Shortwave Radiation")
  vars <- list(var1)
  ncnew <- nc_create(filename2, vars)
  ncvar_put(ncnew, var1, data2)
  ncatt_put(ncnew, "lon", "standard_name", "longitude", prec = "text")
  ncatt_put(ncnew, "lat", "standard_name", "latitude", prec = "text")
  ncatt_put(ncnew, "SIS", "standard_name", "SIS_standard", prec = "text")
  nc_close(ncnew)
}

# Create some (non-realistic) example data where len(time) = 4.
create_example_time2 <- function() {
  filename <- file.path(testdata_dir, "ex_time_dim3.nc")
  lon <- seq(5, 8, 0.5)
  lat <- seq(45, 48, 0.5)
  time <- c(as.Date("2005-01-01"), as.Date("2006-02-01"), as.Date("2007-03-01"),
            as.Date("2008-04-01"))
  origin <- as.Date("1983-01-01 00:00:00")
  time <- as.numeric(difftime(time, origin, units = "hour"))
  data <- array(250:272, dim = c(7, 7, 4))

  x <- ncdim_def(name = "lon", units = "degrees_east", vals = lon)
  y <- ncdim_def(name = "lat", units = "degrees_north", vals = lat)
  t <- ncdim_def(name = "time", units = "hours since 1983-01-01 00:00:00",
                 vals = time, unlim = TRUE)
  var1 <- ncvar_def("SIS", "W m-2", list(x, y, t), -999, prec = "short",
                    longname = "Surface Incoming Shortwave Radiation")
  vars <- list(var1)
  ncnew <- nc_create(filename, vars)
  ncvar_put(ncnew, var1, data)
  ncatt_put(ncnew, "lon", "standard_name", "longitude", prec = "text")
  ncatt_put(ncnew, "lat", "standard_name", "latitude", prec = "text")
  ncatt_put(ncnew, "SIS", "standard_name", "SIS_standard", prec = "text")
  nc_close(ncnew)
}

# Create some (non-realistic) example data where len(lon) is different.
create_different_lon_length <- function() {
  filename <- file.path(testdata_dir, "ex_different_lon_length.nc")
  lon <- seq(5, 7, 0.5)
  lat <- seq(45, 48, 0.5)
  time <- c(as.Date("2000-01-01"), as.Date("2001-02-01"), as.Date("2002-03-01"),
            as.Date("2003-04-01"))
  origin <- as.Date("1983-01-01 00:00:00")
  time <- as.numeric(difftime(time, origin, units = "hour"))
  data <- array(250:272, dim = c(7, 5, 4))

  x <- ncdim_def(name = "lon", units = "degrees_east", vals = lon)
  y <- ncdim_def(name = "lat", units = "degrees_north", vals = lat)
  t <- ncdim_def(name = "time", units = "hours since 1983-01-01 00:00:00",
                 vals = time, unlim = TRUE)
  var1 <- ncvar_def("SIS", "W m-2", list(x, y, t), -999, prec = "short",
                    longname = "Surface Incoming Shortwave Radiation")
  vars <- list(var1)
  ncnew <- nc_create(filename, vars)
  ncvar_put(ncnew, var1, data)
  ncatt_put(ncnew, "lon", "standard_name", "longitude", prec = "text")
  ncatt_put(ncnew, "lat", "standard_name", "latitude", prec = "text")
  ncatt_put(ncnew, "SIS", "standard_name", "SIS_standard", prec = "text")
  nc_close(ncnew)
}

# Create some (non-realistic) example data with additional attr..
create_additional_attr <- function() {
  filename <- file.path(testdata_dir, "ex_additional_attr.nc")
  lon <- seq(5, 8, 0.5)
  lat <- seq(45, 48, 0.5)
  time <- c(as.Date("2000-01-01"))
  origin <- as.Date("1983-01-01 00:00:00")
  time <- as.numeric(difftime(time, origin, units = "hour"))
  data <- array(250:272, dim = c(7, 7, 1))

  x <- ncdim_def(name = "lon", units = "degrees_east", vals = lon)
  y <- ncdim_def(name = "lat", units = "degrees_north", vals = lat)
  t <- ncdim_def(name = "time", units = "hours since 1983-01-01 00:00:00",
                 vals = time, unlim = TRUE)
  var1 <- ncvar_def("SIS", "W m-2", list(x, y, t), -999, prec = "short",
                    longname = "Surface Incoming Shortwave Radiation")
  vars <- list(var1)
  ncnew <- nc_create(filename, vars)
  ncvar_put(ncnew, var1, data)
  ncatt_put(ncnew, "lon", "standard_name", "longitude", prec = "text")
  ncatt_put(ncnew, "lat", "standard_name", "latitude", prec = "text")
  ncatt_put(ncnew, "SIS", "standard_name", "SIS_standard", prec = "text")
  ncatt_put(ncnew, 0, "institution", "This is a test attribute.", prec = "text")
  nc_close(ncnew)
}

# Create some (non-realistic) example data in ncdf version 4.
create_examples_v4 <- function() {
  filename1 <- file.path(testdata_dir, "ex_v4_1.nc")
  filename2 <- file.path(testdata_dir, "ex_v4_2.nc")
  lon <- seq(5, 8, 0.5)
  lat <- seq(45, 48, 0.5)
  time <- c(as.Date("2000-01-01"), as.Date("2001-02-01"))
  origin <- as.Date("1983-01-01 00:00:00")
  time <- as.numeric(difftime(time, origin, units = "hour"))
  data1 <- array(250:272, dim = c(7, 7, 1))
  data2 <- array(230:252, dim = c(7, 7, 1))

  x <- ncdim_def(name = "lon", units = "degrees_east", vals = lon)
  y <- ncdim_def(name = "lat", units = "degrees_north", vals = lat)
  t <- ncdim_def(name = "time", units = "hours since 1983-01-01 00:00:00",
                 vals = time[1], unlim = TRUE)
  var1 <- ncvar_def("SIS", "W m-2", list(x, y, t), -999, prec = "short",
                    longname = "Surface Incoming Shortwave Radiation")
  vars <- list(var1)
  ncnew <- nc_create(filename1, vars, force_v4 = TRUE)
  ncvar_put(ncnew, var1, data1)
  ncatt_put(ncnew, "lon", "standard_name", "longitude", prec = "text")
  ncatt_put(ncnew, "lat", "standard_name", "latitude", prec = "text")
  ncatt_put(ncnew, "SIS", "standard_name", "SIS_standard", prec = "text")
  nc_close(ncnew)

  x <- ncdim_def(name = "lon", units = "degrees_east", vals = lon)
  y <- ncdim_def(name = "lat", units = "degrees_north", vals = lat)
  t <- ncdim_def(name = "time", units = "hours since 1983-01-01 00:00:00",
                 vals = time[2], unlim = TRUE)
  var1 <- ncvar_def("SIS", "W m-2", list(x, y, t), -999, prec = "short",
                    longname = "Surface Incoming Shortwave Radiation")
  vars <- list(var1)
  ncnew <- nc_create(filename2, vars, force_v4 = TRUE)
  ncvar_put(ncnew, var1, data2)
  ncatt_put(ncnew, "lon", "standard_name", "longitude", prec = "text")
  ncatt_put(ncnew, "lat", "standard_name", "latitude", prec = "text")
  ncatt_put(ncnew, "SIS", "standard_name", "SIS_standard", prec = "text")
  nc_close(ncnew)
}

# Create test data for runmax, runmin, runmean, runrange, runsum
create_run_data <- function() {
  filename <- file.path(testdata_dir, "ex_run.nc")
  filename <- "ex_run.nc"
  lon <- seq(5, 7, 0.5)
  lat <- seq(45, 47, 0.5)
  time <- seq(as.Date("2000-01-01"), as.Date("2000-03-31"), "month")
  origin <- as.Date("1983-01-01 00:00:00")
  time <- as.numeric(difftime(time, origin, units = "hour"))
  data <- array(10:100, dim = c(5, 5, 3))
  
  x <- ncdim_def(name = "lon", units = "degrees_east", vals = lon)
  y <- ncdim_def(name = "lat", units = "degrees_north", vals = lat)
  t <- ncdim_def(name = "time", units = "hours since 1983-01-01 00:00:00",
                 vals = time, unlim = TRUE)
  var1 <- ncvar_def("SIS", "W m-2", list(x, y, t), -999, prec = "short",
                    longname = "Surface Incoming Shortwave Radiation")
  vars <- list(var1)
  ncnew <- nc_create(filename, vars)
  ncvar_put(ncnew, var1, data)
  ncatt_put(ncnew, "lon", "standard_name", "longitude", prec = "text")
  ncatt_put(ncnew, "lat", "standard_name", "latitude", prec = "text")
  ncatt_put(ncnew, "SIS", "standard_name", "SIS_standard", prec = "text")
  nc_close(ncnew)
}

# Create test data for cmsaf.abs
create_cmsaf.abs <- function() {
  filename <- file.path(testdata_dir, "ex_cmsaf.abs.nc")
  lon <- seq(5, 7, 1)
  lat <- seq(45, 49, 1)
  time <- seq(as.Date("2000-01-01"), as.Date("2000-03-31"), "month")
  origin <- as.Date("1983-01-01 00:00:00")
  time <- as.numeric(difftime(time, origin, units = "hour"))
  data <- array(-10:50, dim = c(3, 5, 3))
  
  x <- ncdim_def(name = "lon", units = "degrees_east", vals = lon)
  y <- ncdim_def(name = "lat", units = "degrees_north", vals = lat)
  t <- ncdim_def(name = "time", units = "hours since 1983-01-01 00:00:00",
                 vals = time, unlim = TRUE)
  var1 <- ncvar_def("SIS", "W m-2", list(x, y, t), -999, prec = "short",
                    longname = "Surface Incoming Shortwave Radiation")
  vars <- list(var1)
  ncnew <- nc_create(filename, vars)
  ncvar_put(ncnew, var1, data)
  ncatt_put(ncnew, "lon", "standard_name", "longitude", prec = "text")
  ncatt_put(ncnew, "lat", "standard_name", "latitude", prec = "text")
  ncatt_put(ncnew, "SIS", "standard_name", "SIS_standard", prec = "text")
  nc_close(ncnew)
}

# Create test data for dayrange
create_dayrange <- function() {
  filename <- file.path(testdata_dir, "ex_dayrange.nc")
  lon <- seq(5, 8, 0.5)
  lat <- seq(45, 48, 0.5)
  time <- seq(ISOdate(2000, 1, 1), ISOdate(2000, 1, 3), "hours")
  origin <- as.Date("1983-01-01 00:00:00")
  time <- as.numeric(difftime(time, origin, units = "hour"))
  data <- array(250:272, dim = c(7, 7, 49))

  x <- ncdim_def(name = "lon", units = "degrees_east", vals = lon)
  y <- ncdim_def(name = "lat", units = "degrees_north", vals = lat)
  t <- ncdim_def(name = "time", units = "hours since 1983-01-01 00:00:00",
                 vals = time, unlim = TRUE)
  var1 <- ncvar_def("SIS", "W m-2", list(x, y, t), -999, prec = "short",
                    longname = "Surface Incoming Shortwave Radiation")
  vars <- list(var1)
  ncnew <- nc_create(filename, vars)
  ncvar_put(ncnew, var1, data)
  ncatt_put(ncnew, "lon", "standard_name", "longitude", prec = "text")
  ncatt_put(ncnew, "lat", "standard_name", "latitude", prec = "text")
  ncatt_put(ncnew, "SIS", "standard_name", "SIS_standard", prec = "text")
  nc_close(ncnew)
}

# Create test data for dayx
create_dayx <- function() {
  filename <- file.path(testdata_dir, "ex_dayx.nc")
  lon <- seq(5, 8, 0.5)
  lat <- seq(45, 48, 0.5)
  time <- seq(ISOdate(2000, 1, 1), ISOdate(2000, 1, 3), "hours")
  origin <- as.Date("1983-01-01 00:00:00")
  time <- as.numeric(difftime(time, origin, units = "hour"))
  data <- array(250:272, dim = c(7, 7, 49))
  
  x <- ncdim_def(name = "lon", units = "degrees_east", vals = lon)
  y <- ncdim_def(name = "lat", units = "degrees_north", vals = lat)
  t <- ncdim_def(name = "time", units = "hours since 1983-01-01 00:00:00",
                 vals = time, unlim = TRUE)
  var1 <- ncvar_def("SIS", "W m-2", list(x, y, t), -999, prec = "short",
                    longname = "Surface Incoming Shortwave Radiation")
  vars <- list(var1)
  ncnew <- nc_create(filename, vars)
  ncvar_put(ncnew, var1, data)
  ncatt_put(ncnew, "lon", "standard_name", "longitude", prec = "text")
  ncatt_put(ncnew, "lat", "standard_name", "latitude", prec = "text")
  ncatt_put(ncnew, "SIS", "standard_name", "SIS_standard", prec = "text")
  nc_close(ncnew)
}

# Create test data for hourx
create_hourx <- function() {
  filename <- file.path(testdata_dir, "ex_hourx.nc")
  lon <- seq(5, 8, 0.5)
  lat <- seq(45, 48, 0.5)
  
  time <- seq(ISOdate(2000, 1, 1), ISOdate(2000, 1, 2), "mins")
  origin <- as.Date("1983-01-01 00:00:00")
  time <- as.numeric(difftime(time, origin, units = "min"))
  data <- array(250:350, dim = c(7, 7, 1441))
  
  x <- ncdim_def(name = "lon", units = "degrees_east", vals = lon)
  y <- ncdim_def(name = "lat", units = "degrees_north", vals = lat)
  t <- ncdim_def(name = "time", units = "minutes since 1983-01-01 00:00:00",
                 vals = time, unlim = TRUE)
  var1 <- ncvar_def("SIS", "W m-2", list(x, y, t), -999, prec = "short",
                    longname = "Surface Incoming Shortwave Radiation")
  vars <- list(var1)
  ncnew <- nc_create(filename, vars)
  ncvar_put(ncnew, var1, data)
  ncatt_put(ncnew, "lon", "standard_name", "longitude", prec = "text")
  ncatt_put(ncnew, "lat", "standard_name", "latitude", prec = "text")
  ncatt_put(ncnew, "SIS", "standard_name", "SIS_standard", prec = "text")
  nc_close(ncnew)
}

# Create test data for gridboxx
create_gridboxx <- function() {
  filename <- file.path(testdata_dir, "ex_gridboxx.nc")
  lon <- seq(5, 15, 0.5)
  lat <- seq(45, 55, 0.5)
  time <- seq(as.Date("2000-03-01"), as.Date("2000-05-31"), "month")
  origin <- as.Date("1983-01-01 00:00:00")
  time <- as.numeric(difftime(time, origin, units = "hour"))
  data <- array(250:350, dim = c(21, 21, 3))
  
  x <- ncdim_def(name = "lon", units = "degrees_east", vals = lon)
  y <- ncdim_def(name = "lat", units = "degrees_north", vals = lat)
  t <- ncdim_def(name = "time", units = "hours since 1983-01-01 00:00:00",
                 vals = time, unlim = TRUE)
  var1 <- ncvar_def("SIS", "W m-2", list(x, y, t), -999, prec = "short",
                    longname = "Surface Incoming Shortwave Radiation")
  vars <- list(var1)
  ncnew <- nc_create(filename, vars)
  
  ncvar_put(ncnew, var1, data)
  
  ncatt_put(ncnew, "lon", "standard_name", "longitude", prec = "text")
  ncatt_put(ncnew, "lat", "standard_name", "latitude", prec = "text")
  ncatt_put(ncnew, "SIS", "standard_name", "SIS_standard", prec = "text")
  nc_close(ncnew)
}

# Create test data for mon_day_mean
create_mon_day_mean <- function() {
  filename <- file.path(testdata_dir, "ex_mon_day_mean.nc")
  lon <- seq(5, 8, 0.5)
  lat <- seq(45, 48, 0.5)
  time <- seq(ISOdate(2000, 3, 1), ISOdate(2000, 5, 31), "hours")
  origin <- as.Date("1983-01-01 00:00:00")
  time <- as.numeric(difftime(time, origin, units = "hour"))
  data <- array(250:272, dim = c(7, 7, 2185))
  
  x <- ncdim_def(name = "lon", units = "degrees_east", vals = lon)
  y <- ncdim_def(name = "lat", units = "degrees_north", vals = lat)
  t <- ncdim_def(name = "time", units = "hours since 1983-01-01 00:00:00",
                 vals = time, unlim = TRUE)
  var1 <- ncvar_def("SIS", "W m-2", list(x, y, t), -999, prec = "short",
                    longname = "Surface Incoming Shortwave Radiation")
  vars <- list(var1)
  ncnew <- nc_create(filename, vars)
  
  ncvar_put(ncnew, var1, data)
  
  ncatt_put(ncnew, "lon", "standard_name", "longitude", prec = "text")
  ncatt_put(ncnew, "lat", "standard_name", "latitude", prec = "text")
  ncatt_put(ncnew, "SIS", "standard_name", "SIS_standard", prec = "text")
  nc_close(ncnew)
}

# Create test data for divdpm
create_divdpm <- function() {
  filename <- file.path(testdata_dir, "ex_divdpm.nc")
  lon <- seq(5, 6, 0.5)
  lat <- seq(45, 46, 0.5)
  time <- seq(as.Date("2000-01-01"), as.Date("2000-02-28"), "month")
  origin <- as.Date("1983-01-01 00:00:00")
  time <- as.numeric(difftime(time, origin, units = "hour"))
  data <- array(250:272, dim = c(3, 3, 2))

  x <- ncdim_def(name = "lon", units = "degrees_east", vals = lon)
  y <- ncdim_def(name = "lat", units = "degrees_north", vals = lat)
  t <- ncdim_def(name = "time", units = "hours since 1983-01-01 00:00:00",
                 vals = time, unlim = TRUE)
  var1 <- ncvar_def("SIS", "W m-2", list(x, y, t), -999, prec = "short",
                    longname = "Surface Incoming Shortwave Radiation")
  vars <- list(var1)
  ncnew <- nc_create(filename, vars)
  ncvar_put(ncnew, var1, data)
  ncatt_put(ncnew, "lon", "standard_name", "longitude", prec = "text")
  ncatt_put(ncnew, "lat", "standard_name", "latitude", prec = "text")
  ncatt_put(ncnew, "SIS", "standard_name", "SIS_standard", prec = "text")
  nc_close(ncnew)
}

# Create test data for extract.level
create_extract.level <- function() {
  filename <- file.path(testdata_dir, "ex_extract.level.nc")
  lon <- seq(5, 6, 0.5)
  lat <- seq(45, 46, 0.5)
  height <- seq(0, 200, 100)
  time <- seq(as.Date("2000-01-01"), as.Date("2000-02-28"), "month")
  origin <- as.Date("1983-01-01 00:00:00")
  time <- as.numeric(difftime(time, origin, units = "hour"))
  data <- array(250:272, dim = c(3, 3, 3, 2))

  x <- ncdim_def(name = "lon", units = "degrees_east", vals = lon)
  y <- ncdim_def(name = "lat", units = "degrees_north", vals = lat)
  z <- ncdim_def(name = "height", units = "m", vals = height)
  t <- ncdim_def(name = "time", units = "hours since 1983-01-01 00:00:00",
                 vals = time, unlim = TRUE)
  var1 <- ncvar_def("SIS", "W m-2", list(x, y, z, t), -999, prec = "short",
                    longname = "Surface Incoming Shortwave Radiation")
  vars <- list(var1)
  ncnew <- nc_create(filename, vars)
  ncvar_put(ncnew, var1, data)
  ncatt_put(ncnew, "lon", "standard_name", "longitude", prec = "text")
  ncatt_put(ncnew, "lat", "standard_name", "latitude", prec = "text")
  ncatt_put(ncnew, "height", "standard_name", "height", prec = "text")
  ncatt_put(ncnew, "SIS", "standard_name", "SIS_standard", prec = "text")
  nc_close(ncnew)
}

# Create test data for extract.level
create_extract.level2 <- function() {
  filename <- file.path(testdata_dir, "ex_extract.level2.nc")
  lon <- seq(5, 7, 0.5)
  lat <- seq(45, 47, 0.5)
  height <- seq(0, 200, 100)
  time <- seq(as.Date("2000-01-01"), as.Date("2000-02-28"), "month")
  origin <- as.Date("1983-01-01 00:00:00")
  time <- as.numeric(difftime(time, origin, units = "hour"))
  data <- array(250:272, dim = c(5, 5, 3, 2))

  x <- ncdim_def(name = "lon", units = "degrees_east", vals = lon)
  y <- ncdim_def(name = "lat", units = "degrees_north", vals = lat)
  z <- ncdim_def(name = "height", units = "m", vals = height)
  t <- ncdim_def(name = "time", units = "hours since 1983-01-01 00:00:00",
                 vals = time, unlim = TRUE)
  var1 <- ncvar_def("SIS", "W m-2", list(x, y, z, t), -999, prec = "short",
                    longname = "Surface Incoming Shortwave Radiation")
  vars <- list(var1)
  ncnew <- nc_create(filename, vars)
  ncvar_put(ncnew, var1, data)
  ncatt_put(ncnew, "lon", "standard_name", "longitude", prec = "text")
  ncatt_put(ncnew, "lat", "standard_name", "latitude", prec = "text")
  ncatt_put(ncnew, "height", "standard_name", "height", prec = "text")
  ncatt_put(ncnew, "SIS", "standard_name", "SIS_standard", prec = "text")
  nc_close(ncnew)
}

# Create test data for add_grid_info
create_add_grid_info <- function() {
  # Create a file using the Lambert azimuthal projection instead of the
  # standard grid.
  filename1 <- file.path(testdata_dir, "ex_add_grid_info.nc")
  filename2 <- file.path(testdata_dir, "ex_add_grid_info_aux.nc")
  filename3 <- file.path(testdata_dir, "ex_add_grid_info_with_grid_var.nc")
  x <- seq(0, 150, 25)
  y <- seq(0, 100, 25)
  time <- as.Date("2000-01-01")
  origin <- as.Date("1970-01-01 00:00:00")
  time <- as.numeric(difftime(time, origin, units = "day"))
  lat <- aperm(array(c(
    NA, NA, NA, NA, NA, 48.4687004089355, 48.6366996765137,
    NA, NA, NA, NA, 48.4714012145996, 48.6407012939453, 48.8092994689941,
    NA, NA, NA, 48.4714012145996, 48.6421012878418, 48.8120002746582, 48.9810981750488,
    NA, NA, 48.4687004089355, 48.6407012939453, 48.8120002746582, 48.9823989868164, 49.1521987915039,
    NA, 48.4634017944336, 48.6366996765137, 48.8092994689941, 48.9810981750488, 49.1521987915039, 49.3223991394043),
    dim = c(7, 5, 1)), c(2, 1, 3))
  lon <- aperm(array(c(
    NA, NA, NA, NA, NA, -45.673999786377, -45.9021987915039,
    NA, NA, NA, NA, -45.2247009277344, -45.451099395752, -45.6794013977051,
    NA, NA, NA, -44.7752990722656, -45, -45.2265014648438, -45.4547004699707,
    NA, NA, -44.326000213623, -44.548900604248, -44.7734985351562, -45, -45.2282981872559,
    NA, -43.8767013549805, -44.0978012084961, -44.3205986022949, -44.5452995300293, -44.7717018127441, -45),
    dim = c(7, 5, 1)), c(2, 1, 3))
  data <- aperm(array(c(
    NA, NA, NA, 86, 87, 89, 89,
    NA, NA, 87, 86, 88, 89, 89,
    NA, 89, 88, 87, 88, 90, 89,
    89, 89, 88, 88, 89, 89, 89,
    90, 89, 89, 89, 88, 89, 90),
    dim = c(7, 5, 1)), c(2, 1, 3))

  x <- ncdim_def(name = "x", units = "km", vals = x,
                 longname = "x-coordinate in kilometer")
  y <- ncdim_def(name = "y", units = "km", vals = y,
                 longname = "y-coordinate in kilometer")
  t <- ncdim_def(name = "time", units = "days since 1970-01-01 00:00:00",
                 vals = time, unlim = TRUE, calendar = "standard",
                 longname = "time")
  var1 <- ncvar_def(name = "lat",
                    units = "degrees_north",
                    dim = list(x, y),
                    missval = -999,
                    longname = "latitude")
  var2 <- ncvar_def(name = "lon",
                    units = "degrees_east",
                    dim = list(x, y),
                    missval = -999,
                    longname = "longitude")
  var3 <- ncvar_def(name = "cfc",
                    units = "1",
                    dim = list(x, y, t),
                    missval = -999,
                    prec = "short",
                    longname = "Fractional Cloud Cover")

  ncnew <- nc_create(filename1, list(var3))
  ncvar_put(ncnew, var3, data)
  ncatt_put(ncnew, "cfc", "standard_name", "cfc_standard", prec = "text")
  ncatt_put(ncnew, 0, "title", "CM SAF CLoud property dAtAset using SEVIRI (CLAAS), edition 2",
            prec = "text")
  ncatt_put(ncnew, 0, "institution", "EUMETSAT/CMSAF", prec = "text")
  nc_close(ncnew)

  ncnew <- nc_create(filename2, list(var1, var2))
  ncvar_put(ncnew, var1, lat)
  ncvar_put(ncnew, var2, lon)
  ncatt_put(ncnew, 0, "title", "CLAAS Level 2 Auxiliary Data",
            prec = "text")
  ncatt_put(ncnew, 0, "institution", "EUMETSAT/CMSAF", prec = "text")
  nc_close(ncnew)

  ncnew <- nc_create(filename3, list(var1, var2, var3))
  ncvar_put(ncnew, var1, lat)
  ncvar_put(ncnew, var2, lon)
  ncvar_put(ncnew, var3, data)
  ncatt_put(ncnew, "cfc", "standard_name", "cfc_standard", prec = "text")
  ncatt_put(ncnew, 0, "title", "CM SAF CLoud property dAtAset using SEVIRI (CLAAS), edition 2",
            prec = "text")
  ncatt_put(ncnew, 0, "institution", "EUMETSAT/CMSAF", prec = "text")
  nc_close(ncnew)
}

# Create test data for extract.period
create_extract_period <- function() {
  filename <- file.path(testdata_dir, "ex_extract_period.nc")
  lon <- seq(5, 6, 0.5)
  lat <- seq(45, 46, 0.5)
  time <- seq(as.Date("2000-01-01"), as.Date("2000-05-28"), "month")
  origin <- as.Date("1983-01-01 00:00:00")
  time <- as.numeric(difftime(time, origin, units = "hour"))
  data <- aperm(array(250:272, dim = c(3, 3, 5)), c(2, 1, 3))

  x <- ncdim_def(name = "lon", units = "degrees_east", vals = lon)
  y <- ncdim_def(name = "lat", units = "degrees_north", vals = lat)
  t <- ncdim_def(name = "time", units = "hours since 1983-01-01 00:00:00",
                 vals = time, unlim = TRUE)
  var1 <- ncvar_def("SIS", "W m-2", list(x, y, t), -999, prec = "short",
                    longname = "Surface Incoming Shortwave Radiation")
  vars <- list(var1)
  ncnew <- nc_create(filename, vars)
  ncvar_put(ncnew, var1, data)
  ncatt_put(ncnew, "lon", "standard_name", "longitude", prec = "text")
  ncatt_put(ncnew, "lat", "standard_name", "latitude", prec = "text")
  ncatt_put(ncnew, "SIS", "standard_name", "SIS_standard", prec = "text")
  nc_close(ncnew)
}

# Create test data for remap
create_remap <- function() {
  # Create a file where the standard (lon/lat) grid is a variable (matrix)
  # instead of a dimension (two vectors).
  filename <- file.path(testdata_dir, "ex_remap.nc")
  x <- seq(0, 150, 25)
  y <- seq(0, 150, 25)
  time <- as.Date("2000-01-01")
  origin <- as.Date("1970-01-01 00:00:00")
  time <- as.numeric(difftime(time, origin, units = "day"))
  lat <- array(c(
    NA, NA, NA, NA, NA, NA, NA,
    NA, NA, NA, NA, NA, NA, 48.4634017944336,
    NA, NA, NA, NA, NA, 48.4687004089355, 48.6366996765137,
    NA, NA, NA, NA, 48.4714012145996, 48.6407012939453, 48.8092994689941,
    NA, NA, NA, 48.4714012145996, 48.6421012878418, 48.8120002746582, 48.9810981750488,
    NA, NA, 48.4687004089355, 48.6407012939453, 48.8120002746582, 48.9823989868164, 49.1521987915039,
    NA, 48.4634017944336, 48.6366996765137, 48.8092994689941, 48.9810981750488, 49.1521987915039, 49.3223991394043),
    dim = c(7, 7, 1))
  lon <- array(c(
    NA, NA, NA, NA, NA, NA, NA,
    NA, NA, NA, NA, NA, NA, -46.1232986450195,
    NA, NA, NA, NA, NA, -45.673999786377, -45.9021987915039,
    NA, NA, NA, NA, -45.2247009277344, -45.451099395752, -45.6794013977051,
    NA, NA, NA, -44.7752990722656, -45, -45.2265014648438, -45.4547004699707,
    NA, NA, -44.326000213623, -44.548900604248, -44.7734985351562, -45, -45.2282981872559,
    NA, -43.8767013549805, -44.0978012084961, -44.3205986022949, -44.5452995300293, -44.7717018127441, -45),
    dim = c(7, 7, 1))
  data <- array(c(
    NA, NA, NA, NA, NA, 88, 89,
    NA, NA, NA, NA, 88, 88, 90,
    NA, NA, NA, 86, 87, 89, 89,
    NA, NA, 87, 86, 88, 89, 89,
    NA, 89, 88, 87, 88, 90, 89,
    89, 89, 88, 88, 89, 89, 89,
    90, 89, 89, 89, 88, 89, 90),
    dim = c(7, 7, 1))

  x <- ncdim_def(name = "x", units = "km", vals = x,
                 longname = "x-coordinate in kilometer")
  y <- ncdim_def(name = "y", units = "km", vals = y,
                 longname = "y-coordinate in kilometer")
  t <- ncdim_def(name = "time", units = "days since 1970-01-01 00:00:00",
                 vals = time, unlim = TRUE, calendar = "standard",
                 longname = "time")
  var1 <- ncvar_def(name = "lat",
                    units = "degrees_north",
                    dim = list(x, y),
                    missval = -999,
                    longname = "latitude")
  var2 <- ncvar_def(name = "lon",
                    units = "degrees_east",
                    dim = list(x, y),
                    missval = -999,
                    longname = "longitude")
  var3 <- ncvar_def(name = "cfc",
                    units = "1",
                    dim = list(x, y, t),
                    missval = -999,
                    prec = "short",
                    longname = "Fractional Cloud Cover")
  vars <- list(var1, var2, var3)
  ncnew <- nc_create(filename, vars)
  ncvar_put(ncnew, var1, lat)
  ncvar_put(ncnew, var2, lon)
  ncvar_put(ncnew, var3, data)
  ncatt_put(ncnew, "cfc", "standard_name", "cfc_standard", prec = "text")
  ncatt_put(ncnew, 0, "title", "CM SAF cLoud, Albedo and RAdiation dataset",
            prec = "text")
  ncatt_put(ncnew, 0, "institution", "EUMETSAT/CMSAF", prec = "text")
  nc_close(ncnew)
}

# Create some (non-realistic) example data with levels.
create_examples_levels <- function() {
  filename1 <- file.path(testdata_dir, "ex_lev1.nc")
  filename2 <- file.path(testdata_dir, "ex_lev2.nc")
  lon <- seq(5, 8, 0.5)
  lat <- seq(45, 48, 0.5)
  time <- c(as.Date("2000-01-01"), as.Date("2001-02-01"))
  origin <- as.Date("1983-01-01 00:00:00")
  time <- as.numeric(difftime(time, origin, units = "hour"))
  level <- c(1:5)
  data1 <- array(250:272, dim = c(7, 7, 5, 1))
  data2 <- array(230:252, dim = c(7, 7, 5, 1))

  x <- ncdim_def(name = "lon", units = "degrees_east", vals = lon)
  y <- ncdim_def(name = "lat", units = "degrees_north", vals = lat)
  l <- ncdim_def(name = "level", units = "1", vals = level)
  t <- ncdim_def(name = "time", units = "hours since 1983-01-01 00:00:00",
                 vals = time[1], unlim = TRUE)
  var1 <- ncvar_def("SIS", "W m-2", list(x, y, l, t), -999, prec = "short")
  vars <- list(var1)
  ncnew <- nc_create(filename1, vars)
  ncvar_put(ncnew, var1, data1)
  ncatt_put(ncnew, "lon", "standard_name", "longitude", prec = "text")
  ncatt_put(ncnew, "lat", "standard_name", "latitude", prec = "text")
  ncatt_put(ncnew, "level", "standard_name", "level", prec = "text")
  ncatt_put(ncnew, "SIS", "standard_name", "SIS_standard", prec = "text")
  nc_close(ncnew)

  t <- ncdim_def(name = "time", units = "hours since 1983-01-01 00:00:00",
                 vals = time[2], unlim = TRUE)
  var1 <- ncvar_def("SIS", "W m-2", list(x, y, l, t), -999, prec = "short",
                    longname = "Surface Incoming Shortwave Radiation")
  vars <- list(var1)
  ncnew <- nc_create(filename2, vars)
  ncvar_put(ncnew, var1, data2)
  ncatt_put(ncnew, "lon", "standard_name", "longitude", prec = "text")
  ncatt_put(ncnew, "lat", "standard_name", "latitude", prec = "text")
  ncatt_put(ncnew, "level", "standard_name", "level", prec = "text")
  ncatt_put(ncnew, "SIS", "standard_name", "SIS_standard", prec = "text")
  nc_close(ncnew)
}

# Create some (non-realistic) example data with levels and time > 1.
create_examples_levels_time <- function() {
  filename <- file.path(testdata_dir, "ex_time_lev.nc")
  lon <- seq(5, 8, 0.5)
  lat <- seq(45, 48, 0.5)
  time <- c(as.Date("2007-01-01"), as.Date("2008-02-01"))
  origin <- as.Date("1983-01-01 00:00:00")
  time <- as.numeric(difftime(time, origin, units = "hour"))
  level <- c(1:5)
  data1 <- array(250:272, dim = c(7, 7, 5, 2))

  x <- ncdim_def(name = "lon", units = "degrees_east", vals = lon)
  y <- ncdim_def(name = "lat", units = "degrees_north", vals = lat)
  l <- ncdim_def(name = "level", units = "1", vals = level)
  t <- ncdim_def(name = "time", units = "hours since 1983-01-01 00:00:00",
                 vals = time[1:2], unlim = TRUE)
  var1 <- ncvar_def("SIS", "W m-2", list(x, y, l, t), -999, prec = "short",
                    longname = "Surface Incoming Shortwave Radiation")
  vars <- list(var1)
  ncnew <- nc_create(filename, vars)
  ncvar_put(ncnew, var1, data1)
  ncatt_put(ncnew, "lon", "standard_name", "longitude", prec = "text")
  ncatt_put(ncnew, "lat", "standard_name", "latitude", prec = "text")
  ncatt_put(ncnew, "level", "standard_name", "level", prec = "text")
  ncatt_put(ncnew, "SIS", "standard_name", "SIS_standard", prec = "text")
  nc_close(ncnew)
}

# Create test data for fldmax, fldmean and fldmin.
create_fld <- function() {
  filename <- file.path(testdata_dir, "ex_fld.nc")
  lon <- seq(5, 6, 0.5)
  lat <- seq(45, 46, 0.5)
  time <- seq(as.Date("2000-01-01"), as.Date("2000-05-28"), "month")
  origin <- as.Date("1983-01-01 00:00:00")
  time <- as.numeric(difftime(time, origin, units = "hour"))
  data <- aperm(array(250:272, dim = c(3, 3, 5)), c(2, 1, 3))

  x <- ncdim_def(name = "lon", units = "degrees_east", vals = lon)
  y <- ncdim_def(name = "lat", units = "degrees_north", vals = lat)
  t <- ncdim_def(name = "time", units = "hours since 1983-01-01 00:00:00",
                 vals = time, unlim = TRUE)
  var1 <- ncvar_def("SIS", "W m-2", list(x, y, t), -999, prec = "short",
                    longname = "Surface Incoming Shortwave Radiation")
  vars <- list(var1)
  ncnew <- nc_create(filename, vars)
  ncvar_put(ncnew, var1, data)
  ncatt_put(ncnew, "lon", "standard_name", "longitude", prec = "text")
  ncatt_put(ncnew, "lat", "standard_name", "latitude", prec = "text")
  ncatt_put(ncnew, "SIS", "standard_name", "SIS_standard", prec = "text")
  nc_close(ncnew)
}

# Create test data for monmax, monmean, monmin, monsd and monsum.
create_mon <- function() {
  filename <- file.path(testdata_dir, "ex_mon.nc")
  lon <- seq(5, 6, 0.5)
  lat <- seq(45, 47, 0.5)
  time <- seq(as.Date("2000-01-01"), as.Date("2000-03-31"), "days")
  origin <- as.Date("1983-01-01 00:00:00")
  time <- as.numeric(difftime(time, origin, units = "hour"))
  data <- aperm(array(c(1:1000), dim = c(3, 5, 91)), c(2, 1, 3))

  x <- ncdim_def(name = "lon", units = "degrees_east", vals = lon)
  y <- ncdim_def(name = "lat", units = "degrees_north", vals = lat)
  t <- ncdim_def(name = "time", units = "hours since 1983-01-01 00:00:00",
                 vals = time, unlim = TRUE)
  var1 <- ncvar_def("SIS", "W m-2", list(x, y, t), -999, prec = "short",
                    longname = "Surface Incoming Shortwave Radiation")
  vars <- list(var1)
  ncnew <- nc_create(filename, vars)
  ncvar_put(ncnew, var1, data)
  ncatt_put(ncnew, "lon", "standard_name", "longitude", prec = "text")
  ncatt_put(ncnew, "lat", "standard_name", "latitude", prec = "text")
  ncatt_put(ncnew, "SIS", "standard_name", "SIS_standard", prec = "text")
  nc_close(ncnew)
}

# Create example data with timestamp.
create_examples_timestamp <- function() {
  filename1 <- file.path(testdata_dir, "ex_timestamp1.nc")
  filename2 <- file.path(testdata_dir, "ex_timestamp2.nc")
  lon <- seq(5, 8, 0.5)
  lat <- seq(45, 48, 0.5)
  time <- seq(ISOdate(2000, 1, 1), ISOdate(2000, 1, 3), "hours")
  origin <- as.Date("1983-01-01 00:00:00")
  time <- as.numeric(difftime(time, origin, units = "hour"))
  data1 <- array(250:272, dim = c(7, 7, 24))
  data2 <- array(230:252, dim = c(7, 7, 5))

  x <- ncdim_def(name = "lon", units = "degrees_east", vals = lon)
  y <- ncdim_def(name = "lat", units = "degrees_north", vals = lat)
  t <- ncdim_def(name = "time", units = "hours since 1983-01-01 00:00:00",
                 vals = time[1:24], unlim = TRUE)
  var1 <- ncvar_def("SIS", "W m-2", list(x, y, t), -999, prec = "short",
                    longname = "Surface Incoming Shortwave Radiation")
  vars <- list(var1)
  ncnew <- nc_create(filename1, vars)
  ncvar_put(ncnew, var1, data1)
  ncatt_put(ncnew, "lon", "standard_name", "longitude", prec = "text")
  ncatt_put(ncnew, "lat", "standard_name", "latitude", prec = "text")
  ncatt_put(ncnew, "SIS", "standard_name", "SIS_standard", prec = "text")
  nc_close(ncnew)

  x <- ncdim_def(name = "lon", units = "degrees_east", vals = lon)
  y <- ncdim_def(name = "lat", units = "degrees_north", vals = lat)
  t <- ncdim_def(name = "time", units = "hours since 1983-01-01 00:00:00",
                 vals = time[25:29], unlim = TRUE)
  var1 <- ncvar_def("SIS", "W m-2", list(x, y, t), -999, prec = "short",
                    longname = "Surface Incoming Shortwave Radiation")
  vars <- list(var1)
  ncnew <- nc_create(filename2, vars)
  ncvar_put(ncnew, var1, data2)
  ncatt_put(ncnew, "lon", "standard_name", "longitude", prec = "text")
  ncatt_put(ncnew, "lat", "standard_name", "latitude", prec = "text")
  ncatt_put(ncnew, "SIS", "standard_name", "SIS_standard", prec = "text")
  nc_close(ncnew)
}

# Create example with NA and time length 2
create_examples_na <- function() {
  filename1 <- file.path(testdata_dir, "ex_na.nc")
  lon <- seq(5, 8, 0.5)
  lat <- seq(45, 48, 0.5)
  time <- c(as.Date("2012-01-01"), as.Date("2013-02-01"))
  origin <- as.Date("1983-01-01 00:00:00")
  time <- as.numeric(difftime(time, origin, units = "hour"))
  data1 <- array(c(250:271, NA), dim = c(7, 7, 2))

  x <- ncdim_def(name = "lon", units = "degrees_east", vals = lon)
  y <- ncdim_def(name = "lat", units = "degrees_north", vals = lat)
  t <- ncdim_def(name = "time", units = "hours since 1983-01-01 00:00:00",
                 vals = time[1:2], unlim = TRUE)
  var1 <- ncvar_def("SIS", "W m-2", list(x, y, t), -999, prec = "short",
                    longname = "Surface Incoming Shortwave Radiation")
  vars <- list(var1)
  ncnew <- nc_create(filename1, vars)
  ncvar_put(ncnew, var1, data1)
  ncatt_put(ncnew, "lon", "standard_name", "longitude", prec = "text")
  ncatt_put(ncnew, "lat", "standard_name", "latitude", prec = "text")
  ncatt_put(ncnew, "SIS", "standard_name", "SIS_standard", prec = "text")
  nc_close(ncnew)
}

# Create examples with non regular grid with/without lon and lat as variables
create_examples_non_regular <- function() {
  filename1 <- file.path(testdata_dir, "ex_nonreg1.nc")
  filename2 <- file.path(testdata_dir, "ex_nonreg2.nc")
  x_dim <- seq(0, 300, 50)
  y_dim <- seq(0, 300, 50)
  lon <- c(seq(45, 45.6, 0.1), seq(45.8, 47, 0.2), seq(47.5, 50.5, 0.5), seq(51.5, 57.5, 1), seq(58.5, 61.5, 0.5), seq(62, 63.2, 0.2), seq(63.4, 64, 0.1))
  lat <- c(seq(5, 5.6, 0.1), seq(5.8, 7, 0.2), seq(7.5, 10.5, 0.5), seq(11.5, 17.5, 1), seq(18.5, 21.5, 0.5), seq(22, 23.2, 0.2), seq(23.4, 24, 0.1))
  lon <- array(lon, dim = c(7, 7, 1))
  lat <- array(lat, dim = c(7, 7, 1))
  time <- c(as.Date("2016-06-01"), as.Date("2018-08-01"))
  origin <- as.Date("1983-01-01 00:00:00")
  time <- as.numeric(difftime(time, origin, units = "hour"))
  data1 <- array(250:272, dim = c(7, 7, 1))
  data2 <- array(230:252, dim = c(7, 7, 1))

  x <- ncdim_def(name = "x", units = "km", vals = x_dim)
  y <- ncdim_def(name = "y", units = "km", vals = y_dim)
  t <- ncdim_def(name = "time", units = "hours since 1983-01-01 00:00:00",
                 vals = time[1], unlim = TRUE)
  var1 <- ncvar_def("cfc", "1", list(x, y, t), -999, prec = "short",
                    longname = "Fractional Cloud Cover")
  varlon <- ncvar_def("lon", "degrees_east", list(x, y), -999, prec = "double")
  varlat <- ncvar_def("lat", "degrees_north", list(x, y), -999, prec = "double")
  vars <- list(var1, varlon, varlat)
  ncnew <- nc_create(filename1, vars)
  ncvar_put(ncnew, var1, data1)
  ncvar_put(ncnew, varlon, lon)
  ncvar_put(ncnew, varlat, lat)
  ncatt_put(ncnew, "x", "standard_name", "x", prec = "text")
  ncatt_put(ncnew, "y", "standard_name", "y", prec = "text")
  ncatt_put(ncnew, "lon", "standard_name", "longitude", prec = "text")
  ncatt_put(ncnew, "lat", "standard_name", "latitude", prec = "text")
  ncatt_put(ncnew, "cfc", "standard_name", "cfc_standard", prec = "text")
  nc_close(ncnew)

  x <- ncdim_def(name = "x", units = "km", vals = x_dim)
  y <- ncdim_def(name = "y", units = "km", vals = y_dim)
  t <- ncdim_def(name = "time", units = "hours since 1983-01-01 00:00:00",
                 vals = time[2], unlim = TRUE)
  var1 <- ncvar_def("cfc", "1", list(x, y, t), -999, prec = "short",
                    longname = "Fractional Cloud Cover")
  vars <- list(var1)
  ncnew <- nc_create(filename2, vars)
  ncvar_put(ncnew, var1, data2)
  ncatt_put(ncnew, "x", "standard_name", "x", prec = "text")
  ncatt_put(ncnew, "y", "standard_name", "y", prec = "text")
  ncatt_put(ncnew, "cfc", "standard_name", "cfc_standard", prec = "text")
  nc_close(ncnew)
}

# Create examples with time_bnds
create_examples_tb <- function() {
  filename1 <- file.path(testdata_dir, "ex_time_bnds1.nc")
  filename2 <- file.path(testdata_dir, "ex_time_bnds2.nc")
  lon <- seq(5, 8, 0.5)
  lat <- seq(45, 48, 0.5)
  time <- c(as.Date("2007-07-01"), as.Date("2007-08-01"), as.Date("2007-09-01"))
  origin <- as.Date("1983-01-01 00:00:00")
  time <- as.numeric(difftime(time, origin, units = "hour"))
  data1 <- array(250:272, dim = c(7, 7, 1))
  data2 <- array(230:252, dim = c(7, 7, 1))

  time_bnds1 <- array(NA, dim = c(2, 1))
  time_bnds1[1, 1] <- time[1]
  time_bnds1[2, 1] <- time[2]

  time_bnds2 <- array(NA, dim = c(2, 1))
  time_bnds2[1, 1] <- time[2]
  time_bnds2[2, 1] <- time[3]

  nb2 <- c(0, 1)

  x <- ncdim_def(name = "lon", units = "degrees_east", vals = lon)
  y <- ncdim_def(name = "lat", units = "degrees_north", vals = lat)
  t <- ncdim_def(name = "time", units = "hours since 1983-01-01 00:00:00",
                 vals = time[1], unlim = TRUE)
  var1 <- ncvar_def("SIS", "W m-2", list(x, y, t), -999, prec = "short",
                    longname = "Surface Incoming Shortwave Radiation")
  tb <- ncdim_def(name = "nb2", units = "1", vals = nb2)
  var2 <- ncvar_def(name = "time_bnds", units = "1", dim = list(tb, t), prec = "double")
  vars <- list(var1, var2)
  ncnew <- nc_create(filename1, vars)
  ncvar_put(ncnew, var1, data1)
  ncvar_put(ncnew, var2, time_bnds1)
  ncatt_put(ncnew, "lon", "standard_name", "longitude", prec = "text")
  ncatt_put(ncnew, "lat", "standard_name", "latitude", prec = "text")
  ncatt_put(ncnew, "SIS", "standard_name", "SIS_standard", prec = "text")
  nc_close(ncnew)

  x <- ncdim_def(name = "lon", units = "degrees_east", vals = lon)
  y <- ncdim_def(name = "lat", units = "degrees_north", vals = lat)
  t <- ncdim_def(name = "time", units = "hours since 1983-01-01 00:00:00",
                 vals = time[2], unlim = TRUE)
  var1 <- ncvar_def("SIS", "W m-2", list(x, y, t), -999, prec = "short",
                    longname = "Surface Incoming Shortwave Radiation")
  tb <- ncdim_def(name = "nb2", units = "1", vals = nb2)
  var2 <- ncvar_def(name = "time_bnds", units = "1", dim = list(tb, t), prec = "double")
  vars <- list(var1, var2)
  ncnew <- nc_create(filename2, vars)
  ncvar_put(ncnew, var1, data2)
  ncvar_put(ncnew, var2, time_bnds2)
  ncatt_put(ncnew, "lon", "standard_name", "longitude", prec = "text")
  ncatt_put(ncnew, "lat", "standard_name", "latitude", prec = "text")
  ncatt_put(ncnew, "SIS", "standard_name", "SIS_standard", prec = "text")
  nc_close(ncnew)
}

# Create test data for wfldmean.
create_wfldmean <- function() {
  filename <- file.path(testdata_dir, "ex_wfldmean.nc")
  lon <- seq(5, 6, 0.5)
  lat <- seq(45, 46.5, 0.5)
  time <- seq(as.Date("2000-01-01"), as.Date("2000-05-28"), "month")
  origin <- as.Date("1983-01-01 00:00:00")
  time <- as.numeric(difftime(time, origin, units = "hour"))
  data <- aperm(array(1:51, dim = c(3, 4, 5)), c(2, 1, 3))

  x <- ncdim_def(name = "lon", units = "degrees_east", vals = lon)
  y <- ncdim_def(name = "lat", units = "degrees_north", vals = lat)
  t <- ncdim_def(name = "time", units = "hours since 1983-01-01 00:00:00",
                 vals = time, unlim = TRUE)
  var1 <- ncvar_def("SIS", "W m-2", list(x, y, t), -999, prec = "short",
                    longname = "Surface Incoming Shortwave Radiation")
  vars <- list(var1)
  ncnew <- nc_create(filename, vars)
  ncvar_put(ncnew, var1, data)
  ncatt_put(ncnew, "lon", "standard_name", "longitude", prec = "text")
  ncatt_put(ncnew, "lat", "standard_name", "latitude", prec = "text")
  ncatt_put(ncnew, "SIS", "standard_name", "SIS_standard", prec = "text")
  nc_close(ncnew)
}

# Create test data for monmax, monmean, monmin, monsd and monsum.
create_yday <- function() {
  filename <- file.path(testdata_dir, "ex_yday.nc")
  lon <- seq(5, 6, 0.5)
  lat <- seq(45, 47, 0.5)
  time <- seq(as.Date("2000-01-30"), as.Date("2002-02-01"), "days")
  origin <- as.Date("1983-01-01 00:00:00")
  time <- as.numeric(difftime(time, origin, units = "hour"))
  data <- aperm(array(c(1:123), dim = c(3, 5, 734)), c(2, 1, 3))

  x <- ncdim_def(name = "lon", units = "degrees_east", vals = lon)
  y <- ncdim_def(name = "lat", units = "degrees_north", vals = lat)
  t <- ncdim_def(name = "time", units = "hours since 1983-01-01 00:00:00",
                 vals = time, unlim = TRUE)
  var1 <- ncvar_def("SIS", "W m-2", list(x, y, t), -999, prec = "short",
                    longname = "Surface Incoming Shortwave Radiation")
  vars <- list(var1)
  ncnew <- nc_create(filename, vars)
  ncvar_put(ncnew, var1, data)
  ncatt_put(ncnew, "lon", "standard_name", "longitude", prec = "text")
  ncatt_put(ncnew, "lat", "standard_name", "latitude", prec = "text")
  ncatt_put(ncnew, "SIS", "standard_name", "SIS_standard", prec = "text")
  nc_close(ncnew)
}

# Create test data for yearsum, yearmean, yearmin, yearmax, yearsd, yearvar and yearrange
create_year <- function() {
  filename <- file.path(testdata_dir, "ex_year.nc")
  lon <- seq(5, 6, 0.5)
  lat <- seq(45, 47, 0.5)
  time <- seq(as.Date("2000-01-01"), as.Date("2002-12-31"), "month")
  origin <- as.Date("1983-01-01 00:00:00")
  time <- as.numeric(difftime(time, origin, units = "hour"))
  data <- aperm(array(c(1:369), dim = c(3, 5, 36)), c(2, 1, 3))

  x <- ncdim_def(name = "lon", units = "degrees_east", vals = lon)
  y <- ncdim_def(name = "lat", units = "degrees_north", vals = lat)
  t <- ncdim_def(name = "time", units = "hours since 1983-01-01 00:00:00",
                 vals = time, unlim = TRUE)
  var1 <- ncvar_def("SIS", "W m-2", list(x, y, t), -999, prec = "short",
                    longname = "Surface Incoming Shortwave Radiation")
  vars <- list(var1)
  ncnew <- nc_create(filename, vars)
  ncvar_put(ncnew, var1, data)
  ncatt_put(ncnew, "lon", "standard_name", "longitude", prec = "text")
  ncatt_put(ncnew, "lat", "standard_name", "latitude", prec = "text")
  ncatt_put(ncnew, "SIS", "standard_name", "SIS_standard", prec = "text")
  nc_close(ncnew)
}

# Create test data for ymonmax, ymonmean and ymonmin.
create_ymon <- function() {
  filename <- file.path(testdata_dir, "ex_ymon.nc")
  lon <- seq(5, 6, 0.5)
  lat <- seq(45, 47, 0.5)
  time <- c(seq(as.Date("2000-01-01"), as.Date("2000-03-31"), "month"),
            seq(as.Date("2001-01-01"), as.Date("2001-03-31"), "month"),
            seq(as.Date("2002-01-01"), as.Date("2002-03-31"), "month"),
            seq(as.Date("2003-01-01"), as.Date("2003-03-31"), "month")
  )
  origin <- as.Date("1983-01-01 00:00:00")
  time <- as.numeric(difftime(time, origin, units = "hour"))
  data <- aperm(array(c(1:150), dim = c(3, 5, 12)), c(2, 1, 3))

  x <- ncdim_def(name = "lon", units = "degrees_east", vals = lon)
  y <- ncdim_def(name = "lat", units = "degrees_north", vals = lat)
  t <- ncdim_def(name = "time", units = "hours since 1983-01-01 00:00:00",
                 vals = time, unlim = TRUE)
  var1 <- ncvar_def("SIS", "W m-2", list(x, y, t), -999, prec = "short",
                    longname = "Surface Incoming Shortwave Radiation")
  vars <- list(var1)
  ncnew <- nc_create(filename, vars)
  ncvar_put(ncnew, var1, data)
  ncatt_put(ncnew, "lon", "standard_name", "longitude", prec = "text")
  ncatt_put(ncnew, "lat", "standard_name", "latitude", prec = "text")
  ncatt_put(ncnew, "SIS", "standard_name", "SIS_standard", prec = "text")
  ncatt_put(ncnew, 0, "institution", "This is a test attribute.", prec = "text")
  ncatt_put(ncnew, 0, "test", "This is another test attribute.", prec = "text")
  nc_close(ncnew)
}

# Create examples with non regular grid with/without lon and lat as variables
create_ymon_irregular <- function() {
  filename1 <- file.path(testdata_dir, "ex_ymon_irreg1.nc")
  filename2 <- file.path(testdata_dir, "ex_ymon_irreg2.nc")
  x_dim <- seq(0, 250, 50)
  y_dim <- seq(0, 300, 50)
  lon <- c(
    seq(45, 45.6, 0.1),
    seq(45.8, 47, 0.2),
    seq(47.5, 50.5, 0.5),
    seq(51.5, 57.5, 1),
    seq(58.5, 61.5, 0.5),
    seq(62, 63.2, 0.2)
  )
  lat <- c(
    seq(5, 5.6, 0.1),
    seq(5.8, 7, 0.2),
    seq(7.5, 10.5, 0.5),
    seq(11.5, 17.5, 1),
    seq(18.5, 21.5, 0.5),
    seq(22, 23.2, 0.2)
  )
  lon <- array(lon, dim = c(6, 7))
  lat <- array(lat, dim = c(6, 7))
  time <- c(seq(as.Date("2000-01-01"), as.Date("2000-03-31"), "month"),
            seq(as.Date("2001-01-01"), as.Date("2001-03-31"), "month"),
            seq(as.Date("2002-01-01"), as.Date("2002-03-31"), "month"),
            seq(as.Date("2003-01-01"), as.Date("2003-03-31"), "month")
  )
  origin <- as.Date("1983-01-01 00:00:00")
  time <- as.numeric(difftime(time, origin, units = "hour"))
  set.seed(119)
  data1_values <- sample(seq_len(200), 6 * 7 * 12, replace = TRUE)
  data1_values[c(13, 119, 401)] <- NA
  data1 <- array(data1_values, dim = c(6, 7, 12))
  data2 <- array(230:252, dim = c(6, 7, 12))

  x <- ncdim_def(name = "x", units = "km", vals = x_dim)
  y <- ncdim_def(name = "y", units = "km", vals = y_dim)
  t <- ncdim_def(name = "time", units = "hours since 1983-01-01 00:00:00",
                 vals = time, unlim = TRUE)
  var1 <- ncvar_def("cfc", "1", list(x, y, t), -999, prec = "short",
                    "Fractional Cloud Cover")
  varlon <- ncvar_def("lon", "degrees_east", list(x, y), -999, prec = "double")
  varlat <- ncvar_def("lat", "degrees_north", list(x, y), -999, prec = "double")
  vars <- list(var1, varlon, varlat)
  ncnew <- nc_create(filename1, vars)
  ncvar_put(ncnew, var1, data1)
  ncvar_put(ncnew, varlon, lon)
  ncvar_put(ncnew, varlat, lat)
  ncatt_put(ncnew, "x", "standard_name", "x", prec = "text")
  ncatt_put(ncnew, "y", "standard_name", "y", prec = "text")
  ncatt_put(ncnew, "lon", "standard_name", "longitude", prec = "text")
  ncatt_put(ncnew, "lat", "standard_name", "latitude", prec = "text")
  ncatt_put(ncnew, "lat", "standard_name", "latitude", prec = "text")
  ncatt_put(ncnew, "cfc", "standard_name", "cfc_standard", prec = "text")
  ncatt_put(ncnew, 0, "institution", "some institution", prec = "text")
  ncatt_put(ncnew, 0, "test", "This is a test attribute.", prec = "text")

  nc_close(ncnew)

  x <- ncdim_def(name = "x", units = "km", vals = x_dim)
  y <- ncdim_def(name = "y", units = "km", vals = y_dim)
  t <- ncdim_def(name = "time", units = "hours since 1983-01-01 00:00:00",
                 vals = time, unlim = TRUE)
  var1 <- ncvar_def("cfc", "1", list(x, y, t), -999, prec = "short",
                    longname = "Fractional Cloud Cover")
  vars <- list(var1)
  ncnew <- nc_create(filename2, vars)
  ncvar_put(ncnew, var1, data2)
  ncatt_put(ncnew, "x", "standard_name", "x", prec = "text")
  ncatt_put(ncnew, "y", "standard_name", "y", prec = "text")
  ncatt_put(ncnew, "cfc", "standard_name", "cfc_standard", prec = "text")
  ncatt_put(ncnew, 0, "institution", "some institution", prec = "text")
  ncatt_put(ncnew, 0, "test", "This is a test attribute.", prec = "text")
  nc_close(ncnew)
}

# Create test data for yseasmax, yseasmean and yseasmin.
create_yseas <- function() {
  filename <- file.path(testdata_dir, "ex_yseas.nc")
  lon <- seq(5, 6, 0.5)
  lat <- seq(45, 47, 0.5)
  time <- seq(as.Date("2000-01-01"), as.Date("2003-12-31"), "month")
  origin <- as.Date("1983-01-01 00:00:00")
  time <- as.numeric(difftime(time, origin, units = "hour"))
  data <- aperm(array(c(1:631), dim = c(3, 5, 48)), c(2, 1, 3))

  x <- ncdim_def(name = "lon", units = "degrees_east", vals = lon)
  y <- ncdim_def(name = "lat", units = "degrees_north", vals = lat)
  t <- ncdim_def(name = "time", units = "hours since 1983-01-01 00:00:00",
                 vals = time, unlim = TRUE)
  var1 <- ncvar_def("SIS", "W m-2", list(x, y, t), -999, prec = "short",
                    longname = "Surface Incoming Shortwave Radiation")
  vars <- list(var1)
  ncnew <- nc_create(filename, vars)
  ncvar_put(ncnew, var1, data)
  ncatt_put(ncnew, "lon", "standard_name", "longitude", prec = "text")
  ncatt_put(ncnew, "lat", "standard_name", "latitude", prec = "text")
  ncatt_put(ncnew, "SIS", "standard_name", "SIS_standard", prec = "text")
  nc_close(ncnew)
}

# Create test data for mon.anomaly

create_mon2 <- function() {
  filename <- file.path(testdata_dir, "ex_mon2.nc")
  lon <- seq(5, 7, 0.5)
  lat <- seq(45, 47, 0.5)
  time <- seq(as.Date("2000-01-01"), as.Date("2002-12-31"), "month")
  origin <- as.Date("1983-01-01 00:00:00")
  time <- as.numeric(difftime(time, origin, units = "hour"))
  data <- array(1:70, dim = c(5, 5, 36))

  x <- ncdim_def(name = "lon", units = "degrees_east", vals = lon)
  y <- ncdim_def(name = "lat", units = "degrees_north", vals = lat)
  t <- ncdim_def(name = "time", units = "hours since 1983-01-01 00:00:00",
                 vals = time, unlim = TRUE)
  var1 <- ncvar_def("SIS", "W m-2", list(x, y, t), -999, prec = "short",
                    longname = "Surface Incoming Shortwave Radiation")
  vars <- list(var1)
  ncnew <- nc_create(filename, vars)
  ncvar_put(ncnew, var1, data)
  ncatt_put(ncnew, "lon", "standard_name", "longitude", prec = "text")
  ncatt_put(ncnew, "lat", "standard_name", "latitude", prec = "text")
  ncatt_put(ncnew, "SIS", "standard_name", "SIS_standard", prec = "text")
  nc_close(ncnew)
}

#Create remap data
create_examples_remap <- function() {
  filename1 <- file.path(testdata_dir, "ex_remap_reg1.nc")
  filename2 <- file.path(testdata_dir, "ex_remap_reg2.nc")
  filename3 <- file.path(testdata_dir, "ex_remap_nonreg1.nc")
  filename4 <- file.path(testdata_dir, "ex_remap_nonreg2.nc")

  x_dim1 <- seq(0, 300, 50)
  y_dim1 <- seq(0, 300, 50)
  x_dim2 <- seq(0, 150, 30)
  y_dim2 <- seq(0, 150, 30)
  lon1 <- seq(45, 48, 0.5)
  lat1 <- seq(5, 8, 0.5)
  lon2 <- seq(46, 48.1, 0.3)
  lat2 <- seq(6, 8.1, 0.3)
  lon <- c(seq(45, 45.6, 0.1), seq(45.8, 47, 0.2), seq(47.5, 50.5, 0.5), seq(51.5, 57.5, 1), seq(58.5, 61.5, 0.5), seq(62, 63.2, 0.2), seq(63.4, 64, 0.1))
  lat <- c(seq(5, 5.6, 0.1), seq(5.8, 7, 0.2), seq(7.5, 10.5, 0.5), seq(11.5, 17.5, 1), seq(18.5, 21.5, 0.5), seq(22, 23.2, 0.2), seq(23.4, 24, 0.1))
  lon3 <- array(lon, dim = c(7, 7, 1))
  lat3 <- array(lat, dim = c(7, 7, 1))
  lon <- c(seq(46, 46.6, 0.1), seq(46.8, 48, 0.2), seq(48.5, 51.5, 0.5), seq(52.5, 58.5, 1), seq(59.5, 62.5, 0.5), seq(63, 64.2, 0.2), seq(64.4, 65, 0.1))
  lat <- c(seq(6, 6.6, 0.1), seq(6.8, 8, 0.2), seq(8.5, 11.5, 0.5), seq(12.5, 18.5, 1), seq(19.5, 22.5, 0.5), seq(23, 24.2, 0.2), seq(24.4, 25, 0.1))
  lon4 <- array(lon, dim = c(6, 6, 1))
  lat4 <- array(lat, dim = c(6, 6, 1))
  time <- c(as.Date("2015-08-01"), as.Date("2016-08-01"), as.Date("2017-08-01"), as.Date("2018-08-01"), as.Date("2019-08-01"), as.Date("2020-08-01"), as.Date("2021-08-01"))
  origin <- as.Date("1983-01-01 00:00:00")
  time <- as.numeric(difftime(time, origin, units = "hour"))

  time_bnds1 <- array(NA, dim = c(2, 2))
  time_bnds1[1, 1] <- time[1]
  time_bnds1[2, 1] <- time[2]
  time_bnds1[1, 2] <- time[2]
  time_bnds1[2, 2] <- time[3]

  time_bnds2 <- array(NA, dim = c(2, 1))
  time_bnds2[1, 1] <- time[3]
  time_bnds2[2, 1] <- time[4]

  time_bnds3 <- array(NA, dim = c(2, 2))
  time_bnds3[1, 1] <- time[4]
  time_bnds3[2, 1] <- time[5]
  time_bnds3[1, 2] <- time[5]
  time_bnds3[2, 2] <- time[6]

  time_bnds4 <- array(NA, dim = c(2, 1))
  time_bnds4[1, 1] <- time[6]
  time_bnds4[2, 1] <- time[7]

  data1 <- array(100:200, dim = c(7, 7, 2))
  data2 <- array(200:300, dim = c(8, 8, 1))
  data3 <- array(300:400, dim = c(7, 7, 2))
  data4 <- array(400:500, dim = c(6, 6, 1))

  x1 <- ncdim_def(name = "lon", units = "degrees_east", vals = lon1)
  y1 <- ncdim_def(name = "lat", units = "degrees_north", vals = lat1)
  x2 <- ncdim_def(name = "lon", units = "degrees_east", vals = lon2)
  y2 <- ncdim_def(name = "lat", units = "degrees_north", vals = lat2)
  x3 <- ncdim_def(name = "x", units = "km", vals = x_dim1)
  y3 <- ncdim_def(name = "y", units = "km", vals = y_dim1)
  x4 <- ncdim_def(name = "x", units = "km", vals = x_dim2)
  y4 <- ncdim_def(name = "y", units = "km", vals = y_dim2)
  t1 <- ncdim_def(name = "time", units = "hours since 1983-01-01 00:00:00",
                 vals = time[1:2], unlim = TRUE)
  t2 <- ncdim_def(name = "time", units = "hours since 1983-01-01 00:00:00",
                 vals = time[3], unlim = TRUE)
  t3 <- ncdim_def(name = "time", units = "hours since 1983-01-01 00:00:00",
                 vals = time[4:5], unlim = TRUE)
  t4 <- ncdim_def(name = "time", units = "hours since 1983-01-01 00:00:00",
                 vals = time[6], unlim = TRUE)
  tb <- ncdim_def(name = "nb2", units = "1", vals = c(0, 1))
  var1 <- ncvar_def("SIS", "W m-2", list(x1, y1, t1), -999, prec = "short",
                    longname = "Surface Incoming Shortwave Radiation")
  var2 <- ncvar_def("SIS", "W m-2", list(x2, y2, t2), -999, prec = "short",
                    longname = "Surface Incoming Shortwave Radiation")
  var3 <- ncvar_def("cfc", "1", list(x3, y3, t3), -999, prec = "short",
                    longname = "Fractional Cloud Cover")
  var4 <- ncvar_def("cfc", "1", list(x4, y4, t4), -999, prec = "short",
                    longname = "Fractional Cloud Cover")

  varlon1 <- ncvar_def("lon", "degrees_east", list(x3, y3), -999, prec = "double")
  varlat1 <- ncvar_def("lat", "degrees_north", list(x3, y3), -999, prec = "double")
  varlon2 <- ncvar_def("lon", "degrees_east", list(x4, y4), -999, prec = "double")
  varlat2 <- ncvar_def("lat", "degrees_north", list(x4, y4), -999, prec = "double")

  var_tb1 <- ncvar_def(name = "time_bnds", units = "1", dim = list(tb, t1), prec = "double")
  var_tb2 <- ncvar_def(name = "time_bnds", units = "1", dim = list(tb, t2), prec = "double")
  var_tb3 <- ncvar_def(name = "time_bnds", units = "1", dim = list(tb, t3), prec = "double")
  var_tb4 <- ncvar_def(name = "time_bnds", units = "1", dim = list(tb, t4), prec = "double")

  vars1 <- list(var1, var_tb1)
  vars2 <- list(var2, var_tb2)
  vars3 <- list(var3, varlon1, varlat1, var_tb3)
  vars4 <- list(var4, varlon2, varlat2, var_tb4)

  ncnew1 <- nc_create(filename1, vars1)
  ncnew2 <- nc_create(filename2, vars2)
  ncnew3 <- nc_create(filename3, vars3)
  ncnew4 <- nc_create(filename4, vars4)

  ncvar_put(ncnew1, var1, data1)
  ncvar_put(ncnew2, var2, data2)
  ncvar_put(ncnew3, var3, data3)
  ncvar_put(ncnew4, var4, data4)
  ncvar_put(ncnew1, var_tb1, time_bnds1)
  ncvar_put(ncnew2, var_tb2, time_bnds2)
  ncvar_put(ncnew3, var_tb3, time_bnds3)
  ncvar_put(ncnew4, var_tb4, time_bnds4)
  ncvar_put(ncnew3, varlon1, lon3)
  ncvar_put(ncnew3, varlat1, lat3)
  ncvar_put(ncnew4, varlon2, lon4)
  ncvar_put(ncnew4, varlat2, lat4)
  ncatt_put(ncnew3, "x", "standard_name", "x", prec = "text")
  ncatt_put(ncnew3, "y", "standard_name", "y", prec = "text")
  ncatt_put(ncnew4, "x", "standard_name", "x", prec = "text")
  ncatt_put(ncnew4, "y", "standard_name", "y", prec = "text")
  ncatt_put(ncnew1, "lon", "standard_name", "longitude", prec = "text")
  ncatt_put(ncnew1, "lat", "standard_name", "latitude", prec = "text")
  ncatt_put(ncnew2, "lon", "standard_name", "longitude", prec = "text")
  ncatt_put(ncnew2, "lat", "standard_name", "latitude", prec = "text")
  ncatt_put(ncnew3, "lon", "standard_name", "longitude", prec = "text")
  ncatt_put(ncnew3, "lat", "standard_name", "latitude", prec = "text")
  ncatt_put(ncnew1, "SIS", "standard_name", "SIS_standard", prec = "text")
  ncatt_put(ncnew2, "SIS", "standard_name", "SIS_standard", prec = "text")
  ncatt_put(ncnew3, "cfc", "standard_name", "cfc_standard", prec = "text")
  ncatt_put(ncnew4, "cfc", "standard_name", "cfc_standard", prec = "text")
  nc_close(ncnew1)
  nc_close(ncnew2)
  nc_close(ncnew3)
  nc_close(ncnew4)
}

##### create examples for accumulation #####
create_examples_timcumsum <- function() {
  filename1 <- file.path(testdata_dir, "ex_timcumsum1.nc")
  filename2 <- file.path(testdata_dir, "ex_timcumsum2.nc")
  filename3 <- file.path(testdata_dir, "ex_timcumsum3.nc")
  lon <- seq(5, 6, 0.5)
  lat <- seq(45, 46, 0.5)
  time <- c(as.Date("2000-01-01"), as.Date("2000-01-02"), as.Date("2001-02-01"), as.Date("2001-02-02"),
            as.Date("2002-03-01"), as.Date("2002-03-02"), as.Date("2003-04-01"), as.Date("2003-04-02"),
            as.Date("2004-05-01"), as.Date("2004-05-02"), as.Date("2005-06-01"), as.Date("2005-06-02"))
  origin <- as.Date("1983-01-01 00:00:00")
  time <- as.numeric(difftime(time, origin, units = "days"))

  data1 <- array(250:272, dim = c(3, 3, 4))
  data2 <- array(c(1:9, rep(NA, 9), 11:19, 21:29), dim = c(3, 3, 4))
  data3 <- array(c(c(NA,1,NA,1,1,1,NA,1,NA), rep(NA, 9), c(NA,9,NA,9,9,9,NA,9,NA), c(NA,1,NA,2,3,4,NA,5,NA)), dim = c(3, 3, 4))

  x <- ncdim_def(name = "lon", units = "degrees_east", vals = lon)
  y <- ncdim_def(name = "lat", units = "degrees_north", vals = lat)
  t <- ncdim_def(name = "time", units = "days since 1983-01-01 00:00:00",
                 vals = time[1:4], unlim = TRUE)
  var1 <- ncvar_def("SIS", "W m-2", list(x, y, t), -999, prec = "short",
                    longname = "Surface Incoming Shortwave Radiation")
  vars <- list(var1)
  ncnew <- nc_create(filename1, vars)
  ncvar_put(ncnew, var1, data1)
  ncatt_put(ncnew, "lon", "standard_name", "longitude", prec = "text")
  ncatt_put(ncnew, "lat", "standard_name", "latitude", prec = "text")
  ncatt_put(ncnew, "SIS", "standard_name", "SIS_standard", prec = "text")
  nc_close(ncnew)

  x <- ncdim_def(name = "lon", units = "degrees_east", vals = lon)
  y <- ncdim_def(name = "lat", units = "degrees_north", vals = lat)
  t <- ncdim_def(name = "time", units = "days since 1983-01-01 00:00:00",
                 vals = time[5:8], unlim = TRUE)
  var1 <- ncvar_def("SIS", "W m-2", list(x, y, t), -999, prec = "short",
                    longname = "Surface Incoming Shortwave Radiation")
  vars <- list(var1)
  ncnew <- nc_create(filename2, vars)
  ncvar_put(ncnew, var1, data2)
  ncatt_put(ncnew, "lon", "standard_name", "longitude", prec = "text")
  ncatt_put(ncnew, "lat", "standard_name", "latitude", prec = "text")
  ncatt_put(ncnew, "SIS", "standard_name", "SIS_standard", prec = "text")
  nc_close(ncnew)

  x <- ncdim_def(name = "lon", units = "degrees_east", vals = lon)
  y <- ncdim_def(name = "lat", units = "degrees_north", vals = lat)
  t <- ncdim_def(name = "time", units = "days since 1983-01-01 00:00:00",
                 vals = time[9:12], unlim = TRUE)
  var1 <- ncvar_def("SIS", "W m-2", list(x, y, t), -999, prec = "short",
                    longname = "Surface Incoming Shortwave Radiation")
  vars <- list(var1)
  ncnew <- nc_create(filename3, vars)
  ncvar_put(ncnew, var1, data3)
  ncatt_put(ncnew, "lon", "standard_name", "longitude", prec = "text")
  ncatt_put(ncnew, "lat", "standard_name", "latitude", prec = "text")
  ncatt_put(ncnew, "SIS", "standard_name", "SIS_standard", prec = "text")
  nc_close(ncnew)
}

##### create examples for timcor and timcovar #####
create_tim_cor_covar <- function() {
  filename1 <- file.path(testdata_dir, "ex_tim_cor_covar_1.nc")
  filename2 <- file.path(testdata_dir, "ex_tim_cor_covar_2.nc")
  lon <- seq(5, 15, 0.5)
  lat <- seq(45, 55, 0.5)
  time <- seq(as.Date("2000-03-01"), as.Date("2000-05-31"), "month")
  origin <- as.Date("1983-01-01 00:00:00")
  time <- as.numeric(difftime(time, origin, units = "hour"))
  data1 <- get_cor_data(1, 21, 21, 3)
  data2 <- get_cor_data(2, 21, 21, 3)
  
  ## create example NetCDF
  x <- ncdim_def(name = "lon", units = "degrees_east", vals = lon)
  y <- ncdim_def(name = "lat", units = "degrees_north", vals = lat)
  t <- ncdim_def(name = "time", units = "hours since 1983-01-01 00:00:00",
                 vals = time, unlim = TRUE)
  var1 <- ncvar_def("SIS", "W m-2", list(x, y, t), -999, prec = "short", 
                    longname = "Surface Incoming Shortwave Radiation")
  vars <- list(var1)
  ncnew_1 <- nc_create(filename1, vars)
  ncnew_2 <- nc_create(filename2, vars)
  
  ncvar_put(ncnew_1, var1, data1)
  ncvar_put(ncnew_2, var1, data2)
  
  ncatt_put(ncnew_1, "lon", "standard_name", "longitude", prec = "text")
  ncatt_put(ncnew_1, "lat", "standard_name", "latitude", prec = "text")
  ncatt_put(ncnew_1, "SIS", "standard_name", "SIS_standard", prec = "text")
  
  ncatt_put(ncnew_2, "lon", "standard_name", "longitude", prec = "text")
  ncatt_put(ncnew_2, "lat", "standard_name", "latitude", prec = "text")
  ncatt_put(ncnew_2, "SIS", "standard_name", "SIS_standard", prec = "text")
  nc_close(ncnew_1)
  nc_close(ncnew_2)
}

##### create examples for fldcor and fldcovar #####
create_fld_cor_covar <- function() {
  filename1 <- file.path(testdata_dir, "ex_fld_cor_covar_1.nc")
  filename2 <- file.path(testdata_dir, "ex_fld_cor_covar_2.nc")
  lon <- seq(5, 15, 0.5)
  lat <- seq(45, 55, 0.5)
  time <- seq(as.Date("2000-03-01"), as.Date("2000-05-31"), "month")
  origin <- as.Date("1983-01-01 00:00:00")
  time <- as.numeric(difftime(time, origin, units = "hour"))
  data1 <- get_cor_data_fld(1, 21, 21, 3)
  data2 <- get_cor_data_fld(2, 21, 21, 3)
  
  ## create example NetCDF
  x <- ncdim_def(name = "lon", units = "degrees_east", vals = lon)
  y <- ncdim_def(name = "lat", units = "degrees_north", vals = lat)
  t <- ncdim_def(name = "time", units = "hours since 1983-01-01 00:00:00",
                 vals = time, unlim = TRUE)
  var1 <- ncvar_def("SIS", "W m-2", list(x, y, t), -999, prec = "float", 
                    longname = "Surface Incoming Shortwave Radiation")
  vars <- list(var1)
  ncnew_1 <- nc_create(filename1, vars)
  ncnew_2 <- nc_create(filename2, vars)
  
  ncvar_put(ncnew_1, var1, data1)
  ncvar_put(ncnew_2, var1, data2)
  
  ncatt_put(ncnew_1, "lon", "standard_name", "longitude", prec = "text")
  ncatt_put(ncnew_1, "lat", "standard_name", "latitude", prec = "text")
  ncatt_put(ncnew_1, "SIS", "standard_name", "SIS_standard", prec = "text")
  
  ncatt_put(ncnew_2, "lon", "standard_name", "longitude", prec = "text")
  ncatt_put(ncnew_2, "lat", "standard_name", "latitude", prec = "text")
  ncatt_put(ncnew_2, "SIS", "standard_name", "SIS_standard", prec = "text")
  nc_close(ncnew_1)
  nc_close(ncnew_2)
}

create_examples_normal()
create_examples_tb()
create_examples_time()
create_example_time2()
create_different_lon_length()
create_additional_attr()
create_examples_v4()
create_dayrange()
create_divdpm()
create_extract.level()
create_extract.level2()
create_add_grid_info()
create_extract_period()
create_examples_levels()
create_examples_levels_time()
create_fld()
create_mon()
create_mon2()
create_examples_timestamp()
create_examples_na()
create_examples_non_regular()
create_wfldmean()
create_yday()
create_year()
create_ymon()
create_ymon_irregular()
create_yseas()
create_examples_timcumsum()
create_run_data()
create_cmsaf.abs()
create_dayx()
create_hourx()
create_tim_cor_covar()
create_fld_cor_covar()
create_gridboxx()
create_mon_day_mean()
# create_examples_remap()
# create_remap()
