data_dir <- file.path("..", "testdata")
tempfile_nc <- function() {
  tempfile_helper("daysd_")
}

########## output ncdf version 3 ##########
file_out <- tempfile_nc()
daysd("SIS", file.path(data_dir, "ex_dayx.nc"), file_out)
file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file)
  
  expected_data <- c(6.6719675,6.6719675,6.1416364,6.1416364,6.1416364,5.954372,5.954372,
                     5.954372,6.1416364,6.1416364,6.1416364,6.6719675,6.6719675,7.187426,
                     7.414155,7.414155,7.656944,7.6455574,7.6455574,7.656944,7.414155,
                     7.414155,7.187426,6.6719675,6.6719675,6.1416364,6.1416364,6.1416364,
                     5.954372,5.954372,5.954372,6.1416364,6.1416364,6.1416364,6.6719675,
                     6.6719675,7.187426,7.414155,7.414155,7.656944,7.6455574,7.6455574,
                     7.656944,7.414155,7.414155,7.187426,6.6719675,6.6719675,6.1416364,
                     
                     6.6458006,6.661456,6.6833124,6.711309,6.745369,6.785401,6.8313007,
                     6.88295,6.940221,7.0029755,7.0029755,6.940221,6.88295,6.8313007,
                     6.785401,6.745369,6.711309,6.6833124,6.661456,6.6458006,6.6363897,
                     6.6332498,6.6363897,6.6458006,6.661456,6.6833124,6.711309,6.745369,
                     6.785401,6.8313007,6.88295,6.940221,7.0029755,7.0029755,6.940221,
                     6.88295,6.8313007,6.785401,6.745369,6.711309,6.6833124,6.661456,
                     6.6458006,6.6363897,6.6332498,6.6363897,6.6458006,6.661456,6.6833124,
                     
                     7.483315,7.384148,7.384148,7.344106,7.0991154,7.0991154,6.9096737,
                     6.491612,6.491612,6.11744,6.11744,6.11744,6.11744,6.11744,
                     6.11744,6.491612,6.491612,6.9096737,7.0991154,7.0991154,7.344106,
                     7.384148,7.384148,7.483315,7.384148,7.384148,7.344106,7.0991154,
                     7.0991154,6.9096737,6.491612,6.491612,6.11744,6.11744,6.11744,
                     6.11744,6.11744,6.11744,6.491612,6.491612,6.9096737,7.0991154,
                     7.0991154,7.344106,7.384148,7.384148,7.483315,7.384148,7.384148)
  expected <- array(expected_data, dim = c(7, 7, 3))
  
  expect_equivalent(actual, expected)
})

test_that("variable attributes are correct", {
  actual <- ncatt_get(file, "SIS", "units")$value
  expect_equal(actual, "W m-2")
  
  actual <- ncatt_get(file, "SIS", "_FillValue")$value
  expect_equal(actual, -999)
  
  actual <- ncatt_get(file, "SIS", "standard_name")$value
  expect_equal(actual, "SIS_standard")
  
  actual <- ncatt_get(file, "SIS", "long_name")$value
  expect_equal(actual, "Surface Incoming Shortwave Radiation")
  
  actual <- ncatt_get(file, "SIS", "missing_value")$value
  expect_equal(actual, 0)
})

test_that("attributes are correct", {
  actual <- ncatt_get(file, "lon", "units")$value
  expect_equal(actual, "degrees_east")
  
  actual <- ncatt_get(file, "lon", "long_name")$value
  expect_equal(actual, "longitude")
  
  actual <- ncatt_get(file, "lon", "standard_name")$value
  expect_equal(actual, "longitude")
  
  actual <- ncatt_get(file, "lon", "axis")$value
  expect_equal(actual, "X")
  
  actual <- ncatt_get(file, "lat", "units")$value
  expect_equal(actual, "degrees_north")
  
  actual <- ncatt_get(file, "lat", "long_name")$value
  expect_equal(actual, "latitude")
  
  actual <- ncatt_get(file, "lat", "standard_name")$value
  expect_equal(actual, "latitude")
  
  actual <- ncatt_get(file, "lat", "axis")$value
  expect_equal(actual, "Y")
  
  actual <- ncatt_get(file, "time", "units")$value
  expect_equal(actual, "hours since 1983-01-01 00:00:00")
  
  actual <- ncatt_get(file, "time", "long_name")$value
  expect_equal(actual, "time")
  
  actual <- ncatt_get(file, "time", "standard_name")$value
  expect_equal(actual, "time")
  
  actual <- ncatt_get(file, "time", "calendar")$value
  expect_equal(actual, "standard")
  
  actual <- ncatt_get(file, "SIS", "standard_name")$value
  expect_equal(actual, "SIS_standard")
  
  actual <- ncatt_get(file, "SIS", "long_name")$value
  expect_equal(actual, "Surface Incoming Shortwave Radiation")
  
  actual <- ncatt_get(file, "SIS", "units")$value
  expect_equal(actual, "W m-2")
  
  actual <- ncatt_get(file, "SIS", "_FillValue")$value
  expect_equal(actual, -999)
  
  actual <- ncatt_get(file, "SIS", "cmsaf_info")$value
  expect_equal(actual, "cmsaf::daysd for variable SIS")
  
  global_attr <- ncatt_get(file, 0)
  expect_equal(length(global_attr), 1)
  
  actual <- names(global_attr[1])
  expect_equal(actual, "Info")
  
  actual <- global_attr[[1]]
  expect_equal(actual, "Created with the CM SAF R Toolbox.")
})

test_that("coordinates are correct", {
  actual <- ncvar_get(file, "lon")
  expect_identical(actual, array(seq(5, 8, 0.5)))
  
  actual <- ncvar_get(file, "lat")
  expect_identical(actual, array(seq(45, 48, 0.5)))
  
  actual <- ncvar_get(file, "time")
  expect_equal(actual, array(c(149028, 149040, 149064)))
})

nc_close(file)

########## output ncdf version 4 ##########
file_out <- tempfile_nc()
daysd("SIS", file.path(data_dir, "ex_dayx.nc"), file_out, nc34 = 4)
file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file)
  
  expected_data <- c(6.6719675,6.6719675,6.1416364,6.1416364,6.1416364,5.954372,5.954372,
                     5.954372,6.1416364,6.1416364,6.1416364,6.6719675,6.6719675,7.187426,
                     7.414155,7.414155,7.656944,7.6455574,7.6455574,7.656944,7.414155,
                     7.414155,7.187426,6.6719675,6.6719675,6.1416364,6.1416364,6.1416364,
                     5.954372,5.954372,5.954372,6.1416364,6.1416364,6.1416364,6.6719675,
                     6.6719675,7.187426,7.414155,7.414155,7.656944,7.6455574,7.6455574,
                     7.656944,7.414155,7.414155,7.187426,6.6719675,6.6719675,6.1416364,
                     
                     6.6458006,6.661456,6.6833124,6.711309,6.745369,6.785401,6.8313007,
                     6.88295,6.940221,7.0029755,7.0029755,6.940221,6.88295,6.8313007,
                     6.785401,6.745369,6.711309,6.6833124,6.661456,6.6458006,6.6363897,
                     6.6332498,6.6363897,6.6458006,6.661456,6.6833124,6.711309,6.745369,
                     6.785401,6.8313007,6.88295,6.940221,7.0029755,7.0029755,6.940221,
                     6.88295,6.8313007,6.785401,6.745369,6.711309,6.6833124,6.661456,
                     6.6458006,6.6363897,6.6332498,6.6363897,6.6458006,6.661456,6.6833124,
                     
                     7.483315,7.384148,7.384148,7.344106,7.0991154,7.0991154,6.9096737,
                     6.491612,6.491612,6.11744,6.11744,6.11744,6.11744,6.11744,
                     6.11744,6.491612,6.491612,6.9096737,7.0991154,7.0991154,7.344106,
                     7.384148,7.384148,7.483315,7.384148,7.384148,7.344106,7.0991154,
                     7.0991154,6.9096737,6.491612,6.491612,6.11744,6.11744,6.11744,
                     6.11744,6.11744,6.11744,6.491612,6.491612,6.9096737,7.0991154,
                     7.0991154,7.344106,7.384148,7.384148,7.483315,7.384148,7.384148)
  expected <- array(expected_data, dim = c(7, 7, 3))
  
  expect_equivalent(actual, expected)
})

test_that("variable attributes are correct", {
  actual <- ncatt_get(file, "SIS", "units")$value
  expect_equal(actual, "W m-2")
  
  actual <- ncatt_get(file, "SIS", "_FillValue")$value
  expect_equal(actual, -999)
  
  actual <- ncatt_get(file, "SIS", "standard_name")$value
  expect_equal(actual, "SIS_standard")
  
  actual <- ncatt_get(file, "SIS", "long_name")$value
  expect_equal(actual, "Surface Incoming Shortwave Radiation")
  
  actual <- ncatt_get(file, "SIS", "missing_value")$value
  expect_equal(actual, 0)
})

test_that("attributes are correct", {
  actual <- ncatt_get(file, "lon", "units")$value
  expect_equal(actual, "degrees_east")
  
  actual <- ncatt_get(file, "lon", "long_name")$value
  expect_equal(actual, "longitude")
  
  actual <- ncatt_get(file, "lon", "standard_name")$value
  expect_equal(actual, "longitude")
  
  actual <- ncatt_get(file, "lon", "axis")$value
  expect_equal(actual, "X")
  
  actual <- ncatt_get(file, "lat", "units")$value
  expect_equal(actual, "degrees_north")
  
  actual <- ncatt_get(file, "lat", "long_name")$value
  expect_equal(actual, "latitude")
  
  actual <- ncatt_get(file, "lat", "standard_name")$value
  expect_equal(actual, "latitude")
  
  actual <- ncatt_get(file, "lat", "axis")$value
  expect_equal(actual, "Y")
  
  actual <- ncatt_get(file, "time", "units")$value
  expect_equal(actual, "hours since 1983-01-01 00:00:00")
  
  actual <- ncatt_get(file, "time", "long_name")$value
  expect_equal(actual, "time")
  
  actual <- ncatt_get(file, "time", "standard_name")$value
  expect_equal(actual, "time")
  
  actual <- ncatt_get(file, "time", "calendar")$value
  expect_equal(actual, "standard")
  
  global_attr <- ncatt_get(file, 0)
  expect_equal(length(global_attr), 1)
  
  actual <- names(global_attr[1])
  expect_equal(actual, "Info")
  
  actual <- global_attr[[1]]
  expect_equal(actual, "Created with the CM SAF R Toolbox.")
})

test_that("coordinates are correct", {
  actual <- ncvar_get(file, "lon")
  expect_identical(actual, array(seq(5, 8, 0.5)))
  
  actual <- ncvar_get(file, "lat")
  expect_identical(actual, array(seq(45, 48, 0.5)))
  
  actual <- ncvar_get(file, "time")
  expect_equal(actual, array(c(149028, 149040, 149064)))
})

nc_close(file)

########## output ncdf version 7 #########
file_out <- tempfile_nc()
test_that("error is thrown if ncdf version is wrong", {
  expect_error(
    daysd("SIS", file.path(data_dir, "ex_dayx.nc"), file_out, nc34 = 7),
    "nc version must be in c(3, 4), but was 7", fixed = TRUE
  )
})

########## output ncdf version is NULL #########
file_out <- tempfile_nc()
test_that("ncdf version NULL throws an error", {
  expect_error(
    daysd("SIS",
           file.path(data_dir, "ex_dayx.nc"),
           file_out, nc34 = NULL),
    "nc_version must not be NULL"
  )
})

########## variable does not exist #########
file_out <- tempfile_nc()

test_that("warning is shown if var does not exist", {
  expect_warning(daysd("notExist",
                        file.path(data_dir, "ex_dayx.nc"),
                        file_out),
                 "Variable 'notExist' not found. Variable 'SIS' will be used instead.")
})

file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file)
  
  expected_data <- c(6.6719675,6.6719675,6.1416364,6.1416364,6.1416364,5.954372,5.954372,
                     5.954372,6.1416364,6.1416364,6.1416364,6.6719675,6.6719675,7.187426,
                     7.414155,7.414155,7.656944,7.6455574,7.6455574,7.656944,7.414155,
                     7.414155,7.187426,6.6719675,6.6719675,6.1416364,6.1416364,6.1416364,
                     5.954372,5.954372,5.954372,6.1416364,6.1416364,6.1416364,6.6719675,
                     6.6719675,7.187426,7.414155,7.414155,7.656944,7.6455574,7.6455574,
                     7.656944,7.414155,7.414155,7.187426,6.6719675,6.6719675,6.1416364,
                     
                     6.6458006,6.661456,6.6833124,6.711309,6.745369,6.785401,6.8313007,
                     6.88295,6.940221,7.0029755,7.0029755,6.940221,6.88295,6.8313007,
                     6.785401,6.745369,6.711309,6.6833124,6.661456,6.6458006,6.6363897,
                     6.6332498,6.6363897,6.6458006,6.661456,6.6833124,6.711309,6.745369,
                     6.785401,6.8313007,6.88295,6.940221,7.0029755,7.0029755,6.940221,
                     6.88295,6.8313007,6.785401,6.745369,6.711309,6.6833124,6.661456,
                     6.6458006,6.6363897,6.6332498,6.6363897,6.6458006,6.661456,6.6833124,
                     
                     7.483315,7.384148,7.384148,7.344106,7.0991154,7.0991154,6.9096737,
                     6.491612,6.491612,6.11744,6.11744,6.11744,6.11744,6.11744,
                     6.11744,6.491612,6.491612,6.9096737,7.0991154,7.0991154,7.344106,
                     7.384148,7.384148,7.483315,7.384148,7.384148,7.344106,7.0991154,
                     7.0991154,6.9096737,6.491612,6.491612,6.11744,6.11744,6.11744,
                     6.11744,6.11744,6.11744,6.491612,6.491612,6.9096737,7.0991154,
                     7.0991154,7.344106,7.384148,7.384148,7.483315,7.384148,7.384148)
  expected <- array(expected_data, dim = c(7, 7, 3))
  
  expect_equivalent(actual, expected)
})

test_that("variable attributes are correct", {
  actual <- ncatt_get(file, "SIS", "units")$value
  expect_equal(actual, "W m-2")
  
  actual <- ncatt_get(file, "SIS", "_FillValue")$value
  expect_equal(actual, -999)
  
  actual <- ncatt_get(file, "SIS", "standard_name")$value
  expect_equal(actual, "SIS_standard")
  
  actual <- ncatt_get(file, "SIS", "long_name")$value
  expect_equal(actual, "Surface Incoming Shortwave Radiation")
  
  actual <- ncatt_get(file, "SIS", "missing_value")$value
  expect_equal(actual, 0)
})

test_that("attributes are correct", {
  actual <- ncatt_get(file, "lon", "units")$value
  expect_equal(actual, "degrees_east")
  
  actual <- ncatt_get(file, "lon", "long_name")$value
  expect_equal(actual, "longitude")
  
  actual <- ncatt_get(file, "lon", "standard_name")$value
  expect_equal(actual, "longitude")
  
  actual <- ncatt_get(file, "lon", "axis")$value
  expect_equal(actual, "X")
  
  actual <- ncatt_get(file, "lat", "units")$value
  expect_equal(actual, "degrees_north")
  
  actual <- ncatt_get(file, "lat", "long_name")$value
  expect_equal(actual, "latitude")
  
  actual <- ncatt_get(file, "lat", "standard_name")$value
  expect_equal(actual, "latitude")
  
  actual <- ncatt_get(file, "lat", "axis")$value
  expect_equal(actual, "Y")
  
  actual <- ncatt_get(file, "time", "units")$value
  expect_equal(actual, "hours since 1983-01-01 00:00:00")
  
  actual <- ncatt_get(file, "time", "long_name")$value
  expect_equal(actual, "time")
  
  actual <- ncatt_get(file, "time", "standard_name")$value
  expect_equal(actual, "time")
  
  actual <- ncatt_get(file, "time", "calendar")$value
  expect_equal(actual, "standard")
  
  global_attr <- ncatt_get(file, 0)
  expect_equal(length(global_attr), 1)
  
  actual <- names(global_attr[1])
  expect_equal(actual, "Info")
  
  actual <- global_attr[[1]]
  expect_equal(actual, "Created with the CM SAF R Toolbox.")
})

test_that("coordinates are correct", {
  actual <- ncvar_get(file, "lon")
  expect_identical(actual, array(seq(5, 8, 0.5)))
  
  actual <- ncvar_get(file, "lat")
  expect_identical(actual, array(seq(45, 48, 0.5)))
  
  actual <- ncvar_get(file, "time")
  expect_equal(actual, array(c(149028, 149040, 149064)))
})

nc_close(file)

########## variable is null #########
file_out <- tempfile_nc()
test_that("error is thrown if variable is NULL", {
  expect_error(
    daysd(NULL,
           file.path(data_dir, "ex_dayx.nc"),
           file_out),
    "variable must not be NULL"
  )
})

########## variable is empty #########
file_out <- tempfile_nc()

test_that("warning is shown if var is empty", {
  expect_warning(daysd("",
                        file.path(data_dir, "ex_dayx.nc"),
                        file_out),
                 "Variable '' not found. Variable 'SIS' will be used instead.")
})

file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file)
  
  expected_data <- c(6.6719675,6.6719675,6.1416364,6.1416364,6.1416364,5.954372,5.954372,
                     5.954372,6.1416364,6.1416364,6.1416364,6.6719675,6.6719675,7.187426,
                     7.414155,7.414155,7.656944,7.6455574,7.6455574,7.656944,7.414155,
                     7.414155,7.187426,6.6719675,6.6719675,6.1416364,6.1416364,6.1416364,
                     5.954372,5.954372,5.954372,6.1416364,6.1416364,6.1416364,6.6719675,
                     6.6719675,7.187426,7.414155,7.414155,7.656944,7.6455574,7.6455574,
                     7.656944,7.414155,7.414155,7.187426,6.6719675,6.6719675,6.1416364,
                     
                     6.6458006,6.661456,6.6833124,6.711309,6.745369,6.785401,6.8313007,
                     6.88295,6.940221,7.0029755,7.0029755,6.940221,6.88295,6.8313007,
                     6.785401,6.745369,6.711309,6.6833124,6.661456,6.6458006,6.6363897,
                     6.6332498,6.6363897,6.6458006,6.661456,6.6833124,6.711309,6.745369,
                     6.785401,6.8313007,6.88295,6.940221,7.0029755,7.0029755,6.940221,
                     6.88295,6.8313007,6.785401,6.745369,6.711309,6.6833124,6.661456,
                     6.6458006,6.6363897,6.6332498,6.6363897,6.6458006,6.661456,6.6833124,
                     
                     7.483315,7.384148,7.384148,7.344106,7.0991154,7.0991154,6.9096737,
                     6.491612,6.491612,6.11744,6.11744,6.11744,6.11744,6.11744,
                     6.11744,6.491612,6.491612,6.9096737,7.0991154,7.0991154,7.344106,
                     7.384148,7.384148,7.483315,7.384148,7.384148,7.344106,7.0991154,
                     7.0991154,6.9096737,6.491612,6.491612,6.11744,6.11744,6.11744,
                     6.11744,6.11744,6.11744,6.491612,6.491612,6.9096737,7.0991154,
                     7.0991154,7.344106,7.384148,7.384148,7.483315,7.384148,7.384148)
  expected <- array(expected_data, dim = c(7, 7, 3))
  
  expect_equivalent(actual, expected)
})

test_that("variable attributes are correct", {
  actual <- ncatt_get(file, "SIS", "units")$value
  expect_equal(actual, "W m-2")
  
  actual <- ncatt_get(file, "SIS", "_FillValue")$value
  expect_equal(actual, -999)
  
  actual <- ncatt_get(file, "SIS", "standard_name")$value
  expect_equal(actual, "SIS_standard")
  
  actual <- ncatt_get(file, "SIS", "long_name")$value
  expect_equal(actual, "Surface Incoming Shortwave Radiation")
  
  actual <- ncatt_get(file, "SIS", "missing_value")$value
  expect_equal(actual, 0)
})

test_that("attributes are correct", {
  actual <- ncatt_get(file, "lon", "units")$value
  expect_equal(actual, "degrees_east")
  
  actual <- ncatt_get(file, "lon", "long_name")$value
  expect_equal(actual, "longitude")
  
  actual <- ncatt_get(file, "lon", "standard_name")$value
  expect_equal(actual, "longitude")
  
  actual <- ncatt_get(file, "lon", "axis")$value
  expect_equal(actual, "X")
  
  actual <- ncatt_get(file, "lat", "units")$value
  expect_equal(actual, "degrees_north")
  
  actual <- ncatt_get(file, "lat", "long_name")$value
  expect_equal(actual, "latitude")
  
  actual <- ncatt_get(file, "lat", "standard_name")$value
  expect_equal(actual, "latitude")
  
  actual <- ncatt_get(file, "lat", "axis")$value
  expect_equal(actual, "Y")
  
  actual <- ncatt_get(file, "time", "units")$value
  expect_equal(actual, "hours since 1983-01-01 00:00:00")
  
  actual <- ncatt_get(file, "time", "long_name")$value
  expect_equal(actual, "time")
  
  actual <- ncatt_get(file, "time", "standard_name")$value
  expect_equal(actual, "time")
  
  actual <- ncatt_get(file, "time", "calendar")$value
  expect_equal(actual, "standard")
  
  global_attr <- ncatt_get(file, 0)
  expect_equal(length(global_attr), 1)
  
  actual <- names(global_attr[1])
  expect_equal(actual, "Info")
  
  actual <- global_attr[[1]]
  expect_equal(actual, "Created with the CM SAF R Toolbox.")
})

test_that("coordinates are correct", {
  actual <- ncvar_get(file, "lon")
  expect_identical(actual, array(seq(5, 8, 0.5)))
  
  actual <- ncvar_get(file, "lat")
  expect_identical(actual, array(seq(45, 48, 0.5)))
  
  actual <- ncvar_get(file, "time")
  expect_equal(actual, array(c(149028, 149040, 149064)))
})

nc_close(file)

########## input file does not exist #########
file_out <- tempfile_nc()
test_that("error is thrown if input file does not exist", {
  expect_error(
    daysd("SIS",
           file.path(data_dir, "xemaple1.nc"),
           file_out),
    "Input file does not exist")
})

########## input filename is empty #########
file_out <- tempfile_nc()
test_that("error is thrown if input filename is empty", {
  expect_error(
    daysd("SIS", "", file_out),
    "Input file does not exist")
})

########## input filename is NULL #########
file_out <- tempfile_nc()

test_that("error is thrown if input filename is NULL", {
  expect_error(
    daysd("SIS", NULL, file_out),
    "Input filepath must be of length one and not NULL"
  )
})

########## output file already exists #########
file_out <- tempfile_nc()
cat("test\n", file = file_out)

test_that("error is thrown if output file already exists", {
  expect_error(
    daysd("SIS",
           file.path(data_dir, "ex_dayx.nc"),
           file_out),
    paste0("File '",
           file_out,
           "' already exists. Specify 'overwrite = TRUE' if you want to overwrite it."),
    fixed = TRUE
  )
  
  expect_equal(readLines(con = file_out), "test")
})


########## output file already exists (overwrite = TRUE) #########
file_out <- tempfile_nc()
cat("test\n", file = file_out)

test_that("no error is thrown if overwrite = TRUE", {
  expect_error(
    daysd("SIS",
           file.path(data_dir, "ex_dayx.nc"),
           file_out,
           overwrite = TRUE
    ),
    NA
  )
})

file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file)
  
  expected_data <- c(6.6719675,6.6719675,6.1416364,6.1416364,6.1416364,5.954372,5.954372,
                     5.954372,6.1416364,6.1416364,6.1416364,6.6719675,6.6719675,7.187426,
                     7.414155,7.414155,7.656944,7.6455574,7.6455574,7.656944,7.414155,
                     7.414155,7.187426,6.6719675,6.6719675,6.1416364,6.1416364,6.1416364,
                     5.954372,5.954372,5.954372,6.1416364,6.1416364,6.1416364,6.6719675,
                     6.6719675,7.187426,7.414155,7.414155,7.656944,7.6455574,7.6455574,
                     7.656944,7.414155,7.414155,7.187426,6.6719675,6.6719675,6.1416364,
                     
                     6.6458006,6.661456,6.6833124,6.711309,6.745369,6.785401,6.8313007,
                     6.88295,6.940221,7.0029755,7.0029755,6.940221,6.88295,6.8313007,
                     6.785401,6.745369,6.711309,6.6833124,6.661456,6.6458006,6.6363897,
                     6.6332498,6.6363897,6.6458006,6.661456,6.6833124,6.711309,6.745369,
                     6.785401,6.8313007,6.88295,6.940221,7.0029755,7.0029755,6.940221,
                     6.88295,6.8313007,6.785401,6.745369,6.711309,6.6833124,6.661456,
                     6.6458006,6.6363897,6.6332498,6.6363897,6.6458006,6.661456,6.6833124,
                     
                     7.483315,7.384148,7.384148,7.344106,7.0991154,7.0991154,6.9096737,
                     6.491612,6.491612,6.11744,6.11744,6.11744,6.11744,6.11744,
                     6.11744,6.491612,6.491612,6.9096737,7.0991154,7.0991154,7.344106,
                     7.384148,7.384148,7.483315,7.384148,7.384148,7.344106,7.0991154,
                     7.0991154,6.9096737,6.491612,6.491612,6.11744,6.11744,6.11744,
                     6.11744,6.11744,6.11744,6.491612,6.491612,6.9096737,7.0991154,
                     7.0991154,7.344106,7.384148,7.384148,7.483315,7.384148,7.384148)
  expected <- array(expected_data, dim = c(7, 7, 3))
  
  expect_equivalent(actual, expected)
})

test_that("variable attributes are correct", {
  actual <- ncatt_get(file, "SIS", "units")$value
  expect_equal(actual, "W m-2")
  
  actual <- ncatt_get(file, "SIS", "_FillValue")$value
  expect_equal(actual, -999)
  
  actual <- ncatt_get(file, "SIS", "standard_name")$value
  expect_equal(actual, "SIS_standard")
  
  actual <- ncatt_get(file, "SIS", "long_name")$value
  expect_equal(actual, "Surface Incoming Shortwave Radiation")
  
  actual <- ncatt_get(file, "SIS", "missing_value")$value
  expect_equal(actual, 0)
})

test_that("attributes are correct", {
  actual <- ncatt_get(file, "lon", "units")$value
  expect_equal(actual, "degrees_east")
  
  actual <- ncatt_get(file, "lon", "long_name")$value
  expect_equal(actual, "longitude")
  
  actual <- ncatt_get(file, "lon", "standard_name")$value
  expect_equal(actual, "longitude")
  
  actual <- ncatt_get(file, "lon", "axis")$value
  expect_equal(actual, "X")
  
  actual <- ncatt_get(file, "lat", "units")$value
  expect_equal(actual, "degrees_north")
  
  actual <- ncatt_get(file, "lat", "long_name")$value
  expect_equal(actual, "latitude")
  
  actual <- ncatt_get(file, "lat", "standard_name")$value
  expect_equal(actual, "latitude")
  
  actual <- ncatt_get(file, "lat", "axis")$value
  expect_equal(actual, "Y")
  
  actual <- ncatt_get(file, "time", "units")$value
  expect_equal(actual, "hours since 1983-01-01 00:00:00")
  
  actual <- ncatt_get(file, "time", "long_name")$value
  expect_equal(actual, "time")
  
  actual <- ncatt_get(file, "time", "standard_name")$value
  expect_equal(actual, "time")
  
  actual <- ncatt_get(file, "time", "calendar")$value
  expect_equal(actual, "standard")
  
  global_attr <- ncatt_get(file, 0)
  expect_equal(length(global_attr), 1)
  
  actual <- names(global_attr[1])
  expect_equal(actual, "Info")
  
  actual <- global_attr[[1]]
  expect_equal(actual, "Created with the CM SAF R Toolbox.")
})

test_that("coordinates are correct", {
  actual <- ncvar_get(file, "lon")
  expect_identical(actual, array(seq(5, 8, 0.5)))
  
  actual <- ncvar_get(file, "lat")
  expect_identical(actual, array(seq(45, 48, 0.5)))
  
  actual <- ncvar_get(file, "time")
  expect_equal(actual, array(c(149028, 149040, 149064)))
})

nc_close(file)
