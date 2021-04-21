data_dir <- file.path("..", "testdata")
tempfile_nc <- function() {
  tempfile_helper("dayavg_")
}

########## output ncdf version 3 ##########
file_out <- tempfile_nc()
dayavg("SIS", file.path(data_dir, "ex_dayx.nc"), file_out)
file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file)
  
  expected_data <- c(258.83334,259.83334,258.91666,259.91666,260.91666,260.0,261.0,
                     262.0,261.08334,262.08334,263.08334,262.16666,263.16666,262.25,
                     261.33334,262.33334,261.41666,260.5,261.5,260.58334,259.66666,
                     260.66666,259.75,258.83334,259.83334,258.91666,259.91666,260.91666,
                     260.0,261.0,262.0,261.08334,262.08334,263.08334,262.16666,
                     263.16666,262.25,261.33334,262.33334,261.41666,260.5,261.5,
                     260.58334,259.66666,260.66666,259.75,258.83334,259.83334,258.91666,
                     
                     261.08334,261.125,261.16666,261.20834,261.25,261.29166,261.33334,
                     261.375,261.41666,261.45834,260.54166,260.58334,260.625,260.66666,
                     260.70834,260.75,260.79166,260.83334,260.875,260.91666,260.95834,
                     261.0,261.04166,261.08334,261.125,261.16666,261.20834,261.25,
                     261.29166,261.33334,261.375,261.41666,261.45834,260.54166,260.58334,
                     260.625,260.66666,260.70834,260.75,260.79166,260.83334,260.875,
                     260.91666,260.95834,261.0,261.04166,261.08334,261.125,261.16666,
                     
                     261.0,260.23077,261.23077,260.46155,259.69232,260.69232,259.92307,
                     259.15384,260.15384,259.3846,260.3846,261.3846,260.6154,261.6154,
                     262.6154,261.84616,262.84616,262.07693,261.30768,262.30768,261.53845,
                     260.76923,261.76923,261.0,260.23077,261.23077,260.46155,259.69232,
                     260.69232,259.92307,259.15384,260.15384,259.3846,260.3846,261.3846,
                     260.6154,261.6154,262.6154,261.84616,262.84616,262.07693,261.30768,
                     262.30768,261.53845,260.76923,261.76923,261.0,260.23077,261.23077)
  expected <- array(expected_data, dim = c(7, 7, 3))
  
  expect_equivalent(actual, expected, tolerance = 1e-5)
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
  expect_equal(actual, "cmsaf::dayavg for variable SIS")
  
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
dayavg("SIS", file.path(data_dir, "ex_dayx.nc"), file_out, nc34 = 4)
file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file)
  
  expected_data <- c(258.83334,259.83334,258.91666,259.91666,260.91666,260.0,261.0,
                     262.0,261.08334,262.08334,263.08334,262.16666,263.16666,262.25,
                     261.33334,262.33334,261.41666,260.5,261.5,260.58334,259.66666,
                     260.66666,259.75,258.83334,259.83334,258.91666,259.91666,260.91666,
                     260.0,261.0,262.0,261.08334,262.08334,263.08334,262.16666,
                     263.16666,262.25,261.33334,262.33334,261.41666,260.5,261.5,
                     260.58334,259.66666,260.66666,259.75,258.83334,259.83334,258.91666,
                     
                     261.08334,261.125,261.16666,261.20834,261.25,261.29166,261.33334,
                     261.375,261.41666,261.45834,260.54166,260.58334,260.625,260.66666,
                     260.70834,260.75,260.79166,260.83334,260.875,260.91666,260.95834,
                     261.0,261.04166,261.08334,261.125,261.16666,261.20834,261.25,
                     261.29166,261.33334,261.375,261.41666,261.45834,260.54166,260.58334,
                     260.625,260.66666,260.70834,260.75,260.79166,260.83334,260.875,
                     260.91666,260.95834,261.0,261.04166,261.08334,261.125,261.16666,
                     
                     261.0,260.23077,261.23077,260.46155,259.69232,260.69232,259.92307,
                     259.15384,260.15384,259.3846,260.3846,261.3846,260.6154,261.6154,
                     262.6154,261.84616,262.84616,262.07693,261.30768,262.30768,261.53845,
                     260.76923,261.76923,261.0,260.23077,261.23077,260.46155,259.69232,
                     260.69232,259.92307,259.15384,260.15384,259.3846,260.3846,261.3846,
                     260.6154,261.6154,262.6154,261.84616,262.84616,262.07693,261.30768,
                     262.30768,261.53845,260.76923,261.76923,261.0,260.23077,261.23077)
  expected <- array(expected_data, dim = c(7, 7, 3))
  
  expect_equivalent(actual, expected, tolerance = 1e-5)
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
    dayavg("SIS", file.path(data_dir, "ex_dayx.nc"), file_out, nc34 = 7),
    "nc version must be in c(3, 4), but was 7", fixed = TRUE
  )
})

########## output ncdf version is NULL #########
file_out <- tempfile_nc()
test_that("ncdf version NULL throws an error", {
  expect_error(
    dayavg("SIS",
            file.path(data_dir, "ex_dayx.nc"),
            file_out, nc34 = NULL),
    "nc_version must not be NULL"
  )
})

########## variable does not exist #########
file_out <- tempfile_nc()

test_that("warning is shown if var does not exist", {
  expect_warning(dayavg("notExist",
                         file.path(data_dir, "ex_dayx.nc"),
                         file_out),
                 "Variable 'notExist' not found. Variable 'SIS' will be used instead.")
})

file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file)
  
  expected_data <- c(258.83334,259.83334,258.91666,259.91666,260.91666,260.0,261.0,
                     262.0,261.08334,262.08334,263.08334,262.16666,263.16666,262.25,
                     261.33334,262.33334,261.41666,260.5,261.5,260.58334,259.66666,
                     260.66666,259.75,258.83334,259.83334,258.91666,259.91666,260.91666,
                     260.0,261.0,262.0,261.08334,262.08334,263.08334,262.16666,
                     263.16666,262.25,261.33334,262.33334,261.41666,260.5,261.5,
                     260.58334,259.66666,260.66666,259.75,258.83334,259.83334,258.91666,
                     
                     261.08334,261.125,261.16666,261.20834,261.25,261.29166,261.33334,
                     261.375,261.41666,261.45834,260.54166,260.58334,260.625,260.66666,
                     260.70834,260.75,260.79166,260.83334,260.875,260.91666,260.95834,
                     261.0,261.04166,261.08334,261.125,261.16666,261.20834,261.25,
                     261.29166,261.33334,261.375,261.41666,261.45834,260.54166,260.58334,
                     260.625,260.66666,260.70834,260.75,260.79166,260.83334,260.875,
                     260.91666,260.95834,261.0,261.04166,261.08334,261.125,261.16666,
                     
                     261.0,260.23077,261.23077,260.46155,259.69232,260.69232,259.92307,
                     259.15384,260.15384,259.3846,260.3846,261.3846,260.6154,261.6154,
                     262.6154,261.84616,262.84616,262.07693,261.30768,262.30768,261.53845,
                     260.76923,261.76923,261.0,260.23077,261.23077,260.46155,259.69232,
                     260.69232,259.92307,259.15384,260.15384,259.3846,260.3846,261.3846,
                     260.6154,261.6154,262.6154,261.84616,262.84616,262.07693,261.30768,
                     262.30768,261.53845,260.76923,261.76923,261.0,260.23077,261.23077)
  expected <- array(expected_data, dim = c(7, 7, 3))
  
  expect_equivalent(actual, expected, tolerance = 1e-5)
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
    dayavg(NULL,
            file.path(data_dir, "ex_dayx.nc"),
            file_out),
    "variable must not be NULL"
  )
})

########## variable is empty #########
file_out <- tempfile_nc()

test_that("warning is shown if var is empty", {
  expect_warning(dayavg("",
                         file.path(data_dir, "ex_dayx.nc"),
                         file_out),
                 "Variable '' not found. Variable 'SIS' will be used instead.")
})

file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file)
  
  expected_data <- c(258.83334,259.83334,258.91666,259.91666,260.91666,260.0,261.0,
                     262.0,261.08334,262.08334,263.08334,262.16666,263.16666,262.25,
                     261.33334,262.33334,261.41666,260.5,261.5,260.58334,259.66666,
                     260.66666,259.75,258.83334,259.83334,258.91666,259.91666,260.91666,
                     260.0,261.0,262.0,261.08334,262.08334,263.08334,262.16666,
                     263.16666,262.25,261.33334,262.33334,261.41666,260.5,261.5,
                     260.58334,259.66666,260.66666,259.75,258.83334,259.83334,258.91666,
                     
                     261.08334,261.125,261.16666,261.20834,261.25,261.29166,261.33334,
                     261.375,261.41666,261.45834,260.54166,260.58334,260.625,260.66666,
                     260.70834,260.75,260.79166,260.83334,260.875,260.91666,260.95834,
                     261.0,261.04166,261.08334,261.125,261.16666,261.20834,261.25,
                     261.29166,261.33334,261.375,261.41666,261.45834,260.54166,260.58334,
                     260.625,260.66666,260.70834,260.75,260.79166,260.83334,260.875,
                     260.91666,260.95834,261.0,261.04166,261.08334,261.125,261.16666,
                     
                     261.0,260.23077,261.23077,260.46155,259.69232,260.69232,259.92307,
                     259.15384,260.15384,259.3846,260.3846,261.3846,260.6154,261.6154,
                     262.6154,261.84616,262.84616,262.07693,261.30768,262.30768,261.53845,
                     260.76923,261.76923,261.0,260.23077,261.23077,260.46155,259.69232,
                     260.69232,259.92307,259.15384,260.15384,259.3846,260.3846,261.3846,
                     260.6154,261.6154,262.6154,261.84616,262.84616,262.07693,261.30768,
                     262.30768,261.53845,260.76923,261.76923,261.0,260.23077,261.23077)
  expected <- array(expected_data, dim = c(7, 7, 3))
  
  expect_equivalent(actual, expected, tolerance = 1e-5)
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
    dayavg("SIS",
            file.path(data_dir, "xemaple1.nc"),
            file_out),
    "Input file does not exist")
})

########## input filename is empty #########
file_out <- tempfile_nc()
test_that("error is thrown if input filename is empty", {
  expect_error(
    dayavg("SIS", "", file_out),
    "Input file does not exist")
})

########## input filename is NULL #########
file_out <- tempfile_nc()

test_that("error is thrown if input filename is NULL", {
  expect_error(
    dayavg("SIS", NULL, file_out),
    "Input filepath must be of length one and not NULL"
  )
})

########## output file already exists #########
file_out <- tempfile_nc()
cat("test\n", file = file_out)

test_that("error is thrown if output file already exists", {
  expect_error(
    dayavg("SIS",
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
    dayavg("SIS",
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
  
  expected_data <- c(258.83334,259.83334,258.91666,259.91666,260.91666,260.0,261.0,
                     262.0,261.08334,262.08334,263.08334,262.16666,263.16666,262.25,
                     261.33334,262.33334,261.41666,260.5,261.5,260.58334,259.66666,
                     260.66666,259.75,258.83334,259.83334,258.91666,259.91666,260.91666,
                     260.0,261.0,262.0,261.08334,262.08334,263.08334,262.16666,
                     263.16666,262.25,261.33334,262.33334,261.41666,260.5,261.5,
                     260.58334,259.66666,260.66666,259.75,258.83334,259.83334,258.91666,
                     
                     261.08334,261.125,261.16666,261.20834,261.25,261.29166,261.33334,
                     261.375,261.41666,261.45834,260.54166,260.58334,260.625,260.66666,
                     260.70834,260.75,260.79166,260.83334,260.875,260.91666,260.95834,
                     261.0,261.04166,261.08334,261.125,261.16666,261.20834,261.25,
                     261.29166,261.33334,261.375,261.41666,261.45834,260.54166,260.58334,
                     260.625,260.66666,260.70834,260.75,260.79166,260.83334,260.875,
                     260.91666,260.95834,261.0,261.04166,261.08334,261.125,261.16666,
                     
                     261.0,260.23077,261.23077,260.46155,259.69232,260.69232,259.92307,
                     259.15384,260.15384,259.3846,260.3846,261.3846,260.6154,261.6154,
                     262.6154,261.84616,262.84616,262.07693,261.30768,262.30768,261.53845,
                     260.76923,261.76923,261.0,260.23077,261.23077,260.46155,259.69232,
                     260.69232,259.92307,259.15384,260.15384,259.3846,260.3846,261.3846,
                     260.6154,261.6154,262.6154,261.84616,262.84616,262.07693,261.30768,
                     262.30768,261.53845,260.76923,261.76923,261.0,260.23077,261.23077)
  expected <- array(expected_data, dim = c(7, 7, 3))
  
  expect_equivalent(actual, expected, tolerance = 1e-5)
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
