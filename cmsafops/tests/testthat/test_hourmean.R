data_dir <- file.path("..", "testdata")
tempfile_nc <- function() {
  tempfile_helper("hourmean_")
}

########## output ncdf version 3 ##########
file_out <- tempfile_nc()
hourmean("SIS", file.path(data_dir, "ex_hourx.nc"), file_out)
file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file)
  
  expected_data <- c(301.7,302.7,302.01666,301.33334,302.33334,301.65,300.96667,
                     301.96667,301.28333,300.6,301.6,300.91666,300.23334,301.23334,
                     300.55,299.86667,300.86667,300.18332,299.5,300.5,299.81668,
                     299.13333,300.13333,299.45,298.76666,299.76666,299.08334,298.4,
                     299.4,298.71667,298.03333,299.03333,298.35,297.66666,298.66666,
                     297.98334,297.3,298.3,297.61667,296.93332,297.93332,298.93332,
                     298.25,299.25,300.25,299.56668,300.56668,301.56668,300.88333,
                     
                     300.91666,300.23334,301.23334,300.55,299.86667,300.86667,300.18332,
                     299.5,300.5,299.81668,299.13333,300.13333,299.45,298.76666,
                     299.76666,299.08334,298.4,299.4,298.71667,298.03333,299.03333,
                     298.35,297.66666,298.66666,297.98334,297.3,298.3,297.61667,
                     296.93332,297.93332,298.93332,298.25,299.25,300.25,299.56668,
                     300.56668,301.56668,300.88333,301.88333,302.88333,302.2,301.51666,
                     302.51666,301.83334,301.15,302.15,301.46667,300.78333,301.78333,
                     
                     300.13333,299.45,298.76666,299.76666,299.08334,298.4,299.4,
                     298.71667,298.03333,299.03333,298.35,297.66666,298.66666,297.98334,
                     297.3,298.3,297.61667,296.93332,297.93332,298.93332,298.25,
                     299.25,300.25,299.56668,300.56668,301.56668,300.88333,301.88333,
                     302.88333,302.2,301.51666,302.51666,301.83334,301.15,302.15,
                     301.46667,300.78333,301.78333,301.1,300.41666,301.41666,300.73334,
                     300.05,301.05,300.36667,299.68332,300.68332,300.0,299.31668)
  expected <- array(expected_data, dim = c(7, 7, 3))
  
  expect_equivalent(actual[1:147], expected, tolerance = 1e-4)
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
  expect_equal(actual, "minutes since 1983-01-01 00:00:00")
  
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
  expect_equal(actual, "cmsaf::hourmean for variable SIS")
  
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
  expect_equal(actual[1:3], array(c(8941680, 8941740, 8941800)))
})

nc_close(file)

########## output ncdf version 4 ##########
file_out <- tempfile_nc()
hourmean("SIS", file.path(data_dir, "ex_hourx.nc"), file_out, nc34 = 4)
file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file)
  
  expected_data <- c(301.7,302.7,302.01666,301.33334,302.33334,301.65,300.96667,
                     301.96667,301.28333,300.6,301.6,300.91666,300.23334,301.23334,
                     300.55,299.86667,300.86667,300.18332,299.5,300.5,299.81668,
                     299.13333,300.13333,299.45,298.76666,299.76666,299.08334,298.4,
                     299.4,298.71667,298.03333,299.03333,298.35,297.66666,298.66666,
                     297.98334,297.3,298.3,297.61667,296.93332,297.93332,298.93332,
                     298.25,299.25,300.25,299.56668,300.56668,301.56668,300.88333,
                     
                     300.91666,300.23334,301.23334,300.55,299.86667,300.86667,300.18332,
                     299.5,300.5,299.81668,299.13333,300.13333,299.45,298.76666,
                     299.76666,299.08334,298.4,299.4,298.71667,298.03333,299.03333,
                     298.35,297.66666,298.66666,297.98334,297.3,298.3,297.61667,
                     296.93332,297.93332,298.93332,298.25,299.25,300.25,299.56668,
                     300.56668,301.56668,300.88333,301.88333,302.88333,302.2,301.51666,
                     302.51666,301.83334,301.15,302.15,301.46667,300.78333,301.78333,
                     
                     300.13333,299.45,298.76666,299.76666,299.08334,298.4,299.4,
                     298.71667,298.03333,299.03333,298.35,297.66666,298.66666,297.98334,
                     297.3,298.3,297.61667,296.93332,297.93332,298.93332,298.25,
                     299.25,300.25,299.56668,300.56668,301.56668,300.88333,301.88333,
                     302.88333,302.2,301.51666,302.51666,301.83334,301.15,302.15,
                     301.46667,300.78333,301.78333,301.1,300.41666,301.41666,300.73334,
                     300.05,301.05,300.36667,299.68332,300.68332,300.0,299.31668)
  expected <- array(expected_data, dim = c(7, 7, 3))
  
  expect_equivalent(actual[1:147], expected, tolerance = 1e-4)
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
  expect_equal(actual, "minutes since 1983-01-01 00:00:00")
  
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
  expect_equal(actual[1:3], array(c(8941680, 8941740, 8941800)))
})

nc_close(file)

########## output ncdf version 7 #########
file_out <- tempfile_nc()
test_that("error is thrown if ncdf version is wrong", {
  expect_error(
    hourmean("SIS", file.path(data_dir, "ex_hourx.nc"), file_out, nc34 = 7),
    "nc version must be in c(3, 4), but was 7", fixed = TRUE
  )
})

########## output ncdf version is NULL #########
file_out <- tempfile_nc()
test_that("ncdf version NULL throws an error", {
  expect_error(
    hourmean("SIS",
            file.path(data_dir, "ex_hourx.nc"),
            file_out, nc34 = NULL),
    "nc_version must not be NULL"
  )
})

########## variable does not exist #########
file_out <- tempfile_nc()

test_that("warning is shown if var does not exist", {
  expect_warning(hourmean("notExist",
                         file.path(data_dir, "ex_hourx.nc"),
                         file_out),
                 "Variable 'notExist' not found. Variable 'SIS' will be used instead.")
})

file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file)
  
  expected_data <- c(301.7,302.7,302.01666,301.33334,302.33334,301.65,300.96667,
                     301.96667,301.28333,300.6,301.6,300.91666,300.23334,301.23334,
                     300.55,299.86667,300.86667,300.18332,299.5,300.5,299.81668,
                     299.13333,300.13333,299.45,298.76666,299.76666,299.08334,298.4,
                     299.4,298.71667,298.03333,299.03333,298.35,297.66666,298.66666,
                     297.98334,297.3,298.3,297.61667,296.93332,297.93332,298.93332,
                     298.25,299.25,300.25,299.56668,300.56668,301.56668,300.88333,
                     
                     300.91666,300.23334,301.23334,300.55,299.86667,300.86667,300.18332,
                     299.5,300.5,299.81668,299.13333,300.13333,299.45,298.76666,
                     299.76666,299.08334,298.4,299.4,298.71667,298.03333,299.03333,
                     298.35,297.66666,298.66666,297.98334,297.3,298.3,297.61667,
                     296.93332,297.93332,298.93332,298.25,299.25,300.25,299.56668,
                     300.56668,301.56668,300.88333,301.88333,302.88333,302.2,301.51666,
                     302.51666,301.83334,301.15,302.15,301.46667,300.78333,301.78333,
                     
                     300.13333,299.45,298.76666,299.76666,299.08334,298.4,299.4,
                     298.71667,298.03333,299.03333,298.35,297.66666,298.66666,297.98334,
                     297.3,298.3,297.61667,296.93332,297.93332,298.93332,298.25,
                     299.25,300.25,299.56668,300.56668,301.56668,300.88333,301.88333,
                     302.88333,302.2,301.51666,302.51666,301.83334,301.15,302.15,
                     301.46667,300.78333,301.78333,301.1,300.41666,301.41666,300.73334,
                     300.05,301.05,300.36667,299.68332,300.68332,300.0,299.31668)
  expected <- array(expected_data, dim = c(7, 7, 3))
  
  expect_equivalent(actual[1:147], expected, tolerance = 1e-4)
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
  expect_equal(actual, "minutes since 1983-01-01 00:00:00")
  
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
  expect_equal(actual[1:3], array(c(8941680, 8941740, 8941800)))
})

nc_close(file)

########## variable is null #########
file_out <- tempfile_nc()
test_that("error is thrown if variable is NULL", {
  expect_error(
    hourmean(NULL,
            file.path(data_dir, "ex_hourx.nc"),
            file_out),
    "variable must not be NULL"
  )
})

########## variable is empty #########
file_out <- tempfile_nc()

test_that("warning is shown if var is empty", {
  expect_warning(hourmean("",
                         file.path(data_dir, "ex_hourx.nc"),
                         file_out),
                 "Variable '' not found. Variable 'SIS' will be used instead.")
})

file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file)
  
  expected_data <- c(301.7,302.7,302.01666,301.33334,302.33334,301.65,300.96667,
                     301.96667,301.28333,300.6,301.6,300.91666,300.23334,301.23334,
                     300.55,299.86667,300.86667,300.18332,299.5,300.5,299.81668,
                     299.13333,300.13333,299.45,298.76666,299.76666,299.08334,298.4,
                     299.4,298.71667,298.03333,299.03333,298.35,297.66666,298.66666,
                     297.98334,297.3,298.3,297.61667,296.93332,297.93332,298.93332,
                     298.25,299.25,300.25,299.56668,300.56668,301.56668,300.88333,
                     
                     300.91666,300.23334,301.23334,300.55,299.86667,300.86667,300.18332,
                     299.5,300.5,299.81668,299.13333,300.13333,299.45,298.76666,
                     299.76666,299.08334,298.4,299.4,298.71667,298.03333,299.03333,
                     298.35,297.66666,298.66666,297.98334,297.3,298.3,297.61667,
                     296.93332,297.93332,298.93332,298.25,299.25,300.25,299.56668,
                     300.56668,301.56668,300.88333,301.88333,302.88333,302.2,301.51666,
                     302.51666,301.83334,301.15,302.15,301.46667,300.78333,301.78333,
                     
                     300.13333,299.45,298.76666,299.76666,299.08334,298.4,299.4,
                     298.71667,298.03333,299.03333,298.35,297.66666,298.66666,297.98334,
                     297.3,298.3,297.61667,296.93332,297.93332,298.93332,298.25,
                     299.25,300.25,299.56668,300.56668,301.56668,300.88333,301.88333,
                     302.88333,302.2,301.51666,302.51666,301.83334,301.15,302.15,
                     301.46667,300.78333,301.78333,301.1,300.41666,301.41666,300.73334,
                     300.05,301.05,300.36667,299.68332,300.68332,300.0,299.31668)
  expected <- array(expected_data, dim = c(7, 7, 3))
  
  expect_equivalent(actual[1:147], expected, tolerance = 1e-4)
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
  expect_equal(actual, "minutes since 1983-01-01 00:00:00")
  
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
  expect_equal(actual[1:3], array(c(8941680, 8941740, 8941800)))
})

nc_close(file)

########## input file does not exist #########
file_out <- tempfile_nc()
test_that("error is thrown if input file does not exist", {
  expect_error(
    hourmean("SIS",
            file.path(data_dir, "xemaple1.nc"),
            file_out),
    "Input file does not exist")
})

########## input filename is empty #########
file_out <- tempfile_nc()
test_that("error is thrown if input filename is empty", {
  expect_error(
    hourmean("SIS", "", file_out),
    "Input file does not exist")
})

########## input filename is NULL #########
file_out <- tempfile_nc()

test_that("error is thrown if input filename is NULL", {
  expect_error(
    hourmean("SIS", NULL, file_out),
    "Input filepath must be of length one and not NULL"
  )
})

########## output file already exists #########
file_out <- tempfile_nc()
cat("test\n", file = file_out)

test_that("error is thrown if output file already exists", {
  expect_error(
    hourmean("SIS",
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
    hourmean("SIS",
            file.path(data_dir, "ex_hourx.nc"),
            file_out,
            overwrite = TRUE
    ),
    NA
  )
})

file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file)
  
  expected_data <- c(301.7,302.7,302.01666,301.33334,302.33334,301.65,300.96667,
                     301.96667,301.28333,300.6,301.6,300.91666,300.23334,301.23334,
                     300.55,299.86667,300.86667,300.18332,299.5,300.5,299.81668,
                     299.13333,300.13333,299.45,298.76666,299.76666,299.08334,298.4,
                     299.4,298.71667,298.03333,299.03333,298.35,297.66666,298.66666,
                     297.98334,297.3,298.3,297.61667,296.93332,297.93332,298.93332,
                     298.25,299.25,300.25,299.56668,300.56668,301.56668,300.88333,
                     
                     300.91666,300.23334,301.23334,300.55,299.86667,300.86667,300.18332,
                     299.5,300.5,299.81668,299.13333,300.13333,299.45,298.76666,
                     299.76666,299.08334,298.4,299.4,298.71667,298.03333,299.03333,
                     298.35,297.66666,298.66666,297.98334,297.3,298.3,297.61667,
                     296.93332,297.93332,298.93332,298.25,299.25,300.25,299.56668,
                     300.56668,301.56668,300.88333,301.88333,302.88333,302.2,301.51666,
                     302.51666,301.83334,301.15,302.15,301.46667,300.78333,301.78333,
                     
                     300.13333,299.45,298.76666,299.76666,299.08334,298.4,299.4,
                     298.71667,298.03333,299.03333,298.35,297.66666,298.66666,297.98334,
                     297.3,298.3,297.61667,296.93332,297.93332,298.93332,298.25,
                     299.25,300.25,299.56668,300.56668,301.56668,300.88333,301.88333,
                     302.88333,302.2,301.51666,302.51666,301.83334,301.15,302.15,
                     301.46667,300.78333,301.78333,301.1,300.41666,301.41666,300.73334,
                     300.05,301.05,300.36667,299.68332,300.68332,300.0,299.31668)
  expected <- array(expected_data, dim = c(7, 7, 3))
  
  expect_equivalent(actual[1:147], expected, tolerance = 1e-4)
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
  expect_equal(actual, "minutes since 1983-01-01 00:00:00")
  
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
  expect_equal(actual[1:3], array(c(8941680, 8941740, 8941800)))
})

nc_close(file)
