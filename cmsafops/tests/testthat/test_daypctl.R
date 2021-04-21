data_dir <- file.path("..", "testdata")
tempfile_nc <- function() {
  tempfile_helper("daypctl_")
}

########## output ncdf version 3 ##########
file_out <- tempfile_nc()
daypctl(var = "SIS", infile = file.path(data_dir, "ex_dayx.nc"), outfile = file_out)
file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file)
  
  expected_data <- c(269.35,270.35,268.35,269.35,270.35,268.35,269.35,
                     270.35,268.9,269.9,270.9,270.45,271.45,270.9,
                     270.45,271.45,270.9,270.45,271.45,270.9,270.45,
                     271.45,270.9,269.35,270.35,268.35,269.35,270.35,
                     268.35,269.35,270.35,268.9,269.9,270.9,270.45,
                     271.45,270.9,270.45,271.45,270.9,270.45,271.45,
                     270.9,270.45,271.45,270.9,269.35,270.35,268.35,
                     
                     270.85,270.85,270.85,270.85,270.85,270.85,270.85,
                     270.85,271.0,271.85,270.85,270.85,270.85,270.85,
                     270.85,270.85,270.85,270.85,270.85,270.85,270.85,
                     270.85,270.85,270.85,270.85,270.85,270.85,270.85,
                     270.85,270.85,270.85,271.0,271.85,270.85,270.85,
                     270.85,270.85,270.85,270.85,270.85,270.85,270.85,
                     270.85,270.85,270.85,270.85,270.85,270.85,270.85,
                     
                     270.8,270.4,271.4,270.8,270.4,271.4,270.8,
                     269.2,270.2,268.2,269.2,270.2,268.8,269.8,
                     270.8,270.4,271.4,270.8,270.4,271.4,270.8,
                     270.4,271.4,270.8,270.4,271.4,270.8,270.4,
                     271.4,270.8,269.2,270.2,268.2,269.2,270.2,
                     268.8,269.8,270.8,270.4,271.4,270.8,270.4,
                     271.4,270.8,270.4,271.4,270.8,270.4,271.4)
  expected <- array(expected_data, dim = c(7, 7, 3))
  
  expect_equivalent(actual, expected, tolerance = 1e-2)
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
  expect_equal(actual, "cmsaf::daypctl with p = 0.95 for variable SIS")
  
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
daypctl(var = "SIS", infile = file.path(data_dir, "ex_dayx.nc"), outfile = file_out, nc34 = 4)
file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file)
  
  expected_data <- c(269.35,270.35,268.35,269.35,270.35,268.35,269.35,
                     270.35,268.9,269.9,270.9,270.45,271.45,270.9,
                     270.45,271.45,270.9,270.45,271.45,270.9,270.45,
                     271.45,270.9,269.35,270.35,268.35,269.35,270.35,
                     268.35,269.35,270.35,268.9,269.9,270.9,270.45,
                     271.45,270.9,270.45,271.45,270.9,270.45,271.45,
                     270.9,270.45,271.45,270.9,269.35,270.35,268.35,
                     
                     270.85,270.85,270.85,270.85,270.85,270.85,270.85,
                     270.85,271.0,271.85,270.85,270.85,270.85,270.85,
                     270.85,270.85,270.85,270.85,270.85,270.85,270.85,
                     270.85,270.85,270.85,270.85,270.85,270.85,270.85,
                     270.85,270.85,270.85,271.0,271.85,270.85,270.85,
                     270.85,270.85,270.85,270.85,270.85,270.85,270.85,
                     270.85,270.85,270.85,270.85,270.85,270.85,270.85,
                     
                     270.8,270.4,271.4,270.8,270.4,271.4,270.8,
                     269.2,270.2,268.2,269.2,270.2,268.8,269.8,
                     270.8,270.4,271.4,270.8,270.4,271.4,270.8,
                     270.4,271.4,270.8,270.4,271.4,270.8,270.4,
                     271.4,270.8,269.2,270.2,268.2,269.2,270.2,
                     268.8,269.8,270.8,270.4,271.4,270.8,270.4,
                     271.4,270.8,270.4,271.4,270.8,270.4,271.4)
  expected <- array(expected_data, dim = c(7, 7, 3))
  
  expect_equivalent(actual, expected, tolerance = 1e-2)
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
    daypctl(var = "SIS", infile = file.path(data_dir, "ex_dayx.nc"), outfile = file_out, nc34 = 7),
    "nc version must be in c(3, 4), but was 7", fixed = TRUE
  )
})

########## output ncdf version is NULL #########
file_out <- tempfile_nc()
test_that("ncdf version NULL throws an error", {
  expect_error(
    daypctl(var = "SIS",
           infile = file.path(data_dir, "ex_dayx.nc"),
           outfile = file_out, nc34 = NULL),
    "nc_version must not be NULL"
  )
})

########## variable does not exist #########
file_out <- tempfile_nc()

test_that("warning is shown if var does not exist", {
  expect_warning(daypctl(var = "notExist",
                        infile = file.path(data_dir, "ex_dayx.nc"),
                        outfile = file_out),
                 "Variable 'notExist' not found. Variable 'SIS' will be used instead.")
})

file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file)
  
  expected_data <- c(269.35,270.35,268.35,269.35,270.35,268.35,269.35,
                     270.35,268.9,269.9,270.9,270.45,271.45,270.9,
                     270.45,271.45,270.9,270.45,271.45,270.9,270.45,
                     271.45,270.9,269.35,270.35,268.35,269.35,270.35,
                     268.35,269.35,270.35,268.9,269.9,270.9,270.45,
                     271.45,270.9,270.45,271.45,270.9,270.45,271.45,
                     270.9,270.45,271.45,270.9,269.35,270.35,268.35,
                     
                     270.85,270.85,270.85,270.85,270.85,270.85,270.85,
                     270.85,271.0,271.85,270.85,270.85,270.85,270.85,
                     270.85,270.85,270.85,270.85,270.85,270.85,270.85,
                     270.85,270.85,270.85,270.85,270.85,270.85,270.85,
                     270.85,270.85,270.85,271.0,271.85,270.85,270.85,
                     270.85,270.85,270.85,270.85,270.85,270.85,270.85,
                     270.85,270.85,270.85,270.85,270.85,270.85,270.85,
                     
                     270.8,270.4,271.4,270.8,270.4,271.4,270.8,
                     269.2,270.2,268.2,269.2,270.2,268.8,269.8,
                     270.8,270.4,271.4,270.8,270.4,271.4,270.8,
                     270.4,271.4,270.8,270.4,271.4,270.8,270.4,
                     271.4,270.8,269.2,270.2,268.2,269.2,270.2,
                     268.8,269.8,270.8,270.4,271.4,270.8,270.4,
                     271.4,270.8,270.4,271.4,270.8,270.4,271.4)
  expected <- array(expected_data, dim = c(7, 7, 3))
  
  expect_equivalent(actual, expected, tolerance = 1e-2)
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
    daypctl(var = NULL,
           infile = file.path(data_dir, "ex_dayx.nc"),
           outfile = file_out),
    "variable must not be NULL"
  )
})

########## variable is empty #########
file_out <- tempfile_nc()

test_that("warning is shown if var is empty", {
  expect_warning(daypctl(var = "",
                        infile = file.path(data_dir, "ex_dayx.nc"),
                        outfile = file_out),
                 "Variable '' not found. Variable 'SIS' will be used instead.")
})

file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file)
  
  expected_data <- c(269.35,270.35,268.35,269.35,270.35,268.35,269.35,
                     270.35,268.9,269.9,270.9,270.45,271.45,270.9,
                     270.45,271.45,270.9,270.45,271.45,270.9,270.45,
                     271.45,270.9,269.35,270.35,268.35,269.35,270.35,
                     268.35,269.35,270.35,268.9,269.9,270.9,270.45,
                     271.45,270.9,270.45,271.45,270.9,270.45,271.45,
                     270.9,270.45,271.45,270.9,269.35,270.35,268.35,
                     
                     270.85,270.85,270.85,270.85,270.85,270.85,270.85,
                     270.85,271.0,271.85,270.85,270.85,270.85,270.85,
                     270.85,270.85,270.85,270.85,270.85,270.85,270.85,
                     270.85,270.85,270.85,270.85,270.85,270.85,270.85,
                     270.85,270.85,270.85,271.0,271.85,270.85,270.85,
                     270.85,270.85,270.85,270.85,270.85,270.85,270.85,
                     270.85,270.85,270.85,270.85,270.85,270.85,270.85,
                     
                     270.8,270.4,271.4,270.8,270.4,271.4,270.8,
                     269.2,270.2,268.2,269.2,270.2,268.8,269.8,
                     270.8,270.4,271.4,270.8,270.4,271.4,270.8,
                     270.4,271.4,270.8,270.4,271.4,270.8,270.4,
                     271.4,270.8,269.2,270.2,268.2,269.2,270.2,
                     268.8,269.8,270.8,270.4,271.4,270.8,270.4,
                     271.4,270.8,270.4,271.4,270.8,270.4,271.4)
  expected <- array(expected_data, dim = c(7, 7, 3))
  
  expect_equivalent(actual, expected, tolerance = 1e-2)
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
    daypctl(var = "SIS",
           infile = file.path(data_dir, "xemaple1.nc"),
           outfile = file_out),
    "Input file does not exist")
})

########## input filename is empty #########
file_out <- tempfile_nc()
test_that("error is thrown if input filename is empty", {
  expect_error(
    daypctl(var = "SIS", infile = "", outfile = file_out),
    "Input file does not exist")
})

########## input filename is NULL #########
file_out <- tempfile_nc()

test_that("error is thrown if input filename is NULL", {
  expect_error(
    daypctl(var = "SIS", infile = NULL, outfile = file_out),
    "Input filepath must be of length one and not NULL"
  )
})

########## output file already exists #########
file_out <- tempfile_nc()
cat("test\n", file = file_out)

test_that("error is thrown if output file already exists", {
  expect_error(
    daypctl(var = "SIS",
           infile = file.path(data_dir, "ex_dayx.nc"),
           outfile = file_out),
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
    daypctl(var = "SIS",
           infile = file.path(data_dir, "ex_dayx.nc"),
           outfile = file_out,
           overwrite = TRUE
    ),
    NA
  )
})

file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file)
  
  expected_data <- c(269.35,270.35,268.35,269.35,270.35,268.35,269.35,
                     270.35,268.9,269.9,270.9,270.45,271.45,270.9,
                     270.45,271.45,270.9,270.45,271.45,270.9,270.45,
                     271.45,270.9,269.35,270.35,268.35,269.35,270.35,
                     268.35,269.35,270.35,268.9,269.9,270.9,270.45,
                     271.45,270.9,270.45,271.45,270.9,270.45,271.45,
                     270.9,270.45,271.45,270.9,269.35,270.35,268.35,
                     
                     270.85,270.85,270.85,270.85,270.85,270.85,270.85,
                     270.85,271.0,271.85,270.85,270.85,270.85,270.85,
                     270.85,270.85,270.85,270.85,270.85,270.85,270.85,
                     270.85,270.85,270.85,270.85,270.85,270.85,270.85,
                     270.85,270.85,270.85,271.0,271.85,270.85,270.85,
                     270.85,270.85,270.85,270.85,270.85,270.85,270.85,
                     270.85,270.85,270.85,270.85,270.85,270.85,270.85,
                     
                     270.8,270.4,271.4,270.8,270.4,271.4,270.8,
                     269.2,270.2,268.2,269.2,270.2,268.8,269.8,
                     270.8,270.4,271.4,270.8,270.4,271.4,270.8,
                     270.4,271.4,270.8,270.4,271.4,270.8,270.4,
                     271.4,270.8,269.2,270.2,268.2,269.2,270.2,
                     268.8,269.8,270.8,270.4,271.4,270.8,270.4,
                     271.4,270.8,270.4,271.4,270.8,270.4,271.4)
  expected <- array(expected_data, dim = c(7, 7, 3))
  
  expect_equivalent(actual, expected, tolerance = 1e-2)
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
