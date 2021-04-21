data_dir <- file.path("..", "testdata")
tempfile_nc <- function() {
  tempfile_helper("dayvar_")
}

########## output ncdf version 3 ##########
file_out <- tempfile_nc()
dayvar("SIS", file.path(data_dir, "ex_dayx.nc"), file_out)
file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file)
  
  expected_data <- c(44.515152,44.515152,37.719696,37.719696,37.719696,35.454544,35.454544,
                     35.454544,37.719696,37.719696,37.719696,44.515152,44.515152,51.659092,
                     54.969696,54.969696,58.628788,58.454544,58.454544,58.628788,54.969696,
                     54.969696,51.659092,44.515152,44.515152,37.719696,37.719696,37.719696,
                     35.454544,35.454544,35.454544,37.719696,37.719696,37.719696,44.515152,
                     44.515152,51.659092,54.969696,54.969696,58.628788,58.454544,58.454544,
                     58.628788,54.969696,54.969696,51.659092,44.515152,44.515152,37.719696,
                     
                     44.166668,44.375,44.666668,45.041668,45.5,46.041668,46.666668,
                     47.375,48.166668,49.041668,49.041668,48.166668,47.375,46.666668,
                     46.041668,45.5,45.041668,44.666668,44.375,44.166668,44.041668,
                     44.0,44.041668,44.166668,44.375,44.666668,45.041668,45.5,
                     46.041668,46.666668,47.375,48.166668,49.041668,49.041668,48.166668,
                     47.375,46.666668,46.041668,45.5,45.041668,44.666668,44.375,
                     44.166668,44.041668,44.0,44.041668,44.166668,44.375,44.666668,
                     
                     56.0,54.525642,54.525642,53.935898,50.397434,50.397434,47.74359,
                     42.141026,42.141026,37.423077,37.423077,37.423077,37.423077,37.423077,
                     37.423077,42.141026,42.141026,47.74359,50.397434,50.397434,53.935898,
                     54.525642,54.525642,56.0,54.525642,54.525642,53.935898,50.397434,
                     50.397434,47.74359,42.141026,42.141026,37.423077,37.423077,37.423077,
                     37.423077,37.423077,37.423077,42.141026,42.141026,47.74359,50.397434,
                     50.397434,53.935898,54.525642,54.525642,56.0,54.525642,54.525642)
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
  expect_equal(actual, "cmsaf::dayvar for variable SIS")
  
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
dayvar("SIS", file.path(data_dir, "ex_dayx.nc"), file_out, nc34 = 4)
file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file)
  
  expected_data <- c(44.515152,44.515152,37.719696,37.719696,37.719696,35.454544,35.454544,
                     35.454544,37.719696,37.719696,37.719696,44.515152,44.515152,51.659092,
                     54.969696,54.969696,58.628788,58.454544,58.454544,58.628788,54.969696,
                     54.969696,51.659092,44.515152,44.515152,37.719696,37.719696,37.719696,
                     35.454544,35.454544,35.454544,37.719696,37.719696,37.719696,44.515152,
                     44.515152,51.659092,54.969696,54.969696,58.628788,58.454544,58.454544,
                     58.628788,54.969696,54.969696,51.659092,44.515152,44.515152,37.719696,
                     
                     44.166668,44.375,44.666668,45.041668,45.5,46.041668,46.666668,
                     47.375,48.166668,49.041668,49.041668,48.166668,47.375,46.666668,
                     46.041668,45.5,45.041668,44.666668,44.375,44.166668,44.041668,
                     44.0,44.041668,44.166668,44.375,44.666668,45.041668,45.5,
                     46.041668,46.666668,47.375,48.166668,49.041668,49.041668,48.166668,
                     47.375,46.666668,46.041668,45.5,45.041668,44.666668,44.375,
                     44.166668,44.041668,44.0,44.041668,44.166668,44.375,44.666668,
                     
                     56.0,54.525642,54.525642,53.935898,50.397434,50.397434,47.74359,
                     42.141026,42.141026,37.423077,37.423077,37.423077,37.423077,37.423077,
                     37.423077,42.141026,42.141026,47.74359,50.397434,50.397434,53.935898,
                     54.525642,54.525642,56.0,54.525642,54.525642,53.935898,50.397434,
                     50.397434,47.74359,42.141026,42.141026,37.423077,37.423077,37.423077,
                     37.423077,37.423077,37.423077,42.141026,42.141026,47.74359,50.397434,
                     50.397434,53.935898,54.525642,54.525642,56.0,54.525642,54.525642)
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
    dayvar("SIS", file.path(data_dir, "ex_dayx.nc"), file_out, nc34 = 7),
    "nc version must be in c(3, 4), but was 7", fixed = TRUE
  )
})

########## output ncdf version is NULL #########
file_out <- tempfile_nc()
test_that("ncdf version NULL throws an error", {
  expect_error(
    dayvar("SIS",
           file.path(data_dir, "ex_dayx.nc"),
           file_out, nc34 = NULL),
    "nc_version must not be NULL"
  )
})

########## variable does not exist #########
file_out <- tempfile_nc()

test_that("warning is shown if var does not exist", {
  expect_warning(dayvar("notExist",
                        file.path(data_dir, "ex_dayx.nc"),
                        file_out),
                 "Variable 'notExist' not found. Variable 'SIS' will be used instead.")
})

file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file)
  
  expected_data <- c(44.515152,44.515152,37.719696,37.719696,37.719696,35.454544,35.454544,
                     35.454544,37.719696,37.719696,37.719696,44.515152,44.515152,51.659092,
                     54.969696,54.969696,58.628788,58.454544,58.454544,58.628788,54.969696,
                     54.969696,51.659092,44.515152,44.515152,37.719696,37.719696,37.719696,
                     35.454544,35.454544,35.454544,37.719696,37.719696,37.719696,44.515152,
                     44.515152,51.659092,54.969696,54.969696,58.628788,58.454544,58.454544,
                     58.628788,54.969696,54.969696,51.659092,44.515152,44.515152,37.719696,
                     
                     44.166668,44.375,44.666668,45.041668,45.5,46.041668,46.666668,
                     47.375,48.166668,49.041668,49.041668,48.166668,47.375,46.666668,
                     46.041668,45.5,45.041668,44.666668,44.375,44.166668,44.041668,
                     44.0,44.041668,44.166668,44.375,44.666668,45.041668,45.5,
                     46.041668,46.666668,47.375,48.166668,49.041668,49.041668,48.166668,
                     47.375,46.666668,46.041668,45.5,45.041668,44.666668,44.375,
                     44.166668,44.041668,44.0,44.041668,44.166668,44.375,44.666668,
                     
                     56.0,54.525642,54.525642,53.935898,50.397434,50.397434,47.74359,
                     42.141026,42.141026,37.423077,37.423077,37.423077,37.423077,37.423077,
                     37.423077,42.141026,42.141026,47.74359,50.397434,50.397434,53.935898,
                     54.525642,54.525642,56.0,54.525642,54.525642,53.935898,50.397434,
                     50.397434,47.74359,42.141026,42.141026,37.423077,37.423077,37.423077,
                     37.423077,37.423077,37.423077,42.141026,42.141026,47.74359,50.397434,
                     50.397434,53.935898,54.525642,54.525642,56.0,54.525642,54.525642)
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
    dayvar(NULL,
           file.path(data_dir, "ex_dayx.nc"),
           file_out),
    "variable must not be NULL"
  )
})

########## variable is empty #########
file_out <- tempfile_nc()

test_that("warning is shown if var is empty", {
  expect_warning(dayvar("",
                        file.path(data_dir, "ex_dayx.nc"),
                        file_out),
                 "Variable '' not found. Variable 'SIS' will be used instead.")
})

file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file)
  
  expected_data <- c(44.515152,44.515152,37.719696,37.719696,37.719696,35.454544,35.454544,
                     35.454544,37.719696,37.719696,37.719696,44.515152,44.515152,51.659092,
                     54.969696,54.969696,58.628788,58.454544,58.454544,58.628788,54.969696,
                     54.969696,51.659092,44.515152,44.515152,37.719696,37.719696,37.719696,
                     35.454544,35.454544,35.454544,37.719696,37.719696,37.719696,44.515152,
                     44.515152,51.659092,54.969696,54.969696,58.628788,58.454544,58.454544,
                     58.628788,54.969696,54.969696,51.659092,44.515152,44.515152,37.719696,
                     
                     44.166668,44.375,44.666668,45.041668,45.5,46.041668,46.666668,
                     47.375,48.166668,49.041668,49.041668,48.166668,47.375,46.666668,
                     46.041668,45.5,45.041668,44.666668,44.375,44.166668,44.041668,
                     44.0,44.041668,44.166668,44.375,44.666668,45.041668,45.5,
                     46.041668,46.666668,47.375,48.166668,49.041668,49.041668,48.166668,
                     47.375,46.666668,46.041668,45.5,45.041668,44.666668,44.375,
                     44.166668,44.041668,44.0,44.041668,44.166668,44.375,44.666668,
                     
                     56.0,54.525642,54.525642,53.935898,50.397434,50.397434,47.74359,
                     42.141026,42.141026,37.423077,37.423077,37.423077,37.423077,37.423077,
                     37.423077,42.141026,42.141026,47.74359,50.397434,50.397434,53.935898,
                     54.525642,54.525642,56.0,54.525642,54.525642,53.935898,50.397434,
                     50.397434,47.74359,42.141026,42.141026,37.423077,37.423077,37.423077,
                     37.423077,37.423077,37.423077,42.141026,42.141026,47.74359,50.397434,
                     50.397434,53.935898,54.525642,54.525642,56.0,54.525642,54.525642)
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
    dayvar("SIS",
           file.path(data_dir, "xemaple1.nc"),
           file_out),
    "Input file does not exist")
})

########## input filename is empty #########
file_out <- tempfile_nc()
test_that("error is thrown if input filename is empty", {
  expect_error(
    dayvar("SIS", "", file_out),
    "Input file does not exist")
})

########## input filename is NULL #########
file_out <- tempfile_nc()

test_that("error is thrown if input filename is NULL", {
  expect_error(
    dayvar("SIS", NULL, file_out),
    "Input filepath must be of length one and not NULL"
  )
})

########## output file already exists #########
file_out <- tempfile_nc()
cat("test\n", file = file_out)

test_that("error is thrown if output file already exists", {
  expect_error(
    dayvar("SIS",
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
    dayvar("SIS",
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
  
  expected_data <- c(44.515152,44.515152,37.719696,37.719696,37.719696,35.454544,35.454544,
                     35.454544,37.719696,37.719696,37.719696,44.515152,44.515152,51.659092,
                     54.969696,54.969696,58.628788,58.454544,58.454544,58.628788,54.969696,
                     54.969696,51.659092,44.515152,44.515152,37.719696,37.719696,37.719696,
                     35.454544,35.454544,35.454544,37.719696,37.719696,37.719696,44.515152,
                     44.515152,51.659092,54.969696,54.969696,58.628788,58.454544,58.454544,
                     58.628788,54.969696,54.969696,51.659092,44.515152,44.515152,37.719696,
                     
                     44.166668,44.375,44.666668,45.041668,45.5,46.041668,46.666668,
                     47.375,48.166668,49.041668,49.041668,48.166668,47.375,46.666668,
                     46.041668,45.5,45.041668,44.666668,44.375,44.166668,44.041668,
                     44.0,44.041668,44.166668,44.375,44.666668,45.041668,45.5,
                     46.041668,46.666668,47.375,48.166668,49.041668,49.041668,48.166668,
                     47.375,46.666668,46.041668,45.5,45.041668,44.666668,44.375,
                     44.166668,44.041668,44.0,44.041668,44.166668,44.375,44.666668,
                     
                     56.0,54.525642,54.525642,53.935898,50.397434,50.397434,47.74359,
                     42.141026,42.141026,37.423077,37.423077,37.423077,37.423077,37.423077,
                     37.423077,42.141026,42.141026,47.74359,50.397434,50.397434,53.935898,
                     54.525642,54.525642,56.0,54.525642,54.525642,53.935898,50.397434,
                     50.397434,47.74359,42.141026,42.141026,37.423077,37.423077,37.423077,
                     37.423077,37.423077,37.423077,42.141026,42.141026,47.74359,50.397434,
                     50.397434,53.935898,54.525642,54.525642,56.0,54.525642,54.525642)
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
