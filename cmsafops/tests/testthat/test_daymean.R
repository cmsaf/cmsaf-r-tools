data_dir <- file.path("..", "testdata")
tempfile_nc <- function() {
  tempfile_helper("daymean_")
}

########## output ncdf version 3 ##########
file_out <- tempfile_nc()
daymean("SIS", file.path(data_dir, "ex_dayx.nc"), file_out)
file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file)
  
  expected_data <- c(258.833344,	259.833344,	258.916656,	259.916656,	260.916656,	260.000000,	261.000000,
                     262.000000,	261.083344,	262.083344,	263.083344,	262.166656,	263.166656,	262.250000,
                     261.333344,	262.333344,	261.416656,	260.500000,	261.500000,	260.583344,	259.666656,
                     260.666656,	259.750000,	258.833344,	259.833344,	258.916656,	259.916656,	260.916656,
                     260.000000,	261.000000,	262.000000,	261.083344,	262.083344,	263.083344,	262.166656,
                     263.166656,	262.250000,	261.333344,	262.333344,	261.416656,	260.500000,	261.500000,
                     260.583344,	259.666656,	260.666656,	259.750000,	258.833344,	259.833344,	258.916656,
                     
                     261.083344,	261.125000,	261.166656,	261.208344,	261.250000,	261.291656,	261.333344,
                     261.375000,	261.416656,	261.458344,	260.541656,	260.583344,	260.625000,	260.666656,
                     260.708344,	260.750000,	260.791656,	260.833344,	260.875000,	260.916656,	260.958344,
                     261.000000,	261.041656,	261.083344,	261.125000,	261.166656,	261.208344,	261.250000,
                     261.291656,	261.333344,	261.375000,	261.416656,	261.458344,	260.541656,	260.583344,
                     260.625000,	260.666656,	260.708344,	260.750000,	260.791656,	260.833344,	260.875000,
                     260.916656,	260.958344,	261.000000,	261.041656,	261.083344,	261.125000,	261.166656,
                     
                     261.000000,	260.230774,	261.230774,	260.461548,	259.692322,	260.692322,	259.923065,
                     259.153839,	260.153839,	259.384613,	260.384613,	261.384613,	260.615387,	261.615387,
                     262.615387,	261.846161,	262.846161,	262.076935,	261.307678,	262.307678,	261.538452,
                     260.769226,	261.769226,	261.000000,	260.230774,	261.230774,	260.461548,	259.692322,
                     260.692322,	259.923065,	259.153839,	260.153839,	259.384613,	260.384613,	261.384613,
                     260.615387,	261.615387,	262.615387,	261.846161,	262.846161,	262.076935,	261.307678,
                     262.307678,	261.538452,	260.769226,	261.769226,	261.000000,	260.230774,	261.230774)
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
  expect_equal(actual, "cmsaf::daymean for variable SIS")
  
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
daymean("SIS", file.path(data_dir, "ex_dayx.nc"), file_out, nc34 = 4)
file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file)
  
  expected_data <- c(258.833344,	259.833344,	258.916656,	259.916656,	260.916656,	260.000000,	261.000000,
                     262.000000,	261.083344,	262.083344,	263.083344,	262.166656,	263.166656,	262.250000,
                     261.333344,	262.333344,	261.416656,	260.500000,	261.500000,	260.583344,	259.666656,
                     260.666656,	259.750000,	258.833344,	259.833344,	258.916656,	259.916656,	260.916656,
                     260.000000,	261.000000,	262.000000,	261.083344,	262.083344,	263.083344,	262.166656,
                     263.166656,	262.250000,	261.333344,	262.333344,	261.416656,	260.500000,	261.500000,
                     260.583344,	259.666656,	260.666656,	259.750000,	258.833344,	259.833344,	258.916656,
                     
                     261.083344,	261.125000,	261.166656,	261.208344,	261.250000,	261.291656,	261.333344,
                     261.375000,	261.416656,	261.458344,	260.541656,	260.583344,	260.625000,	260.666656,
                     260.708344,	260.750000,	260.791656,	260.833344,	260.875000,	260.916656,	260.958344,
                     261.000000,	261.041656,	261.083344,	261.125000,	261.166656,	261.208344,	261.250000,
                     261.291656,	261.333344,	261.375000,	261.416656,	261.458344,	260.541656,	260.583344,
                     260.625000,	260.666656,	260.708344,	260.750000,	260.791656,	260.833344,	260.875000,
                     260.916656,	260.958344,	261.000000,	261.041656,	261.083344,	261.125000,	261.166656,
                     
                     261.000000,	260.230774,	261.230774,	260.461548,	259.692322,	260.692322,	259.923065,
                     259.153839,	260.153839,	259.384613,	260.384613,	261.384613,	260.615387,	261.615387,
                     262.615387,	261.846161,	262.846161,	262.076935,	261.307678,	262.307678,	261.538452,
                     260.769226,	261.769226,	261.000000,	260.230774,	261.230774,	260.461548,	259.692322,
                     260.692322,	259.923065,	259.153839,	260.153839,	259.384613,	260.384613,	261.384613,
                     260.615387,	261.615387,	262.615387,	261.846161,	262.846161,	262.076935,	261.307678,
                     262.307678,	261.538452,	260.769226,	261.769226,	261.000000,	260.230774,	261.230774)
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
    daymean("SIS", file.path(data_dir, "ex_dayx.nc"), file_out, nc34 = 7),
    "nc version must be in c(3, 4), but was 7", fixed = TRUE
  )
})

########## output ncdf version is NULL #########
file_out <- tempfile_nc()
test_that("ncdf version NULL throws an error", {
  expect_error(
    daymean("SIS",
           file.path(data_dir, "ex_dayx.nc"),
           file_out, nc34 = NULL),
    "nc_version must not be NULL"
  )
})

########## variable does not exist #########
file_out <- tempfile_nc()

test_that("warning is shown if var does not exist", {
  expect_warning(daymean("notExist",
                        file.path(data_dir, "ex_dayx.nc"),
                        file_out),
                 "Variable 'notExist' not found. Variable 'SIS' will be used instead.")
})

file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file)
  
  expected_data <- c(258.833344,	259.833344,	258.916656,	259.916656,	260.916656,	260.000000,	261.000000,
                     262.000000,	261.083344,	262.083344,	263.083344,	262.166656,	263.166656,	262.250000,
                     261.333344,	262.333344,	261.416656,	260.500000,	261.500000,	260.583344,	259.666656,
                     260.666656,	259.750000,	258.833344,	259.833344,	258.916656,	259.916656,	260.916656,
                     260.000000,	261.000000,	262.000000,	261.083344,	262.083344,	263.083344,	262.166656,
                     263.166656,	262.250000,	261.333344,	262.333344,	261.416656,	260.500000,	261.500000,
                     260.583344,	259.666656,	260.666656,	259.750000,	258.833344,	259.833344,	258.916656,
                     
                     261.083344,	261.125000,	261.166656,	261.208344,	261.250000,	261.291656,	261.333344,
                     261.375000,	261.416656,	261.458344,	260.541656,	260.583344,	260.625000,	260.666656,
                     260.708344,	260.750000,	260.791656,	260.833344,	260.875000,	260.916656,	260.958344,
                     261.000000,	261.041656,	261.083344,	261.125000,	261.166656,	261.208344,	261.250000,
                     261.291656,	261.333344,	261.375000,	261.416656,	261.458344,	260.541656,	260.583344,
                     260.625000,	260.666656,	260.708344,	260.750000,	260.791656,	260.833344,	260.875000,
                     260.916656,	260.958344,	261.000000,	261.041656,	261.083344,	261.125000,	261.166656,
                     
                     261.000000,	260.230774,	261.230774,	260.461548,	259.692322,	260.692322,	259.923065,
                     259.153839,	260.153839,	259.384613,	260.384613,	261.384613,	260.615387,	261.615387,
                     262.615387,	261.846161,	262.846161,	262.076935,	261.307678,	262.307678,	261.538452,
                     260.769226,	261.769226,	261.000000,	260.230774,	261.230774,	260.461548,	259.692322,
                     260.692322,	259.923065,	259.153839,	260.153839,	259.384613,	260.384613,	261.384613,
                     260.615387,	261.615387,	262.615387,	261.846161,	262.846161,	262.076935,	261.307678,
                     262.307678,	261.538452,	260.769226,	261.769226,	261.000000,	260.230774,	261.230774)
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
    daymean(NULL,
           file.path(data_dir, "ex_dayx.nc"),
           file_out),
    "variable must not be NULL"
  )
})

########## variable is empty #########
file_out <- tempfile_nc()

test_that("warning is shown if var is empty", {
  expect_warning(daymean("",
                        file.path(data_dir, "ex_dayx.nc"),
                        file_out),
                 "Variable '' not found. Variable 'SIS' will be used instead.")
})

file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file)
  
  expected_data <- c(258.833344,	259.833344,	258.916656,	259.916656,	260.916656,	260.000000,	261.000000,
                     262.000000,	261.083344,	262.083344,	263.083344,	262.166656,	263.166656,	262.250000,
                     261.333344,	262.333344,	261.416656,	260.500000,	261.500000,	260.583344,	259.666656,
                     260.666656,	259.750000,	258.833344,	259.833344,	258.916656,	259.916656,	260.916656,
                     260.000000,	261.000000,	262.000000,	261.083344,	262.083344,	263.083344,	262.166656,
                     263.166656,	262.250000,	261.333344,	262.333344,	261.416656,	260.500000,	261.500000,
                     260.583344,	259.666656,	260.666656,	259.750000,	258.833344,	259.833344,	258.916656,
                     
                     261.083344,	261.125000,	261.166656,	261.208344,	261.250000,	261.291656,	261.333344,
                     261.375000,	261.416656,	261.458344,	260.541656,	260.583344,	260.625000,	260.666656,
                     260.708344,	260.750000,	260.791656,	260.833344,	260.875000,	260.916656,	260.958344,
                     261.000000,	261.041656,	261.083344,	261.125000,	261.166656,	261.208344,	261.250000,
                     261.291656,	261.333344,	261.375000,	261.416656,	261.458344,	260.541656,	260.583344,
                     260.625000,	260.666656,	260.708344,	260.750000,	260.791656,	260.833344,	260.875000,
                     260.916656,	260.958344,	261.000000,	261.041656,	261.083344,	261.125000,	261.166656,
                     
                     261.000000,	260.230774,	261.230774,	260.461548,	259.692322,	260.692322,	259.923065,
                     259.153839,	260.153839,	259.384613,	260.384613,	261.384613,	260.615387,	261.615387,
                     262.615387,	261.846161,	262.846161,	262.076935,	261.307678,	262.307678,	261.538452,
                     260.769226,	261.769226,	261.000000,	260.230774,	261.230774,	260.461548,	259.692322,
                     260.692322,	259.923065,	259.153839,	260.153839,	259.384613,	260.384613,	261.384613,
                     260.615387,	261.615387,	262.615387,	261.846161,	262.846161,	262.076935,	261.307678,
                     262.307678,	261.538452,	260.769226,	261.769226,	261.000000,	260.230774,	261.230774)
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
    daymean("SIS",
           file.path(data_dir, "xemaple1.nc"),
           file_out),
    "Input file does not exist")
})

########## input filename is empty #########
file_out <- tempfile_nc()
test_that("error is thrown if input filename is empty", {
  expect_error(
    daymean("SIS", "", file_out),
    "Input file does not exist")
})

########## input filename is NULL #########
file_out <- tempfile_nc()

test_that("error is thrown if input filename is NULL", {
  expect_error(
    daymean("SIS", NULL, file_out),
    "Input filepath must be of length one and not NULL"
  )
})

########## output file already exists #########
file_out <- tempfile_nc()
cat("test\n", file = file_out)

test_that("error is thrown if output file already exists", {
  expect_error(
    daymean("SIS",
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
    daymean("SIS",
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
  
  expected_data <- c(258.833344,	259.833344,	258.916656,	259.916656,	260.916656,	260.000000,	261.000000,
                     262.000000,	261.083344,	262.083344,	263.083344,	262.166656,	263.166656,	262.250000,
                     261.333344,	262.333344,	261.416656,	260.500000,	261.500000,	260.583344,	259.666656,
                     260.666656,	259.750000,	258.833344,	259.833344,	258.916656,	259.916656,	260.916656,
                     260.000000,	261.000000,	262.000000,	261.083344,	262.083344,	263.083344,	262.166656,
                     263.166656,	262.250000,	261.333344,	262.333344,	261.416656,	260.500000,	261.500000,
                     260.583344,	259.666656,	260.666656,	259.750000,	258.833344,	259.833344,	258.916656,
                     
                     261.083344,	261.125000,	261.166656,	261.208344,	261.250000,	261.291656,	261.333344,
                     261.375000,	261.416656,	261.458344,	260.541656,	260.583344,	260.625000,	260.666656,
                     260.708344,	260.750000,	260.791656,	260.833344,	260.875000,	260.916656,	260.958344,
                     261.000000,	261.041656,	261.083344,	261.125000,	261.166656,	261.208344,	261.250000,
                     261.291656,	261.333344,	261.375000,	261.416656,	261.458344,	260.541656,	260.583344,
                     260.625000,	260.666656,	260.708344,	260.750000,	260.791656,	260.833344,	260.875000,
                     260.916656,	260.958344,	261.000000,	261.041656,	261.083344,	261.125000,	261.166656,
                     
                     261.000000,	260.230774,	261.230774,	260.461548,	259.692322,	260.692322,	259.923065,
                     259.153839,	260.153839,	259.384613,	260.384613,	261.384613,	260.615387,	261.615387,
                     262.615387,	261.846161,	262.846161,	262.076935,	261.307678,	262.307678,	261.538452,
                     260.769226,	261.769226,	261.000000,	260.230774,	261.230774,	260.461548,	259.692322,
                     260.692322,	259.923065,	259.153839,	260.153839,	259.384613,	260.384613,	261.384613,
                     260.615387,	261.615387,	262.615387,	261.846161,	262.846161,	262.076935,	261.307678,
                     262.307678,	261.538452,	260.769226,	261.769226,	261.000000,	260.230774,	261.230774)
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
