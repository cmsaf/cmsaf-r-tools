data_dir <- file.path("..", "testdata")
tempfile_nc <- function() {
  tempfile_helper("gridboxmean_")
}

########## output ncdf version 3 ##########
file_out <- tempfile_nc()
gridboxmean("SIS", 3, 6, file.path(data_dir, "ex_gridboxx.nc"), file_out)
file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file)
  
  expected_data <- c(286.66666,289.66666,292.66666,295.66666,298.66666,296.05554,287.83334,
                     294.83334,297.83334,300.83334,303.83334,295.6111,293.0,296.0,
                     303.0,306.0,309.0,295.16666,298.16666,301.16666,304.16666,
                     313.33334,305.1111,285.66666,288.66666,291.66666,294.66666,297.66666,
                     
                     295.6111,293.0,296.0,299.0,302.0,305.0,308.0,
                     298.16666,301.16666,304.16666,307.16666,310.16666,313.16666,299.33334,
                     306.33334,309.33334,312.33334,304.1111,295.8889,287.66666,290.66666,
                     283.0,286.0,289.0,292.0,295.0,298.0,301.0,
                     
                     310.16666,313.16666,299.33334,302.33334,305.33334,308.33334,311.33334,
                     295.8889,287.66666,290.66666,293.66666,296.66666,299.66666,291.44446,
                     292.83334,295.83334,298.83334,301.83334,304.83334,291.0,294.0,
                     320.0,323.0,326.0,317.77777,298.33334,301.33334,304.33334)
  expected <- array(expected_data, dim = c(4, 7, 3))
  
  expect_equivalent(actual, expected, tolerance = 1e-4)
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
  expect_equal(actual, "cmsaf::gridboxmean for variable SIS")
  
  global_attr <- ncatt_get(file, 0)
  expect_equal(length(global_attr), 1)
  
  actual <- names(global_attr[1])
  expect_equal(actual, "Info")
  
  actual <- global_attr[[1]]
  expect_equal(actual, "Created with the CM SAF R Toolbox.")
})

test_that("coordinates are correct", {
  actual <- ncvar_get(file, "lon")
  expect_identical(actual, array(c(5.5, 7.0, 8.5, 10.0, 11.5, 13.0, 14.5)))
  
  actual <- ncvar_get(file, "lat")
  expect_identical(actual, array(c(46.25, 49.25, 52.25, 54.5)))
  
  actual <- ncvar_get(file, "time")
  expect_equal(actual, array(c(150456, 151200, 151920)))
})

nc_close(file)