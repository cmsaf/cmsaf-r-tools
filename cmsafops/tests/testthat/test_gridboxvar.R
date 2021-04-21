data_dir <- file.path("..", "testdata")
tempfile_nc <- function() {
  tempfile_helper("gridboxvar_")
}

########## output ncdf version 3 ##########
file_out <- tempfile_nc()
gridboxvar("SIS", 3, 6, file.path(data_dir, "ex_gridboxx.nc"), file_out)
file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file)
  
  expected_data <- c(991.2941,991.2941,991.2941,991.2941,991.2941,971.82025,768.5,
                     768.5,768.5,768.5,768.5,773.78107,694.2353,694.2353,
                     694.2353,694.2353,694.2353,768.5,768.5,768.5,768.5,
                     1291.0,1549.1111,1291.0,1291.0,1291.0,1291.0,1291.0,
                     
                     773.78107,694.2353,694.2353,694.2353,694.2353,694.2353,694.2353,
                     768.5,768.5,768.5,768.5,768.5,768.5,991.2941,
                     991.2941,991.2941,991.2941,1198.5752,1198.5752,991.2941,991.2941,
                     331.5,331.5,331.5,331.5,331.5,331.5,331.5,
                     
                     768.5,768.5,991.2941,991.2941,991.2941,991.2941,991.2941,
                     1198.5752,991.2941,991.2941,991.2941,991.2941,991.2941,897.55554,
                     768.5,768.5,768.5,768.5,768.5,694.2353,694.2353,
                     331.5,331.5,331.5,909.44446,1291.0,1291.0,1291.0)
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
  expect_equal(actual, "cmsaf::gridboxvar for variable SIS")
  
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