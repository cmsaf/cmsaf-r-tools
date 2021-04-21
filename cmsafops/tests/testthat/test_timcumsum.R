data_dir <- file.path("..", "testdata")
tempfile_nc <- function() {
  tempfile_helper("timcumsum_")
}

##### no NAs #####
file_in <- file.path(data_dir, "ex_timcumsum1.nc")
file_out <- tempfile_nc()
timcumsum("SIS", file_in, file_out)
file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file)

  expected_data <- c(
    250:258,
    seq(509, 525, 2),
    c(777, 780, 783, 786, 789, 769, 772, 775, 778),
    c(1031, 1035, 1039, 1043, 1047, 1028, 1032, 1036, 1040)
  )
  expected <- array(expected_data, dim = c(3, 3, 4))

  expect_equivalent(actual, expected)
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
  expect_equal(actual, "days since 1983-01-01 00:00:00")

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
  expect_equal(actual,-999)

  actual <- ncatt_get(file, "SIS", "cmsaf_info")$value
  expect_equal(actual, "cmsaf::timcumsum for variable SIS")

  global_attr <- ncatt_get(file, 0)
  expect_equal(length(global_attr), 1)

  actual <- names(global_attr[1])
  expect_equal(actual, "Info")

  actual <- global_attr[[1]]
  expect_equal(actual, "Created with the CM SAF R Toolbox.")
})

test_that("coordinates are correct", {
  actual <- ncvar_get(file, "lon")
  expect_identical(actual, array(seq(5, 6, 0.5)))

  actual <- ncvar_get(file, "lat")
  expect_identical(actual, array(seq(45, 46, 0.5)))

  actual <- ncvar_get(file, "time")
  expect_equal(actual, array(c(6209, 6210, 6606, 6607)))
})

nc_close(file)

##### once NAs #####
file_in <- file.path(data_dir, "ex_timcumsum2.nc")
file_out <- tempfile_nc()
timcumsum("SIS", file_in, file_out)
file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file)

  expected_data <- c(1:9,
                     seq(2, 18, 2),
                     seq(13, 37, 3),
                     seq(34, 66, 4))
  expected <- array(expected_data, dim = c(3, 3, 4))

  expect_equivalent(actual, expected)
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
  expect_equal(actual, "days since 1983-01-01 00:00:00")

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
  expect_equal(actual,-999)

  actual <- ncatt_get(file, "SIS", "cmsaf_info")$value
  expect_equal(actual, "cmsaf::timcumsum for variable SIS")

  global_attr <- ncatt_get(file, 0)
  expect_equal(length(global_attr), 1)

  actual <- names(global_attr[1])
  expect_equal(actual, "Info")

  actual <- global_attr[[1]]
  expect_equal(actual, "Created with the CM SAF R Toolbox.")
})

test_that("coordinates are correct", {
  actual <- ncvar_get(file, "lon")
  expect_identical(actual, array(seq(5, 6, 0.5)))

  actual <- ncvar_get(file, "lat")
  expect_identical(actual, array(seq(45, 46, 0.5)))

  actual <- ncvar_get(file, "time")
  expect_equal(actual, array(c(6999, 7000, 7395, 7396)))
})

nc_close(file)


##### once NAs , previous as #####
file_in <- file.path(data_dir, "ex_timcumsum2.nc")
file_out <- tempfile_nc()
timcumsum("SIS", file_in, file_out, na_replace = "previous")
file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file)

  expected_data <- c(1:9,
                     seq(2, 18, 2),
                     seq(13, 37, 3),
                     seq(34, 66, 4))
  expected <- array(expected_data, dim = c(3, 3, 4))

  expect_equivalent(actual, expected)
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
  expect_equal(actual, "days since 1983-01-01 00:00:00")

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
  expect_equal(actual,-999)

  actual <- ncatt_get(file, "SIS", "cmsaf_info")$value
  expect_equal(actual, "cmsaf::timcumsum for variable SIS")

  global_attr <- ncatt_get(file, 0)
  expect_equal(length(global_attr), 1)

  actual <- names(global_attr[1])
  expect_equal(actual, "Info")

  actual <- global_attr[[1]]
  expect_equal(actual, "Created with the CM SAF R Toolbox.")
})

test_that("coordinates are correct", {
  actual <- ncvar_get(file, "lon")
  expect_identical(actual, array(seq(5, 6, 0.5)))

  actual <- ncvar_get(file, "lat")
  expect_identical(actual, array(seq(45, 46, 0.5)))

  actual <- ncvar_get(file, "time")
  expect_equal(actual, array(c(6999, 7000, 7395, 7396)))
})

nc_close(file)


##### once NAs , general NAs #####
file_in <- file.path(data_dir, "ex_timcumsum3.nc")
file_out <- tempfile_nc()
timcumsum("SIS", file_in, file_out)
file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file)

  temp <- c(NA, 1, NA, 1, 1, 1, NA, 1, NA)
  expected_data <- c(temp,
                     2 * temp,
                     11 * temp,
                     11 * temp + c(NA, 1, NA, 2, 3, 4, NA, 5, NA))
  expected <- array(expected_data, dim = c(3, 3, 4))

  expect_equivalent(actual, expected)
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
  expect_equal(actual, "days since 1983-01-01 00:00:00")

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
  expect_equal(actual,-999)

  actual <- ncatt_get(file, "SIS", "cmsaf_info")$value
  expect_equal(actual, "cmsaf::timcumsum for variable SIS")

  global_attr <- ncatt_get(file, 0)
  expect_equal(length(global_attr), 1)

  actual <- names(global_attr[1])
  expect_equal(actual, "Info")

  actual <- global_attr[[1]]
  expect_equal(actual, "Created with the CM SAF R Toolbox.")
})

test_that("coordinates are correct", {
  actual <- ncvar_get(file, "lon")
  expect_identical(actual, array(seq(5, 6, 0.5)))

  actual <- ncvar_get(file, "lat")
  expect_identical(actual, array(seq(45, 46, 0.5)))

  actual <- ncvar_get(file, "time")
  expect_equal(actual, array(c(7791, 7792, 8187, 8188)))
})

nc_close(file)
