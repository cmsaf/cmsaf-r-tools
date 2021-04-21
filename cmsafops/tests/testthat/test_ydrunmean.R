data_dir <- file.path("..", "testdata")
tempfile_nc <- function() {
  tempfile_helper("ydrunmean_")
}

########## simple case ##########
file_out <- tempfile_nc()
ydrunmean("SIS", nts = 10,
         file.path(data_dir, "ex_yday.nc"),
         file_out)
file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file, "SIS")

  expect_equal(dim(actual), c(3, 5, 366))

  expected <- c(
    62.8,59.65,56.5,
    59.5,62.5,63.8,
    60.65,57.5,60.5,
    63.5,64.8,61.65,
    58.5,61.5,64.5,
    
    65.5,62.35,59.199997,
    62.199997,65.2,66.5,
    63.35,60.199997,63.199997,
    66.2,67.5,64.35,
    61.199997,64.2,67.2,
    
    68.2,65.05,55.75,
    58.75,61.75,69.2,
    66.05,56.75,59.75,
    62.75,70.2,67.05,
    57.75,60.75,63.75
  )
  expect_equivalent(actual[1:45], expected, tolerance = 1e-5)

  expected <- c(
    61.3,64.3,55.0,
    58.0,61.0,62.3,
    65.3,56.0,59.0,
    62.0,63.3,66.3,
    57.0,60.0,63.0,
    
    64.0,67.0,57.699997,
    60.699997,63.699997,65.0,
    68.0,58.699997,61.699997,
    64.7,66.0,69.0,
    59.699997,62.699997,65.7,
    
    66.7,69.7,60.4,
    57.25,60.25,67.7,
    70.7,61.4,58.25,
    61.25,68.7,71.7,
    62.4,59.25,62.25
  )
  expect_equivalent(actual[61:105], expected, tolerance = 1e-5)
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
  expect_equal(actual, "cmsaf::ydrunmean for variable SIS")

  global_attr <- ncatt_get(file, 0)
  expect_equal(length(global_attr), 1)

  actual <- names(global_attr[1])
  expect_equal(actual, "Info")

  actual <- global_attr[[1]]
  expect_equal(actual, "Created with the CM SAF R Toolbox.")
})
