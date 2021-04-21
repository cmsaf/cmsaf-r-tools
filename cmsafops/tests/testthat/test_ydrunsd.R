data_dir <- file.path("..", "testdata")
tempfile_nc <- function() {
  tempfile_helper("ydrunsd_")
}

########## simple case ##########
file_out <- tempfile_nc()
ydrunsd("SIS", nts = 10,
        file.path(data_dir, "ex_yday.nc"),
        file_out)
file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file, "SIS")

  expect_equal(dim(actual), c(3, 5, 366))

  expected <- c(
    2.8722517,1.757026,0.0,
    0.0,0.0,2.8722517,
    1.757026,0.0,0.0,
    0.0,2.8722517,1.757026,
    0.0,0.0,0.0,
    
    0.0,1.757026,2.8722517,
    2.8722517,2.8722517,0.0,
    1.757026,2.8722517,2.8722517,
    2.8722517,0.0,1.757026,
    2.8722517,2.8722517,2.8722517,
    
    2.8722517,5.1562924,5.574435,
    5.574435,5.574435,2.8722517,
    5.1562924,5.574435,5.574435,
    5.574435,2.8722517,5.1562924,
    5.574435,5.574435,5.574435
  )
  
  expect_equivalent(actual[1:45], expected, tolerance = 1e-6)

  expected <- c(
    2.8722517,2.8722517,0.0,
    0.0,0.0,2.8722517,
    2.8722517,0.0,0.0,
    0.0,2.8722517,2.8722517,
    0.0,0.0,0.0,
    
    0.0,0.0,2.8722517,
    2.8722517,2.8722517,0.0,
    0.0,2.8722517,2.8722517,
    2.8722517,0.0,0.0,
    2.8722517,2.8722517,2.8722517,
    
    2.8722517,2.8722517,5.539609,
    5.574435,5.574435,2.8722517,
    2.8722517,5.539609,5.574435,
    5.574435,2.8722517,2.8722517,
    5.539609,5.574435,5.574435
  )
  
  expect_equivalent(actual[61:105], expected, tolerance = 1e-6)
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
  expect_equal(actual, "cmsaf::ydrunsd for variable SIS")

  global_attr <- ncatt_get(file, 0)
  expect_equal(length(global_attr), 1)

  actual <- names(global_attr[1])
  expect_equal(actual, "Info")

  actual <- global_attr[[1]]
  expect_equal(actual, "Created with the CM SAF R Toolbox.")
})
