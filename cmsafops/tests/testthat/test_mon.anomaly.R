data_dir <- file.path("..", "testdata")
tempfile_nc <- function() {
  tempfile_helper("mon.anomaly")
}

testdata_filename <- "ex_mon2.nc"


########## test data ##########
file_out <- tempfile_nc()
file_ref <- nc_open(file.path(data_dir, "out_ref_mon.anomaly.nc"))

mon.anomaly("SIS",
             file.path(data_dir, testdata_filename),
             file_out)
file <- nc_open(file_out)


test_that("data is correct", {
  actual <- ncvar_get(file, "SIS")
  expected <- ncvar_get(file_ref, "SIS")

  expect_equivalent(actual, expected)

})



########## test attributes ##########


test_that("attributes are correct", {
  actual <- ncatt_get(file, "lon", "units")$value
  exp <- expect_equal(actual, "degrees_east")

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

})


test_that("coordinates are correct", {
  actual <- ncvar_get(file, "lon")
  expect_identical(actual, array(seq(5, 7, 0.5)))

  actual <- ncvar_get(file, "lat")
  expect_identical(actual, array(seq(45, 47, 0.5)))

  actual <- ncvar_get(file, "time")
  expected <- ncvar_get(file_ref, "time")
  expect_equal(actual, expected)

})



########## var is empty #########
test_that("no error is thrown if var is empty", {
  file_out <- tempfile_nc()
  expect_warning(mon.anomaly("",
                        file.path(data_dir, testdata_filename),
                        file_out),
                 "Variable '' not found. Variable 'SIS' will be used instead.")
})

########## var is NULL #########
test_that("error is thrown if var is NULL", {
  file_out <- tempfile_nc()
  expect_error(mon.anomaly(NULL,
                      file.path(data_dir, testdata_filename),
                      file_out),
               "variable must not be NULL")
})


nc_close(file)
nc_close(file_ref)
