data_dir <- file.path("..", "testdata")
tempfile_nc <- function() {
  tempfile_helper("monmin_")
}

########## simple case ##########
file_out <- tempfile_nc()
monmin("SIS",
       file.path(data_dir, "ex_mon.nc"),
       file_out)
file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file, "SIS")

  expected_data <- c(seq(1, 13, by = 3),
                     seq(2, 14, by = 3),
                     seq(3, 15, by = 3),
                     seq(466, 478, by = 3),
                     seq(467, 479, by = 3),
                     seq(468, 480, by = 3),
                     6, 9, 12, 15,
                     3, 7, 10, 13,
                     1, 4, 8, 11, 14,
                     2, 5)
  expected <- aperm(array(expected_data, c(3, 5, 3)), c(1, 2, 3))

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
  expect_equal(actual, "cmsaf::monmin for variable SIS")

  global_attr <- ncatt_get(file, 0)
  expect_equal(length(global_attr), 1)

  actual <- names(global_attr[1])
  expect_equal(actual, "Info")

  actual <- global_attr[[1]]
  expect_equal(actual, "Created with the CM SAF R Toolbox.")
})

test_that("coordinates are correct", {
  actual <- ncvar_get(file, "lon")
  expect_identical(actual, array(seq(5, 6, by = 0.5)))

  actual <- ncvar_get(file, "lat")
  expect_identical(actual, array(seq(45, 47, by = 0.5)))

  actual <- ncvar_get(file, "time")
  expect_equal(actual, array(c(149016, 149760, 150456)))
})

nc_close(file)

########## var does not exist #########
test_that("no error is thrown if var does not exist", {
  file_out <- tempfile_nc()
  expect_warning(monmin("someVariable",
                        file.path(data_dir, "ex_mon.nc"),
                        file_out),
                 "Variable 'someVariable' not found. Variable 'SIS' will be used instead.")
})

########## var is empty #########
test_that("no error is thrown if var is empty", {
  file_out <- tempfile_nc()
  expect_warning(monmin("",
                        file.path(data_dir, "ex_mon.nc"),
                        file_out),
                 "Variable '' not found. Variable 'SIS' will be used instead.")
})

########## var is NULL #########
test_that("error is thrown if var is NULL", {
  file_out <- tempfile_nc()
  expect_error(
    monmin(NULL,
           file.path(data_dir, "ex_mon.nc"),
           file_out),
    "variable must not be NULL"
  )
})

########## input file does not exist #########
test_that("error is thrown if input file does not exist", {
  file_out <- tempfile_nc()
  expect_error(
    monmin("SIS",
           file.path(data_dir, "ex_doesNotExist.nc"),
           file_out),
    "Input file does not exist")
})

########## input filename is empty #########
test_that("error is thrown if input file is empty", {
  file_out <- tempfile_nc()
  expect_error(
    monmin("SIS",
           "",
           file_out),
    "Input file does not exist")
})

########## input filename is NULL #########
test_that("error is thrown if input file is NULL", {
  file_out <- tempfile_nc()
  expect_error(
    monmin("SIS",
           NULL,
           file_out),
    "Input filepath must be of length one and not NULL")
})

########## output file already exists #########
test_that("error is thrown if output file already exists", {
  file_out <- tempfile_nc()
  cat("test\n", file = file_out)
  expect_error(
    monmin("SIS",
           file.path(data_dir, "ex_mon.nc"),
           file_out),
    paste0("File '",
           file_out,
           "' already exists. Specify 'overwrite = TRUE' if you want to overwrite it."),
    fixed = TRUE
  )

  expect_equal(readLines(con = file_out), "test")
})

########## output file already exists #########
test_that("no error is thrown if output file already exists", {
  file_out <- tempfile_nc()
  cat("test\n", file = file_out)
  expect_error(
    monmin("SIS",
           file.path(data_dir, "ex_mon.nc"),
           file_out,
           overwrite = TRUE),
    NA
  )
})

########## output file is NULL #########
test_that("no error is thrown if output file already exists", {
  expect_error(
    monmin("SIS",
           file.path(data_dir, "ex_mon.nc"),
           NULL),
    "Output filepath must be of length one and not NULL"
  )
})

#TODO add test case where data is in months instead of days or some other weird input
