data_dir <- file.path("..", "testdata")
tempfile_nc <- function() {
  tempfile_helper("monsum_")
}

########## simple case ##########
file_out <- tempfile_nc()
monsum("SIS",
       file.path(data_dir, "ex_mon.nc"),
       file_out)
file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file, "SIS")

  expected_data <- c(7006, 7099, 7192, 7285, 7378,
                     7037, 7130, 7223, 7316, 7409,
                     7068, 7161, 7254, 7347, 7440,
                     19604, 19691, 19778, 19865, 19952,
                     19633, 19720, 19807, 19894, 19981,
                     19662, 19749, 19836, 19923, 20010,
                     10906, 10999, 11092, 11185,
                     10278, 10937, 11030, 11123,
                     10216, 10309, 10968, 11061, 11154,
                     10247, 10340)
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
  expect_equal(actual, "cmsaf::monsum for variable SIS")

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
  expect_warning(monsum("someVariable",
                        file.path(data_dir, "ex_mon.nc"),
                        file_out),
                 "Variable 'someVariable' not found. Variable 'SIS' will be used instead.")
})

########## var is empty #########
test_that("no error is thrown if var is empty", {
  file_out <- tempfile_nc()
  expect_warning(monsum("",
                        file.path(data_dir, "ex_mon.nc"),
                        file_out),
                 "Variable '' not found. Variable 'SIS' will be used instead.")
})

########## var is NULL #########
test_that("error is thrown if var is NULL", {
  file_out <- tempfile_nc()
  expect_error(
    monsum(NULL,
           file.path(data_dir, "ex_mon.nc"),
           file_out),
    "variable must not be NULL"
  )
})

########## input file does not exist #########
test_that("error is thrown if input file does not exist", {
  file_out <- tempfile_nc()
  expect_error(
    monsum("SIS",
           file.path(data_dir, "ex_doesNotExist.nc"),
           file_out),
    "Input file does not exist")
})

########## input filename is empty #########
test_that("error is thrown if input file is empty", {
  file_out <- tempfile_nc()
  expect_error(
    monsum("SIS",
           "",
           file_out),
    "Input file does not exist")
})

########## input filename is NULL #########
test_that("error is thrown if input file is NULL", {
  file_out <- tempfile_nc()
  expect_error(
    monsum("SIS",
           NULL,
           file_out),
    "Input filepath must be of length one and not NULL")
})

########## output file already exists #########
test_that("no error is thrown if output file already exists", {
  file_out <- tempfile_nc()
  cat("test\n", file = file_out)
  expect_error(
    monsum("SIS",
           file.path(data_dir, "ex_mon.nc"),
           file_out
    ),
    paste0("File '",
           file_out,
           "' already exists. Specify 'overwrite = TRUE' if you want to overwrite it."),
    fixed = TRUE
  )

  expect_equal(readLines(con = file_out), "test")
})

########## output file already exists (overwrite = TRUE) #########
test_that("no error is thrown if overwrite = TRUE", {
  file_out <- tempfile_nc()
  cat("test\n", file = file_out)
  expect_error(
    monsum("SIS",
           file.path(data_dir, "ex_mon.nc"),
           file_out,
           overwrite = TRUE
    ),
    NA
  )
})

########## output file is NULL #########
test_that("no error is thrown if output file already exists", {
  expect_error(
    monsum("SIS",
           file.path(data_dir, "ex_mon.nc"),
           NULL),
    "Output filepath must be of length one and not NULL"
  )
})

#TODO add test case where data is in months instead of days or some other weird input
