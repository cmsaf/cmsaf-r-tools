data_dir <- file.path("..", "testdata")
tempfile_nc <- function() {
  tempfile_helper("extract.period_")
}

########## simple case ##########
file_out <- tempfile_nc()
extract.period("SIS",
               "2000-02-01",
               "2000-04-01",
               file.path(data_dir, "ex_extract_period.nc"),
               file_out)
file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file)

  expected_data <- c(250:258, 263:271)
  expected <- aperm(array(expected_data, dim = c(3, 3, 2)), c(2, 1, 3))

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
  expect_equal(actual, "cmsaf::extract.period for variable SIS")

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
  expect_identical(actual, array(seq(45, 46, by = 0.5)))

  actual <- ncvar_get(file, "time")
  expect_equal(actual, array(c(149016, 151920)))
})

nc_close(file)

########## var does not exist #########
test_that("no error is thrown if var does not exist", {
  file_out <- tempfile_nc()
  expect_warning(extract.period("someVariable",
                                "2000-02-01",
                                "2000-04-01",
                                file.path(data_dir, "ex_extract_period.nc"),
                                file_out),
                 "Variable 'someVariable' not found. Variable 'SIS' will be used instead.")
})

########## var is empty #########
test_that("no error is thrown if var is empty", {
  file_out <- tempfile_nc()
  expect_warning(extract.period("",
                                "2000-02-01",
                                "2000-04-01",
                                file.path(data_dir, "ex_extract_period.nc"),
                                file_out),
                 "Variable '' not found. Variable 'SIS' will be used instead.")
})

########## var is NULL #########
test_that("error is thrown if var is NULL", {
  file_out <- tempfile_nc()
  expect_error(
    extract.period(NULL,
                   "2000-02-01",
                   "2000-04-01",
                   file.path(data_dir, "ex_extract_period.nc"),
                   file_out),
    "variable must not be NULL"
  )
})

########## input file does not exist #########
test_that("error is thrown if input file does not exist", {
  file_out <- tempfile_nc()
  expect_error(
    extract.period("SIS",
                   "2000-02-01",
                   "2000-04-01",
                   file.path(data_dir, "ex_doesNotExist.nc"),
                   file_out),
    "Input file does not exist")
})

########## input filename is empty #########
test_that("error is thrown if input file is empty", {
  file_out <- tempfile_nc()
  expect_error(
    extract.period("SIS",
                   "2000-02-01",
                   "2000-04-01",
                   "",
                   file_out),
    "Input file does not exist")
})

########## input filename is NULL #########
test_that("error is thrown if input file is NULL", {
  file_out <- tempfile_nc()
  expect_error(
    extract.period("SIS",
                   "2000-02-01",
                   "2000-04-01",
                   NULL,
                   file_out),
    "Input filepath must be of length one and not NULL"
  )
})

########## output file already exists #########
test_that("error is thrown if output file already exists", {
  file_out <- tempfile_nc()
  cat("test\n", file = file_out)

  expect_error(
    extract.period("SIS",
                   "2000-02-01",
                   "2000-04-01",
                   file.path(data_dir, "ex_extract_period.nc"),
                   file_out),
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
    extract.period("SIS",
                   "2000-02-01",
                   "2000-04-01",
                   file.path(data_dir, "ex_extract_period.nc"),
                   file_out,
                   overwrite = TRUE
    ),
    NA)
})

########## output file is NULL #########
test_that("no error is thrown if output file already exists", {
  expect_error(
    extract.period("SIS",
                   "2000-02-01",
                   "2000-04-01",
                   file.path(data_dir, "ex_extract_period.nc"),
                   NULL),
    "Output filepath must be of length one and not NULL"
  )
})

########## simple case where only one value is removed ##########
file_out <- tempfile_nc()
extract.period("SIS",
               "2000-03-01",
               "2000-03-01",
               file.path(data_dir, "ex_extract_period.nc"),
               file_out)
file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file)

  expected_data <- c(250:267, 254:271)
  expected <- aperm(array(expected_data, dim = c(3, 3, 4)), c(2, 1, 3))

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
  expect_identical(actual, array(seq(45, 46, by = 0.5)))

  actual <- ncvar_get(file, "time")
  expect_equal(actual, array(c(149016, 149760, 151200, 151920)))
})

nc_close(file)

########## remove value outside of range ##########
test_that("Message is shown if data is outside range", {
  file_out <- tempfile_nc()
  expect_message(extract.period("SIS",
                              "2010-03-01",
                              "2011-03-01",
                              file.path(data_dir, "ex_extract_period.nc"),
                              file_out, verbose = TRUE),
               "Nothing removed.")
})

########## start value is formatted incorrectly ##########
test_that("error is thrown if start date is not formatted correctly", {
  file_out <- tempfile_nc()
  expect_error(extract.period("SIS",
                              "01.03.2000",
                              "2000-03-01",
                              file.path(data_dir, "ex_extract_period.nc"),
                              file_out),
               "An error occured, the start date has the wrong format. Please use 'YYYY-MM-DD' format.")
})

########## end value is not a date ##########
test_that("error is thrown if end date is not a date", {
  file_out <- tempfile_nc()
  expect_error(extract.period("SIS",
                              "2000-01-01",
                              "October",
                              file.path(data_dir, "ex_extract_period.nc"),
                              file_out),
               "An error occured, the end date has the wrong format. Please use 'YYYY-MM-DD' format.")
})


########## start value is NULL ##########
test_that("no error is thrown if start date is NULL", {
  file_out <- tempfile_nc()
  expect_error(extract.period("SIS",
                              NULL,
                              "2000-03-01",
                              file.path(data_dir, "ex_extract_period.nc"),
                              file_out),
               "An error occured, the start date has the wrong format. Please use 'YYYY-MM-DD' format.")
})

########## end value is NULL ##########
test_that("no error is thrown if end date is NULL", {
  file_out <- tempfile_nc()
  expect_error(extract.period("SIS",
                              "2000-02-01",
                              NULL,
                              file.path(data_dir, "ex_extract_period.nc"),
                              file_out),
               "An error occured, the end date has the wrong format. Please use 'YYYY-MM-DD' format.")
})

########## start value is NA ##########
test_that("no error is thrown if start date is NA", {
  file_out <- tempfile_nc()
  expect_error(extract.period("SIS",
                              NA,
                              "2000-03-01",
                              file.path(data_dir, "ex_extract_period.nc"),
                              file_out),
               "An error occured, the start date has the wrong format. Please use 'YYYY-MM-DD' format.")
})

########## end value is NA ##########
test_that("no error is thrown if end date is NA", {
  file_out <- tempfile_nc()
  expect_error(extract.period("SIS",
                              "2000-02-01",
                              NA,
                              file.path(data_dir, "ex_extract_period.nc"),
                              file_out),
               "An error occured, the end date has the wrong format. Please use 'YYYY-MM-DD' format.")
})
