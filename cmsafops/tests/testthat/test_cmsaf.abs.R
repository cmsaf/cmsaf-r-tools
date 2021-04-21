data_dir <- file.path("..", "testdata")
tempfile_nc <- function() {
  tempfile_helper("cmsaf.abs_")
}

########## simple case ##########
file_out <- tempfile_nc()
cmsaf.abs("SIS",
        file.path(data_dir, "ex_cmsaf.abs.nc"),
        file_out)
file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file, "SIS")

  expected_data <- c(
    10.0,9.0,8.0,
    7.0,6.0,5.0,
    4.0,3.0,2.0,
    1,0.0,1.0,
    2.0,3.0,4.0,
    
    5.0,6.0,7.0,
    8.0,9.0,10.0,
    11.0,12.0,13.0,
    14.0,15.0,16.0,
    17.0,18.0,19.0,
    
    20.0,21.0,22.0,
    23.0,24.0,25.0,
    26.0,27.0,28.0,
    29.0,30.0,31.0,
    32.0,33.0,34.0
  )
  expected <- array(expected_data, c(3, 5, 3))

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
  expect_equal(actual, "cmsaf::cmsaf.abs for variable SIS")

  global_attr <- ncatt_get(file, 0)
  expect_equal(length(global_attr), 1)

  actual <- names(global_attr[1])
  expect_equal(actual, "Info")

  actual <- global_attr[[1]]
  expect_equal(actual, "Created with the CM SAF R Toolbox.")
})

test_that("coordinates are correct", {
  actual <- ncvar_get(file, "lon")
  expect_identical(actual, array(seq(5, 7, by = 1)))

  actual <- ncvar_get(file, "lat")
  expect_identical(actual, array(seq(45, 49, by = 1)))

  actual <- ncvar_get(file, "time")
  expect_equal(actual, array(c(149016, 149760, 150456)))
})

nc_close(file)

########## var does not exist #########
test_that("warning is shown if var does not exist", {
  file_out <- tempfile_nc()
  expect_warning(
    cmsaf.abs("someVariable",
            file.path(data_dir, "ex_cmsaf.abs.nc"),
            file_out),
    "Variable 'someVariable' not found. Variable 'SIS' will be used instead.")
})

########## var is empty #########
test_that("warning is shown if var is empty", {
  file_out <- tempfile_nc()
  expect_warning(
    cmsaf.abs("",
            file.path(data_dir, "ex_cmsaf.abs.nc"),
            file_out),
    "Variable '' not found. Variable 'SIS' will be used instead.")
})

########## var is NULL #########
test_that("error is thrown if var is NULL", {
  file_out <- tempfile_nc()
  expect_error(
    cmsaf.abs(NULL,
            file.path(data_dir, "ex_cmsaf.abs.nc"),
            file_out),
    "variable must not be NULL"
  )
})

########## input file does not exist #########
test_that("error is thrown if input file does not exist", {
  file_out <- tempfile_nc()
  expect_error(
    cmsaf.abs("SIS",
            file.path(data_dir, "ex_doesNotExist.nc"),
            file_out),
    "Input file does not exist")
})

########## input filename is NULL #########
test_that("error is thrown if input file is NULL", {
  file_out <- tempfile_nc()
  expect_error(
    cmsaf.abs("SIS",
            NULL,
            file_out),
    "Input filepath must be of length one and not NULL"
  )
})

########## output file already exists #########
test_that("no error is thrown if output file already exists", {
  file_out <- tempfile_nc()
  cat("test\n", file = file_out)
  expect_error(
    cmsaf.abs("SIS",
            file.path(data_dir, "ex_cmsaf.abs.nc"),
            file_out),
    paste0("File '",
           file_out,
           "' already exists. Specify 'overwrite = TRUE' if you want to overwrite it."),
    fixed = TRUE)
})

########## output file already exists (overwrite = TRUE) #########
test_that("no error is thrown if overwrite = TRUE", {
  file_out <- tempfile_nc()
  cat("test\n", file = file_out)
  expect_error(
    cmsaf.abs("SIS",
            file.path(data_dir, "ex_cmsaf.abs.nc"),
            file_out,
            overwrite = TRUE),
    NA)
})

########## output file is NULL #########
test_that("error is thrown if output file is NULL", {
  expect_error(
    cmsaf.abs("SIS",
            file.path(data_dir, "ex_cmsaf.abs.nc"),
            NULL),
    "Output filepath must be of length one and not NULL"
  )
})

#TODO add test case where input data is weird somehow
