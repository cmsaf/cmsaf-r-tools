data_dir <- file.path("..", "testdata")
tempfile_nc <- function() {
  tempfile_helper("selpoint_")
}
tempfile_csv <- function() {
  tempfile_helper_csv("selpoint_")
}

########## output ncdf version 3 ##########
file_in <- file.path(data_dir, "ex_normal1.nc")
file_out <- tempfile_nc()

selpoint("SIS", file_in, file_out, 6.2, 46.7)
file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file)

  expected_data <- c(250)
  expected <- array(expected_data)

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
  expect_equal(actual, "cmsaf::selpoint for variable SIS")

  global_attr <- ncatt_get(file, 0)
  expect_equal(length(global_attr), 1)

  actual <- names(global_attr[1])
  expect_equal(actual, "Info")

  actual <- global_attr[[1]]
  expect_equal(actual, "Created with the CM SAF R Toolbox.")
})

test_that("coordinates are correct", {
  actual <- ncvar_get(file, "lon")
  expect_identical(actual, array(6.0))

  actual <- ncvar_get(file, "lat")
  expect_identical(actual, array(46.5))

  actual <- ncvar_get(file, "time")
  cat(actual)
  expect_equal(actual, array(c(149016)))
})

nc_close(file)

########## output ncdf version 4 ##########
file_in <- file.path(data_dir, "ex_normal1.nc")
file_out <- tempfile_nc()

selpoint("SIS", file_in, file_out, 6.2, 46.7, nc34 = 4)
file <- nc_open(file_out)

test_that("data is correct in version 4", {
  actual <- ncvar_get(file)

  expected_data <- c(250)
  expected <- array(expected_data)

  expect_equivalent(actual, expected)
})

test_that("attributes are correct in version 4", {
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

test_that("coordinates are correct in version 4", {
  actual <- ncvar_get(file, "lon")
  expect_identical(actual, array(6.0))

  actual <- ncvar_get(file, "lat")
  expect_identical(actual, array(46.5))

  actual <- ncvar_get(file, "time")
  expect_equal(actual, array(c(149016)))
})

nc_close(file)

########## output ncdf version 7 #########
file_in <- file.path(data_dir, "ex_normal1.nc")
file_out <- tempfile_nc()

test_that("error is thrown if ncdf version is wrong", {
  expect_error(
    selpoint("SIS", file_in, file_out, 6.2, 46.7, nc34 = 7),
    "nc version must be in c(3, 4), but was 7", fixed = TRUE
  )
})

########## output ncdf version is NULL #########
file_in <- file.path(data_dir, "ex_normal1.nc")
file_out <- tempfile_nc()

test_that("ncdf version NULL throws an error", {
  expect_error(
    selpoint("SIS", file_in, file_out, 6.2, 46.7, nc34 = NULL),
    "nc_version must not be NULL"
  )
})

########## variable does not exist #########
file_in <- file.path(data_dir, "ex_normal1.nc")
file_out <- tempfile_nc()

test_that("warning is shown if var does not exist", {
  expect_warning(
    selpoint("lat", file_in, file_out, 6.2, 46.7),
    "Variable 'lat' not found. Variable 'SIS' will be used instead."
  )
})

file <- nc_open(file_out)

test_that("data is correct if non-existing variable is given", {
  actual <- ncvar_get(file)

  expected_data <- c(250)
  expected <- array(expected_data)

  expect_equivalent(actual, expected)
})

test_that("attributes are correct if non-existing variable is given", {
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

test_that("coordinates are correct if non-existing variable is given", {
  actual <- ncvar_get(file, "lon")
  expect_identical(actual, array(6.0))

  actual <- ncvar_get(file, "lat")
  expect_identical(actual, array(46.5))

  actual <- ncvar_get(file, "time")
  expect_equal(actual, array(c(149016)))
})

nc_close(file)


########## variable is null #########
file_in <- file.path(data_dir, "ex_normal1.nc")
file_out <- tempfile_nc()

test_that("error is thrown if variable is NULL", {
  expect_error(
    selpoint(NULL, file_in, file_out, 6.2, 46.7),
    "variable must not be NULL"
  )
})

########## variable is empty #########
file_in <- file.path(data_dir, "ex_normal1.nc")
file_out <- tempfile_nc()

test_that("warning is shown if var is empty", {
  expect_warning(
    selpoint("", file_in, file_out, 6.2, 46.7),
    "Variable '' not found. Variable 'SIS' will be used instead."
  )
})

file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file)

  expected_data <- c(250)
  expected <- array(expected_data)

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
  expect_identical(actual, array(6.0))

  actual <- ncvar_get(file, "lat")
  expect_identical(actual, array(46.5))

  actual <- ncvar_get(file, "time")
  expect_equal(actual, array(c(149016)))
})

nc_close(file)


########## input file does not exist #########
file_in <- file.path(data_dir, "xex_normal1.nc")
file_out <- tempfile_nc()

test_that("error is thrown if input file does not exist", {
  expect_error(
    selpoint("SIS", file_in, file_out, 6.2, 46.7),
    "Input file does not exist")
})

########## input filename is NULL #########
file_in <- file.path(data_dir, NULL)
file_out <- tempfile_nc()

test_that("error is thrown if input filename is NULL", {
  expect_error(
    selpoint("SIS", file_in, file_out, 6.2, 46.7),
    "Input filepath must be of length one and not NULL"
  )
})

########## output file already exists #########
file_in <- file.path(data_dir, "ex_normal1.nc")
file_out <- tempfile_nc()
cat("test\n", file = file_out)

test_that("error is thrown if output file already exists", {
  expect_error(
    selpoint("SIS", file_in, file_out, 6.2, 46.7),
    paste0("File '",
           file_out,
           "' already exists. Specify 'overwrite = TRUE' if you want to overwrite it."),
    fixed = TRUE
  )

  expect_equal(readLines(con = file_out), "test")
})

########## output file already exists (overwrite = TRUE) #########
file_in <- file.path(data_dir, "ex_normal1.nc")
file_out <- tempfile_nc()
cat("test\n", file = file_out)

test_that("no error is thrown if overwrite = TRUE", {
  expect_error(
    selpoint("SIS", file_in, file_out, 6.2, 46.7, overwrite = TRUE),
    NA)
})

file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file)

  expected_data <- c(250)
  expected <- array(expected_data)

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
  expect_identical(actual, array(6.0))

  actual <- ncvar_get(file, "lat")
  expect_identical(actual, array(46.5))

  actual <- ncvar_get(file, "time")
  expect_equal(actual, array(c(149016)))
})

nc_close(file)

########## time length is 2 ##########
file_in <- file.path(data_dir, "ex_time_dim1.nc")
file_out <- tempfile_nc()
selpoint("SIS", file_in, file_out, 6.2, 46.7)
file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file)

  expected_data <- c(250, 253)
  expected <- array(expected_data)

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
  expect_identical(actual, array(6.0))

  actual <- ncvar_get(file, "lat")
  expect_identical(actual, array(46.5))

  actual <- ncvar_get(file, "time")
  expect_equal(actual, array(c(149016, 158544)))
})

nc_close(file)

########## additional attribute ##########
file_in <- file.path(data_dir, "ex_additional_attr.nc")
file_out <- tempfile_nc()
selpoint("SIS", file_in, file_out, 6.2, 46.7)
file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file)

  expected_data <- c(250)
  expected <- array(expected_data)

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
  expect_equal(length(global_attr), 2)

  actual <- names(global_attr[1])
  expect_equal(actual, "Info")

  actual <- global_attr[[1]]
  expect_equal(actual, "Created with the CM SAF R Toolbox.")

  actual <- names(global_attr[2])
  expect_equal(actual, "institution")

  actual <- global_attr[[2]]
  expect_equal(actual, "This is a test attribute.")
})

test_that("coordinates are correct", {
  actual <- ncvar_get(file, "lon")
  expect_identical(actual, array(6.0))

  actual <- ncvar_get(file, "lat")
  expect_identical(actual, array(46.5))

  actual <- ncvar_get(file, "time")
  expect_equal(actual, array(c(149016)))
})

nc_close(file)

########## input ncdf version 4 ##########
file_in <- file.path(data_dir, "ex_v4_1.nc")
file_out <- tempfile_nc()
selpoint("SIS", file_in, file_out, 6.2, 46.7)
file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file)

  expected_data <- c(250)
  expected <- array(expected_data)

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
  expect_identical(actual, array(6.0))

  actual <- ncvar_get(file, "lat")
  expect_identical(actual, array(46.5))

  actual <- ncvar_get(file, "time")
  expect_equal(actual, array(c(149016)))
})

nc_close(file)

########## input lon & lat in range ##########
file_in <- file.path(data_dir, "ex_normal1.nc")
file_out <- tempfile_nc()
selpoint("SIS", file_in, file_out, 6.2, 46.7)
file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file)

  expected_data <- c(250)
  expected <- array(expected_data)

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
  expect_identical(actual, array(6.0))

  actual <- ncvar_get(file, "lat")
  expect_identical(actual, array(46.5))

  actual <- ncvar_get(file, "time")
  expect_equal(actual, array(c(149016)))
})

nc_close(file)

########## input lon out of range & lat in range ##########
file_in <- file.path(data_dir, "ex_normal1.nc")
file_out <- tempfile_nc()

test_that("ncdf version NULL throws an error", {
  expect_error(
    selpoint("SIS", file_in, file_out, 45, 46.7),
    "Coordinates outside of the domain.")
})

########## input lon in range & lat out ofrange ##########
file_in <- file.path(data_dir, "ex_normal1.nc")
file_out <- tempfile_nc()

test_that("ncdf version NULL throws an error", {
  expect_error(
    selpoint("SIS", file_in, file_out, 6.2, 7),
    "Coordinates outside of the domain.")
})

########## input lon & lat out of range ##########
file_in <- file.path(data_dir, "ex_normal1.nc")
file_out <- tempfile_nc()

test_that("ncdf version NULL throws an error", {
  expect_error(
    selpoint("SIS", file_in, file_out, 45, 6),
    "Coordinates outside of the domain.")
})

########## output format NULL ##########
file_in <- file.path(data_dir, "ex_normal1.nc")
file_out <- tempfile_nc()

test_that("format NULL throws an error", {
  expect_error(
    selpoint("SIS", file_in, file_out, 6.2, 46.7, format = NULL),
    "format must not be NULL"
  )
})

########## output format pdf ##########
file_in <- file.path(data_dir, "ex_normal1.nc")
file_out <- tempfile_nc()

test_that("format NULL throws an error", {
  expect_error(
    selpoint("SIS", file_in, file_out, 6.2, 46.7, format = "pdf"),
    "format must be either nc or csv, but was pdf"
  )
})

########## output format csv ##########
file_in <- file.path(data_dir, "ex_normal1.nc")
file_out <- tempfile_csv()

selpoint("SIS", file_in, file_out, 6.2, 46.7, format = "csv")

test_that("data is correct", {
  actual <- read.csv2(file_out, dec = ".")

  expected_data <- data.frame("2000-01-01", 250)
  expected <- expected_data

  expect_equivalent(actual, expected)
})
