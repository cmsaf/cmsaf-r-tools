data_dir <- file.path("..", "testdata")
tempfile_nc <- function() {
  tempfile_helper("cmsaf.div_")
}

########## output ncdf version 3 ##########
file_out <- tempfile_nc()
cmsaf.div("SIS", "SIS",
          file.path(data_dir, "ex_normal1.nc"),
          file.path(data_dir, "ex_normal2.nc"),
          file_out)
file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file)

  expected_data <- rep(c(1.086957, 1.08658, 1.086207, 1.085837, 1.08547,
                         1.085106, 1.084746, 1.084388, 1.084034, 1.083682,
                         1.083333, 1.082988, 1.082645, 1.082304, 1.081967,
                         1.081633, 1.081301, 1.080972, 1.080645, 1.080321,
                         1.08, 1.079681, 1.079365), 3)
  expected <- array(expected_data, dim = c(7, 7))

  expect_equivalent(actual, expected, tolerance = 1e-06)
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
  expect_equal(actual, "cmsaf::cmsaf.div for variable SIS")

  global_attr <- ncatt_get(file, 0)
  expect_equal(length(global_attr), 1)

  actual <- names(global_attr[1])
  expect_equal(actual, "Info")

  actual <- global_attr[[1]]
  expect_equal(actual, "Created with the CM SAF R Toolbox.")
})

test_that("coordinates are correct", {
  actual <- ncvar_get(file, "lon")
  expect_identical(actual, array(seq(5, 8, 0.5)))

  actual <- ncvar_get(file, "lat")
  expect_identical(actual, array(seq(45, 48, 0.5)))

  actual <- ncvar_get(file, "time")
  expect_equal(actual, array(149016))
})

nc_close(file)

########## output ncdf version 4 ##########
#TODO unknown error
file_out <- tempfile_nc()
cmsaf.div("SIS", "SIS",
          file.path(data_dir, "ex_normal1.nc"),
          file.path(data_dir, "ex_normal2.nc"),
          file_out, nc34 = 4)

file <- nc_open(file_out)

test_that("data is correct in version 4", {
  actual <- ncvar_get(file)

  expected_data <- rep(c(1.086957, 1.08658, 1.086207, 1.085837, 1.08547,
                         1.085106, 1.084746, 1.084388, 1.084034, 1.083682,
                         1.083333, 1.082988, 1.082645, 1.082304, 1.081967,
                         1.081633, 1.081301, 1.080972, 1.080645, 1.080321,
                         1.08, 1.079681, 1.079365), 3)
  expected <- array(expected_data, dim = c(7, 7))

  expect_equivalent(actual, expected, tolerance = 1e-06)
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
  expect_identical(actual, array(seq(5, 8, 0.5)))

  actual <- ncvar_get(file, "lat")
  expect_identical(actual, array(seq(45, 48, 0.5)))

  actual <- ncvar_get(file, "time")
  expect_equal(actual, array(149016))
})

nc_close(file)

########## output ncdf version 7 #########
file_out <- tempfile_nc()

test_that("error is thrown if ncdf version is wrong", {
  expect_error(
    cmsaf.div("SIS", "SIS",
              file.path(data_dir, "ex_normal1.nc"),
              file.path(data_dir, "ex_normal2.nc"),
              file_out, nc34 = 7),
    "nc version must be in c(3, 4), but was 7", fixed = TRUE
  )
})

########## output ncdf version is NULL #########
file_out <- tempfile_nc()

test_that("ncdf version NULL throws an error", {
  expect_error(
    cmsaf.div("SIS", "SIS",
              file.path(data_dir, "ex_normal1.nc"),
              file.path(data_dir, "ex_normal2.nc"),
              file_out, nc34 = NULL),
    "nc_version must not be NULL"
  )
})

########## variable does not exist #########
file_out <- tempfile_nc()

test_that("warning is shown if var does not exist", {
  expect_warning(
    cmsaf.div("lat", "lon",
              file.path(data_dir, "ex_normal1.nc"),
              file.path(data_dir, "ex_normal2.nc"), file_out),
    "Variable 'lat' not found. Variable 'SIS' will be used instead."
  )
})

file <- nc_open(file_out)

test_that("data is correct if non-existing variable is given", {
  actual <- ncvar_get(file)

  expected_data <- rep(c(1.086957, 1.08658, 1.086207, 1.085837, 1.08547,
                         1.085106, 1.084746, 1.084388, 1.084034, 1.083682,
                         1.083333, 1.082988, 1.082645, 1.082304, 1.081967,
                         1.081633, 1.081301, 1.080972, 1.080645, 1.080321,
                         1.08, 1.079681, 1.079365), 3)
  expected <- array(expected_data, dim = c(7, 7))

  expect_equivalent(actual, expected, tolerance = 1e-06)
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
  expect_identical(actual, array(seq(5, 8, 0.5)))

  actual <- ncvar_get(file, "lat")
  expect_identical(actual, array(seq(45, 48, 0.5)))

  actual <- ncvar_get(file, "time")
  expect_equal(actual, array(149016))
})

nc_close(file)


########## variable is null #########
file_out <- tempfile_nc()

test_that("error is thrown if variable is NULL", {
  expect_error(
    cmsaf.div(NULL, "SIS",
              file.path(data_dir, "ex_normal1.nc"),
              file.path(data_dir, "ex_normal2.nc"),
              file_out),
    "variable must not be NULL"
  )
})

########## variable is empty #########
file_out <- tempfile_nc()

test_that("warning is shown if var is empty", {
  expect_warning(
    cmsaf.div("SIS", "",
              file.path(data_dir, "ex_normal1.nc"),
              file.path(data_dir, "ex_normal2.nc"), file_out),
    "Variable '' not found. Variable 'SIS' will be used instead."
  )
})

file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file)

  expected_data <- rep(c(1.086957, 1.08658, 1.086207, 1.085837, 1.08547,
                         1.085106, 1.084746, 1.084388, 1.084034, 1.083682,
                         1.083333, 1.082988, 1.082645, 1.082304, 1.081967,
                         1.081633, 1.081301, 1.080972, 1.080645, 1.080321,
                         1.08, 1.079681, 1.079365), 3)
  expected <- array(expected_data, dim = c(7, 7))

  expect_equivalent(actual, expected, tolerance = 1e-06)
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
  expect_identical(actual, array(seq(5, 8, 0.5)))

  actual <- ncvar_get(file, "lat")
  expect_identical(actual, array(seq(45, 48, 0.5)))

  actual <- ncvar_get(file, "time")
  expect_equal(actual, array(149016))
})

nc_close(file)


########## input file does not exist #########
file_out <- tempfile_nc()
test_that("error is thrown if input file does not exist", {
  expect_error(
    cmsaf.div("SIS", "SIS",
              file.path(data_dir, "xemaple1.nc"),
              file.path(data_dir, "ex_normal2.nc"),
              file_out),
    "Input file does not exist")
})

########## input filename is empty #########
file_out <- tempfile_nc()

test_that("error is thrown if input filename is empty", {
  expect_error(
    cmsaf.div("SIS", "SIS",
              "",
              file.path(data_dir, "ex_normal2.nc"),
              file_out),
    "Input file does not exist")
})

########## input filename is NULL #########
# TODO this should throw an error
file_out <- tempfile_nc()

test_that("error is thrown if input filename is NULL", {
  expect_error(
    cmsaf.div("SIS", "SIS",
              file.path(data_dir, "ex_normal1.nc"),
              NULL,
              file_out),
    "Input filepath must be of length one and not NULL")
})

########## output file already exists #########
file_out <- tempfile_nc()
cat("test\n", file = file_out)

test_that("error is thrown if output file already exists", {
  expect_error(
    cmsaf.div("SIS", "SIS",
              file.path(data_dir, "ex_normal1.nc"),
              file.path(data_dir, "ex_normal2.nc"),
              file_out),
    paste0("File '",
           file_out,
           "' already exists. Specify 'overwrite = TRUE' if you want to overwrite it."),
    fixed = TRUE
  )

  expect_equal(readLines(con = file_out), "test")
})

########## output file already exists (overwrite = TRUE) #########
file_out <- tempfile_nc()
cat("test\n", file = file_out)

test_that("no error is thrown if overwrite = TRUE", {
  expect_error(
    cmsaf.div("SIS", "SIS",
              file.path(data_dir, "ex_normal1.nc"),
              file.path(data_dir, "ex_normal2.nc"),
              file_out,
              overwrite = TRUE),
    NA
  )
})

file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file)

  expected_data <- rep(c(1.086957, 1.08658, 1.086207, 1.085837, 1.08547,
                         1.085106, 1.084746, 1.084388, 1.084034, 1.083682,
                         1.083333, 1.082988, 1.082645, 1.082304, 1.081967,
                         1.081633, 1.081301, 1.080972, 1.080645, 1.080321,
                         1.08, 1.079681, 1.079365), 3)
  expected <- array(expected_data, dim = c(7, 7))

  expect_equivalent(actual, expected, tolerance = 1e-06)
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
  expect_identical(actual, array(seq(5, 8, 0.5)))

  actual <- ncvar_get(file, "lat")
  expect_identical(actual, array(seq(45, 48, 0.5)))

  actual <- ncvar_get(file, "time")
  expect_equal(actual, array(149016))
})

nc_close(file)

########## time length is 2 and 2 ##########
file_out <- tempfile_nc()
cmsaf.div("SIS", "SIS",
          file.path(data_dir, "ex_time_dim1.nc"),
          file.path(data_dir, "ex_time_dim2.nc"),
          file_out)
file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file)

  expected_data <- rep(c(1.086957, 1.08658, 1.086207, 1.085837, 1.08547,
                         1.085106, 1.084746, 1.084388, 1.084034, 1.083682,
                         1.083333, 1.082988, 1.082645, 1.082304, 1.081967,
                         1.081633, 1.081301, 1.080972, 1.080645, 1.080321,
                         1.08, 1.079681, 1.079365), 3)
  expected <- array(expected_data, dim = c(7, 7, 2))

  expect_equivalent(actual, expected, tolerance = 1e-06)
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
  expect_identical(actual, array(seq(5, 8, 0.5)))

  actual <- ncvar_get(file, "lat")
  expect_identical(actual, array(seq(45, 48, 0.5)))

  actual <- ncvar_get(file, "time")
  expect_equal(actual, array(c(149016, 158544)))
})

nc_close(file)

########## time length is 1 and 2 ##########
file_out <- tempfile_nc()
cmsaf.div("SIS", "SIS",
          file.path(data_dir, "ex_normal1.nc"),
          file.path(data_dir, "ex_time_dim2.nc"), file_out)
file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file)

  expected_data <- c(rep(c(1.086957, 1.08658, 1.086207, 1.085837, 1.08547,
                           1.085106, 1.084746, 1.084388, 1.084034, 1.083682,
                           1.083333, 1.082988, 1.082645, 1.082304, 1.081967,
                           1.081633, 1.081301, 1.080972, 1.080645, 1.080321,
                           1.08, 1.079681, 1.079365), 2),
                     1.086957, 1.08658, 1.086207,
                     rep(c(1.072961, 1.07265, 1.07234, 1.072034, 1.07173,
                           1.071429, 1.07113, 1.070833, 1.070539, 1.070248,
                           1.069959, 1.069672, 1.069388, 1.069106, 1.068826,
                           1.068548, 1.068273, 1.068, 1.067729, 1.06746,
                           1.173931, 1.17316, 1.172414), 2),
                     1.072961, 1.07265, 1.07234)
  expected <- array(expected_data, dim = c(7, 7, 2))

  expect_equivalent(actual, expected, tolerance = 1e-06)
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
  expect_identical(actual, array(seq(5, 8, 0.5)))

  actual <- ncvar_get(file, "lat")
  expect_identical(actual, array(seq(45, 48, 0.5)))

  actual <- ncvar_get(file, "time")
  expect_equal(actual, array(c(167976, 177480)))
})

nc_close(file)

########## time length is 2 and 1 ##########
file_out <- tempfile_nc()
cmsaf.div("SIS", "SIS", file.path(data_dir, "ex_time_dim1.nc"), file.path(data_dir,
                                                                          "ex_normal2.nc"), file_out)
file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file)

  expected_data <- c(rep(c(1.086957, 1.08658, 1.086207, 1.085837, 1.08547,
                           1.085106, 1.084746, 1.084388, 1.084034, 1.083682,
                           1.083333, 1.082988, 1.082645, 1.082304, 1.081967,
                           1.081633, 1.081301, 1.080972, 1.080645, 1.080321,
                           1.08, 1.079681, 1.079365), 2),
                     1.086957, 1.08658, 1.086207,
                     rep(c(1.1, 1.099567, 1.099138, 1.098712, 1.098291,
                           1.097872, 1.097458, 1.097046, 1.096639, 1.096234,
                           1.095833, 1.095436, 1.095041, 1.09465, 1.094262,
                           1.093878, 1.093496, 1.093117, 1.092742, 1.092369,
                           1, 1, 1), 2),
                     1.1, 1.099567, 1.099138)
  expected <- array(expected_data, dim = c(7, 7, 2))

  expect_equivalent(actual, expected, tolerance = 1e-06)
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
  expect_identical(actual, array(seq(5, 8, 0.5)))

  actual <- ncvar_get(file, "lat")
  expect_identical(actual, array(seq(45, 48, 0.5)))

  actual <- ncvar_get(file, "time")
  expect_equal(actual, array(c(149016, 158544)))
})

nc_close(file)

########## time length is 4 and 2 ##########
file_out <- tempfile_nc()

test_that("error is thrown if dimensions do not match", {
  expect_error(cmsaf.div("SIS", "SIS",
                         file.path(data_dir, "ex_time_dim3.nc"),
                         file.path(data_dir, "ex_time_dim2.nc"),
                         file_out), "Uncompatible time lengths!")
})

########## different lon lengths ##########
file_out <- tempfile_nc()

test_that("error is thrown if dimensions don't match", {
  expect_error(cmsaf.div("SIS", "SIS",
                         file.path(data_dir, "ex_normal1.nc"),
                         file.path(data_dir, "ex_different_lon_length.nc"),
                         file_out),
               "Dimensions of infiles do not match!")
})

########## additional attribute ##########
file_out <- tempfile_nc()
cmsaf.div("SIS", "SIS",
          file.path(data_dir, "ex_additional_attr.nc"),
          file.path(data_dir, "ex_normal2.nc"), file_out)
file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file)

  expected_data <- rep(c(1.086957, 1.08658, 1.086207, 1.085837, 1.08547,
                         1.085106, 1.084746, 1.084388, 1.084034, 1.083682,
                         1.083333, 1.082988, 1.082645, 1.082304, 1.081967,
                         1.081633, 1.081301, 1.080972, 1.080645, 1.080321,
                         1.08, 1.079681, 1.079365), 3)
  expected <- array(expected_data, dim = c(7, 7))

  expect_equivalent(actual, expected, tolerance = 1e-06)
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
  expect_identical(actual, array(seq(5, 8, 0.5)))

  actual <- ncvar_get(file, "lat")
  expect_identical(actual, array(seq(45, 48, 0.5)))

  actual <- ncvar_get(file, "time")
  expect_equal(actual, array(149016))
})

nc_close(file)

########## input ncdf version 4 ##########
file_out <- tempfile_nc()
cmsaf.div("SIS", "SIS",
          file.path(data_dir, "ex_v4_1.nc"),
          file.path(data_dir, "ex_v4_2.nc"),
          file_out)
file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file)

  expected_data <- rep(c(1.086957, 1.08658, 1.086207, 1.085837, 1.08547,
                         1.085106, 1.084746, 1.084388, 1.084034, 1.083682,
                         1.083333, 1.082988, 1.082645, 1.082304, 1.081967,
                         1.081633, 1.081301, 1.080972, 1.080645, 1.080321,
                         1.08, 1.079681, 1.079365), 3)
  expected <- array(expected_data, dim = c(7, 7))

  expect_equivalent(actual, expected, tolerance = 1e-06)
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
  expect_identical(actual, array(seq(5, 8, 0.5)))

  actual <- ncvar_get(file, "lat")
  expect_identical(actual, array(seq(45, 48, 0.5)))

  actual <- ncvar_get(file, "time")
  expect_equal(actual, array(149016))
})

nc_close(file)
