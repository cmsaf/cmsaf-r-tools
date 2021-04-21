data_dir <- file.path("..", "testdata")
tempfile_nc <- function() {
  tempfile_helper("box_mergetime_")
}

########## output ncdf version 3 ##########
file_out <- tempfile_nc()
box_mergetime("SIS", data_dir, "ex_normal", file_out)
file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file)

  temp1 <- seq(250, 272)
  temp2 <- seq(230, 252)
  expected_data <- c(temp1, temp1, temp1[1:3],
                     temp2, temp2, temp2[1:3])
  expected <- array(expected_data, dim = c(7, 7, 2))

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
  expect_equal(actual, "cmsaf::box_mergetime for variable SIS")

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

########## output ncdf version 4 ##########
file_out <- tempfile_nc()

box_mergetime("SIS", data_dir, "ex_normal", file_out, nc34 = 4)

file <- nc_open(file_out)

test_that("data is correct in version 4", {
  actual <- ncvar_get(file)

  temp1 <- seq(250, 272)
  temp2 <- seq(230, 252)
  expected_data <- c(temp1, temp1, temp1[1:3],
                     temp2, temp2, temp2[1:3])
  expected <- array(expected_data, dim = c(7, 7, 2))

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
  expect_identical(actual, array(seq(5, 8, 0.5)))

  actual <- ncvar_get(file, "lat")
  expect_identical(actual, array(seq(45, 48, 0.5)))

  actual <- ncvar_get(file, "time")
  expect_equal(actual, array(c(149016, 158544)))
})

nc_close(file)

########## output ncdf version 7 #########
file_out <- tempfile_nc()
test_that("error is thrown if ncdf version is wrong", {
  expect_error(
    box_mergetime("SIS", data_dir, "ex_normal", file_out, nc34 = 7),
    "nc version must be in c(3, 4), but was 7", fixed = TRUE
  )
})



########## output ncdf version is NULL #########
file_out <- tempfile_nc()

test_that("ncdf version NULL throws an error", {
  expect_error(
    box_mergetime("SIS", data_dir, "ex_normal", file_out, nc34 = NULL),
    "nc_version must not be NULL"
  )
})

########## variable does not exist #########
file_out <- tempfile_nc()
test_that("warning is shown if var does not exist", {
  expect_warning(
    box_mergetime("lat", data_dir, "ex_normal", file_out),
    "Variable 'lat' not found. Variable 'SIS' will be used instead.")
})

file <- nc_open(file_out)

test_that("data is correct if non-existing variable is given", {
  actual <- ncvar_get(file)

  temp1 <- seq(250, 272)
  temp2 <- seq(230, 252)
  expected_data <- c(temp1, temp1, temp1[1:3],
                     temp2, temp2, temp2[1:3])
  expected <- array(expected_data, dim = c(7, 7, 2))

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
  expect_identical(actual, array(seq(5, 8, 0.5)))

  actual <- ncvar_get(file, "lat")
  expect_identical(actual, array(seq(45, 48, 0.5)))

  actual <- ncvar_get(file, "time")
  expect_equal(actual, array(c(149016, 158544)))
})

nc_close(file)


########## variable is null #########
file_out <- tempfile_nc()

test_that("error is thrown if variable is NULL", {
  expect_error(
    box_mergetime(NULL, data_dir, "ex_normal", file_out),
    "variable must not be NULL"
  )
})

########## variable is empty #########
file_out <- tempfile_nc()

test_that("warning is shown if var is empty", {
  expect_warning(
    box_mergetime("", data_dir, "ex_normal", file_out),
    "Variable '' not found. Variable 'SIS' will be used instead."
  )
})

file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file)

  temp1 <- seq(250, 272)
  temp2 <- seq(230, 252)
  expected_data <- c(temp1, temp1, temp1[1:3],
                     temp2, temp2, temp2[1:3])
  expected <- array(expected_data, dim = c(7, 7, 2))

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
  expect_identical(actual, array(seq(5, 8, 0.5)))

  actual <- ncvar_get(file, "lat")
  expect_identical(actual, array(seq(45, 48, 0.5)))

  actual <- ncvar_get(file, "time")
  expect_equal(actual, array(c(149016, 158544)))
})

nc_close(file)


########## input file does not exist #########
# TODO should this throw an error
file_out <- tempfile_nc()

test_that("no error is thrown if input file does not exist", {
  expect_error(
    box_mergetime("SIS", data_dir, "(xemaple1.nc|ex_normal2.nc)", file_out),
    NA)
})

########## pattern matches no filename #########
file_out <- tempfile_nc()

test_that("error is thrown if input pattern has no match", {
  expect_error(
    box_mergetime("SIS", data_dir, "ex_ex_ex_ex_ex.nc", file_out),
    "No files found that match the pattern")
})

########## input file pattern is NULL #########
file_out <- tempfile_nc()

test_that("error is thrown if input file pattern is NULL", {
  expect_error(
    box_mergetime("SIS", data_dir, NULL, file_out),
    "Missing input: Please provide path and pattern."
  )
})

########## output file already exists #########
file_out <- tempfile_nc()
cat("test\n", file = file_out)

test_that("no error is thrown if output file already exists", {
  expect_error(
    box_mergetime("SIS", data_dir, "ex_normal", file_out),
    paste0("File '",
           file_out,
           "' already exists. Specify 'overwrite = TRUE' if you want to overwrite it."),
    fixed = TRUE)
})

########## time length is 2 and 2 and 4 ##########
file_out <- tempfile_nc()
box_mergetime("SIS", data_dir, "ex_time_dim", file_out)
file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file)

  temp1 <- seq(250, 272)
  temp2 <- seq(230, 252)
  expected_data <- c(rep(temp1, 4), temp1[1:6],
                     rep(temp2, 4), temp2[1:6],
                     rep(temp1, 8), temp1[1:12])
  expected <- array(expected_data, dim = c(7, 7, 8))

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
  expect_identical(actual, array(seq(5, 8, 0.5)))

  actual <- ncvar_get(file, "lat")
  expect_identical(actual, array(seq(45, 48, 0.5)))

  actual <- ncvar_get(file, "time")
  expect_equal(actual, array(c(c(149016, 158544),
                               c(167976, 177480),
                               c(192864, 202368, 211800, 221328))))
})

nc_close(file)

########## time length is 1 and 2 ##########
file_out <- tempfile_nc()
box_mergetime("SIS", data_dir, "(ex_normal1.nc|ex_time_dim2.nc)", file_out)
file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file)

  temp1 <- seq(250, 272)
  temp2 <- seq(230, 252)
  expected_data <- c(temp1, temp1, temp1[1:3],
                     rep(temp2, 7))
  expected <- array(expected_data, dim = c(7, 7, 3))

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
  expect_identical(actual, array(seq(5, 8, 0.5)))

  actual <- ncvar_get(file, "lat")
  expect_identical(actual, array(seq(45, 48, 0.5)))

  actual <- ncvar_get(file, "time")
  expect_equal(actual, array(c(149016, 167976, 177480)))
})

nc_close(file)

########## time length is 2 and 1 ##########
file_out <- tempfile_nc()
box_mergetime("SIS", data_dir, "(ex_time_dim1.nc|ex_normal2.nc)", file_out)
file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file)

  temp1 <- seq(250, 272)
  temp2 <- seq(230, 252)
  expected_data <- c(temp2, temp2, temp2[1:3],
                     rep(temp1, 7))
  expected <- array(expected_data, dim = c(7, 7, 3))

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
  expect_identical(actual, array(seq(5, 8, 0.5)))

  actual <- ncvar_get(file, "lat")
  expect_identical(actual, array(seq(45, 48, 0.5)))

  actual <- ncvar_get(file, "time")
  expect_equal(actual, array(c(158544, 149016, 158544)))
})

nc_close(file)

########## time length is 4 and 2 ##########
file_out <- tempfile_nc()
box_mergetime("SIS", data_dir, "(ex_time_dim3.nc|ex_time_dim2.nc)", file_out)
file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file)

  temp1 <- seq(250, 272)
  temp2 <- seq(230, 252)
  expected_data <- c(rep(temp2, 4), temp2[1:6],
                     c(rep(temp1, 8), temp1[1:12]))
  expected <- array(expected_data, dim = c(7, 7, 6))

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
  expect_identical(actual, array(seq(5, 8, 0.5)))

  actual <- ncvar_get(file, "lat")
  expect_identical(actual, array(seq(45, 48, 0.5)))

  actual <- ncvar_get(file, "time")
  expect_equal(actual, array(c(167976, 177480, 192864, 202368, 211800, 221328)))
})

nc_close(file)

########## different lon lengths ##########
file_out <- tempfile_nc()
box_mergetime("SIS", data_dir, "(ex_normal1.nc|ex_different_lon_length.nc)",
              file_out)
test_that("output file is created if lon lengths do not match", {
  expect_true(file.exists(file_out))
})

file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file)

  temp1 <- seq(250, 272)
  temp2 <- seq(250, 254)
  expected_data <- c(rep(temp1, 6), temp1[1:2],
                     temp2, temp2 + 7, temp2 + 14, c(271, 272, temp2[1:3]),
                     temp2 + 5, temp2 + 12, c(temp1[-1:-19], 250))
  expected <- array(expected_data, dim = c(5, 7, 5))

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
  expect_identical(actual, array(seq(5, 7, 0.5)))

  actual <- ncvar_get(file, "lat")
  expect_identical(actual, array(seq(45, 48, 0.5)))

  actual <- ncvar_get(file, "time")
  expect_equal(actual, array(c(149016, 158544, 167976, 177480, 149016)))
})

nc_close(file)

########## additional attribute ##########
file_out <- tempfile_nc()
box_mergetime("SIS", data_dir, "(ex_additional_attr.nc|ex_normal2.nc)",
              file_out)
file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file)

  temp1 <- seq(250, 272)
  temp2 <- seq(230, 252)
  expected_data <- c(temp1, temp1, temp1[1:3],
                     temp2, temp2, temp2[1:3])
  expected <- array(expected_data, dim = c(7, 7, 2))

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
  expect_identical(actual, array(seq(5, 8, 0.5)))

  actual <- ncvar_get(file, "lat")
  expect_identical(actual, array(seq(45, 48, 0.5)))

  actual <- ncvar_get(file, "time")
  expect_equal(actual, array(c(149016, 158544)))
})

nc_close(file)

########## input ncdf version 4 ##########
file_out <- tempfile_nc()
box_mergetime("SIS", data_dir, "(ex_v4_1.nc|ex_v4_2.nc)", file_out)
file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file)

  temp1 <- seq(250, 272)
  temp2 <- seq(230, 252)
  expected_data <- c(temp1, temp1, temp1[1:3],
                     temp2, temp2, temp2[1:3])
  expected <- array(expected_data, dim = c(7, 7, 2))

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
  expect_identical(actual, array(seq(5, 8, 0.5)))

  actual <- ncvar_get(file, "lat")
  expect_identical(actual, array(seq(45, 48, 0.5)))

  actual <- ncvar_get(file, "time")
  expect_equal(actual, array(c(149016, 158544)))
})

nc_close(file)

########## tests with levels ##########
for (i in 1:5) {
  file_out <- tempfile_nc()
  levbox_mergetime("SIS", i, data_dir, "ex_lev", file_out)
  file <- nc_open(file_out)

  test_that("data is correct", {
    actual <- ncvar_get(file)
    temp1 <- seq(250, 272)
    temp2 <- seq(230, 252)
    expected_data <- c(temp1[(3 * (i - 1) + 1):length(temp1)], temp1, temp1[1:(3 * i)],
                       temp2[(3 * (i - 1) + 1):length(temp2)], temp2, temp2[1:(3 * i)])
    expected <- array(expected_data, dim = c(7, 7, 2))

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
    expect_identical(actual, array(seq(5, 8, 0.5)))

    actual <- ncvar_get(file, "lat")
    expect_identical(actual, array(seq(45, 48, 0.5)))

    actual <- ncvar_get(file, "time")
    expect_equal(actual, array(c(149016, 158544)))
  })
}

########## input lon & lat in range ##########
file_out <- tempfile_nc()
box_mergetime("SIS", data_dir, "ex_normal", file_out, lon1 = 6, lon2 = 7.5, lat1 = 45.5, lat2 = 47)
file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file)

  temp1 <- seq(250, 272)
  temp2 <- seq(230, 252)
  expected_data <- c(temp1[c(10:13, 17:20)], temp1[c(1:4, 8:11)],
                     temp2[c(10:13, 17:20)], temp2[c(1:4, 8:11)])
  expected <- array(expected_data, dim = c(4, 4, 2))

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
  expect_identical(actual, array(seq(6, 7.5, 0.5)))

  actual <- ncvar_get(file, "lat")
  expect_identical(actual, array(seq(45.5, 47, 0.5)))

  actual <- ncvar_get(file, "time")
  expect_equal(actual, array(c(149016, 158544)))
})

nc_close(file)

########## input lon out of range & lat in range ##########
file_out <- tempfile_nc()

test_that("ncdf version NULL throws an error", {
  expect_error(
    box_mergetime("SIS", data_dir, "ex_normal", file_out, lon1 = 10, lon2 = 12, lat1 = 45.5, lat2 = 47),
    "Selected region is outside target area!")
})

########## input lon in range & lat out ofrange ##########
file_out <- tempfile_nc()

test_that("ncdf version NULL throws an error", {
  expect_error(
    box_mergetime("SIS", data_dir, "ex_normal", file_out, lon1 = 6, lon2 = 7.5, lat1 = 49, lat2 = 51),
    "Selected region is outside target area!")
})

########## input lon & lat out of range ##########
file_out <- tempfile_nc()

test_that("ncdf version NULL throws an error", {
  expect_error(
    box_mergetime("SIS", data_dir, "ex_normal", file_out, lon1 = 10, lon2 = 12, lat1 = 49, lat2 = 51),
    "Selected region is outside target area!")
})
