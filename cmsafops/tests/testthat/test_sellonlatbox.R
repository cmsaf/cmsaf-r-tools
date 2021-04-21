data_dir <- file.path("..", "testdata")
tempfile_nc <- function() {
  tempfile_helper("sellonlatbox_")
}

########## output ncdf version 3 ##########
file_in <- file.path(data_dir, "ex_normal1.nc")
file_out <- tempfile_nc()

sellonlatbox("SIS", file_in, file_out)
file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file)
  temp1 <- seq(250, 272)
  expected_data <- c(temp1, temp1, temp1[1:3])
  expected <- array(expected_data, dim = c(7, 7))

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
  expect_equal(actual, "cmsaf::sellonlatbox for variable SIS")

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
  cat(actual)
  expect_equal(actual, array(c(149016)))
})

nc_close(file)

########## output ncdf version 4 ##########
file_in <- file.path(data_dir, "ex_normal1.nc")
file_out <- tempfile_nc()

sellonlatbox("SIS", file_in, file_out, nc34 = 4)
file <- nc_open(file_out)

test_that("data is correct in version 4", {
  actual <- ncvar_get(file)

  temp1 <- seq(250, 272)
  expected_data <- c(temp1, temp1, temp1[1:3])
  expected <- array(expected_data, dim = c(7, 7))

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
  expect_equal(actual, array(c(149016)))
})

nc_close(file)

########## output ncdf version 7 #########
file_in <- file.path(data_dir, "ex_normal1.nc")
file_out <- tempfile_nc()

test_that("error is thrown if ncdf version is wrong", {
  expect_error(
    sellonlatbox("SIS", file_in, file_out, nc34 = 7),
    "nc version must be in c(3, 4), but was 7", fixed = TRUE
  )
})

########## output ncdf version is NULL #########
file_in <- file.path(data_dir, "ex_normal1.nc")
file_out <- tempfile_nc()

test_that("ncdf version NULL throws an error", {
  expect_error(
    sellonlatbox("SIS", file_in, file_out, nc34 = NULL),
    "nc_version must not be NULL"
  )
})

########## variable does not exist #########
file_in <- file.path(data_dir, "ex_normal1.nc")
file_out <- tempfile_nc()

test_that("warning is shown if var does not exist", {
  expect_warning(
    sellonlatbox("lat", file_in, file_out),
    "Variable 'lat' not found. Variable 'SIS' will be used instead."
  )
})

file <- nc_open(file_out)

test_that("data is correct if non-existing variable is given", {
  actual <- ncvar_get(file)

  temp1 <- seq(250, 272)
  expected_data <- c(temp1, temp1, temp1[1:3])
  expected <- array(expected_data, dim = c(7, 7))

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
  expect_equal(actual, array(c(149016)))
})

nc_close(file)


########## variable is null #########
file_in <- file.path(data_dir, "ex_normal1.nc")
file_out <- tempfile_nc()

test_that("error is thrown if variable is NULL", {
  expect_error(
    sellonlatbox(NULL, file_in, file_out),
    "variable must not be NULL"
  )
})

########## variable is empty #########
file_in <- file.path(data_dir, "ex_normal1.nc")
file_out <- tempfile_nc()

test_that("warning is shown if var is empty", {
  expect_warning(
    sellonlatbox("", file_in, file_out),
    "Variable '' not found. Variable 'SIS' will be used instead."
  )
})

file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file)

  temp1 <- seq(250, 272)
  expected_data <- c(temp1, temp1, temp1[1:3])
  expected <- array(expected_data, dim = c(7, 7))

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
  expect_equal(actual, array(c(149016)))
})

nc_close(file)


########## input file does not exist #########
file_in <- file.path(data_dir, "xex_normal1.nc")
file_out <- tempfile_nc()

test_that("error is thrown if input file does not exist", {
  expect_error(
    sellonlatbox("SIS", file_in, file_out),
    "Input file does not exist")
})

########## input filename is NULL #########
file_in <- file.path(data_dir, NULL)
file_out <- tempfile_nc()

test_that("error is thrown if input filename is NULL", {
  expect_error(
    sellonlatbox("SIS", file_in, file_out),
    "Input filepath must be of length one and not NULL"
  )
})

########## output file already exists #########
file_in <- file.path(data_dir, "ex_normal1.nc")
file_out <- tempfile_nc()
cat("test\n", file = file_out)

test_that("error is thrown if output file already exists", {
  expect_error(
    sellonlatbox("SIS", file_in, file_out),
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
    sellonlatbox("SIS", file_in, file_out, overwrite = TRUE),
    NA)
})

file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file)

  temp1 <- seq(250, 272)
  expected_data <- c(temp1, temp1, temp1[1:3])
  expected <- array(expected_data, dim = c(7, 7))

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
  expect_equal(actual, array(c(149016)))
})

nc_close(file)

########## time length is 2 ##########
file_in <- file.path(data_dir, "ex_time_dim1.nc")
file_out <- tempfile_nc()
sellonlatbox("SIS", file_in, file_out)
file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file)

  temp1 <- seq(250, 272)
  expected_data <- c(rep(temp1, 4), temp1[1:6])
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

########## additional attribute ##########
file_in <- file.path(data_dir, "ex_additional_attr.nc")
file_out <- tempfile_nc()
sellonlatbox("SIS", file_in, file_out)
file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file)

  temp1 <- seq(250, 272)
  temp2 <- seq(230, 252)
  expected_data <- c(temp1, temp1, temp1[1:3])
  expected <- array(expected_data, dim = c(7, 7))

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
  expect_equal(actual, array(c(149016)))
})

nc_close(file)

########## input ncdf version 4 ##########
file_in <- file.path(data_dir, "ex_v4_1.nc")
file_out <- tempfile_nc()
sellonlatbox("SIS", file_in, file_out)
file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file)

  temp1 <- seq(250, 272)
  temp2 <- seq(230, 252)
  expected_data <- c(temp1, temp1, temp1[1:3])
  expected <- array(expected_data, dim = c(7, 7))

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
  expect_equal(actual, array(c(149016)))
})

nc_close(file)

########## input lon & lat in range ##########
file_in <- file.path(data_dir, "ex_normal1.nc")
file_out <- tempfile_nc()
sellonlatbox("SIS", file_in, file_out, lon1 = 6, lon2 = 7.5, lat1 = 45.5, lat2 = 47)
file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file)

  temp1 <- seq(250, 272)
  expected_data <- c(temp1[c(10:13, 17:20)], temp1[c(1:4, 8:11)])
  expected <- array(expected_data, dim = c(4, 4))

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
  expect_equal(actual, array(c(149016)))
})

nc_close(file)

########## input lon out of range & lat in range ##########
file_in <- file.path(data_dir, "ex_normal1.nc")
file_out <- tempfile_nc()

test_that("ncdf version NULL throws an error", {
  expect_error(
    sellonlatbox("SIS", file_in, file_out, lon1 = 10, lon2 = 12, lat1 = 45.5, lat2 = 47),
    "Selected region is outside target area!")
})

########## input lon in range & lat out ofrange ##########
file_in <- file.path(data_dir, "ex_normal1.nc")
file_out <- tempfile_nc()

test_that("ncdf version NULL throws an error", {
  expect_error(
    sellonlatbox("SIS", file_in, file_out, lon1 = 6, lon2 = 7.5, lat1 = 49, lat2 = 51),
    "Selected region is outside target area!")
})

########## input lon & lat out of range ##########
file_in <- file.path(data_dir, "ex_normal1.nc")
file_out <- tempfile_nc()

test_that("ncdf version NULL throws an error", {
  expect_error(
    sellonlatbox("SIS", file_in, file_out, lon1 = 10, lon2 = 12, lat1 = 49, lat2 = 51),
    "Selected region is outside target area!")
})

########## nonregular input ##########
file_in <- file.path(data_dir, "ex_nonreg1.nc")
file_out <- tempfile_nc()

sellonlatbox("cfc", file_in, file_out)
file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file, "cfc")
  temp1 <- seq(250, 272)
  expected_data <- c(temp1, temp1, temp1[1:3])
  expected <- array(expected_data, dim = c(7, 7))

  expect_equivalent(actual, expected)
  actual <- ncvar_get(file, "lon")
  temp1 <- c(seq(45, 45.6, 0.1), seq(45.8, 47, 0.2), seq(47.5, 50.5, 0.5), seq(51.5, 57.5, 1), seq(58.5, 61.5, 0.5), seq(62, 63.2, 0.2), seq(63.4, 64, 0.1))
  expected_data <- c(temp1)
  expected <- array(expected_data, dim = c(7, 7))

  expect_equivalent(actual, expected)

  actual <- ncvar_get(file, "lat")
  temp1 <- c(seq(5, 5.6, 0.1), seq(5.8, 7, 0.2), seq(7.5, 10.5, 0.5), seq(11.5, 17.5, 1), seq(18.5, 21.5, 0.5), seq(22, 23.2, 0.2), seq(23.4, 24, 0.1))
  expected_data <- c(temp1)
  expected <- array(expected_data, dim = c(7, 7))

  expect_equivalent(actual, expected)
})

test_that("attributes are correct", {
  actual <- ncatt_get(file, "x", "units")$value
  expect_equal(actual, "km")

  actual <- ncatt_get(file, "x", "standard_name")$value
  expect_equal(actual, "x")

  actual <- ncatt_get(file, "y", "units")$value
  expect_equal(actual, "km")

  actual <- ncatt_get(file, "y", "standard_name")$value
  expect_equal(actual, "y")

  actual <- ncatt_get(file, "lon", "units")$value
  expect_equal(actual, "degrees_east")

  actual <- ncatt_get(file, "lon", "long_name")$value
  expect_equal(actual, "longitude")

  actual <- ncatt_get(file, "lon", "standard_name")$value
  expect_equal(actual, "longitude")

  actual <- ncatt_get(file, "lat", "units")$value
  expect_equal(actual, "degrees_north")

  actual <- ncatt_get(file, "lat", "long_name")$value
  expect_equal(actual, "latitude")

  actual <- ncatt_get(file, "lat", "standard_name")$value
  expect_equal(actual, "latitude")

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
  actual <- ncvar_get(file, "x")
  expect_identical(actual, array(seq(0, 300, 50)))

  actual <- ncvar_get(file, "y")
  expect_identical(actual, array(seq(0, 300, 50)))

  actual <- ncvar_get(file, "time")
  cat(actual)
  expect_equal(actual, array(c(292920)))
})

nc_close(file)


########## nonregular input cut ##########
file_in <- file.path(data_dir, "ex_nonreg1.nc")
file_out <- tempfile_nc()

sellonlatbox("cfc", file_in, file_out, lon1 = 47, lon2 = 62, lat1 = 10, lat2 = 12)
file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file, "cfc")
  expected_data <- c(264, 269, 270, 271, 253, 254)
  expected <- array(expected_data, dim = c(3, 2))

  expect_equivalent(actual, expected)
  actual <- ncvar_get(file, "lon")

  expected_data <- c(47.5, 50.0, 50.5, 51.5, 56.5, 57.5)
  expected <- array(expected_data, dim = c(3, 2))

  expect_equivalent(actual, expected)

  actual <- ncvar_get(file, "lat")

  expected_data <- c(7.5, 10.0, 10.5, 11.5, 16.5, 17.5)
  expected <- array(expected_data, dim = c(3, 2))

  expect_equivalent(actual, expected)
})

test_that("attributes are correct", {
  actual <- ncatt_get(file, "x", "units")$value
  expect_equal(actual, "km")

  actual <- ncatt_get(file, "x", "standard_name")$value
  expect_equal(actual, "x")

  actual <- ncatt_get(file, "y", "units")$value
  expect_equal(actual, "km")

  actual <- ncatt_get(file, "y", "standard_name")$value
  expect_equal(actual, "y")

  actual <- ncatt_get(file, "lon", "units")$value
  expect_equal(actual, "degrees_east")

  actual <- ncatt_get(file, "lon", "long_name")$value
  expect_equal(actual, "longitude")

  actual <- ncatt_get(file, "lon", "standard_name")$value
  expect_equal(actual, "longitude")

  actual <- ncatt_get(file, "lat", "units")$value
  expect_equal(actual, "degrees_north")

  actual <- ncatt_get(file, "lat", "long_name")$value
  expect_equal(actual, "latitude")

  actual <- ncatt_get(file, "lat", "standard_name")$value
  expect_equal(actual, "latitude")

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
  actual <- ncvar_get(file, "x")
  expect_identical(actual, array(c(0, 250, 300)))

  actual <- ncvar_get(file, "y")
  expect_identical(actual, array(c(100, 150)))

  actual <- ncvar_get(file, "time")
  cat(actual)
  expect_equal(actual, array(c(292920)))
})

nc_close(file)

########## nonregular input without lon/lat ##########
file_in <- file.path(data_dir, "ex_nonreg2.nc")
file_out <- tempfile_nc()

test_that("error is thrown if no lon/lat_info exists", {
  expect_error(
    sellonlatbox("cfc", file_in, file_out, lon1 = 10, lon2 = 12, lat1 = 49, lat2 = 51),
    "No lon/lat information found in file, please add by applying add_grid_info")
})

########## nonregular input with lon/lat out of range ##########
file_in <- file.path(data_dir, "ex_nonreg1.nc")
file_out <- tempfile_nc()

test_that("error is thrown if lon/lat_info out of range", {
  expect_error(
    sellonlatbox("cfc", file_in, file_out, lon1 = 10, lon2 = 12, lat1 = 49, lat2 = 51),
    "Selected region is outside target area!")
})


########## with time_bnds ##########
file_in <- file.path(data_dir, "ex_time_bnds1.nc")
file_out <- tempfile_nc()

sellonlatbox("SIS", file_in, file_out)
file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file)
  temp1 <- seq(250, 272)
  expected_data <- c(temp1, temp1, temp1[1:3])
  expected <- array(expected_data, dim = c(7, 7))

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
  expect_equal(actual, array(c(214728)))

  actual <- ncvar_get(file, "time_bnds")
  expect_equal(actual, array(c(214728, 215472)))
})

nc_close(file)
