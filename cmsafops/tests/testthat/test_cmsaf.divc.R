data_dir <- file.path("..", "testdata")
tempfile_nc <- function() {
  tempfile_helper("cmsaf.divc_")
}

########## output ncdf version 3 ##########
file_out <- tempfile_nc()
cmsaf.divc("SIS", 4, file.path(data_dir, "ex_normal1.nc"), file_out)
file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file)

  expected_data <- rep(seq(62.5, 68, by = 0.25), 3)
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
  expect_equal(actual, "cmsaf::cmsaf.divc SIS divided by 4")

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
file_out <- tempfile_nc()

cmsaf.divc("SIS", 4, file.path(data_dir, "ex_normal1.nc"), file_out, nc34 = 4)

file <- nc_open(file_out)

test_that("data is correct in version 4", {
  actual <- ncvar_get(file)

  expected_data <- rep(seq(62.5, 68, by = 0.25), 3)
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
  expect_equal(actual, array(149016))
})

nc_close(file)

########## output ncdf version 7 #########
file_out <- tempfile_nc()

test_that("error is thrown if ncdf version is wrong", {
  expect_error(
    cmsaf.divc("SIS", 4, file.path(data_dir, "ex_normal1.nc"), file_out, nc34 = 7),
    "nc version must be in c(3, 4), but was 7", fixed = TRUE
  )
})

########## output ncdf version is NULL #########
file_out <- tempfile_nc()

test_that("ncdf version NULL throws an error", {
  expect_error(
    cmsaf.divc("SIS", 4,
               file.path(data_dir, "ex_normal1.nc"),
               file_out, nc34 = NULL),
    "nc_version must not be NULL"
  )
})

########## variable does not exist #########
file_out <- tempfile_nc()

test_that("warning is shown if var does not exist", {
  expect_warning(
    cmsaf.divc("lat", 4, file.path(data_dir, "ex_normal1.nc"), file_out),
    "Variable 'lat' not found. Variable 'SIS' will be used instead."
  )
})

file <- nc_open(file_out)

test_that("data is correct if non-existing variable is given", {
  actual <- ncvar_get(file)

  expected_data <- rep(seq(62.5, 68, by = 0.25), 3)
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
  expect_equal(actual, array(149016))
})

nc_close(file)


########## variable is null #########
file_out <- tempfile_nc()

test_that("error is thrown if variable is NULL", {
  expect_error(
    cmsaf.divc(NULL, 4,
               file.path(data_dir, "ex_normal1.nc"),
               file_out),
    "variable must not be NULL"
  )
})

########## variable is empty #########
file_out <- tempfile_nc()

test_that("warning is shown if var is empty", {
  expect_warning(
    cmsaf.divc("", 4, file.path(data_dir, "ex_normal1.nc"), file_out),
    "Variable '' not found. Variable 'SIS' will be used instead."
  )
})

file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file)

  expected_data <- rep(seq(62.5, 68, by = 0.25), 3)
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
  expect_equal(actual, array(149016))
})

nc_close(file)


########## input file does not exist #########
file_out <- tempfile_nc()
test_that("error is thrown if input file does not exist", {
  expect_error(
    cmsaf.divc("SIS", 3,
               file.path(data_dir, "xemaple1.nc"),
               file_out),
    "Input file does not exist")
})

########## input filename is empty #########
file_out <- tempfile_nc()

test_that("error is thrown if input filename is empty", {
  expect_error(
    cmsaf.divc("SIS", 3,
               "",
               file_out),
    "Input file does not exist")
})

########## input filename is NULL #########
file_out <- tempfile_nc()

test_that("error is thrown if input filename is NULL", {
  expect_error(
    cmsaf.divc("SIS", 4, NULL, file_out),
    "Input filepath must be of length one and not NULL"
  )
})

########## output file already exists #########
file_out <- tempfile_nc()
cat("test\n", file = file_out)

test_that("error is thrown if output file already exists", {
  expect_error(
    cmsaf.divc("SIS",
               4,
               file.path(data_dir, "ex_normal1.nc"),
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
  expect_error(cmsaf.divc("SIS",
                          4,
                          file.path(data_dir, "ex_normal1.nc"),
                          file_out,
                          overwrite = TRUE),
               NA
  )
})

########## time length is 2 ##########
file_out <- tempfile_nc()
cmsaf.divc("SIS", 4, file.path(data_dir, "ex_time_dim1.nc"), file_out)
file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file)

  expected_data <- rep(seq(62.5, 68, by = 0.25), 3)
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

########## negative constant ##########
file_out <- tempfile_nc()
cmsaf.divc("SIS", -3, file.path(data_dir, "ex_normal1.nc"), file_out)
file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file)

  temp <- seq(-83, -90, by = -1)
  temp2 <- (rbind(temp, temp, temp))[-1]
  temp3 <- (rep(c(-0.33334, -0.66666, -0), 8))[-24]
  expected_data <- rep((temp2 + temp3), 3)
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

########## constant is NULL #########
# TODO this should throw a clearer error
file_out <- tempfile_nc()

test_that("error is thrown if constant is NULL", {
  expect_error(cmsaf.divc("SIS", NULL,
                          file.path(data_dir, "ex_time_dim1.nc"),
                          file_out),
    "constant must not be NULL"
               )
})

########## time length is 4 ##########
file_out <- tempfile_nc()
cmsaf.divc("SIS", 2, file.path(data_dir, "ex_time_dim3.nc"), file_out)
file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file)

  temp <- seq(125, 136, by = 0.5)
  expected_data <- rep(temp, 9)
  expected <- array(expected_data, dim = c(7, 7, 4))

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
  expect_equal(actual, array(c(192864, 202368, 211800, 221328)))
})

nc_close(file)

########## different lon length ##########
file_out <- tempfile_nc()
cmsaf.divc("SIS", 4,
           file.path(data_dir, "ex_different_lon_length.nc"),
           file_out)

file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file)

  expected_data <- rep(seq(62.5, 68, by = 0.25), 9)
  expected <- array(expected_data, dim = c(5, 7, 4))

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
  expect_equal(actual, array(c(149016, 158544, 167976, 177480)))
})

nc_close(file)

########## additional attribute ##########
file_out <- tempfile_nc()
cmsaf.divc("SIS", 4, file.path(data_dir, "ex_additional_attr.nc"), file_out)
file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file)

  expected_data <- rep(seq(62.5, 68, by = 0.25), 3)
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
  expect_equal(actual, array(149016))
})

nc_close(file)

########## input ncdf version 4 ##########
file_out <- tempfile_nc()
cmsaf.divc("SIS", 4, file.path(data_dir, "ex_v4_1.nc"), file_out)
file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file)

  expected_data <- rep(seq(62.5, 68, by = 0.25), 3)
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
  expect_equal(actual, array(149016))
})

nc_close(file)
