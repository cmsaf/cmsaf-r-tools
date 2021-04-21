data_dir <- file.path("..", "testdata")
tempfile_nc <- function() {
  tempfile_helper("ymonmax_")
}

########## simple case ##########
file_out <- tempfile_nc()
ymonmax("SIS",
        file.path(data_dir, "ex_ymon.nc"),
        file_out)
file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file, "SIS")

  expected_data <- c(
    seq(136, 148, by = 3),
    seq(137, 149, by = 3),
    seq(138, 150, by = 3),
    seq(106, 118, by = 3),
    seq(107, 119, by = 3),
    seq(108, 120, by = 3),
    seq(121, 133, by = 3),
    seq(122, 134, by = 3),
    seq(123, 135, by = 3)
  )
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
  expect_equal(actual, "cmsaf::ymonmax for variable SIS")

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
  expect_identical(actual, array(seq(5, 6, by = 0.5)))

  actual <- ncvar_get(file, "lat")
  expect_identical(actual, array(seq(45, 47, by = 0.5)))

  actual <- ncvar_get(file, "time")
  expect_equal(actual, array(c(149016, 149760, 150456)))
})

nc_close(file)

########## lon/lat as var instead of dim ##########
file_out <- tempfile_nc()
ymonmax("cfc",
        file.path(data_dir, "ex_ymon_irreg1.nc"),
        file_out)
file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file, "cfc")

  expected_data <- c(
    30, 190, 120, 197, 108, 195, 199, 89, 142, 145, 132, 97, 23, 198, 187, 176,
    157, 184, 200, 188, 195, 161, 41, 174, 192, 194, 113, 198, 165, 83, 138,
    126, 185, 189, 177, 192, 171, 197, 191, 187, 165, 175, 136, 169, 148, 169,
    196, 163, 188, 196, 147, 186, 147, 156, 146, 155, 167, 193, 172, 159, 132,
    168, 186, 133, 169, 199, 170, 186, 180, 126, 155, 190, 147, 87, 178, 165,
    200, 163, 139, 157, 193, 163, 191, 113, 187, 130, 192, 196, 175, 192, 198,
    177, 191, 156, 194, 151, 155, 126, 131, 190, 127, 152, 157, 177, 167, 95,
    192, 112, 186, 178, 181, 170, 106, 162, 186, 151, 192, 191, 196, 199, 197,
    185, 151, 176, 170, 107
  )
  expected <- array(expected_data, c(6, 7, 3))

  expect_equivalent(actual, expected)
})

test_that("attributes are correct", {
  actual <- ncatt_get(file, "x", "units")$value
  expect_equal(actual, "km")

  actual <- ncatt_get(file, "x", "long_name")$value
  expect_equal(actual, "x-coordinate in kilometer")

  actual <- ncatt_get(file, "x", "standard_name")$value
  expect_equal(actual, "x")

  actual <- ncatt_get(file, "x", "axis")$value
  expect_equal(actual, "X")

  actual <- ncatt_get(file, "y", "units")$value
  expect_equal(actual, "km")

  actual <- ncatt_get(file, "y", "long_name")$value
  expect_equal(actual, "y-coordinate in kilometer")

  actual <- ncatt_get(file, "y", "standard_name")$value
  expect_equal(actual, "y")

  actual <- ncatt_get(file, "y", "axis")$value
  expect_equal(actual, "Y")

  actual <- ncatt_get(file, "time", "units")$value
  expect_equal(actual, "hours since 1983-01-01 00:00:00")

  actual <- ncatt_get(file, "time", "long_name")$value
  expect_equal(actual, "time")

  actual <- ncatt_get(file, "time", "standard_name")$value
  expect_equal(actual, "time")

  actual <- ncatt_get(file, "time", "calendar")$value
  expect_equal(actual, "standard")

  actual <- ncatt_get(file, "cfc", "standard_name")$value
  expect_equal(actual, "cfc_standard")

  actual <- ncatt_get(file, "cfc", "long_name")$value
  expect_equal(actual, "Fractional Cloud Cover")

  actual <- ncatt_get(file, "cfc", "units")$value
  expect_equal(actual, "1")

  actual <- ncatt_get(file, "cfc", "_FillValue")$value
  expect_equal(actual, -999)

  actual <- ncatt_get(file, "cfc", "cmsaf_info")$value
  expect_equal(actual, "cmsaf::ymonmax for variable cfc")

  global_attr <- ncatt_get(file, 0)
  expect_equal(length(global_attr), 2)

  actual <- names(global_attr[1])
  expect_equal(actual, "Info")

  actual <- global_attr[[1]]
  expect_equal(actual, "Created with the CM SAF R Toolbox.")

  actual <- names(global_attr[2])
  expect_equal(actual, "institution")

  actual <- global_attr[[2]]
  expect_equal(actual, "some institution")
})

test_that("coordinates are correct", {
  actual <- ncvar_get(file, "x")
  expect_identical(actual, array(seq(0, 250, by = 50)))

  actual <- ncvar_get(file, "y")
  expect_identical(actual, array(seq(0, 300, by = 50)))

  actual <- ncvar_get(file, "time")
  expect_equal(actual, array(c(149016, 149760, 150456)))
})

test_that("lon/lat variable is correct", {
  actual <- ncvar_get(file, "lon")

  expected_data <- c(
    45, 45.1, 45.2, 45.3, 45.4, 45.5, 45.6, 45.8, 46, 46.2, 46.4, 46.6, 46.8,
    47, 47.5, 48, 48.5, 49, 49.5, 50, 50.5, 51.5, 52.5, 53.5, 54.5, 55.5, 56.5,
    57.5, 58.5, 59, 59.5, 60, 60.5, 61, 61.5, 62, 62.2, 62.4, 62.6, 62.8, 63,
    63.2
  )
  expected <- array(expected_data, c(6, 7))

  expect_equivalent(actual, expected)

  actual <- ncvar_get(file, "lat")

  expected_data <- c(
    5, 5.1, 5.2, 5.3, 5.4, 5.5, 5.6, 5.8, 6, 6.2, 6.4, 6.6, 6.8, 7, 7.5, 8, 8.5,
    9, 9.5, 10, 10.5, 11.5, 12.5, 13.5, 14.5, 15.5, 16.5, 17.5, 18.5, 19, 19.5,
    20, 20.5, 21, 21.5, 22, 22.2, 22.4, 22.6, 22.8, 23, 23.2
  )
  expected <- array(expected_data, c(6, 7))

  expect_equivalent(actual, expected)
})

nc_close(file)

########## no lon/lat ##########
file_out <- tempfile_nc()
ymonmax("cfc",
        file.path(data_dir, "ex_ymon_irreg2.nc"),
        file_out)
file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file, "cfc")

  expected_data <- c(
    252, 242, 243, 244, 245, 246, 247, 248, 249, 250, 251, 252, 252, 243, 244,
    245, 246, 247, 248, 249, 250, 251, 252, 252, 242, 243, 244, 245, 246, 247,
    248, 249, 250, 251, 252, 252, 243, 244, 245, 246, 247, 248, 249, 250, 251,
    252, 252, 242, 243, 244, 245, 246, 247, 248, 249, 250, 251, 252, 252, 243,
    244, 245, 246, 247, 248, 249, 250, 251, 252, 252, 242, 243, 244, 245, 246,
    247, 248, 249, 250, 251, 252, 252, 243, 244, 245, 246, 247, 248, 249, 250,
    251, 252, 252, 242, 243, 244, 245, 246, 247, 248, 249, 250, 251, 252, 252,
    243, 244, 245, 246, 247, 248, 249, 250, 251, 252, 252, 242, 243, 244, 245,
    246, 247, 248, 249, 250, 251
  )
  expected <- array(expected_data, c(6, 7, 3))

  expect_equivalent(actual, expected)
})

test_that("attributes are correct", {
  actual <- ncatt_get(file, "x", "units")$value
  expect_equal(actual, "km")

  actual <- ncatt_get(file, "x", "long_name")$value
  expect_equal(actual, "x-coordinate in kilometer")

  actual <- ncatt_get(file, "x", "standard_name")$value
  expect_equal(actual, "x")

  actual <- ncatt_get(file, "x", "axis")$value
  expect_equal(actual, "X")

  actual <- ncatt_get(file, "y", "units")$value
  expect_equal(actual, "km")

  actual <- ncatt_get(file, "y", "long_name")$value
  expect_equal(actual, "y-coordinate in kilometer")

  actual <- ncatt_get(file, "y", "standard_name")$value
  expect_equal(actual, "y")

  actual <- ncatt_get(file, "y", "axis")$value
  expect_equal(actual, "Y")

  actual <- ncatt_get(file, "time", "units")$value
  expect_equal(actual, "hours since 1983-01-01 00:00:00")

  actual <- ncatt_get(file, "time", "long_name")$value
  expect_equal(actual, "time")

  actual <- ncatt_get(file, "time", "standard_name")$value
  expect_equal(actual, "time")

  actual <- ncatt_get(file, "time", "calendar")$value
  expect_equal(actual, "standard")

  actual <- ncatt_get(file, "cfc", "standard_name")$value
  expect_equal(actual, "cfc_standard")

  actual <- ncatt_get(file, "cfc", "long_name")$value
  expect_equal(actual, "Fractional Cloud Cover")

  actual <- ncatt_get(file, "cfc", "units")$value
  expect_equal(actual, "1")

  actual <- ncatt_get(file, "cfc", "_FillValue")$value
  expect_equal(actual, -999)

  actual <- ncatt_get(file, "cfc", "cmsaf_info")$value
  expect_equal(actual, "cmsaf::ymonmax for variable cfc")

  global_attr <- ncatt_get(file, 0)
  expect_equal(length(global_attr), 2)

  actual <- names(global_attr[1])
  expect_equal(actual, "Info")

  actual <- global_attr[[1]]
  expect_equal(actual, "Created with the CM SAF R Toolbox.")

  actual <- names(global_attr[2])
  expect_equal(actual, "institution")

  actual <- global_attr[[2]]
  expect_equal(actual, "some institution")
})

test_that("coordinates are correct", {
  actual <- ncvar_get(file, "x")
  expect_identical(actual, array(seq(0, 250, by = 50)))

  actual <- ncvar_get(file, "y")
  expect_identical(actual, array(seq(0, 300, by = 50)))

  actual <- ncvar_get(file, "time")
  expect_equal(actual, array(c(149016, 149760, 150456)))
})

nc_close(file)

########## var does not exist #########
test_that("no error is thrown if var does not exist", {
  file_out <- tempfile_nc()
  expect_warning(
    ymonmax("someVariable",
            file.path(data_dir, "ex_ymon.nc"),
            file_out),
    "Variable 'someVariable' not found. Variable 'SIS' will be used instead.")
})

########## var is empty #########
test_that("no error is thrown if var is empty", {
  file_out <- tempfile_nc()
  expect_warning(
    ymonmax("",
            file.path(data_dir, "ex_ymon.nc"),
            file_out),
    "Variable '' not found. Variable 'SIS' will be used instead.")
})

########## var is NULL #########
test_that("error is thrown if var is NULL", {
  file_out <- tempfile_nc()
  expect_error(
    ymonmax(NULL,
            file.path(data_dir, "ex_ymon.nc"),
            file_out),
    "variable must not be NULL"
  )
})

########## input file does not exist #########
test_that("error is thrown if input file does not exist", {
  file_out <- tempfile_nc()
  expect_error(
    ymonmax("SIS",
            file.path(data_dir, "ex_doesNotExist.nc"),
            file_out),
    "Input file does not exist")
})

########## input filename is NULL #########
test_that("error is thrown if input file does not exist", {
  file_out <- tempfile_nc()
  expect_error(
    ymonmax("SIS",
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
    ymonmax("SIS",
            file.path(data_dir, "ex_ymon.nc"),
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
    ymonmax("SIS",
            file.path(data_dir, "ex_ymon.nc"),
            file_out,
            overwrite = TRUE),
    NA)
})

########## output file is NULL #########
test_that("no error is thrown if output file already exists", {
  expect_error(
    ymonmax("SIS",
            file.path(data_dir, "ex_ymon.nc"),
            NULL),
    "Output filepath must be of length one and not NULL"
  )
})

#TODO add test case where input data is weird somehow
