data_dir <- file.path("..", "testdata")
tempfile_nc <- function() {
  tempfile_helper("add_grid_info_")
}

########## simple case ##########
file_out <- tempfile_nc()
add_grid_info(file.path(data_dir, "ex_add_grid_info.nc"),
              file.path(data_dir, "ex_add_grid_info_aux.nc"),
              file_out)
file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file)

  expected_data <- c(
    NA, NA, NA, 86, 87, 89, 89,
    NA, NA, 87, 86, 88, 89, 89,
    NA, 89, 88, 87, 88, 90, 89,
    89, 89, 88, 88, 89, 89, 89,
    90, 89, 89, 89, 88, 89, 90)
  expected <- aperm(array(expected_data, dim = c(7, 5)))

  expect_equivalent(actual, expected)
})

test_that("lon/lat data is correct", {
  actual <- ncvar_get(file, "lat")

  expected_data <- c(
    NA, NA, NA, NA, NA, 48.4687004089355, 48.6366996765137,
    NA, NA, NA, NA, 48.4714012145996, 48.6407012939453, 48.8092994689941,
    NA, NA, NA, 48.4714012145996, 48.6421012878418, 48.8120002746582, 48.9810981750488,
    NA, NA, 48.4687004089355, 48.6407012939453, 48.8120002746582, 48.9823989868164, 49.1521987915039,
    NA, 48.4634017944336, 48.6366996765137, 48.8092994689941, 48.9810981750488, 49.1521987915039, 49.3223991394043)
  expected <- aperm(array(expected_data, dim = c(7, 5)))

  expect_equivalent(actual, expected)

  actual <- ncvar_get(file, "lon")

  expected_data <- c(
    NA, NA, NA, NA, NA, -45.673999786377, -45.9021987915039,
    NA, NA, NA, NA, -45.2247009277344, -45.451099395752, -45.6794013977051,
    NA, NA, NA, -44.7752990722656, -45, -45.2265014648438, -45.4547004699707,
    NA, NA, -44.326000213623, -44.548900604248, -44.7734985351562, -45, -45.2282981872559,
    NA, -43.8767013549805, -44.0978012084961, -44.3205986022949, -44.5452995300293, -44.7717018127441, -45)
  expected <- aperm(array(expected_data, dim = c(7, 5)))

  expect_equivalent(actual, expected)
})

test_that("attributes are correct", {
  actual <- ncatt_get(file, "lon", "units")$value
  expect_equal(actual, "degrees_east")

  actual <- ncatt_get(file, "lon", "long_name")$value
  expect_equal(actual, "longitude")

  actual <- ncatt_get(file, "lon", "standard_name")$value
  expect_equal(actual, 0)

  actual <- ncatt_get(file, "lon", "axis")$value
  expect_equal(actual, 0)

  actual <- ncatt_get(file, "lat", "units")$value
  expect_equal(actual, "degrees_north")

  actual <- ncatt_get(file, "lat", "long_name")$value
  expect_equal(actual, "latitude")

  actual <- ncatt_get(file, "lat", "standard_name")$value
  expect_equal(actual, 0)

  actual <- ncatt_get(file, "lat", "axis")$value
  expect_equal(actual, 0)

  actual <- ncatt_get(file, "time", "units")$value
  expect_equal(actual, "days since 1970-01-01 00:00:00")

  actual <- ncatt_get(file, "time", "long_name")$value
  expect_equal(actual, "time")

  actual <- ncatt_get(file, "time", "standard_name")$value
  expect_equal(actual, 0)

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

  global_attr <- ncatt_get(file, 0)
  expect_equal(length(global_attr), 2)

  actual <- names(global_attr[1])
  expect_equal(actual, "title")

  actual <- global_attr[[1]]
  expect_equal(actual, "CM SAF CLoud property dAtAset using SEVIRI (CLAAS), edition 2")

  actual <- names(global_attr[2])
  expect_equal(actual, "institution")

  actual <- global_attr[[2]]
  expect_equal(actual, "EUMETSAT/CMSAF")
})

test_that("coordinates are correct", {
  actual <- ncvar_get(file, "x")
  expect_identical(actual, array(seq(0, 150, 25)))

  actual <- ncvar_get(file, "y")
  expect_identical(actual, array(seq(0, 100, 25)))

  actual <- ncvar_get(file, "time")
  expect_equal(actual, array(10957))
})

nc_close(file)

########## input file does not exist #########
test_that("no error is thrown if input file does not exist", {
  file_out <- tempfile_nc()
  expect_error(
    add_grid_info(file.path(data_dir, "doesNotExist.nc"),
                  file.path(data_dir, "ex_add_grid_info_aux.nc"),
                  file_out),
    "Input file does not exist"
  )
})

########## input filename is empty #########
test_that("no error is thrown if input filename is empty", {
  file_out <- tempfile_nc()
  expect_error(
    add_grid_info("",
                  file.path(data_dir, "ex_add_grid_info_aux.nc"),
                  file_out),
    "Input file does not exist")
})

########## input filename is NULL #########
test_that("error is thrown if input filename is NULL", {
  file_out <- tempfile_nc()
  expect_error(
    add_grid_info(NULL,
                  file.path(data_dir, "ex_add_grid_info_aux.nc"),
                  file_out),
    "Input filepath must be of length one and not NULL"
  )
})

########## aux file does not exist #########
test_that("no error is thrown if aux file does not exist", {
  file_out <- tempfile_nc()
  expect_error(
    add_grid_info(file.path(data_dir, "ex_add_grid_info.nc"),
                  file.path(data_dir, "doesNotExist.nc"),
                  file_out),
    "Input file does not exist")
})

########## aux filename is empty #########
test_that("no error is thrown if aux filename is empty", {
  file_out <- tempfile_nc()
  expect_error(
    add_grid_info(file.path(data_dir, "ex_add_grid_info.nc"),
                  "",
                  file_out),
    "Input file does not exist")
})

########## aux filename is NULL #########
test_that("error is thrown if aux filename is NULL", {
  file_out <- tempfile_nc()
  expect_error(
    add_grid_info(file.path(data_dir, "ex_add_grid_info.nc"),
                  NULL,
                  file_out),
    "Input filepath must be of length one and not NULL")
})

########## output file already exists #########
test_that("error is thrown if output file already exists", {
  file_out <- tempfile_nc()
  cat("test\n", file = file_out)
  expect_error(
    add_grid_info(file.path(data_dir, "ex_add_grid_info.nc"),
                  file.path(data_dir, "ex_add_grid_info_aux.nc"),
                  file_out),
    "File '.*' already exists. Specify 'overwrite = TRUE' if you want to overwrite it\\."
  )

  expect_equal(readLines(con = file_out), "test")
})

########## existing output file is overwritten ##########
file_out <- tempfile_nc()
cat("test\n", file = file_out)

add_grid_info(file.path(data_dir, "ex_add_grid_info.nc"),
              file.path(data_dir, "ex_add_grid_info_aux.nc"),
              file_out,
              overwrite = TRUE)
file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file)

  expected_data <- c(
    NA, NA, NA, 86, 87, 89, 89,
    NA, NA, 87, 86, 88, 89, 89,
    NA, 89, 88, 87, 88, 90, 89,
    89, 89, 88, 88, 89, 89, 89,
    90, 89, 89, 89, 88, 89, 90)
  expected <- aperm(array(expected_data, dim = c(7, 5)))

  expect_equivalent(actual, expected)
})

test_that("lon/lat data is correct", {
  actual <- ncvar_get(file, "lat")

  expected_data <- c(
    NA, NA, NA, NA, NA, 48.4687004089355, 48.6366996765137,
    NA, NA, NA, NA, 48.4714012145996, 48.6407012939453, 48.8092994689941,
    NA, NA, NA, 48.4714012145996, 48.6421012878418, 48.8120002746582, 48.9810981750488,
    NA, NA, 48.4687004089355, 48.6407012939453, 48.8120002746582, 48.9823989868164, 49.1521987915039,
    NA, 48.4634017944336, 48.6366996765137, 48.8092994689941, 48.9810981750488, 49.1521987915039, 49.3223991394043)
  expected <- aperm(array(expected_data, dim = c(7, 5)))

  expect_equivalent(actual, expected)

  actual <- ncvar_get(file, "lon")

  expected_data <- c(
    NA, NA, NA, NA, NA, -45.673999786377, -45.9021987915039,
    NA, NA, NA, NA, -45.2247009277344, -45.451099395752, -45.6794013977051,
    NA, NA, NA, -44.7752990722656, -45, -45.2265014648438, -45.4547004699707,
    NA, NA, -44.326000213623, -44.548900604248, -44.7734985351562, -45, -45.2282981872559,
    NA, -43.8767013549805, -44.0978012084961, -44.3205986022949, -44.5452995300293, -44.7717018127441, -45)
  expected <- aperm(array(expected_data, dim = c(7, 5)))

  expect_equivalent(actual, expected)
})

test_that("attributes are correct", {
  actual <- ncatt_get(file, "lon", "units")$value
  expect_equal(actual, "degrees_east")

  actual <- ncatt_get(file, "lon", "long_name")$value
  expect_equal(actual, "longitude")

  actual <- ncatt_get(file, "lon", "standard_name")$value
  expect_equal(actual, 0)

  actual <- ncatt_get(file, "lon", "axis")$value
  expect_equal(actual, 0)

  actual <- ncatt_get(file, "lat", "units")$value
  expect_equal(actual, "degrees_north")

  actual <- ncatt_get(file, "lat", "long_name")$value
  expect_equal(actual, "latitude")

  actual <- ncatt_get(file, "lat", "standard_name")$value
  expect_equal(actual, 0)

  actual <- ncatt_get(file, "lat", "axis")$value
  expect_equal(actual, 0)

  actual <- ncatt_get(file, "time", "units")$value
  expect_equal(actual, "days since 1970-01-01 00:00:00")

  actual <- ncatt_get(file, "time", "long_name")$value
  expect_equal(actual, "time")

  actual <- ncatt_get(file, "time", "standard_name")$value
  expect_equal(actual, 0)

  actual <- ncatt_get(file, "time", "calendar")$value
  expect_equal(actual, "standard")

  global_attr <- ncatt_get(file, 0)
  expect_equal(length(global_attr), 2)

  actual <- names(global_attr[1])
  expect_equal(actual, "title")

  actual <- global_attr[[1]]
  expect_equal(actual, "CM SAF CLoud property dAtAset using SEVIRI (CLAAS), edition 2")

  actual <- names(global_attr[2])
  expect_equal(actual, "institution")

  actual <- global_attr[[2]]
  expect_equal(actual, "EUMETSAT/CMSAF")
})

test_that("coordinates are correct", {
  actual <- ncvar_get(file, "x")
  expect_identical(actual, array(seq(0, 150, 25)))

  actual <- ncvar_get(file, "y")
  expect_identical(actual, array(seq(0, 100, 25)))

  actual <- ncvar_get(file, "time")
  expect_equal(actual, array(10957))
})

nc_close(file)

########## input file is directly edited (no outfile) ##########
file_in <- tempfile_nc()
file.copy(from = file.path(data_dir, "ex_add_grid_info.nc"),
          to = file_in,
          overwrite = TRUE)

add_grid_info(file_in,
              file.path(data_dir, "ex_add_grid_info_aux.nc"),
              NULL)

file <- nc_open(file_in)

test_that("data is correct", {
  actual <- ncvar_get(file)

  expected_data <- c(
    NA, NA, NA, 86, 87, 89, 89,
    NA, NA, 87, 86, 88, 89, 89,
    NA, 89, 88, 87, 88, 90, 89,
    89, 89, 88, 88, 89, 89, 89,
    90, 89, 89, 89, 88, 89, 90)
  expected <- aperm(array(expected_data, dim = c(7, 5)))

  expect_equivalent(actual, expected)
})

test_that("lon/lat data is correct", {
  actual <- ncvar_get(file, "lat")

  expected_data <- c(
    NA, NA, NA, NA, NA, 48.4687004089355, 48.6366996765137,
    NA, NA, NA, NA, 48.4714012145996, 48.6407012939453, 48.8092994689941,
    NA, NA, NA, 48.4714012145996, 48.6421012878418, 48.8120002746582, 48.9810981750488,
    NA, NA, 48.4687004089355, 48.6407012939453, 48.8120002746582, 48.9823989868164, 49.1521987915039,
    NA, 48.4634017944336, 48.6366996765137, 48.8092994689941, 48.9810981750488, 49.1521987915039, 49.3223991394043)
  expected <- aperm(array(expected_data, dim = c(7, 5)))

  expect_equivalent(actual, expected)

  actual <- ncvar_get(file, "lon")

  expected_data <- c(
    NA, NA, NA, NA, NA, -45.673999786377, -45.9021987915039,
    NA, NA, NA, NA, -45.2247009277344, -45.451099395752, -45.6794013977051,
    NA, NA, NA, -44.7752990722656, -45, -45.2265014648438, -45.4547004699707,
    NA, NA, -44.326000213623, -44.548900604248, -44.7734985351562, -45, -45.2282981872559,
    NA, -43.8767013549805, -44.0978012084961, -44.3205986022949, -44.5452995300293, -44.7717018127441, -45)
  expected <- aperm(array(expected_data, dim = c(7, 5)))

  expect_equivalent(actual, expected)
})

test_that("attributes are correct", {
  actual <- ncatt_get(file, "lon", "units")$value
  expect_equal(actual, "degrees_east")

  actual <- ncatt_get(file, "lon", "long_name")$value
  expect_equal(actual, "longitude")

  actual <- ncatt_get(file, "lon", "standard_name")$value
  expect_equal(actual, 0)

  actual <- ncatt_get(file, "lon", "axis")$value
  expect_equal(actual, 0)

  actual <- ncatt_get(file, "lat", "units")$value
  expect_equal(actual, "degrees_north")

  actual <- ncatt_get(file, "lat", "long_name")$value
  expect_equal(actual, "latitude")

  actual <- ncatt_get(file, "lat", "standard_name")$value
  expect_equal(actual, 0)

  actual <- ncatt_get(file, "lat", "axis")$value
  expect_equal(actual, 0)

  actual <- ncatt_get(file, "time", "units")$value
  expect_equal(actual, "days since 1970-01-01 00:00:00")

  actual <- ncatt_get(file, "time", "long_name")$value
  expect_equal(actual, "time")

  actual <- ncatt_get(file, "time", "standard_name")$value
  expect_equal(actual, 0)

  actual <- ncatt_get(file, "time", "calendar")$value
  expect_equal(actual, "standard")

  global_attr <- ncatt_get(file, 0)
  expect_equal(length(global_attr), 2)

  actual <- names(global_attr[1])
  expect_equal(actual, "title")

  actual <- global_attr[[1]]
  expect_equal(actual, "CM SAF CLoud property dAtAset using SEVIRI (CLAAS), edition 2")

  actual <- names(global_attr[2])
  expect_equal(actual, "institution")

  actual <- global_attr[[2]]
  expect_equal(actual, "EUMETSAT/CMSAF")
})

test_that("coordinates are correct", {
  actual <- ncvar_get(file, "x")
  expect_identical(actual, array(seq(0, 150, 25)))

  actual <- ncvar_get(file, "y")
  expect_identical(actual, array(seq(0, 100, 25)))

  actual <- ncvar_get(file, "time")
  expect_equal(actual, array(10957))
})

nc_close(file)

########## infile contains lon/lat data as variable already ##########
file_out <- tempfile_nc()
expect_error(add_grid_info(file.path(data_dir, "ex_add_grid_info_with_grid_var.nc"),
                           file.path(data_dir, "ex_add_grid_info_aux.nc"),
                           file_out), "infile already contains lon/lat data"
)
#
# file <- nc_open(file_out)
#
# test_that("data is correct", {
#   actual <- ncvar_get(file)
#
#   expected_data <- c(
#     NA, NA, NA, 86, 87, 89, 89,
#     NA, NA, 87, 86, 88, 89, 89,
#     NA, 89, 88, 87, 88, 90, 89,
#     89, 89, 88, 88, 89, 89, 89,
#     90, 89, 89, 89, 88, 89, 90)
#   expected <- aperm(array(expected_data, dim = c(7, 5)))
#
#   expect_equivalent(actual, expected)
# })
#
# test_that("original lon/lat data is not changed", {
#   actual <- ncvar_get(file, "lat")
#
#   expected_data <- c(
#     NA, NA, NA, NA, NA, 48.4687004089355, 48.6366996765137,
#     NA, NA, NA, NA, 48.4714012145996, 48.6407012939453, 48.8092994689941,
#     NA, NA, NA, 48.4714012145996, 48.6421012878418, 48.8120002746582, 48.9810981750488,
#     NA, NA, 48.4687004089355, 48.6407012939453, 48.8120002746582, 48.9823989868164, 49.1521987915039,
#     NA, 48.4634017944336, 48.6366996765137, 48.8092994689941, 48.9810981750488, 49.1521987915039, 49.3223991394043)
#   expected <- aperm(array(expected_data, dim = c(7, 5)))
#
#   expect_equivalent(actual, expected)
#
#   actual <- ncvar_get(file, "lon")
#
#   expected_data <- c(
#     NA, NA, NA, NA, NA, -45.673999786377, -45.9021987915039,
#     NA, NA, NA, NA, -45.2247009277344, -45.451099395752, -45.6794013977051,
#     NA, NA, NA, -44.7752990722656, -45, -45.2265014648438, -45.4547004699707,
#     NA, NA, -44.326000213623, -44.548900604248, -44.7734985351562, -45, -45.2282981872559,
#     NA, -43.8767013549805, -44.0978012084961, -44.3205986022949, -44.5452995300293, -44.7717018127441, -45)
#   expected <- aperm(array(expected_data, dim = c(7, 5)))
#
#   expect_equivalent(actual, expected)
# })
#
# test_that("attributes are correct", {
#   actual <- ncatt_get(file, "lon", "units")$value
#   expect_equal(actual, "degrees_east")
#
#   actual <- ncatt_get(file, "lon", "long_name")$value
#   expect_equal(actual, "longitude")
#
#   actual <- ncatt_get(file, "lon", "standard_name")$value
#   expect_equal(actual, 0)
#
#   actual <- ncatt_get(file, "lon", "axis")$value
#   expect_equal(actual, 0)
#
#   actual <- ncatt_get(file, "lat", "units")$value
#   expect_equal(actual, "degrees_north")
#
#   actual <- ncatt_get(file, "lat", "long_name")$value
#   expect_equal(actual, "latitude")
#
#   actual <- ncatt_get(file, "lat", "standard_name")$value
#   expect_equal(actual, 0)
#
#   actual <- ncatt_get(file, "lat", "axis")$value
#   expect_equal(actual, 0)
#
#   actual <- ncatt_get(file, "time", "units")$value
#   expect_equal(actual, "days since 1970-01-01 00:00:00")
#
#   actual <- ncatt_get(file, "time", "long_name")$value
#   expect_equal(actual, "time")
#
#   actual <- ncatt_get(file, "time", "standard_name")$value
#   expect_equal(actual, 0)
#
#   actual <- ncatt_get(file, "time", "calendar")$value
#   expect_equal(actual, "standard")
#
#   global_attr <- ncatt_get(file, 0)
#   expect_equal(length(global_attr), 2)
#
#   actual <- names(global_attr[1])
#   expect_equal(actual, "title")
#
#   actual <- global_attr[[1]]
#   expect_equal(actual, "CM SAF CLoud property dAtAset using SEVIRI (CLAAS), edition 2")
#
#   actual <- names(global_attr[2])
#   expect_equal(actual, "institution")
#
#   actual <- global_attr[[2]]
#   expect_equal(actual, "EUMETSAT/CMSAF")
# })
#
# test_that("coordinates are correct", {
#   actual <- ncvar_get(file, "x")
#   expect_identical(actual, array(seq(0, 150, 25)))
#
#   actual <- ncvar_get(file, "y")
#   expect_identical(actual, array(seq(0, 100, 25)))
#
#   actual <- ncvar_get(file, "time")
#   expect_equal(actual, array(10957))
# })
#
# nc_close(file)

########## infile contains lon/lat data as dimension already ##########
file_out <- tempfile_nc()
expect_error(add_grid_info(file.path(data_dir, "ex_normal1.nc"),
                           file.path(data_dir, "ex_add_grid_info_aux.nc"),
                           file_out), "infile already contains lon/lat data"
)
#
# file <- nc_open(file_out)
#
# test_that("data is unchanged", {
#   actual <- ncvar_get(file)
#
#   expected_data <- rep(seq(250, 272), 3)
#   expected <- array(expected_data, dim = c(7, 7))
#
#   expect_equivalent(actual, expected)
# })
#
# test_that("attributes are unchanged", {
#   actual <- ncatt_get(file, "lon", "units")$value
#   expect_equal(actual, "degrees_east")
#
#   actual <- ncatt_get(file, "lon", "long_name")$value
#   expect_equal(actual, "lon")
#
#   actual <- ncatt_get(file, "lon", "standard_name")$value
#   expect_equal(actual, "longitude")
#
#   actual <- ncatt_get(file, "lon", "axis")$value
#   expect_equal(actual, 0)
#
#   actual <- ncatt_get(file, "lat", "units")$value
#   expect_equal(actual, "degrees_north")
#
#   actual <- ncatt_get(file, "lat", "long_name")$value
#   expect_equal(actual, "lat")
#
#   actual <- ncatt_get(file, "lat", "standard_name")$value
#   expect_equal(actual, "latitude")
#
#   actual <- ncatt_get(file, "lat", "axis")$value
#   expect_equal(actual, 0)
#
#   actual <- ncatt_get(file, "time", "units")$value
#   expect_equal(actual, "hours since 1983-01-01 00:00:00")
#
#   actual <- ncatt_get(file, "time", "long_name")$value
#   expect_equal(actual, "time")
#
#   actual <- ncatt_get(file, "time", "standard_name")$value
#   expect_equal(actual, 0)
#
#   actual <- ncatt_get(file, "time", "calendar")$value
#   expect_equal(actual, 0)
#
#   global_attr <- ncatt_get(file, 0)
#   expect_equal(length(global_attr), 0)
# })
#
# test_that("coordinates are unchanged", {
#   actual <- ncvar_get(file, "lon")
#   expect_identical(actual, array(seq(5, 8, 0.5)))
#
#   actual <- ncvar_get(file, "lat")
#   expect_identical(actual, array(seq(45, 48, 0.5)))
#
#   actual <- ncvar_get(file, "time")
#   expect_equal(actual, array(149016))
# })
#
# nc_close(file)
