data_dir <- file.path("..", "testdata")
tempfile_nc <- function() {
  tempfile_helper("hoursum_")
}

########## output ncdf version 3 ##########
file_out <- tempfile_nc()
hoursum("SIS", file.path(data_dir, "ex_hourx.nc"), file_out)
file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file)
  
  expected_data <- c(18102.0,18162.0,18121.0,18080.0,18140.0,18099.0,18058.0,
                     18118.0,18077.0,18036.0,18096.0,18055.0,18014.0,18074.0,
                     18033.0,17992.0,18052.0,18011.0,17970.0,18030.0,17989.0,
                     17948.0,18008.0,17967.0,17926.0,17986.0,17945.0,17904.0,
                     17964.0,17923.0,17882.0,17942.0,17901.0,17860.0,17920.0,
                     17879.0,17838.0,17898.0,17857.0,17816.0,17876.0,17936.0,
                     17895.0,17955.0,18015.0,17974.0,18034.0,18094.0,18053.0,
                     
                     18055.0,18014.0,18074.0,18033.0,17992.0,18052.0,18011.0,
                     17970.0,18030.0,17989.0,17948.0,18008.0,17967.0,17926.0,
                     17986.0,17945.0,17904.0,17964.0,17923.0,17882.0,17942.0,
                     17901.0,17860.0,17920.0,17879.0,17838.0,17898.0,17857.0,
                     17816.0,17876.0,17936.0,17895.0,17955.0,18015.0,17974.0,
                     18034.0,18094.0,18053.0,18113.0,18173.0,18132.0,18091.0,
                     18151.0,18110.0,18069.0,18129.0,18088.0,18047.0,18107.0,
                     
                     18008.0,17967.0,17926.0,17986.0,17945.0,17904.0,17964.0,
                     17923.0,17882.0,17942.0,17901.0,17860.0,17920.0,17879.0,
                     17838.0,17898.0,17857.0,17816.0,17876.0,17936.0,17895.0,
                     17955.0,18015.0,17974.0,18034.0,18094.0,18053.0,18113.0,
                     18173.0,18132.0,18091.0,18151.0,18110.0,18069.0,18129.0,
                     18088.0,18047.0,18107.0,18066.0,18025.0,18085.0,18044.0,
                     18003.0,18063.0,18022.0,17981.0,18041.0,18000.0,17959.0)
  expected <- array(expected_data, dim = c(7, 7, 3))
  
  expect_equivalent(actual[1:147], expected)
})

test_that("variable attributes are correct", {
  actual <- ncatt_get(file, "SIS", "units")$value
  expect_equal(actual, "W m-2")
  
  actual <- ncatt_get(file, "SIS", "_FillValue")$value
  expect_equal(actual, -999)
  
  actual <- ncatt_get(file, "SIS", "standard_name")$value
  expect_equal(actual, "SIS_standard")
  
  actual <- ncatt_get(file, "SIS", "long_name")$value
  expect_equal(actual, "Surface Incoming Shortwave Radiation")
  
  actual <- ncatt_get(file, "SIS", "missing_value")$value
  expect_equal(actual, 0)
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
  expect_equal(actual, "minutes since 1983-01-01 00:00:00")
  
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
  expect_equal(actual, "cmsaf::hoursum for variable SIS")
  
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
  expect_equal(actual[1:3], array(c(8941680, 8941740, 8941800)))
})

nc_close(file)

########## output ncdf version 4 ##########
file_out <- tempfile_nc()
hoursum("SIS", file.path(data_dir, "ex_hourx.nc"), file_out, nc34 = 4)
file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file)
  
  expected_data <- c(18102.0,18162.0,18121.0,18080.0,18140.0,18099.0,18058.0,
                     18118.0,18077.0,18036.0,18096.0,18055.0,18014.0,18074.0,
                     18033.0,17992.0,18052.0,18011.0,17970.0,18030.0,17989.0,
                     17948.0,18008.0,17967.0,17926.0,17986.0,17945.0,17904.0,
                     17964.0,17923.0,17882.0,17942.0,17901.0,17860.0,17920.0,
                     17879.0,17838.0,17898.0,17857.0,17816.0,17876.0,17936.0,
                     17895.0,17955.0,18015.0,17974.0,18034.0,18094.0,18053.0,
                     
                     18055.0,18014.0,18074.0,18033.0,17992.0,18052.0,18011.0,
                     17970.0,18030.0,17989.0,17948.0,18008.0,17967.0,17926.0,
                     17986.0,17945.0,17904.0,17964.0,17923.0,17882.0,17942.0,
                     17901.0,17860.0,17920.0,17879.0,17838.0,17898.0,17857.0,
                     17816.0,17876.0,17936.0,17895.0,17955.0,18015.0,17974.0,
                     18034.0,18094.0,18053.0,18113.0,18173.0,18132.0,18091.0,
                     18151.0,18110.0,18069.0,18129.0,18088.0,18047.0,18107.0,
                     
                     18008.0,17967.0,17926.0,17986.0,17945.0,17904.0,17964.0,
                     17923.0,17882.0,17942.0,17901.0,17860.0,17920.0,17879.0,
                     17838.0,17898.0,17857.0,17816.0,17876.0,17936.0,17895.0,
                     17955.0,18015.0,17974.0,18034.0,18094.0,18053.0,18113.0,
                     18173.0,18132.0,18091.0,18151.0,18110.0,18069.0,18129.0,
                     18088.0,18047.0,18107.0,18066.0,18025.0,18085.0,18044.0,
                     18003.0,18063.0,18022.0,17981.0,18041.0,18000.0,17959.0)
  expected <- array(expected_data, dim = c(7, 7, 3))
  
  expect_equivalent(actual[1:147], expected)
})

test_that("variable attributes are correct", {
  actual <- ncatt_get(file, "SIS", "units")$value
  expect_equal(actual, "W m-2")
  
  actual <- ncatt_get(file, "SIS", "_FillValue")$value
  expect_equal(actual, -999)
  
  actual <- ncatt_get(file, "SIS", "standard_name")$value
  expect_equal(actual, "SIS_standard")
  
  actual <- ncatt_get(file, "SIS", "long_name")$value
  expect_equal(actual, "Surface Incoming Shortwave Radiation")
  
  actual <- ncatt_get(file, "SIS", "missing_value")$value
  expect_equal(actual, 0)
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
  expect_equal(actual, "minutes since 1983-01-01 00:00:00")
  
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
  expect_equal(actual[1:3], array(c(8941680, 8941740, 8941800)))
})

nc_close(file)

########## output ncdf version 7 #########
file_out <- tempfile_nc()
test_that("error is thrown if ncdf version is wrong", {
  expect_error(
    hoursum("SIS", file.path(data_dir, "ex_hourx.nc"), file_out, nc34 = 7),
    "nc version must be in c(3, 4), but was 7", fixed = TRUE
  )
})

########## output ncdf version is NULL #########
file_out <- tempfile_nc()
test_that("ncdf version NULL throws an error", {
  expect_error(
    hoursum("SIS",
           file.path(data_dir, "ex_hourx.nc"),
           file_out, nc34 = NULL),
    "nc_version must not be NULL"
  )
})

########## variable does not exist #########
file_out <- tempfile_nc()

test_that("warning is shown if var does not exist", {
  expect_warning(hoursum("notExist",
                        file.path(data_dir, "ex_hourx.nc"),
                        file_out),
                 "Variable 'notExist' not found. Variable 'SIS' will be used instead.")
})

file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file)
  
  expected_data <- c(18102.0,18162.0,18121.0,18080.0,18140.0,18099.0,18058.0,
                     18118.0,18077.0,18036.0,18096.0,18055.0,18014.0,18074.0,
                     18033.0,17992.0,18052.0,18011.0,17970.0,18030.0,17989.0,
                     17948.0,18008.0,17967.0,17926.0,17986.0,17945.0,17904.0,
                     17964.0,17923.0,17882.0,17942.0,17901.0,17860.0,17920.0,
                     17879.0,17838.0,17898.0,17857.0,17816.0,17876.0,17936.0,
                     17895.0,17955.0,18015.0,17974.0,18034.0,18094.0,18053.0,
                     
                     18055.0,18014.0,18074.0,18033.0,17992.0,18052.0,18011.0,
                     17970.0,18030.0,17989.0,17948.0,18008.0,17967.0,17926.0,
                     17986.0,17945.0,17904.0,17964.0,17923.0,17882.0,17942.0,
                     17901.0,17860.0,17920.0,17879.0,17838.0,17898.0,17857.0,
                     17816.0,17876.0,17936.0,17895.0,17955.0,18015.0,17974.0,
                     18034.0,18094.0,18053.0,18113.0,18173.0,18132.0,18091.0,
                     18151.0,18110.0,18069.0,18129.0,18088.0,18047.0,18107.0,
                     
                     18008.0,17967.0,17926.0,17986.0,17945.0,17904.0,17964.0,
                     17923.0,17882.0,17942.0,17901.0,17860.0,17920.0,17879.0,
                     17838.0,17898.0,17857.0,17816.0,17876.0,17936.0,17895.0,
                     17955.0,18015.0,17974.0,18034.0,18094.0,18053.0,18113.0,
                     18173.0,18132.0,18091.0,18151.0,18110.0,18069.0,18129.0,
                     18088.0,18047.0,18107.0,18066.0,18025.0,18085.0,18044.0,
                     18003.0,18063.0,18022.0,17981.0,18041.0,18000.0,17959.0)
  expected <- array(expected_data, dim = c(7, 7, 3))
  
  expect_equivalent(actual[1:147], expected)
})

test_that("variable attributes are correct", {
  actual <- ncatt_get(file, "SIS", "units")$value
  expect_equal(actual, "W m-2")
  
  actual <- ncatt_get(file, "SIS", "_FillValue")$value
  expect_equal(actual, -999)
  
  actual <- ncatt_get(file, "SIS", "standard_name")$value
  expect_equal(actual, "SIS_standard")
  
  actual <- ncatt_get(file, "SIS", "long_name")$value
  expect_equal(actual, "Surface Incoming Shortwave Radiation")
  
  actual <- ncatt_get(file, "SIS", "missing_value")$value
  expect_equal(actual, 0)
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
  expect_equal(actual, "minutes since 1983-01-01 00:00:00")
  
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
  expect_equal(actual[1:3], array(c(8941680, 8941740, 8941800)))
})

nc_close(file)

########## variable is null #########
file_out <- tempfile_nc()
test_that("error is thrown if variable is NULL", {
  expect_error(
    hoursum(NULL,
           file.path(data_dir, "ex_hourx.nc"),
           file_out),
    "variable must not be NULL"
  )
})

########## variable is empty #########
file_out <- tempfile_nc()

test_that("warning is shown if var is empty", {
  expect_warning(hoursum("",
                        file.path(data_dir, "ex_hourx.nc"),
                        file_out),
                 "Variable '' not found. Variable 'SIS' will be used instead.")
})

file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file)
  
  expected_data <- c(18102.0,18162.0,18121.0,18080.0,18140.0,18099.0,18058.0,
                     18118.0,18077.0,18036.0,18096.0,18055.0,18014.0,18074.0,
                     18033.0,17992.0,18052.0,18011.0,17970.0,18030.0,17989.0,
                     17948.0,18008.0,17967.0,17926.0,17986.0,17945.0,17904.0,
                     17964.0,17923.0,17882.0,17942.0,17901.0,17860.0,17920.0,
                     17879.0,17838.0,17898.0,17857.0,17816.0,17876.0,17936.0,
                     17895.0,17955.0,18015.0,17974.0,18034.0,18094.0,18053.0,
                     
                     18055.0,18014.0,18074.0,18033.0,17992.0,18052.0,18011.0,
                     17970.0,18030.0,17989.0,17948.0,18008.0,17967.0,17926.0,
                     17986.0,17945.0,17904.0,17964.0,17923.0,17882.0,17942.0,
                     17901.0,17860.0,17920.0,17879.0,17838.0,17898.0,17857.0,
                     17816.0,17876.0,17936.0,17895.0,17955.0,18015.0,17974.0,
                     18034.0,18094.0,18053.0,18113.0,18173.0,18132.0,18091.0,
                     18151.0,18110.0,18069.0,18129.0,18088.0,18047.0,18107.0,
                     
                     18008.0,17967.0,17926.0,17986.0,17945.0,17904.0,17964.0,
                     17923.0,17882.0,17942.0,17901.0,17860.0,17920.0,17879.0,
                     17838.0,17898.0,17857.0,17816.0,17876.0,17936.0,17895.0,
                     17955.0,18015.0,17974.0,18034.0,18094.0,18053.0,18113.0,
                     18173.0,18132.0,18091.0,18151.0,18110.0,18069.0,18129.0,
                     18088.0,18047.0,18107.0,18066.0,18025.0,18085.0,18044.0,
                     18003.0,18063.0,18022.0,17981.0,18041.0,18000.0,17959.0)
  expected <- array(expected_data, dim = c(7, 7, 3))
  
  expect_equivalent(actual[1:147], expected)
})

test_that("variable attributes are correct", {
  actual <- ncatt_get(file, "SIS", "units")$value
  expect_equal(actual, "W m-2")
  
  actual <- ncatt_get(file, "SIS", "_FillValue")$value
  expect_equal(actual, -999)
  
  actual <- ncatt_get(file, "SIS", "standard_name")$value
  expect_equal(actual, "SIS_standard")
  
  actual <- ncatt_get(file, "SIS", "long_name")$value
  expect_equal(actual, "Surface Incoming Shortwave Radiation")
  
  actual <- ncatt_get(file, "SIS", "missing_value")$value
  expect_equal(actual, 0)
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
  expect_equal(actual, "minutes since 1983-01-01 00:00:00")
  
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
  expect_equal(actual[1:3], array(c(8941680, 8941740, 8941800)))
})

nc_close(file)

########## input file does not exist #########
file_out <- tempfile_nc()
test_that("error is thrown if input file does not exist", {
  expect_error(
    hoursum("SIS",
           file.path(data_dir, "xemaple1.nc"),
           file_out),
    "Input file does not exist")
})

########## input filename is empty #########
file_out <- tempfile_nc()
test_that("error is thrown if input filename is empty", {
  expect_error(
    hoursum("SIS", "", file_out),
    "Input file does not exist")
})

########## input filename is NULL #########
file_out <- tempfile_nc()

test_that("error is thrown if input filename is NULL", {
  expect_error(
    hoursum("SIS", NULL, file_out),
    "Input filepath must be of length one and not NULL"
  )
})

########## output file already exists #########
file_out <- tempfile_nc()
cat("test\n", file = file_out)

test_that("error is thrown if output file already exists", {
  expect_error(
    hoursum("SIS",
           file.path(data_dir, "ex_dayx.nc"),
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
    hoursum("SIS",
           file.path(data_dir, "ex_hourx.nc"),
           file_out,
           overwrite = TRUE
    ),
    NA
  )
})

file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file)
  
  expected_data <- c(18102.0,18162.0,18121.0,18080.0,18140.0,18099.0,18058.0,
                     18118.0,18077.0,18036.0,18096.0,18055.0,18014.0,18074.0,
                     18033.0,17992.0,18052.0,18011.0,17970.0,18030.0,17989.0,
                     17948.0,18008.0,17967.0,17926.0,17986.0,17945.0,17904.0,
                     17964.0,17923.0,17882.0,17942.0,17901.0,17860.0,17920.0,
                     17879.0,17838.0,17898.0,17857.0,17816.0,17876.0,17936.0,
                     17895.0,17955.0,18015.0,17974.0,18034.0,18094.0,18053.0,
                     
                     18055.0,18014.0,18074.0,18033.0,17992.0,18052.0,18011.0,
                     17970.0,18030.0,17989.0,17948.0,18008.0,17967.0,17926.0,
                     17986.0,17945.0,17904.0,17964.0,17923.0,17882.0,17942.0,
                     17901.0,17860.0,17920.0,17879.0,17838.0,17898.0,17857.0,
                     17816.0,17876.0,17936.0,17895.0,17955.0,18015.0,17974.0,
                     18034.0,18094.0,18053.0,18113.0,18173.0,18132.0,18091.0,
                     18151.0,18110.0,18069.0,18129.0,18088.0,18047.0,18107.0,
                     
                     18008.0,17967.0,17926.0,17986.0,17945.0,17904.0,17964.0,
                     17923.0,17882.0,17942.0,17901.0,17860.0,17920.0,17879.0,
                     17838.0,17898.0,17857.0,17816.0,17876.0,17936.0,17895.0,
                     17955.0,18015.0,17974.0,18034.0,18094.0,18053.0,18113.0,
                     18173.0,18132.0,18091.0,18151.0,18110.0,18069.0,18129.0,
                     18088.0,18047.0,18107.0,18066.0,18025.0,18085.0,18044.0,
                     18003.0,18063.0,18022.0,17981.0,18041.0,18000.0,17959.0)
  expected <- array(expected_data, dim = c(7, 7, 3))
  
  expect_equivalent(actual[1:147], expected)
})

test_that("variable attributes are correct", {
  actual <- ncatt_get(file, "SIS", "units")$value
  expect_equal(actual, "W m-2")
  
  actual <- ncatt_get(file, "SIS", "_FillValue")$value
  expect_equal(actual, -999)
  
  actual <- ncatt_get(file, "SIS", "standard_name")$value
  expect_equal(actual, "SIS_standard")
  
  actual <- ncatt_get(file, "SIS", "long_name")$value
  expect_equal(actual, "Surface Incoming Shortwave Radiation")
  
  actual <- ncatt_get(file, "SIS", "missing_value")$value
  expect_equal(actual, 0)
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
  expect_equal(actual, "minutes since 1983-01-01 00:00:00")
  
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
  expect_equal(actual[1:3], array(c(8941680, 8941740, 8941800)))
})

nc_close(file)
