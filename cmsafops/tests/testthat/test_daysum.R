data_dir <- file.path("..", "testdata")
tempfile_nc <- function() {
  tempfile_helper("daysum_")
}

########## output ncdf version 3 ##########
file_out <- tempfile_nc()
daysum("SIS", file.path(data_dir, "ex_dayx.nc"), file_out)
file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file)
  
  expected_data <- c(3106.0,	3118.0,	3107.0,	3119.0,	3131.0,	3120.0,	3132.0,
                     3144.0,	3133.0,	3145.0,	3157.0,	3146.0,	3158.0,	3147.0,
                     3136.0,	3148.0,	3137.0,	3126.0,	3138.0,	3127.0,	3116.0,
                     3128.0,	3117.0,	3106.0,	3118.0,	3107.0,	3119.0,	3131.0,
                     3120.0,	3132.0,	3144.0,	3133.0,	3145.0,	3157.0,	3146.0,
                     3158.0,	3147.0,	3136.0,	3148.0,	3137.0,	3126.0,	3138.0,
                     3127.0,	3116.0,	3128.0,	3117.0,	3106.0,	3118.0,	3107.0,
                     
                     6266.0,	6267.0,	6268.0,	6269.0,	6270.0,	6271.0,	6272.0,
                     6273.0,	6274.0,	6275.0,	6253.0,	6254.0,	6255.0,	6256.0,
                     6257.0,	6258.0,	6259.0,	6260.0,	6261.0,	6262.0,	6263.0,
                     6264.0,	6265.0,	6266.0,	6267.0,	6268.0,	6269.0,	6270.0,
                     6271.0,	6272.0,	6273.0,	6274.0,	6275.0,	6253.0,	6254.0,
                     6255.0,	6256.0,	6257.0,	6258.0,	6259.0,	6260.0,	6261.0,
                     6262.0,	6263.0,	6264.0,	6265.0,	6266.0,	6267.0,	6268.0,
                     
                     3393.0,	3383.0,	3396.0,	3386.0,	3376.0,	3389.0,	3379.0,
                     3369.0,	3382.0,	3372.0,	3385.0,	3398.0,	3388.0,	3401.0,
                     3414.0,	3404.0,	3417.0,	3407.0,	3397.0,	3410.0,	3400.0,
                     3390.0,	3403.0,	3393.0,	3383.0,	3396.0,	3386.0,	3376.0,
                     3389.0,	3379.0,	3369.0,	3382.0,	3372.0,	3385.0,	3398.0,
                     3388.0,	3401.0,	3414.0,	3404.0,	3417.0,	3407.0,	3397.0,
                     3410.0,	3400.0,	3390.0,	3403.0,	3393.0,	3383.0,	3396.0)
  expected <- array(expected_data, dim = c(7, 7, 3))
  
  expect_equivalent(actual, expected)
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
  expect_equal(actual, "cmsaf::daysum for variable SIS")
  
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
  expect_equal(actual, array(c(149028, 149040, 149064)))
})

nc_close(file)

########## output ncdf version 4 ##########
file_out <- tempfile_nc()
daysum("SIS", file.path(data_dir, "ex_dayx.nc"), file_out, nc34 = 4)
file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file)
  
  expected_data <- c(3106.0,	3118.0,	3107.0,	3119.0,	3131.0,	3120.0,	3132.0,
                     3144.0,	3133.0,	3145.0,	3157.0,	3146.0,	3158.0,	3147.0,
                     3136.0,	3148.0,	3137.0,	3126.0,	3138.0,	3127.0,	3116.0,
                     3128.0,	3117.0,	3106.0,	3118.0,	3107.0,	3119.0,	3131.0,
                     3120.0,	3132.0,	3144.0,	3133.0,	3145.0,	3157.0,	3146.0,
                     3158.0,	3147.0,	3136.0,	3148.0,	3137.0,	3126.0,	3138.0,
                     3127.0,	3116.0,	3128.0,	3117.0,	3106.0,	3118.0,	3107.0,
                     
                     6266.0,	6267.0,	6268.0,	6269.0,	6270.0,	6271.0,	6272.0,
                     6273.0,	6274.0,	6275.0,	6253.0,	6254.0,	6255.0,	6256.0,
                     6257.0,	6258.0,	6259.0,	6260.0,	6261.0,	6262.0,	6263.0,
                     6264.0,	6265.0,	6266.0,	6267.0,	6268.0,	6269.0,	6270.0,
                     6271.0,	6272.0,	6273.0,	6274.0,	6275.0,	6253.0,	6254.0,
                     6255.0,	6256.0,	6257.0,	6258.0,	6259.0,	6260.0,	6261.0,
                     6262.0,	6263.0,	6264.0,	6265.0,	6266.0,	6267.0,	6268.0,
                     
                     3393.0,	3383.0,	3396.0,	3386.0,	3376.0,	3389.0,	3379.0,
                     3369.0,	3382.0,	3372.0,	3385.0,	3398.0,	3388.0,	3401.0,
                     3414.0,	3404.0,	3417.0,	3407.0,	3397.0,	3410.0,	3400.0,
                     3390.0,	3403.0,	3393.0,	3383.0,	3396.0,	3386.0,	3376.0,
                     3389.0,	3379.0,	3369.0,	3382.0,	3372.0,	3385.0,	3398.0,
                     3388.0,	3401.0,	3414.0,	3404.0,	3417.0,	3407.0,	3397.0,
                     3410.0,	3400.0,	3390.0,	3403.0,	3393.0,	3383.0,	3396.0)
  expected <- array(expected_data, dim = c(7, 7, 3))
  
  expect_equivalent(actual, expected)
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
  expect_equal(actual, array(c(149028, 149040, 149064)))
})

nc_close(file)

########## output ncdf version 7 #########
file_out <- tempfile_nc()
test_that("error is thrown if ncdf version is wrong", {
  expect_error(
    daysum("SIS", file.path(data_dir, "ex_dayx.nc"), file_out, nc34 = 7),
    "nc version must be in c(3, 4), but was 7", fixed = TRUE
  )
})

########## output ncdf version is NULL #########
file_out <- tempfile_nc()
test_that("ncdf version NULL throws an error", {
  expect_error(
    daysum("SIS",
             file.path(data_dir, "ex_dayx.nc"),
             file_out, nc34 = NULL),
    "nc_version must not be NULL"
  )
})

########## variable does not exist #########
file_out <- tempfile_nc()

test_that("warning is shown if var does not exist", {
  expect_warning(daysum("notExist",
                          file.path(data_dir, "ex_dayx.nc"),
                          file_out),
                 "Variable 'notExist' not found. Variable 'SIS' will be used instead.")
})

file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file)
  
  expected_data <- c(3106.0,	3118.0,	3107.0,	3119.0,	3131.0,	3120.0,	3132.0,
                     3144.0,	3133.0,	3145.0,	3157.0,	3146.0,	3158.0,	3147.0,
                     3136.0,	3148.0,	3137.0,	3126.0,	3138.0,	3127.0,	3116.0,
                     3128.0,	3117.0,	3106.0,	3118.0,	3107.0,	3119.0,	3131.0,
                     3120.0,	3132.0,	3144.0,	3133.0,	3145.0,	3157.0,	3146.0,
                     3158.0,	3147.0,	3136.0,	3148.0,	3137.0,	3126.0,	3138.0,
                     3127.0,	3116.0,	3128.0,	3117.0,	3106.0,	3118.0,	3107.0,
                     
                     6266.0,	6267.0,	6268.0,	6269.0,	6270.0,	6271.0,	6272.0,
                     6273.0,	6274.0,	6275.0,	6253.0,	6254.0,	6255.0,	6256.0,
                     6257.0,	6258.0,	6259.0,	6260.0,	6261.0,	6262.0,	6263.0,
                     6264.0,	6265.0,	6266.0,	6267.0,	6268.0,	6269.0,	6270.0,
                     6271.0,	6272.0,	6273.0,	6274.0,	6275.0,	6253.0,	6254.0,
                     6255.0,	6256.0,	6257.0,	6258.0,	6259.0,	6260.0,	6261.0,
                     6262.0,	6263.0,	6264.0,	6265.0,	6266.0,	6267.0,	6268.0,
                     
                     3393.0,	3383.0,	3396.0,	3386.0,	3376.0,	3389.0,	3379.0,
                     3369.0,	3382.0,	3372.0,	3385.0,	3398.0,	3388.0,	3401.0,
                     3414.0,	3404.0,	3417.0,	3407.0,	3397.0,	3410.0,	3400.0,
                     3390.0,	3403.0,	3393.0,	3383.0,	3396.0,	3386.0,	3376.0,
                     3389.0,	3379.0,	3369.0,	3382.0,	3372.0,	3385.0,	3398.0,
                     3388.0,	3401.0,	3414.0,	3404.0,	3417.0,	3407.0,	3397.0,
                     3410.0,	3400.0,	3390.0,	3403.0,	3393.0,	3383.0,	3396.0)
  expected <- array(expected_data, dim = c(7, 7, 3))
  
  expect_equivalent(actual, expected)
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
  expect_equal(actual, array(c(149028, 149040, 149064)))
})

nc_close(file)

########## variable is null #########
file_out <- tempfile_nc()
test_that("error is thrown if variable is NULL", {
  expect_error(
    daysum(NULL,
             file.path(data_dir, "ex_dayx.nc"),
             file_out),
    "variable must not be NULL"
  )
})

########## variable is empty #########
file_out <- tempfile_nc()

test_that("warning is shown if var is empty", {
  expect_warning(daysum("",
                          file.path(data_dir, "ex_dayx.nc"),
                          file_out),
                 "Variable '' not found. Variable 'SIS' will be used instead.")
})

file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file)
  
  expected_data <- c(3106.0,	3118.0,	3107.0,	3119.0,	3131.0,	3120.0,	3132.0,
                     3144.0,	3133.0,	3145.0,	3157.0,	3146.0,	3158.0,	3147.0,
                     3136.0,	3148.0,	3137.0,	3126.0,	3138.0,	3127.0,	3116.0,
                     3128.0,	3117.0,	3106.0,	3118.0,	3107.0,	3119.0,	3131.0,
                     3120.0,	3132.0,	3144.0,	3133.0,	3145.0,	3157.0,	3146.0,
                     3158.0,	3147.0,	3136.0,	3148.0,	3137.0,	3126.0,	3138.0,
                     3127.0,	3116.0,	3128.0,	3117.0,	3106.0,	3118.0,	3107.0,
                     
                     6266.0,	6267.0,	6268.0,	6269.0,	6270.0,	6271.0,	6272.0,
                     6273.0,	6274.0,	6275.0,	6253.0,	6254.0,	6255.0,	6256.0,
                     6257.0,	6258.0,	6259.0,	6260.0,	6261.0,	6262.0,	6263.0,
                     6264.0,	6265.0,	6266.0,	6267.0,	6268.0,	6269.0,	6270.0,
                     6271.0,	6272.0,	6273.0,	6274.0,	6275.0,	6253.0,	6254.0,
                     6255.0,	6256.0,	6257.0,	6258.0,	6259.0,	6260.0,	6261.0,
                     6262.0,	6263.0,	6264.0,	6265.0,	6266.0,	6267.0,	6268.0,
                     
                     3393.0,	3383.0,	3396.0,	3386.0,	3376.0,	3389.0,	3379.0,
                     3369.0,	3382.0,	3372.0,	3385.0,	3398.0,	3388.0,	3401.0,
                     3414.0,	3404.0,	3417.0,	3407.0,	3397.0,	3410.0,	3400.0,
                     3390.0,	3403.0,	3393.0,	3383.0,	3396.0,	3386.0,	3376.0,
                     3389.0,	3379.0,	3369.0,	3382.0,	3372.0,	3385.0,	3398.0,
                     3388.0,	3401.0,	3414.0,	3404.0,	3417.0,	3407.0,	3397.0,
                     3410.0,	3400.0,	3390.0,	3403.0,	3393.0,	3383.0,	3396.0)
  expected <- array(expected_data, dim = c(7, 7, 3))
  
  expect_equivalent(actual, expected)
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
  expect_equal(actual, array(c(149028, 149040, 149064)))
})

nc_close(file)

########## input file does not exist #########
file_out <- tempfile_nc()
test_that("error is thrown if input file does not exist", {
  expect_error(
    daysum("SIS",
             file.path(data_dir, "xemaple1.nc"),
             file_out),
    "Input file does not exist")
})

########## input filename is empty #########
file_out <- tempfile_nc()
test_that("error is thrown if input filename is empty", {
  expect_error(
    daysum("SIS", "", file_out),
    "Input file does not exist")
})

########## input filename is NULL #########
file_out <- tempfile_nc()

test_that("error is thrown if input filename is NULL", {
  expect_error(
    daysum("SIS", NULL, file_out),
    "Input filepath must be of length one and not NULL"
  )
})

########## output file already exists #########
file_out <- tempfile_nc()
cat("test\n", file = file_out)

test_that("error is thrown if output file already exists", {
  expect_error(
    daysum("SIS",
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
    daysum("SIS",
             file.path(data_dir, "ex_dayx.nc"),
             file_out,
             overwrite = TRUE
    ),
    NA
  )
})

file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file)
  
  expected_data <- c(3106.0,	3118.0,	3107.0,	3119.0,	3131.0,	3120.0,	3132.0,
                     3144.0,	3133.0,	3145.0,	3157.0,	3146.0,	3158.0,	3147.0,
                     3136.0,	3148.0,	3137.0,	3126.0,	3138.0,	3127.0,	3116.0,
                     3128.0,	3117.0,	3106.0,	3118.0,	3107.0,	3119.0,	3131.0,
                     3120.0,	3132.0,	3144.0,	3133.0,	3145.0,	3157.0,	3146.0,
                     3158.0,	3147.0,	3136.0,	3148.0,	3137.0,	3126.0,	3138.0,
                     3127.0,	3116.0,	3128.0,	3117.0,	3106.0,	3118.0,	3107.0,
                     
                     6266.0,	6267.0,	6268.0,	6269.0,	6270.0,	6271.0,	6272.0,
                     6273.0,	6274.0,	6275.0,	6253.0,	6254.0,	6255.0,	6256.0,
                     6257.0,	6258.0,	6259.0,	6260.0,	6261.0,	6262.0,	6263.0,
                     6264.0,	6265.0,	6266.0,	6267.0,	6268.0,	6269.0,	6270.0,
                     6271.0,	6272.0,	6273.0,	6274.0,	6275.0,	6253.0,	6254.0,
                     6255.0,	6256.0,	6257.0,	6258.0,	6259.0,	6260.0,	6261.0,
                     6262.0,	6263.0,	6264.0,	6265.0,	6266.0,	6267.0,	6268.0,
                     
                     3393.0,	3383.0,	3396.0,	3386.0,	3376.0,	3389.0,	3379.0,
                     3369.0,	3382.0,	3372.0,	3385.0,	3398.0,	3388.0,	3401.0,
                     3414.0,	3404.0,	3417.0,	3407.0,	3397.0,	3410.0,	3400.0,
                     3390.0,	3403.0,	3393.0,	3383.0,	3396.0,	3386.0,	3376.0,
                     3389.0,	3379.0,	3369.0,	3382.0,	3372.0,	3385.0,	3398.0,
                     3388.0,	3401.0,	3414.0,	3404.0,	3417.0,	3407.0,	3397.0,
                     3410.0,	3400.0,	3390.0,	3403.0,	3393.0,	3383.0,	3396.0)
  expected <- array(expected_data, dim = c(7, 7, 3))
  
  expect_equivalent(actual, expected)
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
  expect_equal(actual, array(c(149028, 149040, 149064)))
})

nc_close(file)
