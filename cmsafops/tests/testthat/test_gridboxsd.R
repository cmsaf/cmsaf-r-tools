data_dir <- file.path("..", "testdata")
tempfile_nc <- function() {
  tempfile_helper("gridboxsd_")
}

########## output ncdf version 3 ##########
file_out <- tempfile_nc()
gridboxsd("SIS", 3, 6, file.path(data_dir, "ex_gridboxx.nc"), file_out)
file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file)
  
  expected_data <- c(31.484823,31.484823,31.484823,31.484823,31.484823,31.174032,27.721832,
                     27.721832,27.721832,27.721832,27.721832,27.81692,26.348345,26.348345,
                     26.348345,26.348345,26.348345,27.721832,27.721832,27.721832,27.721832,
                     35.93049,39.35875,35.93049,35.93049,35.93049,35.93049,35.93049,
                     
                     27.81692,26.348345,26.348345,26.348345,26.348345,26.348345,26.348345,
                     27.721832,27.721832,27.721832,27.721832,27.721832,27.721832,31.484823,
                     31.484823,31.484823,31.484823,34.620445,34.620445,31.484823,31.484823,
                     18.207142,18.207142,18.207142,18.207142,18.207142,18.207142,18.207142,
                     
                     27.721832,27.721832,31.484823,31.484823,31.484823,31.484823,31.484823,
                     34.620445,31.484823,31.484823,31.484823,31.484823,31.484823,29.959232,
                     27.721832,27.721832,27.721832,27.721832,27.721832,26.348345,26.348345,
                     18.207142,18.207142,18.207142,30.156996,35.93049,35.93049,35.93049)
  expected <- array(expected_data, dim = c(4, 7, 3))
  
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
  expect_equal(actual, "cmsaf::gridboxsd for variable SIS")
  
  global_attr <- ncatt_get(file, 0)
  expect_equal(length(global_attr), 1)
  
  actual <- names(global_attr[1])
  expect_equal(actual, "Info")
  
  actual <- global_attr[[1]]
  expect_equal(actual, "Created with the CM SAF R Toolbox.")
})

test_that("coordinates are correct", {
  actual <- ncvar_get(file, "lon")
  expect_identical(actual, array(c(5.5, 7.0, 8.5, 10.0, 11.5, 13.0, 14.5)))
  
  actual <- ncvar_get(file, "lat")
  expect_identical(actual, array(c(46.25, 49.25, 52.25, 54.5)))
  
  actual <- ncvar_get(file, "time")
  expect_equal(actual, array(c(150456, 151200, 151920)))
})

nc_close(file)