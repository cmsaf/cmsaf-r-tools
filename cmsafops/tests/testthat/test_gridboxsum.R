data_dir <- file.path("..", "testdata")
tempfile_nc <- function() {
  tempfile_helper("gridboxsum_")
}

########## output ncdf version 3 ##########
file_out <- tempfile_nc()
gridboxsum("SIS", 3, 6, file.path(data_dir, "ex_gridboxx.nc"), file_out)
file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file)
  
  expected_data <- c(5160,5214,5268,5322,5376,5329,5181,
                     5307,5361,5415,5469,5321,5274,5328,
                     5454,5508,5562,5313,5367,5421,5475,
                     2820,2746,2571,2598,2625,2652,2679,
                     
                     5321,5274,5328,5382,5436,5490,5544,
                     5367,5421,5475,5529,5583,5637,5388,
                     5514,5568,5622,5474,5326,5178,5232,
                     2547,2574,2601,2628,2655,2682,2709,
                     
                     5583,5637,5388,5442,5496,5550,5604,
                     5326,5178,5232,5286,5340,5394,5246,
                     5271,5325,5379,5433,5487,5238,5292,
                     2880,2907,2934,2860,2685,2712,2739)
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
  expect_equal(actual, "cmsaf::gridboxsum for variable SIS")
  
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