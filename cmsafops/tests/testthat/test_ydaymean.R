data_dir <- file.path("..", "testdata")
tempfile_nc <- function() {
  tempfile_helper("ydaymean_")
}

########## simple case ##########
file_out <- tempfile_nc()
ydaymean("SIS",
         file.path(data_dir, "ex_yday.nc"),
         file_out)
file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file, "SIS")

  expect_equal(dim(actual), c(3, 5, 366))

  expected <- c(
    50.5, 53.5, 42.5, 45.5, 48.5, 51.5, 54.5, 43.5, 46.5, 49.5, 52.5, 55.5,
    56.5, 59.5, 62.5, 65.5, 68.5, 57.5, 60.5, 63.5, 66.5, 69.5, 58.5, 61.5,
    64.5, 67.5, 70.5, 71.5, 74.5, 77.5, 80.5, 83.5, 72.5, 75.5, 78.5, 81.5,
    84.5, 73.5, 76.5, 79.5, 82.5, 85.5, 86.5, 89.5, 31, 34, 37, 87.5, 90.5,
    32, 35, 38, 88.5, 91.5, 33, 36, 39, 40, 43, 46, 49, 52, 41, 44, 47, 50,
    53, 42
  )
  expect_equivalent(actual[124:191], expected)

  expected <- c(
    70, 73, 76, 65, 68, 71, 74, 77, 66, 69, 72, 75, 78, 79, 82, 85, 88, 91,
    80, 83, 86, 89, 92, 81, 84, 87, 90, 93, 32.5, 35.5, 38.5, 41.5, 44.5, 33.5,
    36.5, 39.5, 42.5, 45.5, 34.5, 37.5, 40.5, 43.5, 46.5, 47.5, 50.5, 53.5
  )
  expect_equivalent(actual[1023:1068], expected)

  expected <- c(
    74.5, 63.5, 66.5, 69.5, 72.5, 75.5, 64.5, 67.5, 70.5, 73.5, 76.5, 77.5,
    80.5, 83.5, 86.5, 89.5, 78.5, 81.5, 84.5, 87.5, 90.5, 79.5, 82.5, 85.5,
    88.5, 91.5, 31, 34, 37, 40, 43, 32, 35, 38, 41, 44, 33, 36, 39, 42, 45,
    46, 49, 52, 55, 58, 47, 50, 53, 56, 59, 48, 51, 54, 57, 60, 61, 64, 67,
    70, 73, 62, 65, 68, 71, 74, 63, 66, 69, 72, 75, 76, 79, 82, 85, 88, 77,
    80, 83, 86, 89, 78, 81, 84
  )
  expect_equivalent(actual[5390:5473], expected)
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
  expect_equal(actual, "cmsaf::ydaymean for variable SIS")

  global_attr <- ncatt_get(file, 0)
  expect_equal(length(global_attr), 1)

  actual <- names(global_attr[1])
  expect_equal(actual, "Info")

  actual <- global_attr[[1]]
  expect_equal(actual, "Created with the CM SAF R Toolbox.")
})

test_that("coordinates are correct", {
  actual <- ncvar_get(file, "lon")
  expect_identical(actual, array(seq(5, 6, by = 0.5)))

  actual <- ncvar_get(file, "lat")
  expect_identical(actual, array(seq(45, 47, by = 0.5)))

  actual <- ncvar_get(file, "time")
  expect_equal(actual, array(c(
    157800, 157824, 157848, 157872, 157896, 157920, 157944, 157968, 157992,
    158016, 158040, 158064, 158088, 158112, 158136, 158160, 158184, 158208,
    158232, 158256, 158280, 158304, 158328, 158352, 158376, 158400, 158424,
    158448, 158472, 149712, 149736, 149760, 149784, 149808, 149832, 149856,
    149880, 149904, 149928, 149952, 149976, 150000, 150024, 150048, 150072,
    150096, 150120, 150144, 150168, 150192, 150216, 150240, 150264, 150288,
    150312, 150336, 150360, 150384, 150408, 150432, 150456, 150480, 150504,
    150528, 150552, 150576, 150600, 150624, 150648, 150672, 150696, 150720,
    150744, 150768, 150792, 150816, 150840, 150864, 150888, 150912, 150936,
    150960, 150984, 151008, 151032, 151056, 151080, 151104, 151128, 151152,
    151176, 151200, 151224, 151248, 151272, 151296, 151320, 151344, 151368,
    151392, 151416, 151440, 151464, 151488, 151512, 151536, 151560, 151584,
    151608, 151632, 151656, 151680, 151704, 151728, 151752, 151776, 151800,
    151824, 151848, 151872, 151896, 151920, 151944, 151968, 151992, 152016,
    152040, 152064, 152088, 152112, 152136, 152160, 152184, 152208, 152232,
    152256, 152280, 152304, 152328, 152352, 152376, 152400, 152424, 152448,
    152472, 152496, 152520, 152544, 152568, 152592, 152616, 152640, 152664,
    152688, 152712, 152736, 152760, 152784, 152808, 152832, 152856, 152880,
    152904, 152928, 152952, 152976, 153000, 153024, 153048, 153072, 153096,
    153120, 153144, 153168, 153192, 153216, 153240, 153264, 153288, 153312,
    153336, 153360, 153384, 153408, 153432, 153456, 153480, 153504, 153528,
    153552, 153576, 153600, 153624, 153648, 153672, 153696, 153720, 153744,
    153768, 153792, 153816, 153840, 153864, 153888, 153912, 153936, 153960,
    153984, 154008, 154032, 154056, 154080, 154104, 154128, 154152, 154176,
    154200, 154224, 154248, 154272, 154296, 154320, 154344, 154368, 154392,
    154416, 154440, 154464, 154488, 154512, 154536, 154560, 154584, 154608,
    154632, 154656, 154680, 154704, 154728, 154752, 154776, 154800, 154824,
    154848, 154872, 154896, 154920, 154944, 154968, 154992, 155016, 155040,
    155064, 155088, 155112, 155136, 155160, 155184, 155208, 155232, 155256,
    155280, 155304, 155328, 155352, 155376, 155400, 155424, 155448, 155472,
    155496, 155520, 155544, 155568, 155592, 155616, 155640, 155664, 155688,
    155712, 155736, 155760, 155784, 155808, 155832, 155856, 155880, 155904,
    155928, 155952, 155976, 156000, 156024, 156048, 156072, 156096, 156120,
    156144, 156168, 156192, 156216, 156240, 156264, 156288, 156312, 156336,
    156360, 156384, 156408, 156432, 156456, 156480, 156504, 156528, 156552,
    156576, 156600, 156624, 156648, 156672, 156696, 156720, 156744, 156768,
    156792, 156816, 156840, 156864, 156888, 156912, 156936, 156960, 156984,
    157008, 157032, 157056, 157080, 157104, 157128, 157152, 157176, 157200,
    157224, 157248, 157272, 157296, 157320, 157344, 157368, 157392, 157416,
    157440, 157464, 157488, 157512, 157536, 157560, 157584, 157608, 157632,
    157656, 157680, 157704, 157728, 157752, 157776
  )))
})

nc_close(file)

########## var does not exist #########
test_that("no error is thrown if var does not exist", {
  file_out <- tempfile_nc()
  expect_warning(ydaymean("someVariable",
                          file.path(data_dir, "ex_yday.nc"),
                          file_out),
                 "Variable 'someVariable' not found. Variable 'SIS' will be used instead.")
})

########## var is empty #########
test_that("no error is thrown if var is empty", {
  file_out <- tempfile_nc()
  expect_warning(
    ydaymean("",
             file.path(data_dir, "ex_yday.nc"),
             file_out),
    "Variable '' not found. Variable 'SIS' will be used instead.")
})

########## var is NULL #########
test_that("error is thrown if var is NULL", {
  file_out <- tempfile_nc()
  expect_error(
    ydaymean(NULL,
             file.path(data_dir, "ex_yday.nc"),
             file_out),
    "variable must not be NULL"
  )
})

########## input file does not exist #########
test_that("no error is thrown if input file does not exist", {
  file_out <- tempfile_nc()
  expect_error(
    ydaymean("SIS",
             file.path(data_dir, "ex_doesNotExist.nc"),
             file_out),
    "Input file does not exist")
})

########## input filename is NULL #########
test_that("error is thrown if input file is NULL", {
  file_out <- tempfile_nc()
  expect_error(
    ydaymean("SIS",
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
    ydaymean("SIS",
             file.path(data_dir, "ex_yday.nc"),
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
    ydaymean("SIS",
             file.path(data_dir, "ex_yday.nc"),
             file_out,
             overwrite = TRUE),
    NA
  )
})

########## output file is NULL #########
test_that("no error is thrown if output file already exists", {
  expect_error(
    ydaymean("SIS",
             file.path(data_dir, "ex_yday.nc"),
             NULL),
    "Output filepath must be of length one and not NULL"
  )
})

#TODO add test case where input data is weird somehow
