data_dir <- file.path("..", "testdata")
tempfile_nc <- function() {
  tempfile_helper("ydaysum_")
}

########## simple case ##########
file_out <- tempfile_nc()
ydaysum("SIS",
        file.path(data_dir, "ex_yday.nc"),
        file_out)
file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file, "SIS")
  
  expect_equal(dim(actual), c(3, 5, 366))
  
  expected <- c(
    89.0,95.0,101.0,
    107.0,113.0,91.0,
    97.0,103.0,109.0,
    115.0,93.0,99.0,
    105.0,111.0,117.0,
    
    119.0,125.0,131.0,
    137.0,143.0,121.0,
    127.0,133.0,139.0,
    145.0,123.0,129.0,
    135.0,141.0,147.0,
    
    149.0,155.0,161.0,
    167.0,173.0,151.0,
    157.0,163.0,169.0,
    175.0,153.0,159.0,
    165.0,171.0,177.0)
  expect_equivalent(actual[1:45], expected)
  
  expected <- c(
    86.0,92.0,98.0,
    104.0,110.0,88.0,
    94.0,100.0,106.0,
    112.0,90.0,96.0,
    102.0,108.0,114.0,
    
    116.0,122.0,128.0,
    134.0,140.0,118.0,
    124.0,130.0,136.0,
    142.0,120.0,126.0,
    132.0,138.0,144.0,
    
    146.0,152.0,158.0,
    164.0,170.0,148.0,
    154.0,160.0,166.0,
    172.0,150.0,156.0,
    162.0,168.0,174.0)
  expect_equivalent(actual[61:105], expected)
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
  expect_equal(actual, "cmsaf::ydaysum for variable SIS")
  
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
  expect_warning(ydaysum("someVariable",
                         file.path(data_dir, "ex_yday.nc"),
                         file_out),
                 "Variable 'someVariable' not found. Variable 'SIS' will be used instead.")
})

########## var is empty #########
test_that("no error is thrown if var is empty", {
  file_out <- tempfile_nc()
  expect_warning(
    ydaysum("",
            file.path(data_dir, "ex_yday.nc"),
            file_out),
    "Variable '' not found. Variable 'SIS' will be used instead.")
})

########## var is NULL #########
test_that("error is thrown if var is NULL", {
  file_out <- tempfile_nc()
  expect_error(
    ydaysum(NULL,
            file.path(data_dir, "ex_yday.nc"),
            file_out),
    "variable must not be NULL"
  )
})

########## input file does not exist #########
test_that("no error is thrown if input file does not exist", {
  file_out <- tempfile_nc()
  expect_error(
    ydaysum("SIS",
            file.path(data_dir, "ex_doesNotExist.nc"),
            file_out),
    "Input file does not exist")
})

########## input filename is NULL #########
test_that("error is thrown if input file is NULL", {
  file_out <- tempfile_nc()
  expect_error(
    ydaysum("SIS",
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
    ydaysum("SIS",
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
    ydaysum("SIS",
            file.path(data_dir, "ex_yday.nc"),
            file_out,
            overwrite = TRUE),
    NA
  )
})

########## output file is NULL #########
test_that("no error is thrown if output file already exists", {
  expect_error(
    ydaysum("SIS",
            file.path(data_dir, "ex_yday.nc"),
            NULL),
    "Output filepath must be of length one and not NULL"
  )
})

#TODO add test case where input data is weird somehow
