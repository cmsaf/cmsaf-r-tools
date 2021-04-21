data_dir <- file.path("..", "testdata")
tempfile_nc <- function() {
  tempfile_helper("ydrunsum_")
}

########## simple case ##########
file_out <- tempfile_nc()
ydrunsum("SIS", nts = 10,
        file.path(data_dir, "ex_yday.nc"),
        file_out)
file <- nc_open(file_out)

test_that("data is correct", {
  actual <- ncvar_get(file, "SIS")

  expect_equal(dim(actual), c(3, 5, 366))

  expected <- c(
    1256.0,1193.0,1130.0,
    1190.0,1250.0,1276.0,
    1213.0,1150.0,1210.0,
    1270.0,1296.0,1233.0,
    1170.0,1230.0,1290.0,
    
    1310.0,1247.0,1184.0,
    1244.0,1304.0,1330.0,
    1267.0,1204.0,1264.0,
    1324.0,1350.0,1287.0,
    1224.0,1284.0,1344.0,
    
    1364.0,1301.0,1115.0,
    1175.0,1235.0,1384.0,
    1321.0,1135.0,1195.0,
    1255.0,1404.0,1341.0,
    1155.0,1215.0,1275.0)
  expect_equivalent(actual[1:45], expected)

  expected <- c(
    1226.0,1286.0,1100.0,
    1160.0,1220.0,1246.0,
    1306.0,1120.0,1180.0,
    1240.0,1266.0,1326.0,
    1140.0,1200.0,1260.0,
    
    1280.0,1340.0,1154.0,
    1214.0,1274.0,1300.0,
    1360.0,1174.0,1234.0,
    1294.0,1320.0,1380.0,
    1194.0,1254.0,1314.0,
    
    1334.0,1394.0,1208.0,
    1145.0,1205.0,1354.0,
    1414.0,1228.0,1165.0,
    1225.0,1374.0,1434.0,
    1248.0,1185.0,1245.0)
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
  expect_equal(actual, "cmsaf::ydrunsum for variable SIS")

  global_attr <- ncatt_get(file, 0)
  expect_equal(length(global_attr), 1)

  actual <- names(global_attr[1])
  expect_equal(actual, "Info")

  actual <- global_attr[[1]]
  expect_equal(actual, "Created with the CM SAF R Toolbox.")
})
