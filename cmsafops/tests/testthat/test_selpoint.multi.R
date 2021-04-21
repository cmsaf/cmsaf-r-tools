fileNameA <- "Selpoint_A"
fileNameB <- "Selpoint_B"

data_dir <- file.path("..", "testdata")
output_dir <- tempdir()

########## test data ##########

expected_file_refA <- read.csv(file = file.path(data_dir, "SelpointRef_A.csv"),
                               header = TRUE,
                               sep = ",")
expected_file_refB <- read.csv(file = file.path(data_dir, "SelpointRef_B.csv"),
                               header = TRUE,
                               sep = ",")


ncmonexfile <- nc_open(file.path(data_dir, "ex_selpoint2.nc"))
selpointex <- ncvar_get(ncmonexfile, "SIS")

selpoint.multi(var = "SIS",
               file.path(data_dir, "ex_selpoint2.nc"),
               outpath = output_dir,
               lon1 = c(5.5, 6.5),
               lat1 = c(45.5, 46.5),
               station_names = c(fileNameA, fileNameB),
               format = "csv")

actual_file_A <- read.csv(file = file.path(output_dir, "Selpoint_A.csv"),
                          header = TRUE,
                          sep = ",")
actual_file_B <- read.csv(file = file.path(output_dir, "Selpoint_B.csv"),
                          header = TRUE,
                          sep = ",")


selpoint.multi(var = "SIS",
               file.path(data_dir, "ex_selpoint2.nc"),
               outpath = output_dir,
               lon1 = c(5.5, 6.5),
               lat1 = c(45.5, 46.5),
               station_names = c(fileNameA, fileNameB),
               format = "nc")


expected_file_refA_nc <- nc_open(file.path(data_dir, "SelpointRef_A.nc"))
expected_file_refB_nc <- nc_open(file.path(data_dir, "SelpointRef_B.nc"))

expected_A_nc <- ncvar_get(expected_file_refA_nc, "SIS")
expected_B_nc <- ncvar_get(expected_file_refB_nc, "SIS")


actual_file_A_nc <- nc_open(file.path(output_dir, "Selpoint_A.nc"))
actual_file_B_nc <- nc_open(file.path(output_dir, "Selpoint_B.nc"))

actual_A_nc <- ncvar_get(actual_file_A_nc, "SIS")
actual_B_nc <- ncvar_get(actual_file_B_nc, "SIS")


test_that("data is correct", {
  expect_true(identical(actual_A_nc, expected_A_nc))
  expect_true(identical(actual_B_nc, expected_B_nc))

  expect_true(identical(expected_file_refA, expected_file_refA))
  expect_true(identical(expected_file_refB, expected_file_refB))
})

nc_close(ncmonexfile)

nc_close(actual_file_A_nc)
nc_close(actual_file_B_nc)
nc_close(expected_file_refA_nc)
nc_close(expected_file_refB_nc)
