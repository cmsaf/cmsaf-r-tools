# tests/testthat/test_extract.level.R

library(testthat)
library(ncdf4)

# --- Helpers ---------------------------------------------------------------

# Robust testdata locator: tries several common locations
td_in <- function(fname) {
  candidates <- c(
    testthat::test_path("testdata", fname),        # tests/testthat/testdata/...
    testthat::test_path("..", "testdata", fname),  # tests/testdata/...
    system.file("testdata", fname, package = "cmsafops") # inst/testdata/... (if used)
  )
  hit <- candidates[file.exists(candidates)]
  if (length(hit) == 0) stop("Test data not found: ", fname)
  hit[[1]]
}

# Build a level-suffixed filename: "out.nc" -> "out_level1.nc"
lvl_path <- function(base, i) sub("\\.nc$", paste0("_level", i, ".nc"), base)

# Minimal local tempdir helper (no withr / no cleanup needed on CRAN)
local_tmpdir <- function() {
  td <- file.path(tempdir(), paste0("cmsafops-test-", as.integer(runif(1, 1, 1e9))))
  dir.create(td, recursive = TRUE, showWarnings = FALSE)
  td
}

# --- Tests -----------------------------------------------------------------

### Default input ###
test_that("default input produces correct file and data", {
  tmp <- local_tmpdir()
  infile  <- td_in("ex_extract.level.nc")
  outfile <- file.path(tmp, "extract_level_default.nc")
  
  cmsafops::extract.level("SIS", infile, outfile)
  
  f <- nc_open(outfile)
  on.exit(nc_close(f), add = TRUE)
  
  # Data
  actual <- ncvar_get(f)
  expected_data <- c(seq(250, 258), seq(254, 262))
  expected <- array(expected_data, dim = c(3, 3, 2))
  expect_equivalent(actual, expected)
  
  # Variable attributes
  expect_equal(ncatt_get(f, "SIS", "units")$value, "W m-2")
  expect_equal(ncatt_get(f, "SIS", "_FillValue")$value, -999)
  expect_equal(ncatt_get(f, "SIS", "standard_name")$value, "SIS_standard")
  expect_equal(ncatt_get(f, "SIS", "long_name")$value, "Surface Incoming Shortwave Radiation")
  expect_equal(ncatt_get(f, "SIS", "missing_value")$value, 0)
  
  # Coordinate attributes
  expect_equal(ncatt_get(f, "lon", "units")$value, "degrees_east")
  expect_equal(ncatt_get(f, "lon", "long_name")$value, "longitude")
  expect_equal(ncatt_get(f, "lon", "standard_name")$value, "longitude")
  expect_equal(ncatt_get(f, "lon", "axis")$value, "X")
  
  expect_equal(ncatt_get(f, "lat", "units")$value, "degrees_north")
  expect_equal(ncatt_get(f, "lat", "long_name")$value, "latitude")
  expect_equal(ncatt_get(f, "lat", "standard_name")$value, "latitude")
  expect_equal(ncatt_get(f, "lat", "axis")$value, "Y")
  
  expect_equal(ncatt_get(f, "time", "units")$value, "hours since 1983-01-01 00:00:00")
  expect_equal(ncatt_get(f, "time", "long_name")$value, "time")
  expect_equal(ncatt_get(f, "time", "standard_name")$value, "time")
  expect_equal(ncatt_get(f, "time", "calendar")$value, "standard")
  
  # Global attributes
  ga <- ncatt_get(f, 0)
  expect_equal(length(ga), 1L)
  expect_equal(names(ga[1]), "Info")
  expect_equal(ga[[1]], "Created with the CM SAF R Toolbox.")
  
  # Coordinate values
  expect_identical(ncvar_get(f, "lon"), array(seq(5, 6, 0.5)))
  expect_identical(ncvar_get(f, "lat"), array(seq(45, 46, 0.5)))
  expect_equal(ncvar_get(f, "time"), array(c(149016, 149760)))
})

### NetCDF version 4 ###
test_that("output ncdf version 4 works", {
  tmp <- local_tmpdir()
  infile  <- td_in("ex_extract.level.nc")
  outfile <- file.path(tmp, "extract_level_v4.nc")
  
  cmsafops::extract.level("SIS", infile, outfile, nc34 = 4)
  
  f <- nc_open(outfile)
  on.exit(nc_close(f), add = TRUE)
  
  actual <- ncvar_get(f)
  expected_data <- c(seq(250, 258), seq(254, 262))
  expected <- array(expected_data, dim = c(3, 3, 2))
  expect_equivalent(actual, expected)
  
  # A few attribute/coord checks are sufficient here
  expect_equal(ncatt_get(f, "SIS", "units")$value, "W m-2")
  expect_equal(ncatt_get(f, "lat", "axis")$value, "Y")
  expect_equal(ncvar_get(f, "time"), array(c(149016, 149760)))
})

### Wrong ncdf version -> error ###
test_that("ncdf version 7 errors", {
  tmp <- local_tmpdir()
  infile  <- td_in("ex_extract.level.nc")
  outfile <- file.path(tmp, "extract_level_v7.nc")
  
  expect_error(
    cmsafops::extract.level("SIS", infile, outfile, nc34 = 7),
    "nc version must be in c(3, 4), but was 7",
    fixed = TRUE
  )
})

### ncdf version NULL -> error ###
test_that("ncdf version NULL throws an error", {
  tmp <- local_tmpdir()
  infile  <- td_in("ex_extract.level.nc")
  outfile <- file.path(tmp, "extract_level_null.nc")
  
  expect_error(
    cmsafops::extract.level("SIS", infile, outfile, nc34 = NULL),
    "nc_version must not be NULL"
  )
})

### Variable does not exist -> warning + fallback ###
test_that("missing variable warns and falls back to SIS", {
  tmp <- local_tmpdir()
  infile  <- td_in("ex_extract.level.nc")
  outfile <- file.path(tmp, "extract_level_missing_var.nc")
  
  expect_warning(
    cmsafops::extract.level("notExist", infile, outfile),
    "Variable 'notExist' not found. Variable 'SIS' will be used instead."
  )
  
  f <- nc_open(outfile)
  on.exit(nc_close(f), add = TRUE)
  actual <- ncvar_get(f)
  expected_data <- c(seq(250, 258), seq(254, 262))
  expected <- array(expected_data, dim = c(3, 3, 2))
  expect_equivalent(actual, expected)
})

### Variable empty -> warning + fallback ###
test_that("empty variable warns and falls back to SIS", {
  tmp <- local_tmpdir()
  infile  <- td_in("ex_extract.level.nc")
  outfile <- file.path(tmp, "extract_level_empty_var.nc")
  
  expect_warning(
    cmsafops::extract.level("", infile, outfile),
    "Variable '' not found. Variable 'SIS' will be used instead."
  )
  
  f <- nc_open(outfile)
  on.exit(nc_close(f), add = TRUE)
  actual <- ncvar_get(f)
  expected_data <- c(seq(250, 258), seq(254, 262))
  expected <- array(expected_data, dim = c(3, 3, 2))
  expect_equivalent(actual, expected)
})

### Input file does not exist (intentionally missing) ###
test_that("error if input file does not exist", {
  tmp <- local_tmpdir()
  # Use a guaranteed-missing path (do not call td_in() here!)
  infile  <- file.path(tmp, "definitely_missing_example1.nc")
  outfile <- file.path(tmp, "extract_level_noinput.nc")
  
  expect_false(file.exists(infile))
  expect_error(
    cmsafops::extract.level("SIS", infile, outfile),
    "Input file does not exist"
  )
})

### Input filename empty ###
test_that("error if input filename is empty", {
  tmp <- local_tmpdir()
  outfile <- file.path(tmp, "extract_level_empty_in.nc")
  
  expect_error(
    cmsafops::extract.level("SIS", "", outfile),
    "Input file does not exist"
  )
})

### Input filename NULL ###
test_that("error if input filename is NULL", {
  tmp <- local_tmpdir()
  outfile <- file.path(tmp, "extract_level_null_in.nc")
  
  expect_error(
    cmsafops::extract.level("SIS", NULL, outfile),
    "Input filepath must be of length one and not NULL"
  )
})

### Outfile exists already ###
test_that("existing outfile errors unless overwrite = TRUE", {
  tmp <- local_tmpdir()
  infile  <- td_in("ex_extract.level.nc")
  outfile <- file.path(tmp, "extract_level_exists.nc")
  cat("test\n", file = outfile)
  
  expect_error(
    cmsafops::extract.level("SIS", infile, outfile),
    paste0("File '", outfile, "' already exists. Specify 'overwrite = TRUE' if you want to overwrite it."),
    fixed = TRUE
  )
  expect_equal(readLines(outfile), "test")
  
  expect_error(
    cmsafops::extract.level("SIS", infile, outfile, overwrite = TRUE),
    NA
  )
  
  f <- nc_open(outfile)
  on.exit(nc_close(f), add = TRUE)
  actual <- ncvar_get(f)
  expected_data <- c(seq(250, 258), seq(254, 262))
  expected <- array(expected_data, dim = c(3, 3, 2))
  expect_equivalent(actual, expected)
})

### Different level (level = 2) ###
test_that("level = 2 produces expected data", {
  tmp <- local_tmpdir()
  infile  <- td_in("ex_extract.level.nc")
  outfile <- file.path(tmp, "extract_level_l2.nc")
  
  cmsafops::extract.level("SIS", infile, outfile, level = 2)
  
  f <- nc_open(outfile)
  on.exit(nc_close(f), add = TRUE)
  
  actual <- ncvar_get(f)
  expected_data <- c(seq(259, 267), seq(263, 271))
  expected <- array(expected_data, dim = c(3, 3, 2))
  expect_equivalent(actual, expected)
})

### All levels (3x3x3x2) ###
test_that("level = 'all' creates three files (3x3x3x2)", {
  tmp <- local_tmpdir()
  infile  <- td_in("ex_extract.level.nc")
  outfile <- file.path(tmp, "extract_level_all.nc")
  
  cmsafops::extract.level("SIS", infile, outfile, level = "all")
  
  f1p <- lvl_path(outfile, 1)
  f2p <- lvl_path(outfile, 2)
  f3p <- lvl_path(outfile, 3)
  
  expect_true(file.exists(f1p))
  expect_true(file.exists(f2p))
  expect_true(file.exists(f3p))
  
  f1 <- nc_open(f1p); on.exit(nc_close(f1), add = TRUE)
  actual <- ncvar_get(f1)
  expected_data <- c(seq(250, 258), seq(254, 262))
  expected <- array(expected_data, dim = c(3, 3, 2))
  expect_equivalent(actual, expected)
  
  f2 <- nc_open(f2p); on.exit(nc_close(f2), add = TRUE)
  actual <- ncvar_get(f2)
  expected_data <- c(seq(259, 267), seq(263, 271))
  expected <- array(expected_data, dim = c(3, 3, 2))
  expect_equivalent(actual, expected)
  
  f3 <- nc_open(f3p); on.exit(nc_close(f3), add = TRUE)
  actual <- ncvar_get(f3)
  expected_data <- c(seq(268, 272), seq(250, 253), seq(272, 272), seq(250, 257))
  expected <- array(expected_data, dim = c(3, 3, 2))
  expect_equivalent(actual, expected)
  
  # Coordinate/attribute spot checks (f1)
  expect_identical(ncvar_get(f1, "lon"), array(seq(5, 6, 0.5)))
  expect_identical(ncvar_get(f1, "lat"), array(seq(45, 46, 0.5)))
  expect_equal(ncvar_get(f1, "time"), array(c(149016, 149760)))
})

### All levels (5x5x3x2) ###
test_that("level = 'all' creates three files (5x5x3x2)", {
  tmp <- local_tmpdir()
  infile  <- td_in("ex_extract.level2.nc")
  outfile <- file.path(tmp, "extract_level2_all.nc")
  
  cmsafops::extract.level("SIS", infile, outfile, level = "all")
  
  f1p <- lvl_path(outfile, 1)
  f2p <- lvl_path(outfile, 2)
  f3p <- lvl_path(outfile, 3)
  
  expect_true(file.exists(f1p))
  expect_true(file.exists(f2p))
  expect_true(file.exists(f3p))
  
  f1 <- nc_open(f1p); on.exit(nc_close(f1), add = TRUE)
  actual <- ncvar_get(f1)
  expected_data <- c(seq(250, 272), seq(250, 251), seq(256, 272), seq(250, 257))
  expected <- array(expected_data, dim = c(5, 5, 2))
  expect_equivalent(actual, expected)
  
  f2 <- nc_open(f2p); on.exit(nc_close(f2), add = TRUE)
  actual <- ncvar_get(f2)
  expected_data <- c(seq(252, 272), seq(250, 253), seq(258, 272), seq(250, 259))
  expected <- array(expected_data, dim = c(5, 5, 2))
  expect_equivalent(actual, expected)
  
  f3 <- nc_open(f3p); on.exit(nc_close(f3), add = TRUE)
  actual <- ncvar_get(f3)
  expected_data <- c(seq(254, 272), seq(250, 255), seq(260, 272), seq(250, 261))
  expected <- array(expected_data, dim = c(5, 5, 2))
  expect_equivalent(actual, expected)
  
  # Coordinate/attribute spot checks (f1)
  expect_identical(ncvar_get(f1, "lon"), array(seq(5, 7, 0.5)))
  expect_identical(ncvar_get(f1, "lat"), array(seq(45, 47, 0.5)))
  expect_equal(ncvar_get(f1, "time"), array(c(149016, 149760)))
})

test_that("level='all' writes into dirname(outfile)", {
  tmp <- file.path(tempdir(), paste0("cmsafops-", as.integer(runif(1,1,1e9))))
  dir.create(tmp, recursive=TRUE, showWarnings=FALSE)
  infile  <- td_in("ex_extract.level.nc")
  outfile <- file.path(tmp, "extract_level_all.nc")
  cmsafops::extract.level("SIS", infile, outfile, level = "all")
  expect_true(file.exists(file.path(tmp, "extract_level_all_level1.nc")))
  expect_true(file.exists(file.path(tmp, "extract_level_all_level2.nc")))
  expect_true(file.exists(file.path(tmp, "extract_level_all_level3.nc")))
})

