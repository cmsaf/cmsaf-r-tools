#'Extract levels from 4-dimensional NetCDF files.
#'
#'This function extracts one or all levels of a 4-dimensional NetCDF file. A
#'level is defined as a dimension, which does not correspond to longitude,
#'latitude or time. The user can choose either one specific level (given by an
#'integer) or all levels (level = "all").
#'
#'@param var Name of NetCDF variable (character).
#'@param infile Filename of input NetCDF file. This may include the directory
#'  (character).
#'@param outfile Filename of output NetCDF file. This may include the directory
#'  (character).
#'@param level Number of level (default = 1) or all levels (level = "all")
#'  (numeric or character).
#'@param nc34 NetCDF version of output file. If \code{nc34 = 3} the output file will be
#'  in NetCDFv3 format (numeric). Default output is NetCDFv4.
#'@param overwrite logical; should existing output file be overwritten?
#'@param verbose logical; if TRUE, progress messages are shown
#'
#'@return A NetCDF file including the selected level is written. In case of
#'  level = "all" all levels are written in separate NetCDF files and outfile
#'  names are expanded by "_level" and the level number.
#'@export
#'
#'@family selection and removal functions
#'
#' @examples
#'## Create an example NetCDF file with a similar structure as used by CM
#'## SAF. The file is created with the ncdf4 package.  Alternatively
#'## example data can be freely downloaded here: <https://wui.cmsaf.eu/>
#'
#'library(ncdf4)
#'
#'## create some (non-realistic) example data
#'
#'lon <- seq(5, 15, 0.5)
#'lat <- seq(45, 55, 0.5)
#'height <- seq(0, 1000, 100)
#'time <- seq(as.Date("2000-01-01"), as.Date("2010-12-31"), "month")
#'origin <- as.Date("1983-01-01 00:00:00")
#'time <- as.numeric(difftime(time, origin, units = "hour"))
#'data <- array(250:350, dim = c(21, 21, 11, 132))
#'
#'## create example NetCDF
#'
#'x <- ncdim_def(name = "lon", units = "degrees_east", vals = lon)
#'y <- ncdim_def(name = "lat", units = "degrees_north", vals = lat)
#'z <- ncdim_def(name = "height", units = "m", vals = height)
#'t <- ncdim_def(name = "time", units = "hours since 1983-01-01 00:00:00",
#'  vals = time, unlim = TRUE)
#'var1 <- ncvar_def("SIS", "W m-2", list(x, y, z, t), -1, prec = "short")
#'vars <- list(var1)
#'ncnew <- nc_create(file.path(tempdir(),"CMSAF_example_file.nc"), vars)
#'ncvar_put(ncnew, var1, data)
#'ncatt_put(ncnew, "lon", "standard_name", "longitude", prec = "text")
#'ncatt_put(ncnew, "lat", "standard_name", "latitude", prec = "text")
#'ncatt_put(ncnew, "height", "standard_name", "height", prec = "text")
#'nc_close(ncnew)
#'
#'## Extract the first level of the example CM SAF NetCDF file and write
#'## the output to a new file.
#'extract.level("SIS", file.path(tempdir(),"CMSAF_example_file.nc"),
#'  file.path(tempdir(),"CMSAF_example_file_extract.level1.nc"))
#'## Extract all levels of the example CM SAF NetCDF file and write the
#'## output to a new file.
#'extract.level(var = "SIS", infile = file.path(tempdir(),"CMSAF_example_file.nc"),
#'  outfile = file.path(tempdir(),"CMSAF_example_file_extract.level2.nc"),
#'  level = "all")
#'
#'unlink(c(file.path(tempdir(),"CMSAF_example_file.nc"),
#'  file.path(tempdir(),"CMSAF_example_file_extract.level1.nc"),
#'  file.path(tempdir(),"CMSAF_example_file_extract.level2_level[1-9].nc"),
#'  file.path(tempdir(),"CMSAF_example_file_extract.level2_level10.nc"),
#'  file.path(tempdir(),"CMSAF_example_file_extract.level2_level11.nc")))
extract.level <- function(var, infile, outfile, level = 1, nc34 = 4,
                          overwrite = FALSE, verbose = FALSE) {
  check_variable(var)

  check_infile(infile)
  check_outfile(outfile)

  outfile <- correct_filename(outfile)
  check_overwrite(outfile, overwrite)

  check_nc_version(nc34)

  calc_time_start <- Sys.time()

  # get information about dimensions and attributes
  file_data <- read_file(infile, var)

  nc_in <- nc_open(infile)
    # check level
    if (length(names(nc_in$dim) == 4)) {
      start <- c(1, 1, 1, 1)
      count <- c(-1, -1, -1, -1)
      # identify level dimension
      dummy <- match(names(nc_in$dim), c(TIME_NAMES$DEFAULT, LON_NAMES$DEFAULT,
                                         LAT_NAMES$DEFAULT))
      leveldim <- which(is.na(dummy))
      levellen <- nc_in$dim[[leveldim]]$len

      if (level != "all") {
        if (level > levellen) {
          stop(paste0("Dimension ", nc_in$dim[[leveldim]]$name, " has length: ",
                      levellen))
        }
        loop <- 1
        start[leveldim] <- level
        count[leveldim] <- 1
        result1 <- ncvar_get(nc_in, file_data$variable$name, start = start,
                             count = count)
      } else {
        loop <- levellen
        result1 <- ncvar_get(nc_in, file_data$variable$name, start = start,
                             count = count)
      }
    }
  nc_close(nc_in)

  # create netcdf
  nc_format <- get_nc_version(nc34)
  cmsaf_info <- (paste0("cmsaf::extract.level for variable ",
                        file_data$variable$name,
                        " and level ",
                        level))

  ##### prepare output #####
  global_att_list <- names(file_data$global_att)
  global_att_list <- global_att_list[toupper(global_att_list) %in% toupper(GLOBAL_ATT_DEFAULT)]
  global_attributes <- file_data$global_att[global_att_list]

  for (i in seq_len(loop)) {
    if (level != "all") {
      outfile1 <- outfile
      data1 <- result1
    } else {
      outfile1 <- paste0(strsplit(outfile, split = ".nc"), "_level", i, ".nc")
      if (length(dim(result1)) == 3) {
        data1 <- switch(leveldim,
               result1[i, , ],
               result1[, i, ],
               result1[, , i]
               )
      }else if (length(dim(result1)) == 4) {
          data1 <- switch(leveldim,
                          result1[i, , , ],
                          result1[, i, , ],
                          result1[, , i, ],
                          result1[, , , i]
          )
          }
    }

    # if (length(time1)==1) {
    #   dummy <- array(NA,dim=c(dim(data1)[1],dim(data1)[2],1))
    #   dummy[,,1] <- data1
    #   data1 <- dummy
    # }

    data1[is.na(data1)] <- file_data$variable$attributes$missing_value
    result <- data1

    if (file_data$time_info$has_time_bnds) {
      time_bnds <- get_time_bounds_from_file(infile)
      vars_data <- list(result = result, time_bounds = time_bnds)
    }else{
      vars_data <- list(result = result)
    }

    dims <- define_dims(file_data$grid$is_regular,
                        file_data$dimension_data$x,
                        file_data$dimension_data$y,
                        file_data$dimension_data$t,
                        NB2,
                        file_data$time_info$units,
                        with_time_bnds = file_data$time_info$has_time_bnds)

    vars <- define_vars(file_data$variable, dims, nc_format$compression, with_time_bnds = file_data$time_info$has_time_bnds)

    write_output_file(
      outfile1,
      nc_format$force_v4,
      vars,
      vars_data,
      file_data$variable$name,
      file_data$grid$vars, file_data$grid$vars_data,
      cmsaf_info,
      file_data$time_info$calendar,
      file_data$variable$attributes,
      global_attributes,
      with_time_bnds = file_data$time_info$has_time_bnds
    )
  } # end for loop

  calc_time_end <- Sys.time()
  if (verbose) message(get_processing_time_string(calc_time_start, calc_time_end))
}
