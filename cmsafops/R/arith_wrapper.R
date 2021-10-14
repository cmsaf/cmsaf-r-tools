# Wrapper for arithmetics
#
# Wrapper function for basic arithmetic functions (cmsaf.add, cmsaf.div,
# cmsaf.mul, cmsaf.sub). Argument op is one of 1-4, depending on the arithmetic
# function used.
arith_wrapper <- function(op, var1, var2, infile1, infile2, outfile, nc34,
                          overwrite, verbose, nc1 = NULL, nc2 = NULL) {
  if (is.null(nc1)) check_variable(var1)
  if (is.null(nc2)) check_variable(var2)

  check_infile(infile1)
  check_infile(infile2)
  check_outfile(outfile)

  outfile <- correct_filename(outfile)
  check_overwrite(outfile, overwrite)

  check_nc_version(nc34)

  calc_time_start <- Sys.time()

  # get information about dimensions and attributes
  file_data1 <- read_file(infile1, var1, nc = nc1)
  file_data2 <- read_file(infile2, var2, nc = nc2)

  # define variable precision
  if (!op == 4 && exists(file_data1$variable$prec) && exists(file_data2$variable$prec)) {
    prec <- PRECISIONS_VAR
    file_data1$variable$prec <- prec[max(c(which(file_data1$variable$prec == prec),
                                           which(file_data2$variable$prec == prec)))]
  } else {
    file_data1$variable$prec <- "float"
  }

  # check dimensions of infile1 and infile2
  if (file_data1$grid$is_regular != file_data2$grid$is_regular) {
    stop("Please use infiles with the same projection ore use the remap operator")
  }

  if (!(length(file_data1$dimension_data$x) == length(file_data2$dimension_data$x) &&
        length(file_data1$dimension_data$y) == length(file_data2$dimension_data$y))) {
    stop("Dimensions of infiles do not match!")
  }

  result <- array(file_data1$variable$attributes$missing_value,
                  dim = c(length(file_data1$dimension_data$x),
                          length(file_data1$dimension_data$y),
                          1))

  vars_data <- list(result = result)

  if (length(file_data1$dimension_data$t) > 1 && length(file_data2$dimension_data$t) > 1 &&
      length(file_data1$dimension_data$t) != length(file_data2$dimension_data$t)) {
    stop("Uncompatible time lengths!")
  }

  time_len1 <- length(file_data1$dimension_data$t)
  time_len2 <- length(file_data2$dimension_data$t)

  if (time_len1 == time_len2 && time_len1 > 1) {
    case <- 1
    time <- file_data1$dimension_data$t
  }
  if (time_len1 == 1 && time_len2 == 1) {
    case <- 2
    time <- file_data1$dimension_data$t
  }
  if (time_len1 == 1 && time_len2 > 1) {
    case <- 3
    time <- file_data2$dimension_data$t
  }
  if (time_len1 > 1 && time_len2 == 1) {
    case <- 4
    time <- file_data1$dimension_data$t
  }
  time_len <- length(time)

  # create netcdf
  nc_format <- get_nc_version(nc34)

  cmsaf_info <- switch(op,
                       paste0("cmsaf::cmsaf.add for variable ", var1),
                       paste0("cmsaf::cmsaf.sub for variable ", var1),
                       paste0("cmsaf::cmsaf.mul for variable ", var1),
                       paste0("cmsaf::cmsaf.div for variable ", var1)
  )

  ##### prepare output #####
  global_att_list <- names(file_data1$global_att)
  global_att_list <- global_att_list[toupper(global_att_list) %in% toupper(GLOBAL_ATT_DEFAULT)]
  global_attributes <- file_data1$global_att[global_att_list]

  dims <- define_dims(file_data1$grid$is_regular,
                      file_data1$dimension_data$x,
                      file_data1$dimension_data$y,
                      0,
                      NB2,
                      file_data1$time_info$units,
                      with_time_bnds = FALSE
  )

  vars <- define_vars(file_data1$variable, dims, nc_format$compression, with_time_bnds = FALSE)

  write_output_file(
    outfile,
    nc_format$force_v4,
    vars,
    vars_data,
    file_data1$variable$name,
    file_data1$grid$vars, file_data1$grid$vars_data,
    cmsaf_info,
    file_data1$time_info$calendar,
    file_data1$variable$attributes,
    global_attributes,
    with_time_bnds = FALSE
  )

  # get data of infile1 and infile2 and calculate corresponding fields
  if (!is.null(nc1)) nc_in1 <- nc1
  else nc_in1 <- nc_open(infile1)
  if (!is.null(nc2)) nc_in2 <- nc2
  else nc_in2 <- nc_open(infile2)
  nc_out <- nc_open(outfile, write = TRUE)

  switch(case,
         {
           for (i in seq_len(time_len)) {
             dum_dat1 <- ncvar_get(nc_in1, file_data1$variable$name,
                                   start = c(1, 1, i), count = c(-1, -1, 1))
             dum_dat2 <- ncvar_get(nc_in2, file_data2$variable$name,
                                   start = c(1, 1, i), count = c(-1, -1, 1))
             dum_data <- switch(
               op,
               dum_dat1 + dum_dat2,
               dum_dat1 - dum_dat2,
               dum_dat1 * dum_dat2,
               {
                 dum_dat1[dum_dat1 == 0] <- NA
                 dum_dat2[dum_dat2 == 0] <- NA
                 dum_dat1 / dum_dat2
               }
             )
             dum_data[is.na(dum_data)] <- file_data1$variable$attributes$missing_value
             ncvar_put(nc_out, vars[[1]], dum_data, start = c(1, 1, i),
                       count = c(-1, -1, 1))
             ncvar_put(nc_out, dims$t, time[i], start = i, count = 1)
           }
         },
         {
           dum_dat1 <- ncvar_get(nc_in1, file_data1$variable$name,
                                 start = c(1, 1, 1), count = c(-1, -1, 1))
           dum_dat2 <- ncvar_get(nc_in2, file_data2$variable$name,
                                 start = c(1, 1, 1), count = c(-1, -1, 1))
           dum_data <- switch(
             op,
             dum_dat1 + dum_dat2,
             dum_dat1 - dum_dat2,
             dum_dat1 * dum_dat2,
             {
               dum_dat1[dum_dat1 == 0] <- NA
               dum_dat2[dum_dat2 == 0] <- NA
               dum_dat1 / dum_dat2
             }
           )
           dum_data[is.na(dum_data)] <- file_data1$variable$attributes$missing_value
           ncvar_put(nc_out, vars[[1]], dum_data, start = c(1, 1, 1),
                     count = c(-1, -1, 1))
           ncvar_put(nc_out, dims$t, time[1], start = 1, count = 1)
         },
         {
           dum_dat1 <- ncvar_get(nc_in1, file_data1$variable$name,
                                 start = c(1, 1, 1), count = c(-1, -1, 1))
           for (i in seq_len(time_len)) {
             dum_dat2 <- ncvar_get(nc_in2, file_data2$variable$name,
                                   start = c(1, 1, i), count = c(-1, -1, 1))
             dum_data <- switch(
               op,
               dum_dat1 + dum_dat2,
               dum_dat1 - dum_dat2,
               dum_dat1 * dum_dat2,
               {
                 dum_dat1[dum_dat1 == 0] <- NA
                 dum_dat2[dum_dat2 == 0] <- NA
                 dum_dat1 / dum_dat2
               }
             )
             dum_data[is.na(dum_data)] <- file_data1$variable$attributes$missing_value
             ncvar_put(nc_out, vars[[1]], dum_data, start = c(1, 1, i),
                       count = c(-1, -1, 1))
             ncvar_put(nc_out, dims$t, time[i], start = i, count = 1)
           }
         },
         {
           dum_dat2 <- ncvar_get(nc_in2, file_data2$variable$name,
                                 start = c(1, 1, 1), count = c(-1, -1, 1))
           for (i in seq_len(time_len)) {
             dum_dat1 <- ncvar_get(nc_in1, file_data1$variable$name,
                                   start = c(1, 1, i), count = c(-1, -1, 1))
             dum_data <- switch(
               op,
               dum_dat1 + dum_dat2,
               dum_dat1 - dum_dat2,
               dum_dat1 * dum_dat2,
               {
                 dum_dat1[dum_dat1 == 0] <- NA
                 dum_dat2[dum_dat2 == 0] <- NA
                 dum_dat1 / dum_dat2
               }
             )
             dum_data[is.na(dum_data)] <- file_data1$variable$attributes$missing_value
             ncvar_put(nc_out, vars[[1]], dum_data, start = c(1, 1, i), count = c(-1, -1, 1))
             ncvar_put(nc_out, dims$t, time[i], start = i, count = 1)
           }
         }
  )

  if (is.null(nc1)) nc_close(nc_in1)
  if (is.null(nc2)) nc_close(nc_in2)
  nc_close(nc_out)

  calc_time_end <- Sys.time()
  if (verbose) message(get_processing_time_string(calc_time_start, calc_time_end))
}
