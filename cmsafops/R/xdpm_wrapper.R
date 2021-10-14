xdpm_wrapper <- function(op, var, infile, outfile, nc34,
                         overwrite, verbose, nc = NULL) {
  check_variable(var)

  if (is.null(nc)) check_infile(infile)
  check_outfile(outfile)

  outfile <- correct_filename(outfile)
  check_overwrite(outfile, overwrite)

  check_nc_version(nc34)

  calc_time_start <- Sys.time()

  # get information about dimensions and attributes
  file_data <- read_file(infile, var, nc = nc)
  file_data$variable$prec <- "float"

  # extract time information
  date_time <- get_date_time(file_data$dimension_data$t, file_data$time_info$units)
  mon <- date_time$months
  years <- date_time$years

  result <- array(file_data$variable$attributes$missing_value,
                  dim = c(length(file_data$dimension_data$x),
                          length(file_data$dimension_data$y),
                          1))
  time_bnds <- array(NA, dim = c(2, 1))

  vars_data <- list(result = result, time_bounds = time_bnds)

  # create netcdf
  nc_format <- get_nc_version(nc34)

  cmsaf_info <- switch(op,
                       "div" = {"cmsaf::divdpm"},
                       "mul" = {"cmsaf::muldpm"}
  )
  cmsaf_info <- paste0(cmsaf_info, " for variable ", file_data$variable$name)

  # prepare global attributes
  global_att_list <- names(file_data$global_att)
  global_att_list <- global_att_list[toupper(global_att_list) %in% toupper(GLOBAL_ATT_DEFAULT)]
  global_attributes <- file_data$global_att[global_att_list]

  dims <- define_dims(file_data$grid$is_regular,
                      file_data$dimension_data$x,
                      file_data$dimension_data$y,
                      0,
                      NB2,
                      file_data$time_info$units,
                      with_time_bnds = file_data$time_info$has_time_bnds
  )

  vars <- define_vars(file_data$variable, dims, nc_format$compression,
                      with_time_bnds = file_data$time_info$has_time_bnds)

  write_output_file(
    outfile,
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

  # divide each timestep by number of days per month
  if (file_data$time_info$has_time_bnds) {
    time_bnds <- get_time_bounds_from_file(infile, nc = nc)
  }
  if (!is.null(nc)) nc_in <- nc
  else nc_in <- nc_open(infile)
  nc_out <- nc_open(outfile, write = TRUE)

  count <- 1
  for (i in seq_len(length(file_data$dimension_data$t))) {
    dpm <- (switch(mon[i],
                   "01" = 31,
                   "02" = 28 + is_leap_year(years[i]),
                   "03" = 31,
                   "04" = 30,
                   "05" = 31,
                   "06" = 30,
                   "07" = 31,
                   "08" = 31,
                   "09" = 30,
                   "10" = 31,
                   "11" = 30,
                   "12" = 31))

    dum_dat <- ncvar_get(nc_in, file_data$variable$name, start = c(1, 1, i),
                         count = c(-1, -1, 1))
    switch(op,
           "mul" = {dum_dat <- dum_dat * dpm},
           "div" = {
             if (dpm != 0) {
               dum_dat <- dum_dat / dpm
             }
           }
    )
    dum_dat[is.na(dum_dat)] <- file_data$variable$attributes$missing_value
    ncvar_put(nc_out, vars[[1]], dum_dat, start = c(1, 1, count),
              count = c(-1, -1, 1))
    ncvar_put(nc_out, dims$t, file_data$dimension_data$t[i], start = count,
              count = 1)
    if (file_data$time_info$has_time_bnds) {
      ncvar_put(nc_out, vars[[2]], time_bnds[, i], start = c(1, count),
                count = c(-1, 1))
    }
    count <- count + 1
  }
  if (is.null(nc)) nc_close(nc_in)
  nc_close(nc_out)

  calc_time_end <- Sys.time()
  if (verbose) message(get_processing_time_string(calc_time_start, calc_time_end))
}
