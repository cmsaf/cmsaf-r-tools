yseasx_wrapper <- function(op, var, infile, outfile, nc34, overwrite, verbose) {
  calc_time_start <- Sys.time()

  check_variable(var)
  check_infile(infile)
  check_outfile(outfile)
  outfile <- correct_filename(outfile)
  check_overwrite(outfile, overwrite)
  check_nc_version(nc34)

  ##### extract data from file #####
  file_data <- read_file(infile, var)
  if (op > 2) {
    file_data$variable$prec <- "float"
  }
  date_time <- get_date_time(file_data$dimension_data$t, file_data$time_info$units)
  months_all <- date_time$months
  months_unique <- sort(unique(months_all))
  years_all <- date_time$years
  years_unique <- sort(unique(years_all))
  seasons <- set_seasons(years_unique, years_all, months_all)

  # Use placeholder for result so that it can be calculated later without the
  # need to have all input data in memory concurrently.
  data_placeholder <- array(
    file_data$variable$attributes$missing_value,
    dim = c(length(file_data$dimension_data$x),
            length(file_data$dimension_data$y),
            1)
  )
  time_bnds <- get_time_bounds_1(
    file_data$dimension_data$t
  )
  vars_data <- list(result = data_placeholder, time_bounds = time_bnds)

  nc_format <- get_nc_version(nc34)

  cmsaf_info <- switch(
    op,
    paste0("cmsaf::yseasmax for variable ", file_data$variable$name),
    paste0("cmsaf::yseasmin for variable ", file_data$variable$name),
    paste0("cmsaf::yseasmean for variable ", file_data$variable$name),
    paste0("cmsaf::yseassd for variable ", file_data$variable$name)
  )

  ##### prepare output #####
  global_att_list <- names(file_data$global_att)
  global_att_list <- global_att_list[toupper(global_att_list) %in% toupper(GLOBAL_ATT_DEFAULT)]
  global_attributes <- file_data$global_att[global_att_list]

  dims <- define_dims(file_data$grid$is_regular,
                      file_data$dimension_data$x,
                      file_data$dimension_data$y,
                      time_data = 0,
                      NB2,
                      file_data$time_info$units)

  vars <- define_vars(file_data$variable, dims, nc_format$compression)

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
    global_attributes
  )

  ##### calculate and write result #####
  nc_in <- nc_open(infile)
  nc_out <- nc_open(outfile, write = TRUE)
  dummy_vec <- seq_along(months_all)

  count <- 1
  for (season in seasons) {
    mon_dummy <- season
    if (!sum(is.finite(mon_dummy)) >= 3) {
      if (verbose) message("Not enough data to calculate a seasonal maximum!")
      next()
    }
    dum_dat <- array(NA, dim = c(length(file_data$dimension_data$x),
                              length(file_data$dimension_data$y),
                              length(mon_dummy)))
    for (i in seq_along(mon_dummy)) {
      if (!is.na(mon_dummy[i])) {
        dum_dat[, , i] <- ncvar_get(nc_in, file_data$variable$name, start = c(1, 1, mon_dummy[i]), count = c(-1, -1, 1), collapse_degen = FALSE)
      }
    }

    switch(op,
           {
             if (verbose) message(paste0("apply multi-year seasonal maximum ", count, " of 4"))
             data <- do.call(pmax, c(na.rm = TRUE, lapply(seq_len(dim(dum_dat)[3]), function(i) dum_dat[, , i])))
           },
           {
             if (verbose) message(paste0("apply multi-year seasonal minimum ", count, " of 4"))
             data <- do.call(pmin, c(na.rm = TRUE, lapply(seq_len(dim(dum_dat)[3]), function(i) dum_dat[, , i])))
           },
           {
             if (verbose) message(paste0("apply multi-year seasonal mean ", count, " of 4"))
             data <- rowMeans(dum_dat, dims = 2, na.rm = TRUE)
           },
           {
             if (verbose) message(paste0("apply multi-year seasonal standard deviation ", count, " of 4"))
             data <- apply(dum_dat, c(1, 2), stats::sd, na.rm = TRUE)
           }
    )

    data[is.na(data)] <- file_data$variable$attributes$missing_value
    tdum <- min(file_data$dimension_data$t[mon_dummy], na.rm = TRUE)
    time_bnds[1, 1] <- min(file_data$dimension_data$t[mon_dummy], na.rm = TRUE)
    time_bnds[2, 1] <- max(file_data$dimension_data$t[mon_dummy], na.rm = TRUE)
    ncvar_put(nc_out, vars[[1]], data, start = c(1, 1, count), count = c(-1, -1, 1))
    ncvar_put(nc_out, dims$t, tdum, start = count, count = 1)
    ncvar_put(nc_out, vars[[2]], time_bnds, start = c(1, count), count = c(-1, 1))
    count <- count + 1
  }

  nc_close(nc_out)
  nc_close(nc_in)

  calc_time_end <- Sys.time()
  if (verbose) message(get_processing_time_string(calc_time_start, calc_time_end))
}
