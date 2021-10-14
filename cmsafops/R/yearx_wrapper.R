yearx_wrapper <- function(op, var, infile, outfile, nc34, overwrite, verbose, nc = NULL) {
  calc_time_start <- Sys.time()
  gc()
  check_variable(var)
  if (is.null(nc)) check_infile(infile)
  check_outfile(outfile)
  outfile <- correct_filename(outfile)
  check_overwrite(outfile, overwrite)
  check_nc_version(nc34)

  ##### extract data from file #####
  file_data <- read_file(infile, var, nc = nc)

  if (op > 2) {
    file_data$variable$prec <- "float"
  }

  date_time <- get_date_time(file_data$dimension_data$t, file_data$time_info$units)
  years_all <- date_time$years
  years_unique <- sort(unique(years_all))

  # Use placeholder for result so that it can be calculated later without the
  # need to have all input data in memory concurrently.
  data_placeholder <- array(
    file_data$variable$attributes$missing_value,
    dim = c(length(file_data$dimension_data$x),
            length(file_data$dimension_data$y),
            length(years_unique))
  )

  time_bnds <- get_time_bounds_year(file_data$dimension_data$t, years_all, years_unique)

  vars_data <- list(result = data_placeholder, time_bounds = time_bnds)
  nc_format <- get_nc_version(nc34)

  cmsaf_info <- switch(
    op,
    paste0("cmsaf::yearmax for variable ", file_data$variable$name),
    paste0("cmsaf::yearmin for variable ", file_data$variable$name),
    paste0("cmsaf::yearsd for variable ", file_data$variable$name),
    paste0("cmsaf::yearvar for variable ", file_data$variable$name),
    paste0("cmsaf::yearrange for variable ", file_data$variable$name),
  )

  time_data <- time_bnds[1, ]

  ##### prepare output #####
  global_att_list <- names(file_data$global_att)
  global_att_list <- global_att_list[toupper(global_att_list) %in% toupper(GLOBAL_ATT_DEFAULT)]
  global_attributes <- file_data$global_att[global_att_list]

  dims <- define_dims(file_data$grid$is_regular,
                      file_data$dimension_data$x,
                      file_data$dimension_data$y,
                      time_data,
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
  nc_out <- nc_open(outfile, write = TRUE)
  dummy_vec <- seq_along(years_all)
  
  if (!is.null(nc)) nc_in <- nc
  else nc_in <- nc_open(infile)

  for (i in seq_along(years_unique)) {
    year_dummy <- which(years_all == years_unique[i])

    startt <- min(dummy_vec[year_dummy])
    countt <- length(year_dummy)

    # read data from infile
    dum_dat <- ncvar_get(
      nc_in,
      file_data$variable$name,
      start = c(1, 1, startt),
      count = c(-1, -1, countt),
      collapse_degen = FALSE
    )

    switch(op,
           {
              if (verbose) message(paste0("apply annual maximum ", i," of ", length(years_unique)))
              data <- do.call(pmax, c(na.rm = TRUE, lapply(seq_len(dim(dum_dat)[3]), function(i) dum_dat[, , i])))
           },
           {
              if (verbose) message(paste0("apply annual minimum ", i," of ", length(years_unique)))
              data <- do.call(pmin, c(na.rm = TRUE, lapply(seq_len(dim(dum_dat)[3]), function(i) dum_dat[, , i])))
           },
           {
              if (verbose) message(paste0("apply annual standard deviation ", i," of ", length(years_unique)))
              data <- apply(dum_dat, c(1, 2), stats::sd, na.rm = TRUE)
           },
           {
              if (verbose) message(paste0("apply annual variance ", i," of ", length(years_unique)))
              data <- apply(dum_dat, c(1, 2), stats::var, na.rm = TRUE)
           },
           {
              if (verbose) message(paste0("apply annual range ", i," of ", length(years_unique)))
              data.max <- do.call(pmax, c(na.rm = TRUE, lapply(seq_len(dim(dum_dat)[3]), function(i) dum_dat[, , i])))
              data.min <- do.call(pmin, c(na.rm = TRUE, lapply(seq_len(dim(dum_dat)[3]), function(i) dum_dat[, , i])))
              data <- data.max - data.min
           }
    )

    data[is.na(data)] <- file_data$variable$attributes$missing_value
    ncvar_put(nc_out, vars[[1]], data, start = c(1, 1, i), count = c(-1, -1, 1))
  }

  if (is.null(nc)) nc_close(nc_in)
  nc_close(nc_out)
  
  calc_time_end <- Sys.time()
  if (verbose) message(get_processing_time_string(calc_time_start, calc_time_end))
}