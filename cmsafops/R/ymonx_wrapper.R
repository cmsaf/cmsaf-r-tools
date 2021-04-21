ymonx_wrapper <- function(op, var, infile, outfile, nc34, overwrite, verbose) {
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
  months_all <- get_date_time(file_data$dimension_data$t, file_data$time_info$units)$months
  months_unique <- sort(unique(months_all))

  # Use placeholder for result so that it can be calculated later without the
  # need to have all input data in memory concurrently.
  data_placeholder <- array(
    file_data$variable$attributes$missing_value,
    dim = c(length(file_data$dimension_data$x),
            length(file_data$dimension_data$y),
            length(months_unique))
  )
  time_bnds <- get_time_bounds(
    file_data$dimension_data$t, months_all, months_unique
  )
  vars_data <- list(result = data_placeholder, time_bounds = time_bnds)

  nc_format <- get_nc_version(nc34)

  cmsaf_info <- switch(
    op,
    paste0("cmsaf::ymonmax for variable ", file_data$variable$name),
    paste0("cmsaf::ymonmin for variable ", file_data$variable$name),
    paste0("cmsaf::ymonmean for variable ", file_data$variable$name),
    paste0("cmsaf::ymonsum for variable ", file_data$variable$name),
    paste0("cmsaf::ymonsd for variable ", file_data$variable$name)
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
  dummy_vec <- seq_along(months_all)

  for (j in seq_along(months_unique)) {
    mon_dummy <- which(months_all == months_unique[j])
    startt <- dummy_vec[mon_dummy]
    dum_dat <- array(
      NA,
      dim = c(length(file_data$dimension_data$x),
              length(file_data$dimension_data$y),
              length(startt))
    )
    nc_in <- nc_open(infile)
    for (i in seq_along(startt)) {
      dum_dat[, , i] <- ncvar_get(
        nc_in,
        file_data$variable$name,
        start = c(1, 1, startt[i]),
        count = c(-1, -1, 1),
        collapse_degen = FALSE
      )
    }
    nc_close(nc_in)

    switch(op,
           {
             if (verbose) message(paste0("apply multi-year monthly maximum ", j))
             data <- do.call(pmax, c(na.rm = TRUE, lapply(seq_len(dim(dum_dat)[3]), function(i) dum_dat[, , i])))
           },
           {
             if (verbose) message(paste0("apply multi-year monthly minimum ", j))
             data <- do.call(pmin, c(na.rm = TRUE, lapply(seq_len(dim(dum_dat)[3]), function(i) dum_dat[, , i])))
           },
           {
             if (verbose) message(paste0("apply multi-year monthly mean ", j))
             data <- rowMeans(dum_dat, dims = 2, na.rm = TRUE)
           },
           {
             if (verbose) message(paste0("apply multi-year monthly sum ", j))
             data <- rowSums(dum_dat, dims = 2, na.rm = TRUE)
           },
           {
             if (verbose) message(paste0("apply multi-year monthly standard deviation ", j))
             data <- apply(dum_dat, c(1, 2), stats::sd, na.rm = TRUE)
           }
    )

    data[is.na(data)] <- file_data$variable$attributes$missing_value
    ncvar_put(nc_out, vars[[1]], data, start = c(1, 1, j), count = c(-1, -1, 1))
  }

  nc_close(nc_out)

  calc_time_end <- Sys.time()
  if (verbose) message(get_processing_time_string(calc_time_start, calc_time_end))
}
