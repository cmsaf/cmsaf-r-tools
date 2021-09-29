monx_wrapper <- function(op, var, infile, outfile, nc34, overwrite, verbose, p = NULL,
                         nc = NULL) {
  calc_time_start <- Sys.time()

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
  
  if (op == 7) {
    if (length(p) > 1) {
      p <- p[1]
    }
    if (p < 0 || p > 1) {
      if (verbose) message("Your given p-value is outside [0,1]. The default will be used (0.95).")
      p <- 0.95
    }
  }

  date_time <- get_date_time(file_data$dimension_data$t, file_data$time_info$units)
  months_all <- date_time$months
  months_unique <- sort(unique(months_all))
  years_all <- date_time$years
  years_unique <- sort(unique(years_all))

  nmonmeans <- length(years_unique) * length(months_unique)
  mul <- months_all * years_all

  testnum <- mul
  test_count <- 0
  test <- -999

  for (i in seq_along(mul)) {
    if (sum(testnum == mul[i]) >= 1) {
      test_count <- test_count + 1
      test <- cbind(test, mul[i])
      testnum[testnum == mul[i]] <- -999
    }
  }

  # Use placeholder for result so that it can be calculated later without the
  # need to have all input data in memory concurrently.
  data_placeholder <- array(
    file_data$variable$attributes$missing_value,
    dim = c(length(file_data$dimension_data$x),
            length(file_data$dimension_data$y),
            test_count)
  )
  time_bnds <- get_time_bounds_mul(
    file_data$dimension_data$t, test, test_count, mul
  )
  vars_data <- list(result = data_placeholder, time_bounds = time_bnds)

  nc_format <- get_nc_version(nc34)

  cmsaf_info <- switch(
    op,
    paste0("cmsaf::monmax for variable ", file_data$variable$name),
    paste0("cmsaf::monmin for variable ", file_data$variable$name),
    paste0("cmsaf::monmean for variable ", file_data$variable$name),
    paste0("cmsaf::monsum for variable ", file_data$variable$name),
    paste0("cmsaf::monsd for variable ", file_data$variable$name),
    paste0("cmsaf::monvar for variable ", file_data$variable$name),
    paste0("cmsaf::monpctl with p = ", p, " for variable ", file_data$variable$name),
    paste0("cmsaf::monavg for variable ", file_data$variable$name),
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
  count <- 1
  for (j in seq_len(test_count)) {
    mon_dummy <- which(mul == test[j + 1])

    if (length(mon_dummy) < 1) {
      if (verbose) message(paste0("length of month ", j, " not sufficient"))
      next()
    }

    startt <- min(dummy_vec[mon_dummy])
    countt <- length(mon_dummy)

    if (!is.null(nc)) nc_in <- nc
    else nc_in <- nc_open(infile)
    dum_dat <- ncvar_get(nc_in, file_data$variable$name, start = c(1, 1, startt), count = c(-1, -1, countt), collapse_degen = FALSE)

    switch(op,
           {
             if (verbose) message(paste0("apply monthly maximum ", count))
             data <- do.call(pmax, c(na.rm = TRUE, lapply(seq_len(dim(dum_dat)[3]), function(i) dum_dat[, , i])))
           },
           {
             if (verbose) message(paste0("apply monthly minimum ", count))
             data <- do.call(pmin, c(na.rm = TRUE, lapply(seq_len(dim(dum_dat)[3]), function(i) dum_dat[, , i])))
           },
           {
             if (verbose) message(paste0("apply monthly mean ", count))
             data <- rowMeans(dum_dat, dims = 2, na.rm = TRUE)
           },
           {
             if (verbose) message(paste0("apply monthly sum ", count))
             data <- rowSums(dum_dat, dims = 2, na.rm = TRUE) * ifelse(rowSums(is.na(dum_dat), dims = 2) == dim(dum_dat)[3], NA, 1)
           },
           {
             if (verbose) message(paste0("apply monthly standard deviation ", count))
             data <- apply(dum_dat, c(1, 2), stats::sd, na.rm = TRUE)
           },
           {
             if (verbose) message(paste0("apply monthly variance ", count))
             data <- apply(dum_dat, c(1, 2), stats::var, na.rm = TRUE)
           },
           {
             if (verbose) message(paste0("apply monthly percentile ", count))
             data = apply(dum_dat, c(1, 2), stats::quantile, probs = p, names = FALSE, na.rm = TRUE)
           },
           {
             if (verbose) message(paste0("apply monthly average ", count))
             data <- rowMeans(dum_dat, dims = 2, na.rm = FALSE)
           }
    )

    data[is.na(data)] <- file_data$variable$attributes$missing_value
    ncvar_put(nc_out, vars[[1]], data, start = c(1, 1, count), count = c(-1, -1, 1))
    count <- count + 1

    if (is.null(nc)) nc_close(nc_in)
  }

  nc_close(nc_out)

  calc_time_end <- Sys.time()
  if (verbose) message(get_processing_time_string(calc_time_start, calc_time_end))
}
