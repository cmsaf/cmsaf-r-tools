dayx_wrapper <- function(op, var, infile, outfile, nc34, overwrite, p = NULL, verbose) {
  calc_time_start <- Sys.time()
  gc()
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
  
  if (op == 7) {
    if (length(p) > 1) {
      p <- p[1]
    }
    if (p < 0 || p > 1) {
      if (verbose) message("Your given p-value is outside [0,1]. The default will be used (0.95).")
      p <- 0.95
    }
  }

  dateID <- as.Date(get_time(file_data$time_info$units, file_data$dimension_data$t))
  
  # Use placeholder for result so that it can be calculated later without the
  # need to have all input data in memory concurrently.
  data_placeholder <- array(
    file_data$variable$attributes$missing_value,
    dim = c(length(file_data$dimension_data$x),
            length(file_data$dimension_data$y),
            length(unique(dateID)))
  )
  
  time_bnds <- get_time_bounds_doy(file_data$dimension_data$t, dateID)
  
  vars_data <- list(result = data_placeholder, time_bounds = time_bnds)
  
  nc_format <- get_nc_version(nc34)
  
  cmsaf_info <- switch(
    op,
    paste0("cmsaf::daymax for variable ", file_data$variable$name),
    paste0("cmsaf::daymin for variable ", file_data$variable$name),
    paste0("cmsaf::daymean for variable ", file_data$variable$name),
    paste0("cmsaf::daysum for variable ", file_data$variable$name),
    paste0("cmsaf::daysd for variable ", file_data$variable$name),
    paste0("cmsaf::dayvar for variable ", file_data$variable$name),
    paste0("cmsaf::daypctl with p = ", p, " for variable ", file_data$variable$name),
    paste0("cmsaf::dayavg for variable ", file_data$variable$name),
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
  dummy_vec <- seq_along(dateID)
  
  nc_in <- nc_open(infile)
  file_data_missing_value <- file_data$variable$attributes$missing_value
  count <- 1
  for (j in sort(unique(dateID))) {
    day_dummy <- which(dateID == j)

    if (length(day_dummy) < 1) {
      if (verbose) message(paste0("length of day ", j, " not sufficient"))
      next()
    }
  
    startt <- min(dummy_vec[day_dummy])
    countt <- length(day_dummy)
  
    dum_dat <- ncvar_get(nc_in, file_data$variable$name, start = c(1, 1, startt), count = c(-1, -1, countt), collapse_degen = FALSE)
  
    switch(op,
           {
             if (verbose) message(paste0("apply daily maximum ", count))
             data <- do.call(pmax, c(na.rm = TRUE, lapply(seq_len(dim(dum_dat)[3]), function(i) dum_dat[, , i])))
           },
           {
             if (verbose) message(paste0("apply daily minimum ", count))
             data <- do.call(pmin, c(na.rm = TRUE, lapply(seq_len(dim(dum_dat)[3]), function(i) dum_dat[, , i])))
           },
           {
             if (verbose) message(paste0("apply daily mean ", count))
             data <- rowMeans(dum_dat, dims = 2, na.rm = TRUE)
           },
           {
             if (verbose) message(paste0("apply daily sum ", count))
             data <- rowSums(dum_dat, dims = 2, na.rm = TRUE) * ifelse(rowSums(is.na(dum_dat), dims = 2) == dim(dum_dat)[3], NA, 1)
           },
           {
             if (verbose) message(paste0("apply daily standard deviation ", count))
             data <- apply(dum_dat, c(1, 2), stats::sd, na.rm = TRUE)
           },
           {
             if (verbose) message(paste0("apply daily variance ", count))
             data <- apply(dum_dat, c(1, 2), stats::var, na.rm = TRUE)
           },
           {
             if (verbose) message(paste0("apply daily percentile ", count))
             data = apply(dum_dat, c(1, 2), stats::quantile, probs = p, names = FALSE, na.rm = TRUE)
           },
           {
             if (verbose) message(paste0("apply daily average ", count))
             data <- rowMeans(dum_dat, dims = 2, na.rm = FALSE)
           }
    )
  
    data[is.na(data)] <- file_data_missing_value
    ncvar_put(nc_out, vars[[1]], data, start = c(1, 1, count), count = c(-1, -1, 1))
    count <- count + 1
  }
  nc_close(nc_in)
  nc_close(nc_out)
  
  calc_time_end <- Sys.time()
  if (verbose) message(get_processing_time_string(calc_time_start, calc_time_end))
}
