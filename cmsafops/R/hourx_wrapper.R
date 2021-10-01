hourx_wrapper <- function(op, var, infile, outfile, nc34, overwrite, verbose, nc = NULL) {
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
  
  file_data$variable$prec <- "float"
  
  
  test_date <- get_time(file_data$time_info$units, file_data$dimension_data$t)
  hours_all <- substr(test_date, 1, 13)
  
  date_time <- get_date_time(file_data$dimension_data$t, file_data$time_info$units)
  times_all <- date_time$times
  
  error_msg <- "The time steps must be available at least every hour. Please try daysum for diurnal aggregation. "
  if(!("" %in% times_all)) {
    
    # Use placeholder for result so that it can be calculated later without the
    # need to have all input data in memory concurrently.
    data_placeholder <- array(
      file_data$variable$attributes$missing_value,
      dim = c(length(file_data$dimension_data$x),
              length(file_data$dimension_data$y),
              length(unique(hours_all)))
    )
  
    time_bnds <- get_time_bounds_hour(file_data$dimension_data$t, hours_all)
  
    vars_data <- list(result = data_placeholder, time_bounds = time_bnds)
  
    nc_format <- get_nc_version(nc34)
  
    cmsaf_info <- switch(
      op,
      paste0("cmsaf::hourmean for variable ", file_data$variable$name),
      paste0("cmsaf::hoursum for variable ", file_data$variable$name),
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
      global_attributes,
      write_result = FALSE   # avoid memory overflow
    )
  
    ##### calculate and write result #####
    nc_out <- nc_open(outfile, write = TRUE)
    if (!is.null(nc)) nc_in <- nc
    else nc_in <- nc_open(infile)
    dummy_vec <- seq_along(hours_all)
  
    count <- 1
    for (j in sort(unique(hours_all))) {
      hour_dummy <- which(hours_all == j)
      
      if (length(hour_dummy) < 1) {
        if (verbose) message(paste0("length of hour ", j, " not sufficient"))
        next()
      }
  
      startt <- min(dummy_vec[hour_dummy])
      countt <- length(hour_dummy)
  
      dum_dat <- ncvar_get(nc_in, file_data$variable$name, start = c(1, 1, startt), count = c(-1, -1, countt), collapse_degen = FALSE)
  
      switch(op,
             {
               if (verbose) message(paste0("apply hourly mean ", count))
               data <- rowMeans(dum_dat, dims = 2, na.rm = TRUE)
             },
             {
               if (verbose) message(paste0("apply hourly sum ", count))
               data <- rowSums(dum_dat, dims = 2, na.rm = TRUE) * ifelse(rowSums(is.na(dum_dat), dims = 2) == dim(dum_dat)[3], NA, 1)
             }
      )
  
      data[is.na(data)] <- file_data$variable$attributes$missing_value
      ncvar_put(nc_out, vars[[1]], data, start = c(1, 1, count), count = c(-1, -1, 1))
      count <- count + 1
    }
  
    if (is.null(nc)) nc_close(nc_in)
    nc_close(nc_out)
  }
  else{
    stop(error_msg)
  }
  calc_time_end <- Sys.time()
  if (verbose) message(get_processing_time_string(calc_time_start, calc_time_end))
}