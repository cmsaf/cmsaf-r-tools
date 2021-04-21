ydayx_wrapper <- function(op, var, infile, outfile, nc34, overwrite, verbose) {
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
  
  date.time <- as.Date(get_time(file_data$time_info$units, file_data$dimension_data$t))
  dateID <- getDateID(date.time)
 
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
    paste0("cmsaf::ydaymax for variable ", file_data$variable$name),
    paste0("cmsaf::ydaymin for variable ", file_data$variable$name),
    paste0("cmsaf::ydaysum for variable ", file_data$variable$name),
    paste0("cmsaf::ydaysd for variable ", file_data$variable$name),
    paste0("cmsaf::ydayrange for variable ", file_data$variable$name),
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
  )
  
  ##### calculate and write result #####
  nc_out <- nc_open(outfile, write = TRUE)
  nc_in <- nc_open(infile)
  
  for (j in seq_along(unique(dateID))) {
    day_dummy <- which(dateID == sort(unique(dateID))[j])
    
    startt <- day_dummy
    dum_dat <- array(
      NA,
      dim = c(length(file_data$dimension_data$x),
              length(file_data$dimension_data$y),
              length(startt))
    )
    
    for (i in seq_along(startt)) {
      dum_dat[, , i] <- ncvar_get(
        nc_in,
        file_data$variable$name,
        start = c(1, 1, startt[i]),
        count = c(-1, -1, 1),
        collapse_degen = FALSE
      )
    }
    
    switch(op,
           {
             if (verbose) message(paste0("apply multi-year daily maximum ", j))
             data <- do.call(pmax, c(na.rm = TRUE, lapply(seq_len(dim(dum_dat)[3]), function(i) dum_dat[, , i])))
           },
           {
             if (verbose) message(paste0("apply multi-year daily minimum ", j))
             data <- do.call(pmin, c(na.rm = TRUE, lapply(seq_len(dim(dum_dat)[3]), function(i) dum_dat[, , i])))
           },
           {
             if (verbose) message(paste0("apply multi-year daily sum ", j))
             data <- rowSums(dum_dat, dims = 2, na.rm = TRUE) * ifelse(rowSums(is.na(dum_dat), dims = 2) == dim(dum_dat)[3], NA, 1)
           },
           {
             if (verbose) message(paste0("apply multi-year daily standard deviation ", j))
             data <- apply(dum_dat, c(1, 2), stats::sd, na.rm = TRUE)
           },
           {
             if (verbose) message(paste0("apply multi-year daily range ", j))
             data.max <- do.call(pmax, c(na.rm = TRUE, lapply(seq_len(dim(dum_dat)[3]), function(i) dum_dat[, , i])))
             data.min <- do.call(pmin, c(na.rm = TRUE, lapply(seq_len(dim(dum_dat)[3]), function(i) dum_dat[, , i])))
             data <- data.max - data.min
           }
    )
    
    data[is.na(data)] <- file_data$variable$attributes$missing_value
    ncvar_put(nc_out, vars[[1]], data, start = c(1, 1, j), count = c(-1, -1, 1))
  }
  
  nc_close(nc_in)
  nc_close(nc_out)
  
  calc_time_end <- Sys.time()
  if (verbose) message(get_processing_time_string(calc_time_start, calc_time_end))
}
