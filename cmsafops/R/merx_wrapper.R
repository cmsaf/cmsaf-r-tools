merx_wrapper <- function(op, var, infile, outfile, nc34, overwrite, verbose, nc = NULL) {
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
  
  if (op < 3) {
    file_data$variable$prec <- "float"
  }
  
  date_time <- get_date_time(file_data$dimension_data$t, file_data$time_info$units)
  
  # Use placeholder for result so that it can be calculated later without the
  # need to have all input data in memory concurrently.
  data_placeholder <- array(
    file_data$variable$attributes$missing_value,
    dim = c(length(file_data$dimension_data$x),
            1,
            length(file_data$dimension_data$t))
  )

  time_data <- file_data$dimension_data$t
  vars_data <- list(result = data_placeholder, time_bounds = time_data)

  nc_format <- get_nc_version(nc34)

  cmsaf_info <- switch(
    op,
    paste0("cmsaf::mermean for variable ", file_data$variable$name),
  )

  ##### prepare output #####
  global_att_list <- names(file_data$global_att)
  global_att_list <- global_att_list[toupper(global_att_list) %in% toupper(GLOBAL_ATT_DEFAULT)]
  global_attributes <- file_data$global_att[global_att_list]

  dims <- define_dims(file_data$grid$is_regular,
                      file_data$dimension_data$x,
                      0,
                      time_data,
                      NB2,
                      file_data$time_info$units,
                      FALSE)

  vars <- define_vars(file_data$variable, dims, nc_format$compression, with_time_bnds = FALSE)

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
    with_time_bnds = FALSE
  )

  ##### calculate and write result #####
  nc_out <- nc_open(outfile, write = TRUE)
  if (!is.null(nc)) nc_in <- nc
  else nc_in <- nc_open(infile)
  
  # read data from infile
  # dum_dat <- ncvar_get(
  #   nc_in,
  #   file_data$variable$name,
  #   collapse_degen = FALSE
  # )
  
  #nc_close(nc_in)
  
  length_time <- length(file_data$dimension_data$t)

  lapply(1:length_time, function(i){
    
    # read data from infile
    dum_dat <- ncvar_get(
      nc_in,
      file_data$variable$name,
      start = c(1, 1, i), count = c(-1, -1, 1),
      collapse_degen = FALSE
    )
    
    switch(op,
           {
             if (verbose) message(paste0("apply meridional mean ", i," of ", length_time))
             data <- rowMeans(dum_dat, na.rm = TRUE)
           },
    )

    data[is.na(data)] <- file_data$variable$attributes$missing_value
    ncvar_put(nc_out, vars[[1]], data, start = c(1, 1, i), count = c(-1, -1, 1))
  })
  if (is.null(nc)) nc_close(nc_in)
  nc_close(nc_out)
  
  calc_time_end <- Sys.time()
  if (verbose) message(get_processing_time_string(calc_time_start, calc_time_end))
}
  