timselx_wrapper <- function(op, var, nts, infile, outfile, nc34, overwrite, verbose) {
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
  if(nts <= length(file_data$dimension_data$t))
  {
    file_data$variable$prec <- "float"
  
    length.dimension.x <- length(file_data$dimension_data$x)
    length.dimension.y <- length(file_data$dimension_data$y)
    length.dimension.t <- length(file_data$dimension_data$t)
  
    time_bnds <- get_time_bounds_timrange(file_data$dimension_data$t, nts)
  
    vars_data <- list(time_bounds = time_bnds)
    nc_format <- get_nc_version(nc34)
  
    cmsaf_info <- switch(
      op,
      paste0("cmsaf::timselsum for variable ", file_data$variable$name),
      paste0("cmsaf::timselmean for variable ", file_data$variable$name),
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
      write_result = FALSE # avoid vector memory exhaustion, Can be adjust
    )
  
    ##### calculate and write result #####
    nc_out <- nc_open(outfile, write = TRUE)
    nc_in <- nc_open(infile)
  
    startt <- 1
    countt <- nts
    
    # length of running time steps
    tmp_result <- length.dimension.t%%nts
    
    if(tmp_result != 0){
      tmp_result2 <- (nts - tmp_result)+length.dimension.t
      length_time <- tmp_result2/nts
    }
    else{
      length_time <- length.dimension.t/nts
    }
    
    for (i in 1:length_time) {
      if((startt+countt-1) > length.dimension.t){
        countt <- length.dimension.t-(startt-1)
      }
      
      # read data from infile
      dum_dat <- ncvar_get(
        nc_in,
        file_data$variable$name,
        start = c(1, 1, startt),
        count = c(-1, -1, countt),
        collapse_degen = FALSE
      )
      startt <- startt + countt
  
      switch(op,
             {
               if (verbose) message(paste0("apply time selection sum ", i," of ", length_time))
               data <- rowSums(dum_dat, dims = 2, na.rm = TRUE) * ifelse(rowSums(is.na(dum_dat), dims = 2) == dim(dum_dat)[3], NA, 1)
             },
             {
               if (verbose) message(paste0("apply time selection mean ", i," of ", length_time))
               data <- rowMeans(dum_dat, dims = 2, na.rm = TRUE)
             }
      )
  
      data[is.na(data)] <- file_data$variable$attributes$missing_value
      ncvar_put(nc_out, vars[[1]], data, start = c(1, 1, i), count = c(-1, -1, 1))
    }
    
    nc_close(nc_in)
    nc_close(nc_out)
  }
  else{
    stop("The input of the time steps must not be greater than the number of time steps from the input file")
  }
  calc_time_end <- Sys.time()
  if (verbose) message(get_processing_time_string(calc_time_start, calc_time_end))
}
